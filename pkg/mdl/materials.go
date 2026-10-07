package mdl

import (
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"sync"
)

// The game compiler writes tangents for a mesh whose effective render hint is
// NormalAndSpecMapped. The hint is the mesh's own "renderhint" line, or else the
// one in its material: <materialname>.mtr, or <texture0>.mtr when there is no
// material name. Checked against the engine's output for 463 meshes (66 with
// tangents, 391 without) it predicted every one, once the materials were in
// reach. Meshes that rely on a texture-named .mtr — most Enhanced Edition PBR
// tile content — therefore need the materials on disk to get tangents baked.

var mtrHintRe = regexp.MustCompile(`(?im)^\s*renderhint\s+(\S+)`)

type materialIndex struct {
	once   sync.Once
	dir    string
	byName map[string]string // lower-case material name -> path
}

var (
	materialIndexes sync.Map // dir -> *materialIndex
	materialHints   sync.Map // path -> lower-case renderhint ("" if none)
)

func (m *materialIndex) load() map[string]string {
	m.once.Do(func() {
		m.byName = make(map[string]string)
		filepath.WalkDir(m.dir, func(p string, d fs.DirEntry, err error) error {
			if err != nil || d.IsDir() {
				return nil
			}
			if name := d.Name(); len(name) > 4 && strings.EqualFold(name[len(name)-4:], ".mtr") {
				key := strings.ToLower(name[:len(name)-4])
				if _, dup := m.byName[key]; !dup {
					m.byName[key] = p
				}
			}
			return nil
		})
	})
	return m.byName
}

// findMaterial returns the path of <name>.mtr in the first directory that has
// it, or "". Directories are indexed recursively once and cached for the life
// of the process, so a batch compile does not rescan them per model.
func findMaterial(name string, dirs []string) string {
	name = strings.ToLower(strings.TrimSpace(name))
	if name == "" || name == "null" {
		return ""
	}
	for _, dir := range dirs {
		v, _ := materialIndexes.LoadOrStore(dir, &materialIndex{dir: dir})
		if p, ok := v.(*materialIndex).load()[name]; ok {
			return p
		}
	}
	return ""
}

func materialRenderHint(path string) string {
	if v, ok := materialHints.Load(path); ok {
		return v.(string)
	}
	hint := ""
	if data, err := os.ReadFile(path); err == nil {
		if m := mtrHintRe.FindSubmatch(data); m != nil {
			hint = strings.ToLower(string(m[1]))
		}
	}
	materialHints.Store(path, hint)
	return hint
}

// isNormalMapped reports whether the mesh's effective render hint is
// NormalAndSpecMapped: its own hint, or its material's when dirs can find one.
func isNormalMapped(mesh *MeshData, dirs []string) bool {
	if mesh == nil {
		return false
	}
	if mesh.RenderHint == "NormalAndSpecMapped" {
		return true
	}
	if len(dirs) == 0 {
		return false
	}
	for _, name := range []string{mesh.MaterialName, mesh.Bitmap} {
		if p := findMaterial(name, dirs); p != "" {
			return materialRenderHint(p) == "normalandspecmapped"
		}
	}
	return false
}
