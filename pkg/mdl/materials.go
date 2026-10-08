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

// findMaterial returns <name>.mtr from the first location that has it, or nil.
// Directories are indexed recursively once and cached for the life of the
// process, so a batch compile does not rescan them per model; archives and game
// installs are indexed by resources.go.
func findMaterial(name string, locs []string) *resourceRef {
	name = strings.ToLower(strings.TrimSpace(name))
	if name == "" || name == "null" {
		return nil
	}
	for _, loc := range locs {
		if a := openArchive(loc); a != nil {
			if read, ok := a.entries[archiveKey{name, resTypeMTR}]; ok {
				return &resourceRef{id: loc + "#" + name + ".mtr", read: read}
			}
			continue
		}
		v, _ := materialIndexes.LoadOrStore(loc, &materialIndex{dir: loc})
		if p, ok := v.(*materialIndex).load()[name]; ok {
			return &resourceRef{id: p, read: func() ([]byte, error) { return os.ReadFile(p) }}
		}
	}
	return nil
}

func materialRenderHint(ref *resourceRef) string {
	if v, ok := materialHints.Load(ref.id); ok {
		return v.(string)
	}
	hint := ""
	if data, err := ref.read(); err == nil {
		if m := mtrHintRe.FindSubmatch(data); m != nil {
			hint = strings.ToLower(string(m[1]))
		}
	}
	materialHints.Store(ref.id, hint)
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
		if ref := findMaterial(name, dirs); ref != nil {
			return materialRenderHint(ref) == "normalandspecmapped"
		}
	}
	return false
}
