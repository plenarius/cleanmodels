package mdl

import (
	"fmt"
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
	if !materialLookedUp(name) {
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

// materialLookedUp reports whether findMaterial searches for name at all: an
// empty name or NULL is no material.
func materialLookedUp(name string) bool {
	name = strings.ToLower(strings.TrimSpace(name))
	return name != "" && name != "null"
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

// isNormalMapped reports whether the mesh's effective render hint is one the
// game generates tangents for (NormalAndSpecMapped or NormalTangents): its own
// hint, or its material's when dirs can find one. Each material looked up is
// passed to found, if set.
func isNormalMapped(mesh *MeshData, dirs []string, found func(name string, ref *resourceRef)) bool {
	if mesh == nil {
		return false
	}
	if hintWantsTangents(mesh.RenderHint) {
		return true
	}
	if len(dirs) == 0 {
		return false
	}
	for _, name := range []string{mesh.MaterialName, mesh.Bitmap} {
		ref := findMaterial(name, dirs)
		if found != nil && materialLookedUp(name) {
			found(name, ref)
		}
		if ref != nil {
			return hintWantsTangents(materialRenderHint(ref))
		}
	}
	return false
}

// meshNormalMapped is isNormalMapped for a mesh being compiled: it records each
// material looked up, and warns when the mesh's materialname names a material
// that none of the resource locations has. A missing .mtr is otherwise silent,
// and it decides tangents: with it, a normal-mapped material gets them; without
// it, the mesh gets none. A bitmap with no .mtr is not warned about: most
// textures have none, and that cannot be told from a missing one. With no
// resource locations at all nothing is looked up, so nothing is warned about
// (as for a supermodel).
func (c *compiler) meshNormalMapped(mesh *MeshData, n *Node) bool {
	if mesh == nil {
		return false
	}
	named := materialLookedUp(mesh.MaterialName)
	var mat, bitmapMat *resourceRef
	calls := 0
	normalMapped := isNormalMapped(mesh, c.resourceDirs, func(name string, ref *resourceRef) {
		c.recordMaterial(name, ref)
		calls++
		if named && calls == 1 {
			mat = ref
		} else {
			bitmapMat = ref
		}
	})
	if !named || len(c.resourceDirs) == 0 {
		return normalMapped
	}
	if calls == 0 {
		// The mesh's own render hint settled it, so isNormalMapped did not
		// look the material up; the warning still needs to know.
		mat = findMaterial(mesh.MaterialName, c.resourceDirs)
		c.recordMaterial(mesh.MaterialName, mat)
	}
	if mat == nil && c.warn != nil && n != nil {
		c.warn(missingMaterialWarning(n.Name, mesh, bitmapMat != nil, c.resourceDirs))
	}
	return normalMapped
}

// missingMaterialWarning is the warning for a mesh whose materialname is in
// none of dirs. viaBitmap is whether its bitmap's .mtr was found instead.
func missingMaterialWarning(node string, mesh *MeshData, viaBitmap bool, dirs []string) string {
	var consequence string
	switch {
	case hintWantsTangents(mesh.RenderHint):
		consequence = "its own renderhint still gives it tangents"
	case viaBitmap:
		consequence = fmt.Sprintf("its render hint is taken from %s.mtr, named by its bitmap, instead", strings.TrimSpace(mesh.Bitmap))
	default:
		consequence = "it gets no tangents, which the game generates when that material is normal-mapped"
	}
	return fmt.Sprintf("mesh %q: material %s.mtr not found in %s; %s",
		node, strings.TrimSpace(mesh.MaterialName), strings.Join(dirs, ", "), consequence)
}
