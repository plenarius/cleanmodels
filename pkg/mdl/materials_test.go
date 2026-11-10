package mdl

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// materialQuad is a textured quad with normals and UVs, so tangents can be made,
// and no renderhint of its own.
const materialQuad = `newmodel mq
setsupermodel mq NULL
classification TILE
setanimationscale 1.0
beginmodelgeom mq
  node dummy mq
    parent NULL
  endnode
  node trimesh quad
    parent mq
    bitmap %s
%s    verts 4
      0 0 0
      1 0 0
      1 1 0
      0 1 0
    tverts 4
      0 0 0
      1 0 0
      1 1 0
      0 1 0
    faces 2
      0 1 2 1 0 1 2 0
      0 2 3 1 0 2 3 0
  endnode
endmodelgeom
donemodel mq
`

func hasTangents(t *testing.T, bin []byte) bool {
	t.Helper()
	u32 := func(o int) uint32 { return binary.LittleEndian.Uint32(bin[o:]) }
	root := 12 + int(u32(12+72))
	for i := 0; i < int(u32(root+76)); i++ {
		n := 12 + int(u32(12+int(u32(root+72))+4*i))
		if u32(n+108)&0x20 != 0 {
			return u32(n+112+488) != 0xFFFFFFFF
		}
	}
	t.Fatal("no mesh node found")
	return false
}

func compileQuad(t *testing.T, bitmap, extra string, dirs []string) []byte {
	t.Helper()
	m := mustParseASCII(t, fmt.Sprintf(materialQuad, bitmap, extra))
	var buf bytes.Buffer
	if err := CompileWithOptions(m, &buf, CompileOptions{ResourceDirs: dirs}); err != nil {
		t.Fatal(err)
	}
	return buf.Bytes()
}

// TestTangentsFollowTheMaterial pins the game compiler's rule: a mesh gets
// tangents when its own renderhint, or the renderhint of its material, is
// NormalAndSpecMapped. The material is <materialname>.mtr, or <texture0>.mtr
// when there is no material name. Meshes the engine gave tangents to with no
// hint of their own (20 of 23 in the tdc01 tile model) all had such a material.
func TestTangentsFollowTheMaterial(t *testing.T) {
	dir := t.TempDir()
	write := func(name, hint string) {
		body := "renderhint " + hint + "\ntexture0 x\n"
		if err := os.WriteFile(filepath.Join(dir, name), []byte(body), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	write("walltex.mtr", "NormalAndSpecMapped")
	write("plaintex.mtr", "none")
	write("MATFILE.MTR", "NormalAndSpecMapped") // case differs on purpose
	dirs := []string{dir}

	cases := []struct {
		name, bitmap, extra string
		dirs                []string
		want                bool
	}{
		{"texture-named material, normal mapped", "walltex", "", dirs, true},
		{"texture-named material, not normal mapped", "plaintex", "", dirs, false},
		{"no material for the texture", "bare", "", dirs, false},
		{"materialname wins over the texture", "plaintex", "    materialname matfile\n", dirs, true},
		{"unknown materialname falls back to the texture", "walltex", "    materialname nosuch\n", dirs, true},
		{"material exists but no resource dirs", "walltex", "", nil, false},
		{"the mesh's own renderhint still works", "bare", "    renderhint NormalAndSpecMapped\n", nil, true},
	}
	for _, c := range cases {
		if got := hasTangents(t, compileQuad(t, c.bitmap, c.extra, c.dirs)); got != c.want {
			t.Errorf("%s: tangents = %v, want %v", c.name, got, c.want)
		}
	}
}

// TestMissingMaterialWarns: a materialname that none of the resource
// locations has is warned about, since it silently decides tangents. A bitmap
// with no .mtr is the usual case for a texture and is not, nor is anything
// when there are no resource locations to look in.
func TestMissingMaterialWarns(t *testing.T) {
	dir := t.TempDir()
	writeFiles(t, dir, map[string]string{
		"walltex.mtr": "renderhint NormalAndSpecMapped\n",
		"matfile.mtr": "renderhint NormalAndSpecMapped\n",
	})
	dirs := []string{dir}
	prefix := fmt.Sprintf(`mesh "quad": material nosuch.mtr not found in %s; `, dir)
	cases := []struct {
		name, bitmap, extra string
		dirs                []string
		want                string // "" for no warning
	}{
		{"missing, nothing else", "bare", "    materialname nosuch\n", dirs,
			prefix + "it gets no tangents, which the game generates when that material is normal-mapped"},
		{"missing, the bitmap's material used", "walltex", "    materialname nosuch\n", dirs,
			prefix + "its render hint is taken from walltex.mtr, named by its bitmap, instead"},
		{"missing, own renderhint", "bare", "    materialname nosuch\n    renderhint NormalAndSpecMapped\n", dirs,
			prefix + "its own renderhint still gives it tangents"},
		{"found", "bare", "    materialname MATFILE\n", dirs, ""},
		{"found, own renderhint", "bare", "    materialname matfile\n    renderhint NormalAndSpecMapped\n", dirs, ""},
		{"NULL", "bare", "    materialname NULL\n", dirs, ""},
		{"bitmap with no material", "bare", "", dirs, ""},
		{"no resource locations", "bare", "    materialname nosuch\n", nil, ""},
	}
	for _, c := range cases {
		var warns []string
		m := mustParseASCII(t, fmt.Sprintf(materialQuad, c.bitmap, c.extra))
		var buf bytes.Buffer
		opts := CompileOptions{ResourceDirs: c.dirs, Warn: func(s string) { warns = append(warns, s) }}
		if err := CompileWithOptions(m, &buf, opts); err != nil {
			t.Fatal(err)
		}
		var got []string
		for _, w := range warns {
			if strings.Contains(w, ".mtr") {
				got = append(got, w)
			}
		}
		switch {
		case c.want == "" && len(got) != 0:
			t.Errorf("%s: warned %q, want no warning", c.name, got)
		case c.want != "" && (len(got) != 1 || got[0] != c.want):
			t.Errorf("%s: warned %q, want %q", c.name, got, c.want)
		}
	}
}

// TestMissingMaterialIsADependency: with the mesh's own renderhint, the
// material does not decide tangents, but its warning depends on it, so the
// lookup is recorded.
func TestMissingMaterialIsADependency(t *testing.T) {
	dir := t.TempDir()
	m := mustParseASCII(t, fmt.Sprintf(materialQuad, "bare", "    materialname nosuch\n    renderhint NormalAndSpecMapped\n"))
	opts := CompileOptions{ResourceDirs: []string{dir}}
	deps := compileDeps(t, m, opts)
	if d, ok := findDep(deps, "mtr", "nosuch"); !ok || d.Found {
		t.Errorf("nosuch: %+v (present %v), want recorded as not found", d, ok)
	}
}
