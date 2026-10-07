package mdl

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"os"
	"path/filepath"
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
