package mdl

import (
	"bytes"
	"testing"
)

const shadowSrc = `newmodel shd
setsupermodel shd NULL
classification CHARACTER
setanimationscale 1.0
beginmodelgeom shd
  node dummy shd
    parent NULL
  endnode
  node trimesh tri_default
    parent shd
  endnode
  node trimesh tri_off
    parent shd
    shadow 0
  endnode
  node skin skin_default
    parent shd
  endnode
  node danglymesh dangly_default
    parent shd
  endnode
  node aabb walkmesh
    parent shd
  endnode
endmodelgeom
donemodel shd
`

// TestShadowDefaultsMatchEngine: with no "shadow" line the game compiler wrote
// shadow 1 on every trimesh, skin and danglymesh (180 of 180 compared) and 0 on
// every walkmesh (3 of 3). An explicit value is kept. A source with no line used
// to compile to shadow 0, so the model cast no shadow in game.
func TestShadowDefaultsMatchEngine(t *testing.T) {
	m := mustParseASCII(t, shadowSrc)
	want := map[string]int32{"tri_default": 1, "tri_off": 0, "skin_default": 1, "dangly_default": 1, "walkmesh": 0}
	check := func(label string, m *Model) {
		t.Helper()
		for _, n := range m.Nodes {
			if w, ok := want[n.Name]; ok && n.Mesh != nil && n.Mesh.Shadow != w {
				t.Errorf("%s: %s shadow = %d, want %d", label, n.Name, n.Mesh.Shadow, w)
			}
		}
	}
	check("parsed", m)

	var buf bytes.Buffer
	if err := Compile(m, &buf); err != nil {
		t.Fatal(err)
	}
	back, err := Decompile(bytes.NewReader(buf.Bytes()), int64(buf.Len()))
	if err != nil {
		t.Fatal(err)
	}
	check("compiled", back)
}
