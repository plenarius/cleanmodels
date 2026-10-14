package mdl

import (
	"math"
	"testing"
)

// The UV of a ground vertex follows its place in the tile, not the mesh's own
// coordinates: a ground mesh offset in X and Y, or sitting under an offset
// parent, must still tile seamlessly with its neighbours.
func TestRetileUVsUseTileCoordinates(t *testing.T) {
	src := `newmodel retile
setsupermodel retile NULL
classification TILE
setanimationscale 1.00
beginmodelgeom retile
  node dummy retile
    parent NULL
  endnode
  node trimesh ground
    parent retile
    position 2 -1 0
    bitmap tdc01_ground
    verts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2 1 0 1 2 0
  endnode
  node trimesh centred
    parent retile
    bitmap tdc01_ground
    verts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2 1 0 1 2 0
  endnode
endmodelgeom
donemodel retile
`
	m := mustParseASCII(t, src)
	if msgs := RetileUVs(m, "ground", 1); len(msgs) != 2 {
		t.Fatalf("messages: %v", msgs)
	}
	near := func(a, b float32) bool { return math.Abs(float64(a-b)) < 1e-5 }
	g := m.FindNode("ground").Mesh.TVerts
	if !near(g[0].X, 0.7) || !near(g[0].Y, 0.4) || !near(g[1].X, 0.8) || !near(g[2].Y, 0.5) {
		t.Errorf("offset mesh UVs %+v, want vertex 0 at (0.7, 0.4): (2+5)*0.1, (-1+5)*0.1", g)
	}
	c := m.FindNode("centred").Mesh.TVerts
	if !near(c[0].X, 0.5) || !near(c[0].Y, 0.5) {
		t.Errorf("a mesh at the origin keeps (0.5, 0.5): %+v", c)
	}
}
