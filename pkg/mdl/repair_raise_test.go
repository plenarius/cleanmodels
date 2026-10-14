package mdl

import (
	"math"
	"testing"
)

// A tile with a mesh under the root, a mesh under that mesh, an animated part
// under the animation parent "ttilea", and a walkmesh. Every one of them has a
// vertex at world Z computed from the positions above it, so the test can check
// the one thing a raise has to guarantee: every vertex moves by exactly the
// amount, once.
const raiseTileSrc = `newmodel ttile
setsupermodel ttile NULL
classification TILE
setanimationscale 1.00
beginmodelgeom ttile
  node dummy ttile
    parent NULL
  endnode
  node trimesh floor
    parent ttile
    position 0 0 0.5
    verts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2 1 0 1 2 0
  endnode
  node trimesh detail
    parent floor
    position 0 0 1
    verts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2 1 0 1 2 0
  endnode
  node dummy ttilea
    parent ttile
  endnode
  node trimesh spinner
    parent ttilea
    position 1 1 2
    verts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2 1 0 1 2 0
  endnode
  node aabb walkmesh
    parent ttile
    verts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2 1 0 1 2 4
    aabb 1
      0 0 0 1 1 0 0
  endnode
endmodelgeom
newanim spin ttile
  length 1.0
  transtime 0.0
  animroot ttile
  node trimesh spinner
    parent ttilea
    positionkey 2
      0.0 1 1 2
      1.0 1 1 2
  endnode
doneanim spin ttile
donemodel ttile
`

func mustParseFixture(t *testing.T, name string) *Model {
	t.Helper()
	pr, err := ParseFile("../../tests/fixtures/oracle/ascii/" + name + ".mdl")
	if err != nil {
		t.Fatal(err)
	}
	return pr.Model
}

func worldZ(t *testing.T, m *Model, name string) []float32 {
	t.Helper()
	n := m.FindNode(name)
	if n == nil {
		t.Fatalf("no node %q", name)
	}
	var z []float32
	for _, v := range WorldVertices(m, n) {
		z = append(z, v.Z)
	}
	return z
}

// The reported bug: raising by 5 m raised the tile by 15 m.
func TestRaiseMovesEveryVertexByExactlyTheAmount(t *testing.T) {
	for _, tc := range []struct {
		dir  string
		want float32
	}{{"raise", 5}, {"lower", -5}} {
		m := mustParseASCII(t, raiseTileSrc)
		names := []string{"floor", "detail", "spinner", "walkmesh"}
		before := map[string][]float32{}
		for _, n := range names {
			before[n] = worldZ(t, m, n)
			if len(before[n]) != 3 {
				t.Fatalf("%s: %d vertices", n, len(before[n]))
			}
		}
		if msgs := RaiseLowerTile(m, tc.dir, 5); len(msgs) == 0 {
			t.Fatal("no message")
		}
		for _, n := range names {
			after := worldZ(t, m, n)
			for i := range after {
				if got := after[i] - before[n][i]; math.Abs(float64(got-tc.want)) > 1e-5 {
					t.Errorf("%s %s: vertex %d moved by %g in world Z, want %g", tc.dir, n, i, got, tc.want)
				}
			}
		}
	}
}

// Each piece is moved in exactly one place, so nothing is counted twice.
func TestRaiseMovesEachPieceInOnePlace(t *testing.T) {
	m := mustParseASCII(t, raiseTileSrc)
	RaiseLowerTile(m, "raise", 5)

	pos := func(name string) float32 { return m.FindNode(name).Position.Z }
	if pos("ttile") != 0 {
		t.Errorf("root position moved: %g", pos("ttile"))
	}
	if pos("floor") != 5.5 {
		t.Errorf("a node under the root should be shifted once: %g, want 5.5", pos("floor"))
	}
	if pos("detail") != 1 {
		t.Errorf("a node under that one follows its parent and is not shifted: %g, want 1", pos("detail"))
	}
	if pos("ttilea") != 0 {
		t.Errorf("the animation parent itself is not shifted: %g", pos("ttilea"))
	}
	if pos("spinner") != 7 {
		t.Errorf("a child of the animation parent: %g, want 7", pos("spinner"))
	}
	if k := m.Animations[0].Nodes[0].PositionKeys; len(k) != 2 || k[0].Value.Z != 7 || k[1].Value.Z != 7 {
		t.Errorf("animation position keys: %+v, want Z 7", k)
	}

	wm := m.FindNode("walkmesh")
	if pos("walkmesh") != 0 {
		t.Errorf("the walkmesh keeps its position: %g", pos("walkmesh"))
	}
	for i, v := range wm.Mesh.Verts {
		if v.Z != 5 {
			t.Errorf("walkmesh vertex %d Z = %g, want 5", i, v.Z)
		}
	}
	if e := wm.Aabb.Entries[0]; e.BoundMin.Z != 5 || e.BoundMax.Z != 5 {
		t.Errorf("walkmesh tree bounds not shifted with its vertices: %+v", e)
	}
	// Local vertices of ordinary meshes are never touched.
	for _, name := range []string{"floor", "detail", "spinner"} {
		for i, v := range m.FindNode(name).Mesh.Verts {
			if v.Z != 0 {
				t.Errorf("%s vertex %d local Z = %g, want 0", name, i, v.Z)
			}
		}
	}
}

func TestRaiseByNothingChangesNothing(t *testing.T) {
	m := mustParseASCII(t, raiseTileSrc)
	if msgs := RaiseLowerTile(m, "raise", 0); len(msgs) != 0 {
		t.Errorf("messages for a zero raise: %v", msgs)
	}
	if m.FindNode("floor").Position.Z != 0.5 {
		t.Error("a zero raise moved something")
	}
}

// The same guarantee on real tiles: every vertex of every mesh moves by exactly
// the amount in world space.
func TestRaiseOnRealTilesMovesEveryVertexByExactlyTheAmount(t *testing.T) {
	for _, name := range []string{"tdc01_a01_03", "tdc01_g02_03", "tin01_d11_01", "tcei0_a01_01", "wsf10_p01_01"} {
		m := mustParseFixture(t, name)
		if m.Classification != "TILE" {
			t.Logf("%s is %s, skipped", name, m.Classification)
			continue
		}
		before := map[string][]Vec3{}
		for _, n := range m.Nodes {
			if n != nil && n.Mesh != nil {
				before[n.Name] = WorldVertices(m, n)
			}
		}
		RaiseLowerTile(m, "raise", 5)
		checked := 0
		for _, n := range m.Nodes {
			if n == nil || n.Mesh == nil {
				continue
			}
			after := WorldVertices(m, n)
			for i := range after {
				if got := after[i].Z - before[n.Name][i].Z; math.Abs(float64(got-5)) > 1e-4 {
					t.Fatalf("%s node %s vertex %d moved by %g, want 5", name, n.Name, i, got)
				}
				checked++
			}
		}
		if checked == 0 {
			t.Errorf("%s: no vertices checked", name)
		}
	}
}
