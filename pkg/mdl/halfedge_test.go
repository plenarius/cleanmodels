package mdl

import "testing"

func TestBuildDirectedEdgeMap_AndExterior(t *testing.T) {
	faces := []Face{
		{Verts: [3]int32{0, 1, 2}},
		{Verts: [3]int32{0, 2, 3}},
	}
	m := BuildDirectedEdgeMap(faces)

	if got := len(m); got != 6 {
		t.Errorf("expected 6 directed edges across 2 triangles, got %d", got)
	}
	if !IsExteriorDirectedEdge(m, 0, 1) {
		t.Error("0->1 should be exterior")
	}
	if IsExteriorDirectedEdge(m, 0, 2) {
		t.Error("0->2 has twin 2->0; should not be exterior")
	}
}

func TestDirectedEdgeMap_RecordsThirdVertex(t *testing.T) {
	faces := []Face{{Verts: [3]int32{4, 5, 6}}}
	m := BuildDirectedEdgeMap(faces)
	got := m[DirectedEdge{4, 5}]
	if len(got) != 1 || got[0].V3 != 6 || got[0].Face != 0 {
		t.Fatalf("expected DirectedEdgeFace{V3:6,Face:0}, got %+v", got)
	}
}
