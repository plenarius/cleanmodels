package checks

import (
	"strings"
	"testing"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

// Three faces on the edge between vertices 0 and 1.
func fanMesh() *mdl.MeshData {
	mesh := mdl.NewMeshData()
	mesh.Shadow = 1
	mesh.Verts = []mdl.Vec3{{X: 0}, {X: 1}, {X: 0.5, Y: 1}, {X: 0.5, Y: -1}, {X: 0.5, Z: 1}}
	mesh.Faces = []mdl.Face{
		{Verts: [3]int32{0, 1, 2}},
		{Verts: [3]int32{1, 0, 3}},
		{Verts: [3]int32{0, 1, 4}},
	}
	return mesh
}

func multipleEdges(mesh *mdl.MeshData) *mdl.CheckResult {
	m := &mdl.Model{Nodes: []*mdl.Node{{Name: "trunk", Mesh: mesh}}}
	return findCheck(checkMultipleEdges(m, "t.mdl", false), "multiple_edges")
}

func TestMultipleEdgesASCII(t *testing.T) {
	r := multipleEdges(fanMesh())
	if r == nil || !strings.Contains(r.Message, "--split-multiedge") {
		t.Fatalf("ASCII fan: %+v", r)
	}
}

// A binary is judged by what it stores: three faces linked along the edge.
func TestMultipleEdgesStored(t *testing.T) {
	mesh := fanMesh()
	mesh.StoredAdjacency = [][3]int16{{1, -1, -1}, {0, -1, -1}, {0, -1, -1}}
	r := multipleEdges(mesh)
	if r == nil || !strings.Contains(r.Message, "stored neighbours") || !strings.Contains(r.Message, "--open-multiedges") {
		t.Fatalf("binary fan: %+v", r)
	}
	// Opened (every slot on the edge -1): nothing for the game to tear.
	mesh.StoredAdjacency = [][3]int16{{-1, -1, -1}, {-1, -1, -1}, {-1, -1, -1}}
	if r := multipleEdges(mesh); r != nil {
		t.Errorf("opened fan still reported: %+v", r)
	}
}

// The decompiled faces of a binary share merged vertices, so three faces can
// meet at an edge the file never linked (a double-sided card, say). The stored
// neighbours decide; the merged faces alone used to be reported.
func TestMultipleEdgesBinaryMergedNotReported(t *testing.T) {
	mesh := fanMesh()
	mesh.StoredAdjacency = [][3]int16{{1, -1, -1}, {0, -1, -1}, {-1, -1, -1}}
	if r := multipleEdges(mesh); r != nil {
		t.Errorf("reported an edge the file does not link: %+v", r)
	}
}
