package mdl

import (
	"bytes"
	"testing"
)

// A quad whose two faces share positions 0 and 2 but meet at a UV seam: the
// compile splits those two vertices (different UVs), so the binary has 6.
// apart: two triangles on the same edge, as separate positions with their own
// UVs (the source kept them apart, so the faces are not neighbours, and the
// binary keeps the six vertices). Sharing must not join them.
const sharePositionsSrc = `newmodel sp
setsupermodel sp NULL
classification PLACEABLE
setanimationscale 1.0
beginmodelgeom sp
  node dummy sp
    parent NULL
  endnode
  node trimesh seam
    parent sp
    bitmap blank
    verts 4
      0 0 0
      1 0 0
      1 1 0
      0 1 0
    tverts 6
      0 0 0
      1 0 0
      1 1 0
      0.5 0 0
      0.5 0.5 0
      0 0.5 0
    faces 2
      0 1 2  1  0 1 2  0
      0 2 3  1  3 4 5  0
  endnode
  node trimesh apart
    parent sp
    bitmap blank
    verts 6
      0 0 0
      1 0 0
      1 1 0
      0 0 0
      1 1 0
      0 1 0
    tverts 2
      0 0 0
      1 1 0
    faces 2
      0 1 2  1  0 0 0  0
      3 4 5  1  1 1 1  0
  endnode
endmodelgeom
donemodel sp
`

func adjacencyOf(t *testing.T, data []byte, node string) [][3]int16 {
	t.Helper()
	m, err := DecompileWithOptions(bytes.NewReader(data), int64(len(data)), DecompileOptions{KeepVertexSplits: true})
	if err != nil {
		t.Fatal(err)
	}
	return m.FindNode(node).Mesh.StoredAdjacency
}

func TestSharePositionsRestoresSeamNeighbours(t *testing.T) {
	first := mustCompile(t, mustParseASCII(t, sharePositionsSrc))
	if a := adjacencyOf(t, first, "seam"); a[0][2] != 1 || a[1][0] != 0 {
		t.Fatalf("the source links the faces across the seam: %v", a)
	}

	shared := mustDecompile(t, first)
	seam := shared.FindNode("seam").Mesh
	if len(seam.Verts) != 4 || len(seam.TVerts) != 6 {
		t.Errorf("seam: %d positions and %d tverts, want 4 and 6", len(seam.Verts), len(seam.TVerts))
	}
	if apart := shared.FindNode("apart").Mesh; len(apart.Verts) != 6 {
		t.Errorf("apart: %d positions, want the source's 6 kept apart", len(apart.Verts))
	}

	again := mustCompile(t, shared)
	if !bytes.Equal(again, first) {
		a, b := adjacencyOf(t, first, "seam"), adjacencyOf(t, again, "seam")
		t.Errorf("recompiled binary differs; seam adjacency %v, was %v", b, a)
	}

	split, err := DecompileWithOptions(bytes.NewReader(first), int64(len(first)), DecompileOptions{KeepVertexSplits: true})
	if err != nil {
		t.Fatal(err)
	}
	if n := len(split.FindNode("seam").Mesh.Verts); n != 6 {
		t.Errorf("KeepVertexSplits: %d positions, want 6", n)
	}
	if a := adjacencyOf(t, mustCompile(t, split), "seam"); a[0][2] != -1 {
		t.Errorf("split positions should lose the seam link (that is what sharing fixes): %v", a)
	}
}

// A stored link between vertices whose normals differ is not followed: the
// ASCII writes normals per position, so joining them would change a normal.
func TestSharePositionsKeepsDifferentNormalsApart(t *testing.T) {
	n := &Node{Name: "hard", Mesh: &MeshData{
		Verts:   []Vec3{{0, 0, 0}, {1, 0, 0}, {1, 1, 0}, {0, 0, 0}, {1, 1, 0}, {0, 1, 0}},
		Normals: []Vec3{{0, 0, 1}, {0, 0, 1}, {0, 0, 1}, {1, 0, 0}, {1, 0, 0}, {1, 0, 0}},
		TVerts:  make([]Vec3, 6),
		Faces: []Face{
			{Verts: [3]int32{0, 1, 2}, UVs: [3]int32{0, 1, 2}},
			{Verts: [3]int32{3, 4, 5}, UVs: [3]int32{3, 4, 5}},
		},
		StoredAdjacency: [][3]int16{{-1, -1, 1}, {0, -1, -1}},
	}}
	sharePositions(n)
	if len(n.Mesh.Verts) != 6 {
		t.Errorf("%d positions, want 6 (normals differ)", len(n.Mesh.Verts))
	}
	n.Mesh.Normals[3], n.Mesh.Normals[4] = Vec3{0, 0, 1}, Vec3{0, 0, 1}
	sharePositions(n)
	if len(n.Mesh.Verts) != 4 || n.Mesh.Faces[1].Verts != [3]int32{0, 2, 3} || n.Mesh.Faces[1].UVs != [3]int32{3, 4, 5} {
		t.Errorf("equal normals: verts %d, face 1 %+v", len(n.Mesh.Verts), n.Mesh.Faces[1])
	}
}
