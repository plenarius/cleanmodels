package mdl

import "testing"

// tileNode wraps mesh in a model whose root sits at the origin so world-space
// vertices equal local-space ones.
func tileNode(mesh *MeshData) (*Model, *Node) {
	root := &Node{Name: "tile", Parent: "NULL"}
	n := &Node{Name: "Plane", Parent: "tile", Mesh: mesh}
	return &Model{Name: "tile", Classification: "TILE", Nodes: []*Node{root, n}}, n
}

// twoTileWedge is a 5x5 plane sitting against the +X tile boundary with one
// open edge along X = +5. Adding a chamfer should produce two new triangles.
func twoTileWedge() *MeshData {
	mesh := NewMeshData()
	mesh.Verts = []Vec3{
		{0, -2.5, 0},
		{5, -2.5, 0},
		{5, 2.5, 0},
		{0, 2.5, 0},
	}
	mesh.TVerts = []Vec3{{0, 0, 0}, {1, 0, 0}, {1, 1, 0}, {0, 1, 0}}
	mesh.Faces = []Face{
		{Verts: [3]int32{0, 1, 2}, UVs: [3]int32{0, 1, 2}},
		{Verts: [3]int32{0, 2, 3}, UVs: [3]int32{0, 2, 3}},
	}
	return mesh
}

func TestAddChamfers_PlusXBoundary(t *testing.T) {
	model, _ := tileNode(twoTileWedge())
	msgs := AddChamfers(model)
	if len(msgs) != 1 {
		t.Fatalf("expected 1 chamfer message, got %d (%v)", len(msgs), msgs)
	}
	mesh := model.Nodes[1].Mesh
	chamferFaces := 0
	for _, f := range mesh.Faces {
		if f.SmoothGroup == chamferSmoothGroup && f.Material == chamferMaterial {
			chamferFaces++
		}
	}
	if chamferFaces != 2 {
		t.Fatalf("expected 2 chamfer faces, got %d (faces=%+v)", chamferFaces, mesh.Faces)
	}
	for _, f := range mesh.Faces {
		if f.SmoothGroup != chamferSmoothGroup {
			continue
		}
		for k := 0; k < 3; k++ {
			v := mesh.Verts[f.Verts[k]]
			if v.X < -5.1 || v.X > 5.1 {
				continue
			}
			if v.Z < -0.05 && v.X > 5 {
				return
			}
		}
	}
}

func TestDeleteChamfers_RemovesAndCompacts(t *testing.T) {
	model, _ := tileNode(twoTileWedge())
	AddChamfers(model)
	mesh := model.Nodes[1].Mesh
	beforeFaces := len(mesh.Faces)

	msgs := DeleteChamfers(model)
	if len(msgs) != 1 {
		t.Fatalf("expected 1 delete message, got %d (%v)", len(msgs), msgs)
	}
	if len(mesh.Faces) >= beforeFaces {
		t.Errorf("expected face count to drop after DeleteChamfers (before=%d after=%d)", beforeFaces, len(mesh.Faces))
	}
	for _, f := range mesh.Faces {
		if f.SmoothGroup == chamferSmoothGroup {
			t.Errorf("chamfer face survived delete: %+v", f)
		}
	}
	for fi, f := range mesh.Faces {
		for k := 0; k < 3; k++ {
			if int(f.Verts[k]) >= len(mesh.Verts) {
				t.Errorf("face %d vertex %d out of range after compaction (verts=%d)", fi, k, len(mesh.Verts))
			}
		}
	}
}

func TestDeleteChamfers_NoChamferIsNoop(t *testing.T) {
	model, _ := tileNode(twoTileWedge())
	if msgs := DeleteChamfers(model); len(msgs) != 0 {
		t.Errorf("expected no-op when there are no chamfer faces, got %d msgs", len(msgs))
	}
}

func TestIsChamferTriangle(t *testing.T) {
	if !isChamferTriangle(Vec3{-5, 0, 0}, Vec3{-5, 1, 0}, Vec3{-5.03, 0.5, -0.03}) {
		t.Error("expected axis-aligned chamfer triangle to match")
	}
	if !isChamferTriangle(Vec3{5, 0, 0}, Vec3{5.03, 1, -0.03}, Vec3{5.03, -1, -0.03}) {
		t.Error("expected corner-aligned chamfer triangle to match")
	}
	if isChamferTriangle(Vec3{0, 0, 0}, Vec3{1, 0, 0}, Vec3{0, 1, 0}) {
		t.Error("non-boundary triangle should not match")
	}
}
