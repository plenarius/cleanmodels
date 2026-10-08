package mdl

import "testing"

func TestWeldVertices_ExactDuplicates(t *testing.T) {
	mesh := NewMeshData()
	mesh.Verts = []Vec3{{0, 0, 0}, {1, 0, 0}, {0, 0, 0}, {0, 1, 0}}
	mesh.Faces = []Face{
		{Verts: [3]int32{0, 1, 3}},
		{Verts: [3]int32{2, 1, 3}},
	}
	n := trimeshNode("m", "NULL", mesh)

	removed := WeldVertices(n, WeldOptions{Eps: 0, DropUnused: false})
	if removed != 1 {
		t.Fatalf("expected 1 vertex removed, got %d (verts now %d)", removed, len(mesh.Verts))
	}
	if mesh.Faces[1].Verts[0] != 0 {
		t.Errorf("face 1 vertex 0 should remap to 0, got %d", mesh.Faces[1].Verts[0])
	}
}

func TestWeldVertices_DropUnused(t *testing.T) {
	mesh := NewMeshData()
	mesh.Verts = []Vec3{{0, 0, 0}, {1, 0, 0}, {2, 2, 2}, {0, 1, 0}}
	mesh.Faces = []Face{{Verts: [3]int32{0, 1, 3}}}
	n := trimeshNode("m", "NULL", mesh)

	removed := WeldVertices(n, WeldOptions{Eps: 0, DropUnused: true})
	if removed != 1 {
		t.Fatalf("expected 1 unused vertex removed, got %d", removed)
	}
	if len(mesh.Verts) != 3 {
		t.Fatalf("expected 3 verts, got %d", len(mesh.Verts))
	}
}

func TestWeldVertices_PreservesNormalsColors(t *testing.T) {
	mesh := NewMeshData()
	mesh.Verts = []Vec3{{0, 0, 0}, {1, 0, 0}, {0, 0, 0}}
	mesh.Normals = []Vec3{{0, 0, 1}, {1, 0, 0}, {0, 1, 0}}
	mesh.Colors = []Vec3{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}
	mesh.Faces = []Face{{Verts: [3]int32{0, 1, 2}}}
	n := trimeshNode("m", "NULL", mesh)

	WeldVertices(n, WeldOptions{Eps: 0})
	if len(mesh.Normals) != len(mesh.Verts) {
		t.Fatalf("normals/verts length mismatch after weld")
	}
	if len(mesh.Colors) != len(mesh.Verts) {
		t.Fatalf("colors/verts length mismatch after weld")
	}
}
