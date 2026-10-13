package mdl

import (
	"math"
	"testing"
)

func TestTessellateMesh_NoLongEdge(t *testing.T) {
	mesh := NewMeshData()
	mesh.Verts = []Vec3{{0, 0, 0}, {1, 0, 0}, {0, 1, 0}}
	mesh.TVerts = []Vec3{{0, 0, 0}, {1, 0, 0}, {0, 1, 0}}
	mesh.Faces = []Face{{Verts: [3]int32{0, 1, 2}, UVs: [3]int32{0, 1, 2}}}
	n := trimeshNode("m", "NULL", mesh)

	count := TessellateMesh(n, 2.0)
	if count != 0 {
		t.Fatalf("expected 0 bisections on 1m triangle, got %d", count)
	}
}

func TestTessellateMesh_SinglePass(t *testing.T) {
	mesh := NewMeshData()
	mesh.Verts = []Vec3{{0, 0, 0}, {5, 0, 0}, {0, 4, 0}}
	mesh.TVerts = []Vec3{{0, 0, 0}, {1, 0, 0}, {0, 1, 0}}
	mesh.Faces = []Face{{Verts: [3]int32{0, 1, 2}, UVs: [3]int32{0, 1, 2}}}
	n := trimeshNode("m", "NULL", mesh)

	count := TessellateMesh(n, 2.0)
	if count == 0 {
		t.Fatal("expected at least one bisection on 5m edge")
	}
	for _, f := range mesh.Faces {
		for k := 0; k < 3; k++ {
			a := mesh.Verts[f.Verts[k]]
			b := mesh.Verts[f.Verts[(k+1)%3]]
			d := math.Hypot(math.Hypot(float64(a.X-b.X), float64(a.Y-b.Y)), float64(a.Z-b.Z))
			if d > 2.0+1e-4 {
				t.Errorf("edge length %.3f exceeds pitch 2.0", d)
			}
		}
	}
}

func TestTessellateMesh_TileQuad(t *testing.T) {
	mesh := NewMeshData()
	mesh.Verts = []Vec3{{-5, -5, 0}, {5, -5, 0}, {5, 5, 0}, {-5, 5, 0}}
	mesh.TVerts = []Vec3{{0, 0, 0}, {1, 0, 0}, {1, 1, 0}, {0, 1, 0}}
	mesh.Faces = []Face{
		{Verts: [3]int32{0, 1, 2}, UVs: [3]int32{0, 1, 2}},
		{Verts: [3]int32{0, 2, 3}, UVs: [3]int32{0, 2, 3}},
	}
	n := trimeshNode("m", "NULL", mesh)

	TessellateMesh(n, 2.0)
	for fi, f := range mesh.Faces {
		for k := 0; k < 3; k++ {
			a := mesh.Verts[f.Verts[k]]
			b := mesh.Verts[f.Verts[(k+1)%3]]
			d := math.Hypot(math.Hypot(float64(a.X-b.X), float64(a.Y-b.Y)), float64(a.Z-b.Z))
			if d > 2.0+1e-4 {
				t.Errorf("face %d: edge length %.3f exceeds 2m pitch", fi, d)
			}
		}
	}
}
