package mdl

import "testing"

func tileWaterModel(bitmap string) (*Model, *Node) {
	mesh := NewMeshData()
	mesh.Verts = []Vec3{{-5, -5, 0}, {5, -5, 0}, {5, 5, 0}, {-5, 5, 0}}
	mesh.TVerts = []Vec3{{0, 0, 0}, {1, 0, 0}, {1, 1, 0}, {0, 1, 0}}
	mesh.Faces = []Face{
		{Verts: [3]int32{0, 1, 2}, UVs: [3]int32{0, 1, 2}},
		{Verts: [3]int32{0, 2, 3}, UVs: [3]int32{0, 2, 3}},
	}
	mesh.Bitmap = bitmap
	n := &Node{Name: "WaterPlane", Parent: "tile", Mesh: mesh, AnimMesh: &AnimMeshData{}}
	root := &Node{Name: "tile", Parent: "NULL"}
	return &Model{Name: "tile", Classification: "TILE", Nodes: []*Node{root, n}}, n
}

func TestConvertWateryToTrimesh(t *testing.T) {
	model, n := tileWaterModel("tdt01_water01")
	if n.AnimMesh == nil {
		t.Fatalf("setup: AnimMesh should be set")
	}
	msgs := ConvertWateryToTrimesh(model, "")
	if len(msgs) != 1 {
		t.Fatalf("expected 1 reclassification message, got %d", len(msgs))
	}
	if n.AnimMesh != nil {
		t.Error("AnimMesh should be cleared after reclassification")
	}
}

func TestConvertWateryToTrimesh_KeyOnly(t *testing.T) {
	model, n := tileWaterModel("foo_water_b")
	if msgs := ConvertWateryToTrimesh(model, "water"); len(msgs) != 1 {
		t.Fatalf("expected key-based match to reclassify, got %d msgs", len(msgs))
	}
	if n.AnimMesh != nil {
		t.Error("AnimMesh should be cleared")
	}
}

func TestApplyWavyWater_TessellatesAndAnimates(t *testing.T) {
	model, n := tileWaterModel("tdt01_water01")
	originalVerts := len(n.Mesh.Verts)

	msgs := ApplyWavyWater(model, WavyWaterOptions{WaveHeight: 1.0})
	if len(msgs) != 1 {
		t.Fatalf("expected 1 message, got %d", len(msgs))
	}
	if len(n.Mesh.Verts) <= originalVerts {
		t.Errorf("expected tessellation to add verts; before=%d after=%d", originalVerts, len(n.Mesh.Verts))
	}

	if n.AnimMesh == nil {
		t.Fatal("expected AnimMesh to be populated")
	}
	want := 6 * len(n.Mesh.Verts)
	if got := len(n.AnimMesh.AnimVerts); got != want {
		t.Errorf("expected %d animverts (6 frames x %d verts), got %d", want, len(n.Mesh.Verts), got)
	}

	if len(model.Animations) == 0 {
		t.Fatal("expected default animation to be created")
	}
	if want, got := "default", model.Animations[0].Name; got != want {
		t.Errorf("animation name = %q, want %q", got, want)
	}
}

func TestApplyWavyWater_Deterministic(t *testing.T) {
	m1, n1 := tileWaterModel("tdt01_water01")
	m2, n2 := tileWaterModel("tdt01_water01")
	ApplyWavyWater(m1, WavyWaterOptions{WaveHeight: 1.0})
	ApplyWavyWater(m2, WavyWaterOptions{WaveHeight: 1.0})

	if len(n1.AnimMesh.AnimVerts) != len(n2.AnimMesh.AnimVerts) {
		t.Fatalf("animvert lengths differ: %d vs %d", len(n1.AnimMesh.AnimVerts), len(n2.AnimMesh.AnimVerts))
	}
	for i := range n1.AnimMesh.AnimVerts {
		if n1.AnimMesh.AnimVerts[i] != n2.AnimMesh.AnimVerts[i] {
			t.Fatalf("animvert %d diverges: %v vs %v", i, n1.AnimMesh.AnimVerts[i], n2.AnimMesh.AnimVerts[i])
		}
	}
}

func TestApplyWavyWater_NonTileNoop(t *testing.T) {
	model, _ := tileWaterModel("tdt01_water01")
	model.Classification = "CHARACTER"
	if msgs := ApplyWavyWater(model, WavyWaterOptions{WaveHeight: 1.0}); len(msgs) != 0 {
		t.Errorf("expected no-op on non-TILE classification, got %d msgs", len(msgs))
	}
}

func TestPerturbWave_FlatWhenAmplitudeZero(t *testing.T) {
	z1, z2, z3, z4 := perturbWave(0, 0, 0, 0, 0, 1, 1, 0)
	if z1 != 0 || z2 != 0 || z3 != 0 || z4 != 0 {
		t.Errorf("perturbWave with zero amplitudes should leave Z untouched, got %v %v %v %v", z1, z2, z3, z4)
	}
}
