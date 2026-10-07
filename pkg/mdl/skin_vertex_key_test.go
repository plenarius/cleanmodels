package mdl

import "testing"

// twoMaterialQuad is two triangles that share a corner exactly (same position,
// UV, normal) but belong to faces with different material indices.
func twoMaterialQuad() *MeshData {
	m := NewMeshData()
	m.Verts = []Vec3{{0, 0, 0}, {1, 0, 0}, {0, 1, 0}, {1, 1, 0}}
	m.TVerts = []Vec3{{0, 0, 0}, {1, 0, 0}, {0, 1, 0}, {1, 1, 0}}
	m.Normals = []Vec3{{0, 0, 1}, {0, 0, 1}, {0, 0, 1}, {0, 0, 1}}
	m.Faces = []Face{
		{Verts: [3]int32{0, 1, 2}, UVs: [3]int32{0, 1, 2}, Material: 0},
		{Verts: [3]int32{1, 3, 2}, UVs: [3]int32{1, 3, 2}, Material: 1},
	}
	return m
}

// TestSkinVertexKeyIgnoresMaterial pins the game compiler's behaviour: plain
// meshes split vertices at material boundaries, skin meshes do not. Seen on the
// centaur skin HorseBody (taur_pheno hak): 215 engine vertices against 224 when
// material was part of the key.
func TestSkinVertexKeyIgnoresMaterial(t *testing.T) {
	plain, err := buildExpandedMeshOpts(twoMaterialQuad(), true)
	if err != nil {
		t.Fatal(err)
	}
	skin, err := buildExpandedMeshOpts(twoMaterialQuad(), false)
	if err != nil {
		t.Fatal(err)
	}
	// Shared corners are vertices 1 and 2; they split across the two materials
	// for a plain mesh and merge for a skin.
	if len(plain.positions) != 6 {
		t.Errorf("plain mesh: %d vertices, want 6 (shared corners split by material)", len(plain.positions))
	}
	if len(skin.positions) != 4 {
		t.Errorf("skin mesh: %d vertices, want 4 (shared corners merged)", len(skin.positions))
	}
}
