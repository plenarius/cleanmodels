package mdl

import (
	"math"
	"testing"
)

func normalsQuad(sgA, sgB int32, flipB bool) *MeshData {
	m := &MeshData{
		Verts: []Vec3{{0, 0, 0}, {1, 0, 0}, {1, 1, 0}, {0, 1, 0}},
		Faces: []Face{
			{Verts: [3]int32{0, 1, 2}, SmoothGroup: sgA},
			{Verts: [3]int32{0, 2, 3}, SmoothGroup: sgB},
		},
	}
	if flipB {
		m.Faces[1].Verts = [3]int32{0, 3, 2}
	}
	return m
}

func nearVec(a, b Vec3) bool {
	const eps = 1e-5
	return math.Abs(float64(a.X-b.X)) < eps && math.Abs(float64(a.Y-b.Y)) < eps && math.Abs(float64(a.Z-b.Z)) < eps
}

// Opposed faces sharing vertices and a smoothing group sum to zero. The game
// gives each its own face normal; we used to emit +Z for both.
func TestGenerateNormalsBackToBackFaces(t *testing.T) {
	m := normalsQuad(1, 1, true)
	generateNormals(m)
	// Face 0 normal is +Z. Face 1 is wound the other way, so -Z.
	for vi := 0; vi < 3; vi++ {
		if got := m.CornerNormals[0][vi]; !nearVec(got, Vec3{Z: 1}) {
			t.Errorf("face 0 corner %d = %v, want +Z", vi, got)
		}
		if got := m.CornerNormals[1][vi]; !nearVec(got, Vec3{Z: -1}) {
			t.Errorf("face 1 corner %d = %v, want -Z", vi, got)
		}
	}
}

func TestGenerateNormalsSharedSmoothingGroup(t *testing.T) {
	// Two coplanar faces in one group share the same normal at shared vertices.
	m := normalsQuad(1, 1, false)
	generateNormals(m)
	if !nearVec(m.CornerNormals[0][0], Vec3{Z: 1}) || !nearVec(m.CornerNormals[1][0], Vec3{Z: 1}) {
		t.Errorf("coplanar faces should both be +Z, got %v and %v", m.CornerNormals[0][0], m.CornerNormals[1][0])
	}
}

func TestGenerateNormalsAreaWeighted(t *testing.T) {
	// A large flat face and a small perpendicular face sharing an edge in one
	// group: the shared corner leans toward the larger face.
	m := &MeshData{
		Verts: []Vec3{{0, 0, 0}, {4, 0, 0}, {4, 4, 0}, {0, 0, 1}},
		Faces: []Face{
			{Verts: [3]int32{0, 1, 2}, SmoothGroup: 1},
			{Verts: [3]int32{0, 3, 1}, SmoothGroup: 1},
		},
	}
	generateNormals(m)
	n := m.CornerNormals[0][0]
	if n.Z < 0.9 || n.Z >= 1 {
		t.Errorf("shared corner = %v, want mostly +Z with a small tilt", n)
	}
	if m.CornerNormals[1][0] != n {
		t.Errorf("later face should reuse the earlier face's normal: %v vs %v", m.CornerNormals[1][0], n)
	}
}

func TestGenerateNormalsSmoothingGroupBoundaries(t *testing.T) {
	// Group 0 never joins another face; disjoint bits stay separate.
	for name, sgs := range map[string][2]int32{"zero": {0, 0}, "disjoint": {1, 2}} {
		m := &MeshData{
			Verts: []Vec3{{0, 0, 0}, {1, 0, 0}, {1, 1, 0}, {0, 0, 1}},
			Faces: []Face{
				{Verts: [3]int32{0, 1, 2}, SmoothGroup: sgs[0]},
				{Verts: [3]int32{0, 3, 1}, SmoothGroup: sgs[1]},
			},
		}
		generateNormals(m)
		if !nearVec(m.CornerNormals[0][0], Vec3{Z: 1}) {
			t.Errorf("%s: face 0 corner 0 = %v, want its own +Z", name, m.CornerNormals[0][0])
		}
		if !nearVec(m.CornerNormals[1][0], Vec3{Y: 1}) {
			t.Errorf("%s: face 1 corner 0 = %v, want its own +Y", name, m.CornerNormals[1][0])
		}
	}
}

func TestGenerateNormalsKeepsExistingNormals(t *testing.T) {
	m := normalsQuad(1, 1, false)
	m.Normals = []Vec3{{X: 1}, {X: 1}, {X: 1}, {X: 1}}
	generateNormals(m)
	if m.CornerNormals != nil {
		t.Error("existing normals should be left alone")
	}
}

// The game compares vertex attributes with float ==, so -0 and +0 normals
// must not split a vertex.
func TestDedupTreatsNegativeZeroAsZero(t *testing.T) {
	negZero := float32(math.Copysign(0, -1))
	if f32bits(negZero) != f32bits(0) {
		t.Errorf("f32bits(-0) = %#x, want %#x", f32bits(negZero), f32bits(0))
	}
	if f32bits(1) == f32bits(-1) {
		t.Error("f32bits must still distinguish 1 and -1")
	}
}

// A face with two corners that resolve to the same GPU vertex is dropped, as
// the game does, but only when stripping is on (AABB walkmeshes keep theirs).
// Verts 1 and 3 share coordinates, so face 1 (0,1,3) collapses to a line.
func degenerateMesh() *MeshData {
	return &MeshData{
		Verts: []Vec3{{0, 0, 0}, {1, 0, 0}, {1, 1, 0}, {1, 0, 0}},
		Faces: []Face{
			{Verts: [3]int32{0, 1, 2}, UVs: [3]int32{0, 1, 2}},
			{Verts: [3]int32{0, 1, 3}, UVs: [3]int32{0, 1, 1}},
			{Verts: [3]int32{0, 2, 1}, UVs: [3]int32{0, 2, 1}},
		},
		TVerts: []Vec3{{0, 0, 0}, {1, 0, 0}, {1, 1, 0}},
	}
}

func TestBuildExpandedMeshStripsDegenerateFaces(t *testing.T) {
	exp, err := buildExpandedMeshOpts(degenerateMesh(), true)
	if err != nil {
		t.Fatal(err)
	}
	if len(exp.faceVerts) != 2 || len(exp.faceSrc) != 2 {
		t.Fatalf("kept %d faces, want 2", len(exp.faceVerts))
	}
	if exp.faceSrc[0] != 0 || exp.faceSrc[1] != 2 {
		t.Errorf("faceSrc = %v, want [0 2]", exp.faceSrc)
	}
}

func TestBuildExpandedMeshKeepsDegenerateFacesWhenAsked(t *testing.T) {
	exp, err := buildExpandedMeshOpts(degenerateMesh(), false)
	if err != nil {
		t.Fatal(err)
	}
	if len(exp.faceVerts) != 3 {
		t.Fatalf("kept %d faces, want 3", len(exp.faceVerts))
	}
	if exp.faceVerts[1][1] != exp.faceVerts[1][2] {
		t.Errorf("degenerate face should share a vertex: %v", exp.faceVerts[1])
	}
}
