package mdl

import (
	"math"
	"strings"
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
	exp, err := buildExpandedMeshOpts(degenerateMesh(), expandOpts{stripDegenerate: true})
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
	exp, err := buildExpandedMeshOpts(degenerateMesh(), expandOpts{})
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

// Skin vertices that agree on position, UV and normal but carry different
// bone influences must not share a GPU vertex, or the second one's weights
// are lost.
func TestBuildExpandedMeshSplitsSkinVerticesByInfluence(t *testing.T) {
	mesh := &MeshData{
		Verts:  []Vec3{{0, 0, 0}, {0, 0, 0}, {1, 0, 0}, {1, 1, 0}},
		TVerts: []Vec3{{0, 0, 0}, {1, 0, 0}, {1, 1, 0}},
		Faces: []Face{
			{Verts: [3]int32{0, 2, 3}, UVs: [3]int32{0, 1, 2}},
			{Verts: [3]int32{1, 2, 3}, UVs: [3]int32{0, 1, 2}},
		},
	}
	skin := &SkinData{Weights: []VertexWeight{
		{Bones: []string{"pelvis"}, Weights: []float32{1}},
		{Bones: []string{"torso"}, Weights: []float32{1}},
		{Bones: []string{"pelvis"}, Weights: []float32{1}},
		{Bones: []string{"pelvis"}, Weights: []float32{1}},
	}}

	plain, err := buildExpandedMesh(mesh)
	if err != nil {
		t.Fatal(err)
	}
	withSkin, err := buildExpandedMeshOpts(mesh, expandOpts{skinKeys: skinMergeKeys(skin)})
	if err != nil {
		t.Fatal(err)
	}
	if len(plain.positions) != 3 {
		t.Fatalf("without skin keys: %d vertices, want 3 (verts 0 and 1 merge)", len(plain.positions))
	}
	if len(withSkin.positions) != 4 {
		t.Fatalf("with skin keys: %d vertices, want 4 (different bones stay apart)", len(withSkin.positions))
	}
}

// On AABB nodes the game only reuses a vertex through an earlier face that
// shares at least two equal corners. Here faces 0 and 1 share an edge and
// merge, but face 2 touches face 0 at a single corner only, so its copy of
// that corner stays a separate vertex.
func TestBuildExpandedMeshAABBMergeNeedsSharedEdge(t *testing.T) {
	mesh := &MeshData{
		Verts: []Vec3{{0, 0, 0}, {1, 0, 0}, {0, 1, 0}, {1, 1, 0}, {-1, 0, 0}, {0, -1, 0}},
		Faces: []Face{
			{Verts: [3]int32{0, 1, 2}},
			{Verts: [3]int32{1, 3, 2}},
			{Verts: [3]int32{0, 4, 5}},
		},
	}
	global, err := buildExpandedMeshOpts(mesh, expandOpts{})
	if err != nil {
		t.Fatal(err)
	}
	aabb, err := buildExpandedMeshOpts(mesh, expandOpts{aabbMerge: true})
	if err != nil {
		t.Fatal(err)
	}
	if len(global.positions) != 6 {
		t.Errorf("global merge: %d vertices, want 6", len(global.positions))
	}
	if len(aabb.positions) != 7 {
		t.Errorf("AABB merge: %d vertices, want 7 (single shared corner is not merged)", len(aabb.positions))
	}
	if aabb.faceVerts[1][0] != aabb.faceVerts[0][1] || aabb.faceVerts[1][2] != aabb.faceVerts[0][2] {
		t.Errorf("faces sharing an edge should reuse vertices: %v", aabb.faceVerts)
	}
}

// Faces with different mirrorlist values never share a GPU vertex, even when
// every other attribute matches.
func TestBuildExpandedMeshMirrorListSplitsVertices(t *testing.T) {
	mesh := &MeshData{
		Verts:  []Vec3{{0, 0, 0}, {1, 0, 0}, {1, 1, 0}, {0, 1, 0}},
		TVerts: []Vec3{{0, 0, 0}, {1, 0, 0}, {1, 1, 0}, {0, 1, 0}},
		Faces: []Face{
			{Verts: [3]int32{0, 1, 2}, UVs: [3]int32{0, 1, 2}},
			{Verts: [3]int32{0, 2, 3}, UVs: [3]int32{0, 2, 3}},
		},
	}
	merged, err := buildExpandedMesh(mesh)
	if err != nil {
		t.Fatal(err)
	}
	if len(merged.positions) != 4 {
		t.Fatalf("without mirrorlist: %d vertices, want 4", len(merged.positions))
	}
	mesh.MirrorList = []int32{0, 1}
	split, err := buildExpandedMesh(mesh)
	if err != nil {
		t.Fatal(err)
	}
	if len(split.positions) != 6 {
		t.Fatalf("with differing mirrorlist: %d vertices, want 6 (the two shared corners split)", len(split.positions))
	}
	// Only the low byte counts.
	mesh.MirrorList = []int32{1, 257}
	same, _ := buildExpandedMesh(mesh)
	if len(same.positions) != 4 {
		t.Errorf("values 1 and 257 share a low byte: %d vertices, want 4", len(same.positions))
	}
}

func TestMirrorListASCIIRoundTrip(t *testing.T) {
	src := `newmodel mirrortest
setsupermodel mirrortest NULL
classification PLACEABLE
setanimationscale 1.00
beginmodelgeom mirrortest
  node dummy mirrortest
    parent NULL
  endnode
  node trimesh m
    parent mirrortest
    bitmap NULL
    render 1
    verts 4
      0 0 0
      1 0 0
      1 1 0
      0 1 0
    faces 2
      0 1 2 1 0 0 0 0
      0 2 3 1 0 0 0 0
    mirrorlist 2
      0
      1
  endnode
endmodelgeom
donemodel mirrortest
`
	m := mustParseASCII(t, src)
	mesh := m.FindNode("m").Mesh
	if len(mesh.MirrorList) != 2 || mesh.MirrorList[0] != 0 || mesh.MirrorList[1] != 1 {
		t.Fatalf("parsed MirrorList = %v, want [0 1]", mesh.MirrorList)
	}
	var out strings.Builder
	if err := Write(m, &out); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(out.String(), "mirrorlist 2") {
		t.Errorf("writer dropped mirrorlist:\n%s", out.String())
	}
	back := mustParseASCII(t, out.String())
	if got := back.FindNode("m").Mesh.MirrorList; len(got) != 2 || got[1] != 1 {
		t.Errorf("round-tripped MirrorList = %v", got)
	}
}
