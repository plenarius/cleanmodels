package mdl

import (
	"math"
	"testing"
)

const transformEps = 1e-5

func vec3Approx(t *testing.T, got, want Vec3) {
	t.Helper()
	dx := math.Abs(float64(got.X - want.X))
	dy := math.Abs(float64(got.Y - want.Y))
	dz := math.Abs(float64(got.Z - want.Z))
	if dx > transformEps || dy > transformEps || dz > transformEps {
		t.Fatalf("vec3 mismatch: got %+v want %+v", got, want)
	}
}

func TestRotateVectorAxisAngle_Identity(t *testing.T) {
	v := Vec3{1, 2, 3}
	got := RotateVectorAxisAngle(v, Vec4{0, 0, 1, 0})
	vec3Approx(t, got, v)
}

func TestRotateVectorAxisAngle_ZeroAxis(t *testing.T) {
	v := Vec3{1, 2, 3}
	got := RotateVectorAxisAngle(v, Vec4{0, 0, 0, 1.5})
	vec3Approx(t, got, v)
}

func TestRotateVectorAxisAngle_HalfTurn(t *testing.T) {
	v := Vec3{1, 0, 0}
	got := RotateVectorAxisAngle(v, Vec4{0, 0, 1, math.Pi})
	vec3Approx(t, got, Vec3{-1, 0, 0})
}

func TestRotateVectorAxisAngle_QuarterTurn(t *testing.T) {
	got := RotateVectorAxisAngle(Vec3{1, 0, 0}, Vec4{0, 0, 1, math.Pi / 2})
	vec3Approx(t, got, Vec3{0, 1, 0})
}

func TestLocalToWorld_NestedTranslation(t *testing.T) {
	root := &Node{Name: "root", Parent: "NULL", Position: Vec3{X: 10}}
	child := &Node{Name: "child", Parent: "root", Position: Vec3{Y: 5}}
	model := &Model{Name: "root", Nodes: []*Node{root, child}}
	idx := nodeIndex(model)

	w := LocalToWorld(idx, child, Vec3{Z: 1})
	vec3Approx(t, w, Vec3{X: 10, Y: 5, Z: 1})
}

func TestLocalToWorld_RotationThenTranslation(t *testing.T) {
	root := &Node{Name: "root", Parent: "NULL"}
	child := &Node{Name: "child", Parent: "root", Position: Vec3{X: 1}, Orientation: Vec4{Z: 1, W: math.Pi / 2}}
	model := &Model{Name: "root", Nodes: []*Node{root, child}}
	idx := nodeIndex(model)

	w := LocalToWorld(idx, child, Vec3{X: 1})
	vec3Approx(t, w, Vec3{X: 1, Y: 1})
}

func TestWorldToLocal_RoundTrip(t *testing.T) {
	root := &Node{Name: "root", Parent: "NULL", Position: Vec3{X: 3, Y: 4}}
	child := &Node{Name: "child", Parent: "root", Position: Vec3{X: 1, Z: 2}, Orientation: Vec4{Z: 1, W: math.Pi / 3}}
	model := &Model{Name: "root", Nodes: []*Node{root, child}}
	idx := nodeIndex(model)

	cases := []Vec3{{1, 2, 3}, {-2, 0.5, -1}, {0, 0, 0}}
	for _, v := range cases {
		w := LocalToWorld(idx, child, v)
		back := WorldToLocal(idx, child, w)
		vec3Approx(t, back, v)
	}
}

// TestParentChain_DuplicateNodeName pins the fix for a bug where nodeIndex
// resolved a node's parent by a global "last node with this name wins" name
// lookup, instead of the "nearest preceding node in declaration order" rule
// resolveGeomTree uses to build the actual compiled tree (see
// compiler_tree.go). MDL node names are not unique in real content (bilateral
// rig dummies, mesh-plus-hook idioms, repeated emitters), so a child whose
// Parent names an earlier duplicate must resolve to that nearer occurrence,
// not to a later one declared further down the node list.
func TestParentChain_DuplicateNodeName(t *testing.T) {
	root := &Node{Name: "root", Parent: "NULL"}
	dummyA1 := &Node{Name: "dummyA", Parent: "root", Position: Vec3{X: 0}}
	child := &Node{Name: "child", Parent: "dummyA", Position: Vec3{X: 1}}
	dummyA2 := &Node{Name: "dummyA", Parent: "root", Position: Vec3{X: 100}}
	model := &Model{Name: "root", Nodes: []*Node{root, dummyA1, child, dummyA2}}
	idx := nodeIndex(model)

	// child must hang off the nearest preceding "dummyA" (dummyA1, X=0), not
	// the later one (dummyA2, X=100) that a name-keyed map would find.
	w := LocalToWorld(idx, child, Vec3{})
	vec3Approx(t, w, Vec3{X: 1})
}

func TestModelBounds_DuplicateNodeName(t *testing.T) {
	root := &Node{Name: "root", Parent: "NULL"}
	dummyA1 := &Node{Name: "dummyA", Parent: "root", Position: Vec3{X: 0}}
	child := &Node{
		Name:   "child",
		Parent: "dummyA",
		Mesh:   &MeshData{Verts: []Vec3{{X: 1}}},
	}
	dummyA2 := &Node{Name: "dummyA", Parent: "root", Position: Vec3{X: 100}}
	model := &Model{Name: "root", Nodes: []*Node{root, dummyA1, child, dummyA2}}

	// Every node's own origin already extends the box out to X=100 (via
	// dummyA2), so the only thing distinguishing correct from buggy
	// resolution is where child's own vertex lands: X=1 relative to the
	// nearest preceding "dummyA" (dummyA1), vs X=101 if it were wrongly
	// resolved to the later dummyA2. A name-keyed "last wins" lookup gives
	// the latter and pushes bmax.X to 101.
	_, bmax, _ := modelBounds(model)
	vec3Approx(t, bmax, Vec3{X: 100})
}
