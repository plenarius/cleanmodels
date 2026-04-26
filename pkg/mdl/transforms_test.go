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
