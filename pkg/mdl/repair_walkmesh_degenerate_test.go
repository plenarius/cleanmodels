package mdl

import (
	"errors"
	"testing"
)

// A walkmesh whose faces include stacked copies of one triangle (the centroids
// coincide on every axis), as real walkmeshes do.
func stackedWalkmesh(copies int) *Node {
	mesh := NewMeshData()
	mesh.Verts = []Vec3{{0, 0, 0}, {1, 0, 0}, {0, 1, 0}, {5, 5, 0}, {6, 5, 0}, {5, 6, 0}}
	for i := 0; i < copies; i++ {
		mesh.Faces = append(mesh.Faces, Face{Verts: [3]int32{0, 1, 2}})
	}
	mesh.Faces = append(mesh.Faces, Face{Verts: [3]int32{3, 4, 5}})
	return &Node{Name: "wok", Mesh: mesh, Aabb: &AabbData{}}
}

// walkTree reads the entries as a depth-first pre-order tree and returns the leaf
// faces in order, failing on a malformed tree.
func walkTree(t *testing.T, entries []AabbEntry) []int32 {
	t.Helper()
	pos := 0
	var leaves []int32
	var walk func() (min, max Vec3)
	walk = func() (Vec3, Vec3) {
		if pos >= len(entries) {
			t.Fatalf("tree ends early at entry %d of %d", pos, len(entries))
		}
		e := entries[pos]
		pos++
		if e.LeafFace >= 0 {
			leaves = append(leaves, e.LeafFace)
			return e.BoundMin, e.BoundMax
		}
		lmin, lmax := walk()
		rmin, rmax := walk()
		if got, want := vecMin(lmin, rmin), e.BoundMin; got != want {
			t.Errorf("interior box min %+v is not the union of its children's %+v", want, got)
		}
		if got, want := vecMax(lmax, rmax), e.BoundMax; got != want {
			t.Errorf("interior box max %+v is not the union of its children's %+v", want, got)
		}
		return e.BoundMin, e.BoundMax
	}
	walk()
	if pos != len(entries) {
		t.Errorf("%d entries left over after the tree", len(entries)-pos)
	}
	return leaves
}

// The reported failure: "cannot rebuild AABB tree: degenerate centroid
// arrangement" on walkmeshes with faces stacked on each other. Any partition of
// such faces is a valid tree (every box is the union of its faces'), so it must be
// built, with every face in exactly one leaf.
func TestRebuildAABBHandlesFacesWithTheSameCentroid(t *testing.T) {
	for _, copies := range []int{2, 3, 5, 8} {
		n := stackedWalkmesh(copies)
		if err := RebuildAABB(n); err != nil {
			if errors.Is(err, ErrDegenerateAABBTree) {
				t.Fatalf("%d stacked faces: still the degenerate-centroid failure", copies)
			}
			t.Fatalf("%d stacked faces: %v", copies, err)
		}
		leaves := walkTree(t, n.Aabb.Entries)
		if len(leaves) != len(n.Mesh.Faces) {
			t.Fatalf("%d stacked faces: %d leaves for %d faces", copies, len(leaves), len(n.Mesh.Faces))
		}
		seen := map[int32]bool{}
		for _, f := range leaves {
			if seen[f] {
				t.Errorf("face %d appears in two leaves", f)
			}
			seen[f] = true
		}
		root := n.Aabb.Entries[0]
		if root.BoundMin != (Vec3{0, 0, 0}) || root.BoundMax != (Vec3{6, 6, 0}) {
			t.Errorf("root box %+v..%+v is not the mesh's bounds", root.BoundMin, root.BoundMax)
		}
	}
}
