package mdl

import "testing"

// walkmeshWithTree is a one-face walkmesh whose authored tree is just the
// face's own box, so a repair that moves the vertices has to move it too.
func walkmeshWithTree(position Vec3) *Node {
	mesh := NewMeshData()
	mesh.Verts = []Vec3{{-2.001, -2.001, 0.5}, {2.001, -2.001, 0.5}, {0.002, 2.001, 3.001}}
	mesh.Faces = []Face{{Verts: [3]int32{0, 1, 2}}}
	return &Node{
		Name: "wok", Parent: "NULL", Mesh: mesh, Position: position,
		Aabb: &AabbData{Entries: []AabbEntry{{
			BoundMin: Vec3{-2.001, -2.001, 0.5}, BoundMax: Vec3{2.001, 2.001, 3.001}, LeafFace: 0,
		}}},
	}
}

func treeCoversMesh(t *testing.T, n *Node) {
	t.Helper()
	bmin, bmax := facesAABB(n.Mesh, []int{0})
	e := n.Aabb.Entries[0]
	if e.BoundMin != bmin || e.BoundMax != bmax {
		t.Errorf("tree box %v..%v does not match its face %v..%v", e.BoundMin, e.BoundMax, bmin, bmax)
	}
}

func TestRepairPivotsMovesTheWalkmeshTreeWithItsVertices(t *testing.T) {
	n := walkmeshWithTree(Vec3{99, 99, 99})
	if len(RepairPivots(testModel(n), PivotOptions{})) == 0 || n.Position == (Vec3{99, 99, 99}) {
		t.Fatal("test needs the pivot to move")
	}
	treeCoversMesh(t, n)
}

func TestSnapVerticesSnapsTheWalkmeshTreeToo(t *testing.T) {
	n := walkmeshWithTree(Vec3{})
	SnapVertices(testModel(n), "decimal")
	treeCoversMesh(t, n)
}
