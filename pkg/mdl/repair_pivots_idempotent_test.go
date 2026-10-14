package mdl

import (
	"strings"
	"testing"
)

// pivotTile is a 10x10 tile floor with a raised middle, authored with its
// walkmesh node placed at pos and turned by orient.
func pivotTile(pos Vec3, orient Vec4) (*Model, *Node) {
	mesh := NewMeshData()
	mesh.Verts = []Vec3{
		{-5, -5, 0}, {5, -5, 0}, {5, 5, 0}, {-5, 5, 0},
		{-1, -1, 2}, {1, -1, 2}, {1, 1, 2}, {-1, 1, 2},
	}
	mesh.Faces = []Face{
		{Verts: [3]int32{0, 1, 5}, Normal: Vec3{Z: 1}}, {Verts: [3]int32{0, 5, 4}, Normal: Vec3{Z: 1}},
		{Verts: [3]int32{1, 2, 6}, Normal: Vec3{Z: 1}}, {Verts: [3]int32{1, 6, 5}, Normal: Vec3{Z: 1}},
		{Verts: [3]int32{2, 3, 7}, Normal: Vec3{Z: 1}}, {Verts: [3]int32{2, 7, 6}, Normal: Vec3{Z: 1}},
		{Verts: [3]int32{3, 0, 4}, Normal: Vec3{Z: 1}}, {Verts: [3]int32{3, 4, 7}, Normal: Vec3{Z: 1}},
	}
	node := &Node{Name: "wok", Parent: "NULL", Mesh: mesh, Aabb: &AabbData{}, Position: pos, Orientation: orient}
	return testModel(node), node
}

// TestRepairPivotsSecondRunChangesNothing is issue #74: the constraints were
// built from the mesh in the node's own frame but tested against the node's
// position in its parent's, so a node that had just been given a valid pivot
// failed the check again and moved a little further on every run.
func TestRepairPivotsSecondRunChangesNothing(t *testing.T) {
	for name, tc := range map[string]struct {
		pos    Vec3
		orient Vec4
	}{
		"far outside":  {Vec3{99, 99, 99}, Vec4{}},
		"outside in x": {Vec3{-7, 3, 1.75}, Vec4{}},
		"outside in y": {Vec3{0.5, 12, 0.25}, Vec4{}},
		"turned":       {Vec3{40, -7, 5}, Vec4{Z: 1, W: 1.5707964}},
	} {
		t.Run(name, func(t *testing.T) {
			model, node := pivotTile(tc.pos, tc.orient)
			idx := nodeIndex(model)
			world := func() Vec3 { return LocalToWorld(idx, node, node.Mesh.Verts[5]) }
			before := world()
			first := strings.Join(RepairPivots(model, PivotOptions{}), "\n")
			if !strings.Contains(first, "pivot set") {
				t.Skipf("nothing to repair: %q", first)
			}
			pos, verts, w1 := node.Position, append([]Vec3(nil), node.Mesh.Verts...), world()
			if d := vecLen(Vec3{X: w1.X - before.X, Y: w1.Y - before.Y, Z: w1.Z - before.Z}); d > 1e-3 {
				t.Errorf("repair moved the geometry by %g", d)
			}
			for run := 2; run <= 4; run++ {
				for _, m := range RepairPivots(model, PivotOptions{}) {
					if strings.Contains(m, "pivot set") {
						t.Fatalf("run %d changed the pivot again: %s", run, m)
					}
				}
			}
			if node.Position != pos {
				t.Errorf("position drifted from %v to %v", pos, node.Position)
			}
			for i := range verts {
				if node.Mesh.Verts[i] != verts[i] {
					t.Fatalf("vertex %d drifted", i)
				}
			}
		})
	}
}
