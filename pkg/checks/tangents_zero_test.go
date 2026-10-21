package checks

import (
	"testing"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

func TestTangentValidationZeroLength(t *testing.T) {
	mesh := mdl.NewMeshData()
	mesh.Tangents = []mdl.Vec4{{X: 1, W: 1}, {W: 1}}
	m := &mdl.Model{Nodes: []*mdl.Node{{Name: "wall", Mesh: mesh}}}
	r := findCheck(checkTangentValidation(m, "t.mdl", false), "tangent_validation")
	if r == nil || r.Severity != mdl.SevWarning || r.Fixed || mesh.Tangents == nil {
		t.Fatalf("without fix: %+v", r)
	}
	r = findCheck(checkTangentValidation(m, "t.mdl", true), "tangent_validation")
	if r == nil || !r.Fixed || mesh.Tangents != nil {
		t.Errorf("with fix: %+v, tangents %v", r, mesh.Tangents)
	}
}
