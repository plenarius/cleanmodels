package mdl

import (
	"strings"
	"testing"
)

// A one-value parameter with a missing or unusable value keeps its default,
// as before, but is now reported with the line number instead of silently.
func TestScalarParamsReportBadValues(t *testing.T) {
	src := `newmodel params
setsupermodel params NULL
classification CHARACTER
setanimationscale 1.0x
beginmodelgeom params
  node trimesh m
    parent NULL
    shininess abc
    shadow
    alpha 0.5
    verts 2x
    faces 0
  endnode
endmodelgeom
donemodel params
`
	pr, err := Parse(strings.NewReader(src))
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{
		`"1.0x" is not a usable number for "setanimationscale"`,
		`"abc" is not a usable number for "shininess"`,
		`"shadow" has no value`,
		`"2x" is not a usable count for "verts"`,
	} {
		if !hasError(pr.Errors, want) {
			t.Errorf("missing warning %q in %+v", want, pr.Errors)
		}
	}
	if hasError(pr.Errors, `"alpha"`) {
		t.Errorf("a good value must not warn: %+v", pr.Errors)
	}
	for _, e := range pr.Errors {
		if e.Line == 0 {
			t.Errorf("warning without a line number: %+v", e)
		}
	}
	n := pr.Model.RootNode()
	if n == nil || n.Mesh == nil || n.Mesh.Alpha != 0.5 || n.Mesh.Shadow != 1 {
		t.Errorf("good value should apply and bad ones keep the default: %+v", n)
	}
}
