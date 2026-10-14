package mdl

import (
	"bytes"
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

// Emitter flags written as true/false (NWNCQ) are read as 1/0, not dropped.
func TestAffectedByWindAcceptsTrueFalse(t *testing.T) {
	for _, tc := range []struct {
		val  string
		want int32
		warn bool
	}{{"true", 1, false}, {"False", 0, false}, {"1", 1, false}, {"maybe", 0, true}} {
		src := `newmodel w
setsupermodel w NULL
classification TILE
setanimationscale 1.0
beginmodelgeom w
  node emitter e
    parent NULL
    affectedByWind ` + tc.val + `
  endnode
endmodelgeom
donemodel w
`
		pr, err := Parse(strings.NewReader(src))
		if err != nil {
			t.Fatal(err)
		}
		n := pr.Model.RootNode()
		if n == nil || n.Emitter == nil || n.Emitter.AffectedByWind != tc.want {
			t.Errorf("%s: got %+v, want %d", tc.val, n, tc.want)
		}
		if hasError(pr.Errors, "affectedByWind") != tc.warn {
			t.Errorf("%s: warnings %+v, want warn=%v", tc.val, pr.Errors, tc.warn)
		}
	}
}

// Static values on an animation node (position, a colour, clip parameters)
// used to throw away an unusable number without a word.
func TestAnimNodeStaticValuesReportBadNumbers(t *testing.T) {
	_, errs := animNode(t, "dummy", `    position 1 x 3
    orientation 0 0 0 1
    colorStart 0.1 0.2 q`)
	for _, want := range []string{`"x" is not a usable number for value 2 of "position"`, `"q" is not a usable number for value 3 of "colorStart"`} {
		if !hasError(errs, want) {
			t.Errorf("missing warning %q in %+v", want, errs)
		}
	}
	if hasError(errs, `"orientation"`) {
		t.Errorf("a good line must not warn: %+v", errs)
	}

	node, errs := animNode(t, "dummy", `    position 1 2 3`)
	if len(errs) != 0 || len(node.PositionKeys) != 1 || node.PositionKeys[0].Value.Z != 3 {
		t.Errorf("good position changed: %+v %+v", node.PositionKeys, errs)
	}
}

func TestAnimMeshClipParamsReportBadNumbers(t *testing.T) {
	_, errs := animNode(t, "animmesh", `    sampleperiod fast
    clipu 0.5`)
	if !hasError(errs, `"fast" is not a usable number for "sampleperiod"`) {
		t.Errorf("missing warning in %+v", errs)
	}
	if hasError(errs, `"clipu"`) {
		t.Errorf("a good value must not warn: %+v", errs)
	}
}

// Vector parameters (position, diffuse, ...) used to be skipped silently when the
// line was short or held an unusable number, leaving the default in place.
func TestVectorParamsReportShortAndBadLines(t *testing.T) {
	src := `newmodel vec
setsupermodel vec NULL
classification CHARACTER
setanimationscale 1.0
beginmodelgeom vec
  node trimesh m
    parent NULL
    position 1 2
    orientation 0 0 1 q
    diffuse 0.5 0.5 zz
    ambient 0.1 0.2 0.3
    wirecolor 1
  endnode
endmodelgeom
donemodel vec
`
	pr, err := Parse(strings.NewReader(src))
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{
		`"position" has 2 value(s), expected 3`,
		`"orientation" has an unusable number (0 0 1 q)`,
		`"diffuse" has an unusable number (0.5 0.5 zz)`,
		`"wirecolor" has 1 value(s), expected 3`,
	} {
		if !hasError(pr.Errors, want) {
			t.Errorf("missing warning %q in %+v", want, pr.Errors)
		}
	}
	if hasError(pr.Errors, `"ambient"`) {
		t.Errorf("a good line must not warn: %+v", pr.Errors)
	}
	n := pr.Model.RootNode()
	if n == nil || n.Mesh == nil || n.Mesh.Ambient != (Vec3{X: 0.1, Y: 0.2, Z: 0.3}) {
		t.Errorf("good value should apply: %+v", n)
	}
}

func TestAnimNodeShortStaticLinesAreReported(t *testing.T) {
	_, errs := animNode(t, "dummy", `    position 1 2
    orientation 0 0 1
    colorStart 0.1 0.2`)
	for _, want := range []string{`"position" has 2 value(s), expected 3`, `"orientation" has 3 value(s), expected 4`, `"colorStart" has 2 value(s), expected 3`} {
		if !hasError(errs, want) {
			t.Errorf("missing warning %q in %+v", want, errs)
		}
	}
}

// The first line of every file we write names the tool. It used to say
// "cleanmodels-go", the name of the rewrite's branch, not of the product.
func TestWriterHeaderNamesTheTool(t *testing.T) {
	m := mustParseASCII(t, aabbDegenerateSrc)
	var buf bytes.Buffer
	if err := Write(m, &buf); err != nil {
		t.Fatal(err)
	}
	first, _, _ := strings.Cut(buf.String(), "\n")
	if first != "# Rewritten by cleanmodels" {
		t.Errorf("first line = %q", first)
	}
}
