package checks

import (
	"math"
	"strings"
	"testing"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

func tvertModel(sets ...[]mdl.Vec3) *mdl.Model {
	m := &mdl.MeshData{}
	for i, s := range sets {
		switch i {
		case 0:
			m.TVerts = s
		case 1:
			m.TVerts1 = s
		case 2:
			m.TVerts2 = s
		case 3:
			m.TVerts3 = s
		}
	}
	return &mdl.Model{Nodes: []*mdl.Node{{Name: "Cylinder382", Parent: "NULL", Mesh: m}}}
}

// The case from issue #2: plc_i05's Cylinder382 has one tvert of 7.34001e+23
// among otherwise sane ones. Only the bad component is replaced, so the good
// one is kept.
func TestTvertRangeFixesOnlyTheBadComponent(t *testing.T) {
	model := tvertModel([]mdl.Vec3{{X: 0, Y: 0}, {X: 0.25, Y: 7.34001e+23}, {X: 1, Y: 1}})
	res := checkTvertRange(model, "plc_i05.mdl", true)
	if len(res) != 1 || !res[0].Fixed || res[0].Node != "Cylinder382" {
		t.Fatalf("results = %+v", res)
	}
	tv := model.Nodes[0].Mesh.TVerts
	if tv[1].X != 0.25 || tv[1].Y != 0 {
		t.Errorf("tvert 1 = %+v, want (0.25, 0)", tv[1])
	}
	if tv[0].X != 0 || tv[2].X != 1 || tv[2].Y != 1 {
		t.Errorf("the sane tverts changed: %+v", tv)
	}
	if !strings.Contains(res[0].Message, "7.34001e+23") || !strings.Contains(res[0].Message, "set to 0") {
		t.Errorf("message should name the value and the action: %q", res[0].Message)
	}
}

// Without fix the check only reports.
func TestTvertRangeDetectOnly(t *testing.T) {
	model := tvertModel([]mdl.Vec3{{X: 1e30, Y: 0.5}})
	res := checkTvertRange(model, "f.mdl", false)
	if len(res) != 1 || res[0].Fixed || res[0].Severity != mdl.SevWarning {
		t.Fatalf("results = %+v", res)
	}
	if got := model.Nodes[0].Mesh.TVerts[0]; got.X != 1e30 {
		t.Errorf("detection must not change data, got %+v", got)
	}
	if !strings.Contains(res[0].Message, "would be set to 0") || strings.Contains(res[0].Message, "--fix") {
		t.Errorf("message should say what would happen, without naming a flag: %q", res[0].Message)
	}
}

// Large tiling is legitimate and must be left alone: the tde01 tiles in
// NWNCQ reach ~34,000, and the limit only bites where float32 can no longer
// hold a fraction.
func TestTvertRangeLeavesLargeButMeaningfulValues(t *testing.T) {
	keep := []mdl.Vec3{{X: 34626.3, Y: -5328.39}, {X: 192347, Y: 0}, {X: 1000, Y: 1500}, {X: float32(1<<24 - 1), Y: 0}}
	model := tvertModel(append([]mdl.Vec3(nil), keep...))
	if res := checkTvertRange(model, "f.mdl", true); len(res) != 0 {
		t.Fatalf("legitimate values flagged: %+v", res)
	}
	for i, v := range model.Nodes[0].Mesh.TVerts {
		if v != keep[i] {
			t.Errorf("tvert %d changed: %+v", i, v)
		}
	}
}

// The limit itself is included, and so are negatives.
func TestTvertRangeBoundaryAndSign(t *testing.T) {
	model := tvertModel([]mdl.Vec3{{X: float32(1 << 24), Y: 0}, {X: 0, Y: -float32(1 << 24)}, {X: 0, Y: -1e30}})
	res := checkTvertRange(model, "f.mdl", true)
	if len(res) != 1 || !strings.Contains(res[0].Message, "3 texture coordinate") {
		t.Fatalf("results = %+v", res)
	}
	for i, v := range model.Nodes[0].Mesh.TVerts {
		if v.X != 0 || v.Y != 0 {
			t.Errorf("tvert %d = %+v, want zeros", i, v)
		}
	}
}

// Binary models can carry NaN and Inf, which the ASCII parser never produces.
func TestTvertRangeNonFinite(t *testing.T) {
	nan, inf := float32(math.NaN()), float32(math.Inf(1))
	model := tvertModel([]mdl.Vec3{{X: nan, Y: 0.5}, {X: 0.5, Y: inf}, {X: 0.5, Y: float32(math.Inf(-1))}})
	res := checkTvertRange(model, "f.mdl", true)
	if len(res) != 1 {
		t.Fatalf("results = %+v", res)
	}
	for i, v := range model.Nodes[0].Mesh.TVerts {
		if math.IsNaN(float64(v.X)) || math.IsInf(float64(v.Y), 0) || v.X != 0 && i == 0 {
			t.Errorf("tvert %d not repaired: %+v", i, v)
		}
	}
	if tv := model.Nodes[0].Mesh.TVerts[1]; tv.X != 0.5 || tv.Y != 0 {
		t.Errorf("good component must be kept: %+v", tv)
	}
}

// Each UV set is reported on its own.
func TestTvertRangeReportsEachUVSet(t *testing.T) {
	model := tvertModel(
		[]mdl.Vec3{{X: 7.34001e+23, Y: 0}},
		[]mdl.Vec3{{X: 0, Y: 0}},
		[]mdl.Vec3{{X: 0, Y: 1e30}},
	)
	res := checkTvertRange(model, "f.mdl", true)
	if len(res) != 2 {
		t.Fatalf("want 2 results (sets 0 and 2), got %+v", res)
	}
	if !strings.Contains(res[0].Message, "UV set 0") || !strings.Contains(res[1].Message, "UV set 2") {
		t.Errorf("messages: %q / %q", res[0].Message, res[1].Message)
	}
}

// A clean model must not be touched or reported.
func TestTvertRangeCleanModel(t *testing.T) {
	model := tvertModel([]mdl.Vec3{{X: 0, Y: 0}, {X: 1, Y: 1}})
	if res := checkTvertRange(model, "f.mdl", true); len(res) != 0 {
		t.Fatalf("clean model produced %+v", res)
	}
	if res := checkTvertRange(nil, "f.mdl", true); res != nil {
		t.Fatalf("nil model produced %+v", res)
	}
}

// The check is registered, fixable, and part of RunAll, and it sits alongside
// the existing clean-fixture guarantee that known-good models stay warning free.
func TestTvertRangeRegistered(t *testing.T) {
	var found bool
	for _, e := range ListAll() {
		if e.Name == "tvert_range" {
			found = true
			if !e.Fixable || e.Category != "geometry" {
				t.Errorf("entry = %+v", e)
			}
		}
	}
	if !found {
		t.Fatal("tvert_range is not registered")
	}
	model := tvertModel([]mdl.Vec3{{X: 0, Y: 7.34001e+23}})
	if findCheck(RunAll(model, "f.mdl", true), "tvert_range") == nil {
		t.Error("RunAll did not run tvert_range")
	}
}

// End to end through the ASCII parser, as the reporter's file would arrive.
func TestTvertRangeFromASCII(t *testing.T) {
	src := `newmodel plc_i05
setsupermodel plc_i05 NULL
classification PLACEABLE
setanimationscale 1.00
beginmodelgeom plc_i05
  node dummy plc_i05
    parent NULL
  endnode
  node trimesh Cylinder382
    parent plc_i05
    bitmap NULL
    render 1
    verts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2 1 0 1 2 0
    tverts 3
      0.0 0.0 0
      0.0 7.34001e+023 0
      0.0 0.0 0
  endnode
endmodelgeom
donemodel plc_i05
`
	pr, err := mdl.Parse(strings.NewReader(src))
	if err != nil {
		t.Fatal(err)
	}
	res := RunAll(pr.Model, "plc_i05.mdl", true)
	r := findCheck(res, "tvert_range")
	if r == nil || !r.Fixed || r.Node != "Cylinder382" {
		t.Fatalf("tvert_range result = %+v (all: %+v)", r, res)
	}
	if got := pr.Model.FindNode("Cylinder382").Mesh.TVerts[1]; got.X != 0 || got.Y != 0 {
		t.Errorf("tvert 1 = %+v, want (0, 0)", got)
	}
}

// The third component is not used for 2D texturing, but garbage there is still
// garbage; only the bad component is replaced.
func TestTvertRangeChecksZ(t *testing.T) {
	model := tvertModel([]mdl.Vec3{{X: 0.25, Y: 0.5, Z: 1e30}, {X: 0.1, Y: 0.2, Z: 0}})
	res := checkTvertRange(model, "f.mdl", true)
	if len(res) != 1 || !strings.Contains(res[0].Message, "1 texture coordinate") {
		t.Fatalf("results = %+v", res)
	}
	if got := model.Nodes[0].Mesh.TVerts[0]; got != (mdl.Vec3{X: 0.25, Y: 0.5, Z: 0}) {
		t.Errorf("tvert 0 = %+v, want only Z zeroed", got)
	}
	if got := model.Nodes[0].Mesh.TVerts[1]; got != (mdl.Vec3{X: 0.1, Y: 0.2, Z: 0}) {
		t.Errorf("good tvert changed: %+v", got)
	}
}

// animmesh AnimTVerts, on a geometry node and on an animation node.
func TestTvertRangeAnimTVerts(t *testing.T) {
	model := &mdl.Model{
		Nodes: []*mdl.Node{{Name: "flag", Parent: "NULL", AnimMesh: &mdl.AnimMeshData{
			AnimTVerts: []mdl.Vec3{{X: 7.34e23, Y: 0.5}, {X: 0.5, Y: 0.5}},
		}}},
		Animations: []mdl.Animation{{Name: "wave", Nodes: []mdl.AnimNode{{Name: "flag", AnimMesh: &mdl.AnimMeshData{
			AnimTVerts: []mdl.Vec3{{X: 0.5, Y: float32(math.Inf(1))}},
		}}}}},
	}
	res := checkTvertRange(model, "f.mdl", false)
	if len(res) != 2 {
		t.Fatalf("results = %+v", res)
	}
	if model.Nodes[0].AnimMesh.AnimTVerts[0].X != 7.34e23 {
		t.Error("detect-only changed the data")
	}
	res = checkTvertRange(model, "f.mdl", true)
	if len(res) != 2 || !res[0].Fixed {
		t.Fatalf("fix results = %+v", res)
	}
	if got := model.Nodes[0].AnimMesh.AnimTVerts[0]; got.X != 0 || got.Y != 0.5 {
		t.Errorf("geometry AnimTVerts[0] = %+v", got)
	}
	if got := model.Animations[0].Nodes[0].AnimMesh.AnimTVerts[0]; got.X != 0.5 || got.Y != 0 {
		t.Errorf("animation AnimTVerts[0] = %+v", got)
	}
	if !strings.Contains(res[1].Message, `animation "wave"`) {
		t.Errorf("message does not name the animation: %s", res[1].Message)
	}
}
