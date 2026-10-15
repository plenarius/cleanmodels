package checks

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

// skinModel is a three-bone tree with a skin node under the last bone. The
// weights are the caller's, one entry per vertex.
func skinModel(weights []mdl.VertexWeight) *mdl.Model {
	mesh := mdl.NewMeshData()
	for range weights {
		mesh.Verts = append(mesh.Verts, mdl.Vec3{})
	}
	return &mdl.Model{
		Name: "t",
		Nodes: []*mdl.Node{
			{Name: "t", Parent: "NULL"},
			{Name: "torso_g", Parent: "t"},
			{Name: "neck_g", Parent: "torso_g"},
			{Name: "head_g", Parent: "neck_g"},
			{Name: "skin_head", Parent: "head_g", Mesh: mesh, Skin: &mdl.SkinData{Weights: weights}},
		},
	}
}

func TestSkinUnknownBoneNamesTheBoneAndItsVertices(t *testing.T) {
	m := skinModel([]mdl.VertexWeight{
		{Bones: []string{"head_g", "Spine_X"}, Weights: []float32{0.6, 0.4}},
		{Bones: []string{"head_g", "spine_x"}, Weights: []float32{0.9, 0.1}},
		{Bones: []string{"head_g"}, Weights: []float32{1}},
	})
	r := findCheck(RunAll(m, "t.mdl", false), "skin_unknown_bone")
	if r == nil {
		t.Fatal("weights naming a bone that is not a node were not reported")
	}
	if r.Severity != mdl.SevWarning || r.Node != "skin_head" {
		t.Errorf("severity %v node %q", r.Severity, r.Node)
	}
	for _, want := range []string{"Spine_X (2 vertices)", "1 bone(s)"} {
		if !strings.Contains(r.Message, want) {
			t.Errorf("message lacks %q: %s", want, r.Message)
		}
	}
}

func TestSkinUnknownBoneIgnoresBonesThatAreNodesAndZeroWeights(t *testing.T) {
	m := skinModel([]mdl.VertexWeight{
		{Bones: []string{"HEAD_G", "torso_g"}, Weights: []float32{0.7, 0.3}},
		{Bones: []string{"head_g", "ghost"}, Weights: []float32{1, 0}},
	})
	if r := findCheck(RunAll(m, "t.mdl", false), "skin_unknown_bone"); r != nil {
		t.Errorf("reported a clean skin: %s", r.Message)
	}
}

func TestSkinInfluenceCountReportsMoreThanFour(t *testing.T) {
	m := skinModel([]mdl.VertexWeight{
		{Bones: []string{"t", "torso_g", "neck_g", "head_g", "skin_head"}, Weights: []float32{.2, .2, .2, .2, .2}},
		{Bones: []string{"head_g"}, Weights: []float32{1}},
	})
	r := findCheck(RunAll(m, "t.mdl", false), "skin_influence_count")
	if r == nil {
		t.Fatal("five influences on a vertex were not reported")
	}
	if !strings.Contains(r.Message, "1 of 2 vertices") || !strings.Contains(r.Message, "most: 5") {
		t.Errorf("message: %s", r.Message)
	}
	clean := skinModel([]mdl.VertexWeight{{Bones: []string{"t", "torso_g", "neck_g", "head_g"}, Weights: []float32{.25, .25, .25, .25}}})
	if r := findCheck(RunAll(clean, "t.mdl", false), "skin_influence_count"); r != nil {
		t.Errorf("four influences were reported: %s", r.Message)
	}
}

func TestSkinSummaryHasStructureAndNoCoordinates(t *testing.T) {
	m := skinModel([]mdl.VertexWeight{
		{Bones: []string{"head_g", "torso_g"}, Weights: []float32{0.905, 0.095}},
		{Bones: []string{"head_g", "ghost"}, Weights: []float32{0.5, 0.5}},
	})
	m.Nodes[4].Mesh.Verts[0] = mdl.Vec3{X: 12.3456, Y: 7.891, Z: -4.5}
	r := findCheck(RunAll(m, "t.mdl", false), "skin_summary")
	if r == nil {
		t.Fatal("no summary")
	}
	if r.Severity != mdl.SevInfo {
		t.Errorf("severity %v, want info", r.Severity)
	}
	for _, want := range []string{
		`skin "skin_head" @4`,
		"under [t > torso_g > neck_g > head_g]",
		"2 vertices", "3 bone(s)",
		"torso_g@1 (ancestor)", "head_g@3 (ancestor)", "ghost (NOT IN MODEL)",
		"influences per vertex 2-2", "weight sum 1.000-1.000",
	} {
		if !strings.Contains(r.Message, want) {
			t.Errorf("summary lacks %q:\n%s", want, r.Message)
		}
	}
	for _, leak := range []string{"12.3456", "7.891", "-4.5"} {
		if strings.Contains(r.Message, leak) {
			t.Errorf("summary leaks a coordinate %q", leak)
		}
	}
}

func TestSkinSummaryFlagsWeightCountMismatch(t *testing.T) {
	m := skinModel([]mdl.VertexWeight{{Bones: []string{"head_g"}, Weights: []float32{1}}})
	m.Nodes[4].Mesh.Verts = append(m.Nodes[4].Mesh.Verts, mdl.Vec3{}, mdl.Vec3{})
	r := findCheck(RunAll(m, "t.mdl", false), "skin_summary")
	if r == nil || !strings.Contains(r.Message, "WEIGHT COUNT DIFFERS FROM VERTEX COUNT (1 vs 3)") {
		t.Errorf("mismatch not flagged: %+v", r)
	}
}

// c_marilith2's tail is a real skin compiled by the engine: its bones are all
// nodes and its weights are tidy, so only the summary speaks.
func TestSkinChecksOnARealSkin(t *testing.T) {
	m := decompileFixture(t, "oracle/game_binary", "c_marilith2.mdl")
	results := RunAll(m, "c_marilith2.mdl", false)
	for _, name := range []string{"skin_unknown_bone", "skin_influence_count"} {
		if r := findCheck(results, name); r != nil {
			t.Errorf("%s reported on the engine's own skin: %s", name, r.Message)
		}
	}
	r := findCheck(results, "skin_summary")
	if r == nil || !strings.Contains(r.Message, `skin "tailmesh"`) || strings.Contains(r.Message, "NOT IN MODEL") {
		t.Errorf("summary: %+v", r)
	}
}

// neckPart is a body part shaped like Beamdog's HD neck: a root dummy, the
// skin and a shadow mesh. Its bones belong to the pmh0 skeleton, not to it.
func neckPart(weights []mdl.VertexWeight) *mdl.Model {
	mesh := mdl.NewMeshData()
	for range weights {
		mesh.Verts = append(mesh.Verts, mdl.Vec3{})
	}
	return &mdl.Model{
		Name: "pmh0_neck001",
		Nodes: []*mdl.Node{
			{Name: "pmh0_neck001", Parent: "NULL"},
			{Name: "pmh0_neck001g", Parent: "pmh0_neck001", Mesh: mesh, Skin: &mdl.SkinData{Weights: weights}},
			{Name: "pmh0_neck001gs", Parent: "pmh0_neck001", Mesh: mdl.NewMeshData()},
		},
	}
}

func TestSkinPartBindingAcceptsSkeletonBones(t *testing.T) {
	m := neckPart([]mdl.VertexWeight{
		{Bones: []string{"neck_g", "torso_g"}, Weights: []float32{0.6, 0.4}},
		{Bones: []string{"head_g"}, Weights: []float32{1}},
	})
	res := RunAll(m, "pmh0_neck001.mdl", false)
	for _, check := range []string{"skin_part_binding", "skin_unknown_bone"} {
		if r := findCheck(res, check); r != nil {
			t.Errorf("%s reported a part bound to skeleton bones: %s", check, r.Message)
		}
	}
}

// What the decompiler used to make of the engine's HD neck: torso_g named
// after the part's shadow mesh, neck_g and head_g lost.
func TestSkinPartBindingReportsThePartsOwnNodes(t *testing.T) {
	m := neckPart([]mdl.VertexWeight{
		{Bones: []string{"pmh0_neck001gs"}, Weights: []float32{1}},
		{Bones: []string{"neck_g", "pmh0_neck001gs"}, Weights: []float32{0.9, 0.1}},
		{Bones: []string{"neck_g"}, Weights: []float32{1}},
	})
	r := findCheck(RunAll(m, "pmh0_neck001.mdl", false), "skin_part_binding")
	if r == nil {
		t.Fatal("a body part weighted to its own shadow mesh was not reported")
	}
	if r.Severity != mdl.SevWarning || r.Node != "pmh0_neck001g" {
		t.Errorf("severity %v node %q", r.Severity, r.Node)
	}
	for _, want := range []string{"pmh0_neck001gs (2 vertices)", "skeleton pmh0", "1 vertices have no other bone"} {
		if !strings.Contains(r.Message, want) {
			t.Errorf("message lacks %q: %s", want, r.Message)
		}
	}
}

// Beamdog's HD robes carry trimeshes named like skeleton nodes (torso_g,
// pelvis_g). The game binds bones by name, so weights on them are skeleton
// bones, not the part's own nodes; an earlier version of the check flagged
// every robe.
func TestSkinPartBindingAcceptsMeshesNamedLikeSkeletonNodes(t *testing.T) {
	mesh := mdl.NewMeshData()
	mesh.Verts = []mdl.Vec3{{}}
	m := &mdl.Model{
		Name: "pfa0_robe003",
		Nodes: []*mdl.Node{
			{Name: "pfa0_robe003", Parent: "NULL"},
			{Name: "pfa0_robe003g", Parent: "pfa0_robe003", Mesh: mesh, Skin: &mdl.SkinData{Weights: []mdl.VertexWeight{
				{Bones: []string{"torso_g", "pelvis_g"}, Weights: []float32{0.5, 0.5}},
			}}},
			{Name: "rootdummy", Parent: "pfa0_robe003"},
			{Name: "torso_g", Parent: "rootdummy", Mesh: mdl.NewMeshData()},
			{Name: "pelvis_g", Parent: "rootdummy", Mesh: mdl.NewMeshData()},
		},
	}
	if r := findCheck(RunAll(m, "pfa0_robe003.mdl", false), "skin_part_binding"); r != nil {
		t.Errorf("reported a robe weighted to skeleton-named meshes: %s", r.Message)
	}
}

func TestSkinUnboundVertex(t *testing.T) {
	// Whole model: vertex 2 has only a zero weight, vertex 3 only a bone that
	// is not a node; vertices 0 and 1 are fine.
	m := skinModel([]mdl.VertexWeight{
		{Bones: []string{"head_g"}, Weights: []float32{1}},
		{Bones: []string{"head_g", "ghost"}, Weights: []float32{0.5, 0.5}},
		{Bones: []string{"head_g"}, Weights: []float32{0}},
		{Bones: []string{"ghost"}, Weights: []float32{1}},
	})
	r := findCheck(RunAll(m, "t.mdl", false), "skin_unbound_vertex")
	if r == nil || !strings.Contains(r.Message, "2 of 4 vertices") {
		t.Fatalf("got %v, want 2 of 4 unbound vertices", r)
	}
	// Body part: skeleton bones are usable even though the file lacks them;
	// a bone named after the part is not.
	p := neckPart([]mdl.VertexWeight{
		{Bones: []string{"neck_g"}, Weights: []float32{1}},
		{Bones: []string{"pmh0_neck001gs"}, Weights: []float32{1}},
	})
	r = findCheck(RunAll(p, "pmh0_neck001.mdl", false), "skin_unbound_vertex")
	if r == nil || !strings.Contains(r.Message, "1 of 2 vertices") {
		t.Fatalf("part: got %v, want 1 of 2 unbound", r)
	}
	if r := findCheck(RunAll(neckPart([]mdl.VertexWeight{{Bones: []string{"neck_g"}, Weights: []float32{1}}}), "pmh0_neck001.mdl", false), "skin_unbound_vertex"); r != nil {
		t.Errorf("clean part reported: %s", r.Message)
	}
}

func TestSkinSummaryLabelsSkeletonBonesOnParts(t *testing.T) {
	r := findCheck(RunAll(neckPart([]mdl.VertexWeight{{Bones: []string{"neck_g"}, Weights: []float32{1}}}), "pmh0_neck001.mdl", true), "skin_summary")
	if r == nil || !strings.Contains(r.Message, "neck_g (skeleton pmh0)") || strings.Contains(r.Message, "NOT IN MODEL") {
		t.Fatalf("summary: %v", r)
	}
}

func TestSkinUnknownBoneExplainsHeads(t *testing.T) {
	mesh := mdl.NewMeshData()
	mesh.Verts = []mdl.Vec3{{}}
	m := &mdl.Model{Name: "pmh0_head001", Nodes: []*mdl.Node{
		{Name: "pmh0_head001", Parent: "NULL"},
		{Name: "pmh0_head001g", Parent: "pmh0_head001", Mesh: mesh, Skin: &mdl.SkinData{Weights: []mdl.VertexWeight{{Bones: []string{"neck_g"}, Weights: []float32{1}}}}},
	}}
	r := findCheck(RunAll(m, "pmh0_head001.mdl", false), "skin_unknown_bone")
	if r == nil || !strings.Contains(r.Message, "separate object") || !strings.Contains(r.Message, "regular body-part slot") {
		t.Fatalf("head: %v", r)
	}
}

const testPtx0Skeleton = `newmodel ptx0
setsupermodel ptx0 NULL
beginmodelgeom ptx0
node dummy ptx0
  parent NULL
endnode
node dummy torso_g
  parent ptx0
endnode
node dummy neck_g
  parent torso_g
endnode
endmodelgeom ptx0
donemodel ptx0
`

func ptx0Neck(bones ...string) *mdl.Model {
	mesh := mdl.NewMeshData()
	var w []mdl.VertexWeight
	for _, b := range bones {
		mesh.Verts = append(mesh.Verts, mdl.Vec3{})
		w = append(w, mdl.VertexWeight{Bones: []string{b}, Weights: []float32{1}})
	}
	return &mdl.Model{Name: "ptx0_neck001", Nodes: []*mdl.Node{
		{Name: "ptx0_neck001", Parent: "NULL"},
		{Name: "ptx0_neck001g", Parent: "ptx0_neck001", Mesh: mesh, Skin: &mdl.SkinData{Weights: w}},
	}}
}

func TestSkeletonAwareSkinChecks(t *testing.T) {
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "ptx0.mdl"), []byte(testPtx0Skeleton), 0o644); err != nil {
		t.Fatal(err)
	}
	run := func(m *mdl.Model, locs ...string) []mdl.CheckResult {
		return RunFilteredContext(m, "ptx0_neck001.mdl", map[string]bool{"skin_skeleton_bone": true, "part_attach_node": true}, nil, false, Context{Locations: locs})
	}

	// Skeleton found, every bone in it: nothing to report.
	if res := run(ptx0Neck("neck_g", "torso_g"), dir); len(res) != 0 {
		t.Errorf("clean part: %v", res)
	}
	// A bone the skeleton lacks.
	r := findCheck(run(ptx0Neck("neck_g", "jaw_g"), dir), "skin_skeleton_bone")
	if r == nil || !strings.Contains(r.Message, "jaw_g (1 vertices)") {
		t.Errorf("missing bone: %v", r)
	}
	// No Locations: the checks cannot resolve anything and stay silent.
	if res := run(ptx0Neck("jaw_g")); len(res) != 0 {
		t.Errorf("no locations: %v", res)
	}
	// Skeleton not found where we looked.
	r = findCheck(run(ptx0Neck("neck_g"), t.TempDir()), "part_attach_node")
	if r == nil || !strings.Contains(r.Message, "skeleton ptx0, which was not found") {
		t.Errorf("no skeleton: %v", r)
	}
	// capart.2da hangs NECK from a node the skeleton lacks.
	if err := os.WriteFile(filepath.Join(dir, "capart.2da"), []byte("2DA V2.0\n\n NAME MDLNAME NODENAME\n0 0 NECK j_kubi\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	r = findCheck(run(ptx0Neck("neck_g"), dir), "part_attach_node")
	if r == nil || !strings.Contains(r.Message, `"j_kubi"`) || !strings.Contains(r.Message, "capart.2da") {
		t.Errorf("missing attach node: %v", r)
	}
}
