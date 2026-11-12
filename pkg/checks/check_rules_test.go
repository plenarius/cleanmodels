package checks

import (
	"bytes"
	"reflect"
	"strings"
	"testing"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

// Tests for the rules corrected after the Phase 2 check of Layonara's haks
// (94,719 ASCII models), where each of them reported valid models.

func parseASCII(t *testing.T, src string) *mdl.Model {
	t.Helper()
	pr, err := mdl.Parse(strings.NewReader(src))
	if err != nil {
		t.Fatal(err)
	}
	pr.Model.FileType = "ascii"
	return pr.Model
}

// A quad with UVs and normals, enough for the compiler to generate tangents.
const quadGeometry = `
    verts 4
      0 0 0
      1 0 0
      1 1 0
      0 1 0
    tverts 4
      0 0 0
      1 0 0
      1 1 0
      0 1 0
    faces 2
      0 1 2 1 0 1 2 0
      0 2 3 1 0 2 3 0
`

func meshModel(name, root, meshName, meshParent, meshFields string) string {
	return "newmodel " + name + "\nsetsupermodel " + name + " NULL\nclassification CHARACTER\nbeginmodelgeom " + name + "\n" +
		root +
		"  node trimesh " + meshName + "\n    parent " + meshParent + "\n" + meshFields + quadGeometry + "  endnode\n" +
		"endmodelgeom " + name + "\ndonemodel " + name + "\n"
}

func dummyRoot(name string) string {
	return "  node dummy " + name + "\n    parent NULL\n  endnode\n"
}

// pfb0_legl082 and 145 other Layonara models: a dummy root named after the
// model, and a mesh with the same name under it. The model has its root; the
// rule used to find "the root" by name, land on the mesh, and its fix
// detached the mesh from the root.
func TestBaseDummyParentNullUsesTheNullParentNode(t *testing.T) {
	m := parseASCII(t, meshModel("pfb0_legl082", dummyRoot("pfb0_legl082"), "pfb0_legl082", "pfb0_legl082", "    bitmap pfb0_legl082\n"))
	for _, fix := range []bool{false, true} {
		if res := checkBaseDummyParentNull(m, "pfb0_legl082.mdl", fix); len(res) != 0 {
			t.Errorf("fix=%v: a model with a NULL-parent root got %+v", fix, res)
		}
	}
	if p := m.Nodes[1].Parent; p != "pfb0_legl082" {
		t.Errorf("the mesh was moved: parent %q", p)
	}
}

// A model with no NULL-parent node at all still gets the finding and the fix.
func TestBaseDummyParentNullNoRoot(t *testing.T) {
	src := strings.Replace(meshModel("box", "  node dummy box\n    parent rootdummy\n  endnode\n", "mesh", "box", "    bitmap box\n"),
		"node dummy box", "node dummy rootdummy\n    parent box\n  endnode\n  node dummy box", 1)
	m := parseASCII(t, src)
	res := checkBaseDummyParentNull(m, "box.mdl", false)
	if len(res) != 1 || res[0].Fixed || res[0].Severity != mdl.SevError || !strings.Contains(res[0].Message, "would be set to NULL") {
		t.Fatalf("without fix: %+v", res)
	}
	res = checkBaseDummyParentNull(m, "box.mdl", true)
	if len(res) != 1 || !res[0].Fixed {
		t.Fatalf("with fix: %+v", res)
	}
	for _, n := range m.Nodes {
		if n.Name == "box" && n.Parent != "NULL" {
			t.Errorf("box parent %q after the fix", n.Parent)
		}
	}
}

// pmh0_chest201 and pmh0_head154: the mesh is the root. They compile and draw
// correctly in game, so the rule warns rather than calling them fatal.
func TestBaseDummyWrongTypeIsAWarning(t *testing.T) {
	src := "newmodel pmh0_chest201\nsetsupermodel pmh0_chest201 NULL\nclassification CHARACTER\nbeginmodelgeom pmh0_chest201\n" +
		"  node trimesh pmh0_chest201\n    parent NULL\n    bitmap pmh0_chest201\n" + quadGeometry + "  endnode\n" +
		"endmodelgeom pmh0_chest201\ndonemodel pmh0_chest201\n"
	res := checkBaseDummyWrongType(parseASCII(t, src), "pmh0_chest201.mdl", false)
	if len(res) != 1 || res[0].Severity != mdl.SevWarning {
		t.Fatalf("results = %+v", res)
	}
	for _, e := range ListAll() {
		if e.Name == "base_dummy_wrong_type" && !strings.Contains(e.Description, "warning") {
			t.Errorf("the rule's description should say why it is a warning: %q", e.Description)
		}
	}
}

// tmd18c and plc_hd: a renderhint with no materialname, and a materialname
// with no renderhint, are valid EE setups (MDL-MESH-040/041). The rule notes
// them at info level and never clears them, with or without fix.
func TestRenderhintConsistencyKeepsValidSetups(t *testing.T) {
	for _, fields := range []string{
		"    bitmap tmd18c_floor\n    renderhint NormalAndSpecMapped\n",
		"    bitmap plc_hd_wood\n    materialname plc_hd_wood\n",
	} {
		m := parseASCII(t, meshModel("tmd18c_a_001_2", dummyRoot("tmd18c_a_001_2"), "floor_060", "tmd18c_a_001_2", fields))
		mesh := m.Nodes[1].Mesh
		hint, mat := mesh.RenderHint, mesh.MaterialName
		for _, fix := range []bool{false, true} {
			res := checkRenderhintConsistency(m, "t.mdl", fix)
			if len(res) != 1 || res[0].Severity != mdl.SevInfo || res[0].Fixed {
				t.Errorf("%q fix=%v: %+v", fields, fix, res)
			}
			if mesh.RenderHint != hint || mesh.MaterialName != mat {
				t.Errorf("%q fix=%v: cleared to %q/%q", fields, fix, mesh.RenderHint, mesh.MaterialName)
			}
		}
	}
	for _, e := range ListAll() {
		if e.Name == "renderhint_consistency" && e.Fixable {
			t.Error("renderhint_consistency must not be fixable")
		}
	}
}

// PLC_A04's Box1194: an ASCII mesh with NormalAndSpecMapped and no tangents is
// fine, because the compiler generates them (MDL-MESH-042).
func TestTangentValidationASCIINeedsNoTangents(t *testing.T) {
	m := parseASCII(t, meshModel("plc_a04", dummyRoot("plc_a04"), "Box1194", "plc_a04", "    bitmap plc_a04\n    renderhint NormalAndSpecMapped\n"))
	if res := checkTangentValidation(m, "PLC_A04.mdl", false); len(res) != 0 {
		t.Errorf("ASCII input: %+v", res)
	}
}

// A binary is drawn with the tangents it stores (E31): a normal-mapped mesh of
// a binary that has none is still reported.
func TestTangentValidationBinaryWithoutTangents(t *testing.T) {
	mesh := mdl.NewMeshData()
	mesh.RenderHint = "NormalAndSpecMapped"
	m := &mdl.Model{FileType: "binary", Nodes: []*mdl.Node{{Name: "Box1194", Mesh: mesh}}}
	res := checkTangentValidation(m, "plc_a04.mdl", false)
	if len(res) != 1 || res[0].Severity != mdl.SevWarning {
		t.Fatalf("results = %+v", res)
	}
}

// A binary that stores tangents a compile would generate again decompiles
// without them (omitGeneratedTangents). The binary has them: no finding.
func TestTangentValidationBinaryWithOmittedTangents(t *testing.T) {
	src := meshModel("plc_a04", dummyRoot("plc_a04"), "Box1194", "plc_a04", "    bitmap plc_a04\n    renderhint NormalAndSpecMapped\n")
	var bin bytes.Buffer
	if err := mdl.Compile(parseASCII(t, src), &bin); err != nil {
		t.Fatal(err)
	}
	m, err := mdl.Decompile(bytes.NewReader(bin.Bytes()), int64(bin.Len()))
	if err != nil {
		t.Fatal(err)
	}
	var mesh *mdl.MeshData
	for _, n := range m.Nodes {
		if strings.EqualFold(n.Name, "Box1194") {
			mesh = n.Mesh
		}
	}
	if mesh == nil || len(mesh.Tangents) != 0 || !mesh.TangentsOmitted {
		t.Fatalf("expected the decompile to leave the generated tangents out and say so: %+v", mesh)
	}
	if res := checkTangentValidation(m, "plc_a04.mdl", false); len(res) != 0 {
		t.Errorf("binary with tangents: %+v", res)
	}
}

// Armour parts are textured from their PLT whatever their bitmap (E30): no
// null_bitmaps finding, and its fix (render 0) must not hide them.
func TestNullBitmapsSkipsArmourParts(t *testing.T) {
	for _, name := range []string{"pfb0_legl081", "pmh2_head042", "pfd0_forer115", "pmd0_bicepl109", "pfs0_chest001"} {
		m := parseASCII(t, meshModel(name, dummyRoot(name), name+"g", name, ""))
		mesh := m.Nodes[1].Mesh
		if mesh.Bitmap != "" || mesh.Render != 1 {
			t.Fatalf("%s: test model has bitmap %q render %d", name, mesh.Bitmap, mesh.Render)
		}
		for _, fix := range []bool{false, true} {
			if res := checkNullBitmaps(m, "parts/"+name+".mdl", fix); len(res) != 0 {
				t.Errorf("%s fix=%v: %+v", name, fix, res)
			}
		}
		if mesh.Render != 1 {
			t.Errorf("%s: render set to %d", name, mesh.Render)
		}
	}
	// Other models, including placeables whose names start with p, still get it.
	for _, name := range []string{"plc_chunk2", "c_jester", "pmh0_foo", "pfb0_legl"} {
		m := parseASCII(t, meshModel(name, dummyRoot(name), "mesh", name, ""))
		res := checkNullBitmaps(m, name+".mdl", false)
		if len(res) != 1 || res[0].Fixed || !strings.Contains(res[0].Message, "would set render=0") {
			t.Errorf("%s: %+v", name, res)
		}
	}
}

// Every fixable rule run without fix reports what it would do and does not
// say it did it.
func TestFixableMessagesWithoutFix(t *testing.T) {
	src := "newmodel m\nsetsupermodel m NULL\nclassification CHARACTER\nbeginmodelgeom m\n" +
		dummyRoot("m") + dummyRoot("dup") + "  node dummy dup\n    parent m\n  endnode\n" +
		"  node dummy orphan\n    parent nowhere\n  endnode\n" +
		"  node trimesh mesh\n    parent m\n" + quadGeometry + "  endnode\n" +
		"endmodelgeom m\n" +
		"newanim walk m\n  length 1\n  transtime -1\n  animroot missing\n  node dummy m\n    parent NULL\n  endnode\ndoneanim walk m\n" +
		"newanim walk m\n  length 1\n  node dummy m\n    parent NULL\n  endnode\ndoneanim walk m\n" +
		"donemodel m\n"
	m := parseASCII(t, src)
	res := RunAll(m, "m.mdl", false)
	if len(res) == 0 {
		t.Fatal("no findings")
	}
	pastTense := []string{" renamed to", " reparented to", " merged", " set to ", " clamped to", " truncated", " adjusted", " renormalized", "set render=0", " cleared"}
	for _, r := range res {
		if r.Fixed {
			t.Errorf("%s: fixed without fix", r.Check)
		}
		for _, p := range pastTense {
			if strings.Contains(r.Message, p) && !strings.Contains(r.Message, "would") {
				t.Errorf("%s says it changed the model: %q", r.Check, r.Message)
			}
		}
	}
}

// layonara/nwn-haks#104: four tps01 doors had a pasted tail after donemodel.
// check reports it, naming the nodes the game does not read and the
// animations it does.
func TestTextAfterDoneModel(t *testing.T) {
	src := meshModel("tps01_gdoor_01", dummyRoot("tps01_gdoor_01"), "door", "tps01_gdoor_01", "    bitmap door\n") +
		"e\nnode dummy tps01_gdoor_01impc\n  parent tps01_gdoor_01\nendnode\n" +
		"newanim opened2 tps01_gdoor_01\n  length 1\n  node dummy tps01_gdoor_01\n    parent NULL\n  endnode\ndoneanim opened2 tps01_gdoor_01\n"
	r := findCheck(RunAll(parseASCII(t, src), "tps01_gdoor_01.mdl", false), "text_after_donemodel")
	if r == nil || r.Severity != mdl.SevWarning || r.Fixed {
		t.Fatalf("result = %+v", r)
	}
	for _, want := range []string{`nodes not read: "tps01_gdoor_01impc"`, `animations read: "opened2"`} {
		if !strings.Contains(r.Message, want) {
			t.Errorf("message lacks %q: %q", want, r.Message)
		}
	}
	clean := parseASCII(t, meshModel("box", dummyRoot("box"), "mesh", "box", "    bitmap box\n"))
	if r := findCheck(RunAll(clean, "box.mdl", false), "text_after_donemodel"); r != nil {
		t.Errorf("a model that ends at donemodel: %+v", r)
	}
}

// zspec_animdup (nwn-mdl-spec MDL-ASCII-062): swing (1 s), idle, swing (2 s),
// SWING (3 s). The game keeps all four but plays only the first swing. The
// rule warns and names that copy; its fix keeps it unchanged and drops the
// later ones, never merging them into it.
func TestDuplicateAnimationsKeepsTheFirst(t *testing.T) {
	pr, err := mdl.ParseFile("../../tests/fixtures/engine/zspec_animdup.ascii.mdl")
	if err != nil {
		t.Fatal(err)
	}
	m := pr.Model
	if len(m.Animations) != 4 {
		t.Fatalf("%d animations in the probe", len(m.Animations))
	}
	first := m.Animations[0]
	firstNodes := append([]mdl.AnimNode(nil), first.Nodes...)

	res := checkDuplicateAnimations(m, "zspec_animdup.mdl", false)
	if len(res) != 1 || res[0].Severity != mdl.SevWarning || res[0].Fixed {
		t.Fatalf("without fix: %+v", res)
	}
	for _, want := range []string{`animation "swing" (animation 1 of 4)`, `"swing", "SWING" (animation 3, 4)`, "plays only the first", "would be dropped"} {
		if !strings.Contains(res[0].Message, want) {
			t.Errorf("message lacks %q: %q", want, res[0].Message)
		}
	}
	if len(m.Animations) != 4 {
		t.Fatal("check without fix changed the animations")
	}

	res = checkDuplicateAnimations(m, "zspec_animdup.mdl", true)
	if len(res) != 1 || !res[0].Fixed {
		t.Fatalf("with fix: %+v", res)
	}
	var names []string
	for _, a := range m.Animations {
		names = append(names, a.Name)
	}
	if strings.Join(names, " ") != "swing idle" {
		t.Fatalf("animations after the fix: %v", names)
	}
	kept := m.Animations[0]
	if kept.Length != first.Length || kept.TransTime != first.TransTime || len(kept.Events) != len(first.Events) ||
		!reflect.DeepEqual(kept.Nodes, firstNodes) {
		t.Errorf("the first swing changed: length %g (was %g), %d events (was %d)", kept.Length, first.Length, len(kept.Events), len(first.Events))
	}
}

// Junk after donemodel that the game ignores entirely is information.
func TestTextAfterDoneModelJunkIsInfo(t *testing.T) {
	src := meshModel("c_1b_hu_fem01", dummyRoot("c_1b_hu_fem01"), "mesh", "c_1b_hu_fem01", "    bitmap m\n") +
		"</snoopstart>\n</snoopend>\ndonemodel c_1b_hu_fem01\n"
	r := findCheck(RunAll(parseASCII(t, src), "c_1b_hu_fem01.mdl", false), "text_after_donemodel")
	if r == nil || r.Severity != mdl.SevInfo || !strings.Contains(r.Message, "none of which the game reads") {
		t.Fatalf("result = %+v", r)
	}
}

// The spec's probe zspec_animroot (MDL-NODE-045): the engine prunes an
// animation only to a node it has of exactly the animroot's name. An animroot
// that matches none (nowhere, another case, a node only the geometry has)
// plays the whole animation: a warning, never fixed, since any other animroot
// would make a compile prune. c_wraith_b1, wbwsh_t_011, wbwxh_t_011 and
// wwhip_t_001 were reported as errors, with a fix that pruned them.
func TestAnimationRootMatchingNoNodeIsAWarning(t *testing.T) {
	pr, err := mdl.ParseFile("../../tests/fixtures/engine/zspec_animroot.ascii.mdl")
	if err != nil {
		t.Fatal(err)
	}
	m := pr.Model
	want := map[string]string{
		"missing":  `animroot "nosuchnode", which matches no node`,
		"case":     `animroot "ARM", which names node "arm" only in another case`,
		"geomonly": `animroot "leg", which names node "leg", which the animation does not have`,
	}
	roots := map[string]string{}
	for _, a := range m.Animations {
		roots[a.Name] = a.Root
	}
	for _, fix := range []bool{false, true} {
		res := checkAnimationRoot(m, "zspec_animroot.mdl", fix)
		if len(res) != len(want) {
			t.Fatalf("fix=%v: %d findings, want %d: %+v", fix, len(res), len(want), res)
		}
		for _, r := range res {
			w, ok := want[r.Node]
			if !ok {
				t.Errorf("fix=%v: finding on animation %q, which has a node of its animroot's exact name: %s", fix, r.Node, r.Message)
				continue
			}
			if r.Severity != mdl.SevWarning || r.Fixed {
				t.Errorf("fix=%v %s: severity %v fixed %v, want a warning not fixed", fix, r.Node, r.Severity, r.Fixed)
			}
			for _, s := range []string{w, "the whole animation plays", "left as written"} {
				if !strings.Contains(r.Message, s) {
					t.Errorf("fix=%v %s: message lacks %q: %q", fix, r.Node, s, r.Message)
				}
			}
		}
		for _, a := range m.Animations {
			if a.Root != roots[a.Name] {
				t.Errorf("fix=%v changed the animroot of %s from %q to %q", fix, a.Name, roots[a.Name], a.Root)
			}
		}
	}
	// The full fix run leaves every animation's tree and animroot alone.
	before := append([]mdl.Animation(nil), m.Animations...)
	RunAll(m, "zspec_animroot.mdl", true)
	for i, a := range m.Animations {
		if a.Root != before[i].Root || len(a.Nodes) != len(before[i].Nodes) {
			t.Errorf("RunAll with fix changed %s: animroot %q (was %q), %d nodes (was %d)", a.Name, a.Root, before[i].Root, len(a.Nodes), len(before[i].Nodes))
		}
	}
}

// NULL, in any case, and the model's own name prune nothing: no finding.
func TestAnimationRootNullAndModelName(t *testing.T) {
	src := meshModel("c_test", dummyRoot("c_test"), "body", "c_test", "    bitmap b\n") +
		"newanim a c_test\n  length 1\n  animroot null\n  node dummy c_test\n    parent NULL\n  endnode\ndoneanim a c_test\n" +
		"newanim b c_test\n  length 1\n  animroot C_TEST\n  node dummy c_test\n    parent NULL\n  endnode\ndoneanim b c_test\n" +
		"donemodel c_test\n"
	m := parseASCII(t, src)
	if len(m.Animations) != 2 {
		t.Fatalf("%d animations", len(m.Animations))
	}
	if res := checkAnimationRoot(m, "c_test.mdl", true); len(res) != 0 {
		t.Fatalf("findings: %+v", res)
	}
	if m.Animations[0].Root != "null" || m.Animations[1].Root != "C_TEST" {
		t.Fatalf("animroots changed: %q %q", m.Animations[0].Root, m.Animations[1].Root)
	}
}

func nameModel(names ...string) string {
	var b strings.Builder
	b.WriteString(dummyRoot("c_test"))
	for _, n := range names {
		b.WriteString("  node dummy " + n + "\n    parent c_test\n  endnode\n")
	}
	return "newmodel c_test\nsetsupermodel c_test NULL\nclassification CHARACTER\nbeginmodelgeom c_test\n" +
		b.String() + "endmodelgeom c_test\ndonemodel c_test\n"
}

func nodeNames(m *mdl.Model) []string {
	var out []string
	for _, n := range m.Nodes {
		out = append(out, n.Name)
	}
	return out
}

// MDL-BIN-007: the engine keeps the first 31 characters of a node name. A
// 32-character name whose cut no other node has (29 names in 19 Layonara
// heads, approved in Phase 2 as name-cut-31) finds the same node: a warning,
// and the fix cuts it as the engine does.
func TestNodeNameLengthUniqueCutIsAWarning(t *testing.T) {
	long := "model001_25_mamiyahead_0_5_0_001" // 32
	cut := long[:31]
	// The same name twice is no new collision (pfh0_head073, pmh0_head216).
	m := parseASCII(t, nameModel(long, "short", strings.ToUpper(long)))
	res := checkNodeNameLength(m, "pfh0_head058.mdl", false)
	if len(res) != 2 {
		t.Fatalf("%d findings: %+v", len(res), res)
	}
	for _, r := range res {
		if r.Severity != mdl.SevWarning || r.Fixed {
			t.Errorf("severity %v fixed %v, want an unfixed warning: %s", r.Severity, r.Fixed, r.Message)
		}
		for _, s := range []string{"is 32 characters", "keeps the first 31", `"` + cut + `"`, "would be cut"} {
			if !strings.Contains(strings.ToLower(r.Message), s) {
				t.Errorf("message lacks %q: %q", s, r.Message)
			}
		}
	}
	if nodeNames(m)[1] != long {
		t.Fatal("check without fix renamed the node")
	}

	res = checkNodeNameLength(m, "pfh0_head058.mdl", true)
	if len(res) == 0 || res[0].Severity != mdl.SevWarning || !res[0].Fixed {
		t.Fatalf("with fix: %+v", res)
	}
	if got := strings.Join(nodeNames(m), " "); got != "c_test "+cut+" short "+cut {
		t.Fatalf("names after the fix: %s", got)
	}

	// 31 characters is within the limit.
	m = parseASCII(t, nameModel(cut))
	if res := checkNodeNameLength(m, "x.mdl", false); len(res) != 0 {
		t.Fatalf("31 characters: %+v", res)
	}
}

// Two names that are equal once cut (case aside) can make the engine find the
// wrong node: an error, and the fix gives the long name a unique short form.
func TestNodeNameLengthCollisionIsAnError(t *testing.T) {
	a := "model001_5_unknownmaterial_5__00" // 32
	for _, other := range []string{
		"model001_5_unknownmaterial_5__01",   // another 32-character name, the same cut
		"model001_5_unknownmaterial_5__0",    // the cut itself
		"MODEL001_5_UNKNOWNMATERIAL_5__0XYZ", // the cut in another case
	} {
		m := parseASCII(t, nameModel(a, other))
		res := checkNodeNameLength(m, "pmh0_head088.mdl", false)
		var errs int
		for _, r := range res {
			if r.Severity == mdl.SevError {
				errs++
				if !strings.Contains(r.Message, "can find the wrong node") {
					t.Errorf("message: %q", r.Message)
				}
			}
		}
		if wantErrs := 1 + boolInt(len(other) > 31); errs != wantErrs || len(res) != wantErrs {
			t.Fatalf("%s: %d errors in %d findings, want %d: %+v", other, errs, len(res), wantErrs, res)
		}

		checkNodeNameLength(m, "pmh0_head088.mdl", true)
		seen := map[string]bool{}
		for _, n := range nodeNames(m) {
			if len(n) > 31 {
				t.Errorf("%s: %q still over 31 after the fix", other, n)
			}
			if seen[strings.ToLower(cutNodeName(n))] {
				t.Errorf("%s: names still collide after the fix: %v", other, nodeNames(m))
			}
			seen[strings.ToLower(cutNodeName(n))] = true
		}
	}
}

func boolInt(b bool) int {
	if b {
		return 1
	}
	return 0
}
