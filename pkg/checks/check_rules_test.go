package checks

import (
	"bytes"
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
