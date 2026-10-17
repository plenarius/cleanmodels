package main

import (
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

func TestRobeTargets(t *testing.T) {
	got, err := robeTargets("", "pmh0")
	if err != nil {
		t.Fatal(err)
	}
	want := []string{"pma0", "pma2", "pmd0", "pmd2", "pme0", "pme2", "pmg0", "pmg2", "pmh2", "pmo0", "pmo2"}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("default targets for pmh0: %v, want %v", got, want)
	}
	if got, _ := robeTargets("", "pfe2"); len(got) != 11 || got[0] != "pfa0" {
		t.Errorf("default targets for pfe2: %v", got)
	}
	if got, _ := robeTargets(" PMD0, pme2 ", "pmh0"); !reflect.DeepEqual(got, []string{"pmd0", "pme2"}) {
		t.Errorf("--to list: %v", got)
	}
	if _, err := robeTargets("dwarf", "pmh0"); err == nil {
		t.Error("want an error for a target that is not a body prefix")
	}
}

// robeFixture writes a small robe (pmh0_robe999, one skin on two bones) and
// pmd0's copy of it, the bone template, with lower hips.
func robeFixture(t *testing.T, dir string) string {
	t.Helper()
	write := func(name string, hip float32, skin bool) string {
		m := &mdl.Model{Name: name, SuperModel: name[:4], Classification: "CHARACTER", AnimationScale: 1}
		add := func(n *mdl.Node) { n.Orientation = mdl.Vec4{Z: 1}; n.Scale = 1; m.Nodes = append(m.Nodes, n) }
		add(&mdl.Node{Name: name, Parent: "NULL"})
		add(&mdl.Node{Name: "pelvis_g", Parent: name, Position: mdl.Vec3{Z: hip}})
		add(&mdl.Node{Name: "lthigh_g", Parent: "pelvis_g", Position: mdl.Vec3{X: -0.1, Z: -0.4}})
		if skin {
			mesh := mdl.NewMeshData()
			mesh.Verts = []mdl.Vec3{{X: -0.1, Z: 0.5}, {X: 0.1, Z: 0.5}, {Z: 1}}
			mesh.TVerts = []mdl.Vec3{{}, {X: 1}, {Y: 1}}
			mesh.Faces = []mdl.Face{{Verts: [3]int32{0, 1, 2}, UVs: [3]int32{0, 1, 2}, SmoothGroup: 1}}
			mesh.Bitmap = "pmh0_robe999"
			add(&mdl.Node{Name: "robe", Parent: name, Mesh: mesh, Skin: &mdl.SkinData{Weights: []mdl.VertexWeight{
				{Bones: []string{"lthigh_g"}, Weights: []float32{1}},
				{Bones: []string{"lthigh_g"}, Weights: []float32{1}},
				{Bones: []string{"pelvis_g"}, Weights: []float32{1}},
			}}})
		}
		p := filepath.Join(dir, name+".mdl")
		if err := mdl.WriteFile(m, p); err != nil {
			t.Fatal(err)
		}
		return p
	}
	write("pmd0_robe999", 0.7, false)
	return write("pmh0_robe999", 1, true)
}

func TestRobesCommandWritesVariants(t *testing.T) {
	dir := t.TempDir()
	src := robeFixture(t, dir)
	out := filepath.Join(dir, "out")
	var code int
	stdout, _ := captureStdio(t, func() { code = cmdRobes([]string{"--to", "pmd0", src, out}) })
	if code != exitOK {
		t.Fatalf("exit %d: %s", code, stdout)
	}
	if !strings.Contains(stdout, "1 of 1 variants written") {
		t.Errorf("summary missing: %s", stdout)
	}
	res, err := mdl.ParseFile(filepath.Join(out, "pmd0_robe999.mdl"))
	if err != nil {
		t.Fatal(err)
	}
	if res.Model.Name != "pmd0_robe999" || res.Model.SuperModel != "pmd0" {
		t.Errorf("variant named %q, supermodel %q", res.Model.Name, res.Model.SuperModel)
	}
	if z := res.Model.FindNode("robe").Mesh.Verts[2].Z; z < 0.69 || z > 0.71 {
		t.Errorf("pelvis vertex at height %v, want the template's hip height 0.7", z)
	}

	// Again: the file exists now and is kept without --overwrite.
	captureStdio(t, func() { code = cmdRobes([]string{"--to", "pmd0", src, out}) })
	if code != exitErrors {
		t.Errorf("second run without --overwrite: exit %d, want %d", code, exitErrors)
	}
	captureStdio(t, func() { code = cmdRobes([]string{"--overwrite", "--compile", "--to", "pmd0", src, out}) })
	if code != exitOK {
		t.Errorf("--overwrite --compile: exit %d", code)
	}
	if b, err := os.ReadFile(filepath.Join(out, "pmd0_robe999.mdl")); err != nil || len(b) < 4 || b[0] != 0 {
		t.Errorf("--compile did not write a binary model (%v)", err)
	}
}

func TestRobesCommandMissingTemplate(t *testing.T) {
	dir := t.TempDir()
	src := robeFixture(t, dir)
	var code int
	stdout, _ := captureStdio(t, func() { code = cmdRobes([]string{"--to", "pme0", "--game-dir", dir, src, dir}) })
	if code != exitErrors || !strings.Contains(stdout, "no robe for pme0") {
		t.Errorf("exit %d, output %q: want a failure naming the missing template", code, stdout)
	}
}
