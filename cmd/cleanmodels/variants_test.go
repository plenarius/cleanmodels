package main

import (
	"encoding/json"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

func TestVariantTargets(t *testing.T) {
	// No bodies found: the stock races in phenotypes 0 and 2.
	got, err := variantTargets("", "pmh0", "robe", nil)
	if err != nil {
		t.Fatal(err)
	}
	want := []string{"pma0", "pma2", "pmd0", "pmd2", "pme0", "pme2", "pmg0", "pmg2", "pmh2", "pmo0", "pmo2"}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("default targets for pmh0: %v, want %v", got, want)
	}
	if got, _ := variantTargets("", "pfe2", "robe", nil); len(got) != 11 || got[0] != "pfa0" {
		t.Errorf("default targets for pfe2: %v", got)
	}
	// Bodies found: those of the source's gender with models of its kind.
	bodies := []mdl.Body{
		{Prefix: "pmh0", Gender: "m", Slots: map[string]int{"robe": 3, "chest": 9}},
		{Prefix: "pmw0", Gender: "m", Slots: map[string]int{"robe": 1, "chest": 2}},
		{Prefix: "pmh15", Gender: "m", Slots: map[string]int{"robe": 2}},
		{Prefix: "pfw0", Gender: "f", Slots: map[string]int{"robe": 1}},
	}
	if got, _ := variantTargets("", "pmh0", "robe", bodies); !reflect.DeepEqual(got, []string{"pmw0", "pmh15"}) {
		t.Errorf("robe targets from bodies: %v", got)
	}
	if got, _ := variantTargets("", "pmh0", "chest", bodies); !reflect.DeepEqual(got, []string{"pmw0"}) {
		t.Errorf("chest targets from bodies (pmh15 has no chests: the game gives it phenotype 0's): %v", got)
	}
	if got, _ := variantTargets(" PMD0, pmh15 ", "pmh0", "robe", nil); !reflect.DeepEqual(got, []string{"pmd0", "pmh15"}) {
		t.Errorf("--to list: %v", got)
	}
	if _, err := variantTargets("dwarf", "pmh0", "robe", nil); err == nil {
		t.Error("want an error for a target that is not a body prefix")
	}
}

// variantFixture writes a small robe (pmh0_robe999, one skin on two bones) and
// pmd0's copy of it, the bone template, with lower hips.
func variantFixture(t *testing.T, dir string) string {
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

func TestVariantsCommandWritesVariants(t *testing.T) {
	dir := t.TempDir()
	src := variantFixture(t, dir)
	out := filepath.Join(dir, "out")
	var code int
	stdout, _ := captureStdio(t, func() { code = cmdVariants([]string{"--to", "pmd0", src, out}) })
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
	captureStdio(t, func() { code = cmdVariants([]string{"--to", "pmd0", src, out}) })
	if code != exitErrors {
		t.Errorf("second run without --overwrite: exit %d, want %d", code, exitErrors)
	}
	captureStdio(t, func() { code = cmdVariants([]string{"--overwrite", "--compile", "--to", "pmd0", src, out}) })
	if code != exitOK {
		t.Errorf("--overwrite --compile: exit %d", code)
	}
	if b, err := os.ReadFile(filepath.Join(out, "pmd0_robe999.mdl")); err != nil || len(b) < 4 || b[0] != 0 {
		t.Errorf("--compile did not write a binary model (%v)", err)
	}
}

func TestVariantsCommandMissingTemplate(t *testing.T) {
	dir := t.TempDir()
	src := variantFixture(t, dir)
	var code int
	stdout, _ := captureStdio(t, func() { code = cmdVariants([]string{"--to", "pme0", "--game-dir", dir, src, dir}) })
	if code != exitErrors || !strings.Contains(stdout, "no robe of pme0") {
		t.Errorf("exit %d, output %q: want a failure naming the missing template", code, stdout)
	}
}

// TestVariantsJSONLines pins the event stream the GUI reads: start and done
// (or error) per target, in order, then a summary; done carries the target,
// the source and the absolute output path.
func TestVariantsJSONLines(t *testing.T) {
	dir := t.TempDir()
	src := variantFixture(t, dir)
	out := filepath.Join(dir, "out")
	var code int
	stdout, _ := captureStdio(t, func() { code = cmdVariants([]string{"--json-lines", "--to", "pmd0", src, out}) })
	if code != exitOK {
		t.Fatalf("exit %d: %s", code, stdout)
	}
	var events []map[string]any
	for _, line := range strings.Split(strings.TrimSpace(stdout), "\n") {
		var e map[string]any
		if err := json.Unmarshal([]byte(line), &e); err != nil {
			t.Fatalf("not JSON: %q", line)
		}
		events = append(events, e)
	}
	types := []string{}
	for _, e := range events {
		types = append(types, e["type"].(string))
	}
	if strings.Join(types, ",") != "start,done,summary" {
		t.Fatalf("event types %v, want start, done, summary", types)
	}
	done := events[1]
	want := filepath.Join(out, "pmd0_robe999.mdl")
	if done["target"] != "pmd0" || done["file"] != "pmd0_robe999.mdl" || done["output"] != want || done["index"] != float64(1) || done["total"] != float64(1) {
		t.Errorf("done event %v", done)
	}
	if res, ok := done["result"].(map[string]any); !ok || res["file"] == nil {
		t.Errorf("done event has no result: %v", done)
	}

	// With previews: a source event first, and the preview paths on done.
	pv := filepath.Join(dir, "pv")
	for _, name := range []string{"pmh0", "pmd0"} {
		sk := &mdl.Model{Name: name, Classification: "CHARACTER", AnimationScale: 1, Nodes: []*mdl.Node{
			{Name: name, Parent: "NULL", Orientation: mdl.Vec4{Z: 1}, Scale: 1},
			{Name: "pelvis_g", Parent: name, Position: mdl.Vec3{Z: 1}, Orientation: mdl.Vec4{Z: 1}, Scale: 1},
		}}
		if err := mdl.WriteFile(sk, filepath.Join(dir, name+".mdl")); err != nil {
			t.Fatal(err)
		}
	}
	stdout, _ = captureStdio(t, func() {
		code = cmdVariants([]string{"--json-lines", "--overwrite", "--to", "pmd0", "--preview-dir", pv, "--width", "1.05", src, out})
	})
	if code != exitOK || !strings.HasPrefix(stdout, `{"type":"source"`) {
		t.Fatalf("exit %d, output %s: want a source event first", code, stdout)
	}
	for _, f := range []string{"pmh0_robe999.fit.mdl", "pmd0_robe999.fit.mdl", "pmd0_robe999.body.mdl"} {
		if _, err := os.Stat(filepath.Join(pv, f)); err != nil {
			t.Errorf("preview %s not written: %v", f, err)
		}
	}
	var preview string
	for _, line := range strings.Split(strings.TrimSpace(stdout), "\n") {
		var e map[string]any
		if json.Unmarshal([]byte(line), &e) == nil && e["type"] == "done" {
			preview, _ = e["preview"].(string)
		}
	}
	if preview != filepath.Join(pv, "pmd0_robe999.fit.mdl") || strings.Contains(stdout, "no preview") {
		t.Errorf("done event has no preview path (%q): %s", preview, stdout)
	}
	captureStdio(t, func() { code = cmdVariants([]string{"--width", "3", src, out}) })
	if code != exitUsage {
		t.Errorf("--width 3: exit %d, want a usage error", code)
	}

	// Again without --overwrite: an error event, still followed by the summary.
	stdout, _ = captureStdio(t, func() { code = cmdVariants([]string{"--json-lines", "--to", "pmd0", src, out}) })
	if code != exitErrors || !strings.Contains(stdout, `"type":"error"`) || !strings.Contains(stdout, `"message":"`) || !strings.Contains(stdout, `"0 of 1 variants written"`) {
		t.Errorf("exit %d, output %s: want an error event and the summary", code, stdout)
	}
}

// TestVariantsBodyGroups: with no --to, the bodies found are the targets, and
// a body of the same shape as another (pmd15, a fighting style of pmd0) gets
// that body's variant, renamed.
func TestVariantsBodyGroups(t *testing.T) {
	dir := t.TempDir()
	src := variantFixture(t, dir)
	write := func(m *mdl.Model) {
		if err := mdl.WriteFile(m, filepath.Join(dir, m.Name+".mdl")); err != nil {
			t.Fatal(err)
		}
	}
	for name, hip := range map[string]float32{"pmh0": 1, "pmd0": 0.7, "pmd15": 0.7} {
		write(&mdl.Model{Name: name, Classification: "CHARACTER", AnimationScale: 1, Nodes: []*mdl.Node{
			{Name: name, Parent: "NULL", Orientation: mdl.Vec4{Z: 1}, Scale: 1},
			{Name: "pelvis_g", Parent: name, Position: mdl.Vec3{Z: hip}, Orientation: mdl.Vec4{Z: 1}, Scale: 1},
			{Name: "lthigh_g", Parent: "pelvis_g", Position: mdl.Vec3{X: -0.1, Z: -0.4}, Orientation: mdl.Vec4{Z: 1}, Scale: 1},
		}})
	}
	// Base thighs, so the bodies' parts are known (pmd15 wears pmd0's).
	for _, name := range []string{"pmh0_legl001", "pmd0_legl001"} {
		mesh := mdl.NewMeshData()
		mesh.Verts = []mdl.Vec3{{X: -0.05}, {X: 0.05}, {Y: 0.05}}
		mesh.TVerts = []mdl.Vec3{{}, {X: 1}, {Y: 1}}
		mesh.Faces = []mdl.Face{{Verts: [3]int32{0, 1, 2}, UVs: [3]int32{0, 1, 2}, SmoothGroup: 1}}
		write(&mdl.Model{Name: name, Classification: "CHARACTER", AnimationScale: 1, Nodes: []*mdl.Node{
			{Name: name, Parent: "NULL", Orientation: mdl.Vec4{Z: 1}, Scale: 1},
			{Name: name + "g", Parent: name, Mesh: mesh, Orientation: mdl.Vec4{Z: 1}, Scale: 1},
		}})
	}
	tmpl, err := mdl.ParseFile(filepath.Join(dir, "pmd0_robe999.mdl"))
	if err != nil {
		t.Fatal(err)
	}
	write(mdl.RenameBody(tmpl.Model, "pmd0", "pmd15"))
	if err := os.WriteFile(filepath.Join(dir, "phenotype.2da"), []byte("2DA V2.0\n\n   Label DefaultPhenoType\n0  Normal 0\n15 Kensai 0\n"), 0o644); err != nil {
		t.Fatal(err)
	}

	var code int
	stdout, _ := captureStdio(t, func() { code = cmdVariants([]string{"--list-bodies", "--json", "--game-dir", dir, src}) })
	var listed []map[string]any
	if err := json.Unmarshal([]byte(stdout), &listed); code != exitOK || err != nil || len(listed) != 3 {
		t.Logf("listed: %s", stdout)
		t.Fatalf("--list-bodies: exit %d, %v, output %s", code, err, stdout)
	}
	if listed[0]["prefix"] != "pmh0" || listed[0]["source"] != true || listed[2]["prefix"] != "pmd15" || listed[2]["sameShape"] != "pmd0" {
		t.Errorf("--list-bodies: %v", listed)
	}

	out := filepath.Join(dir, "out")
	stdout, _ = captureStdio(t, func() { code = cmdVariants([]string{"--json-lines", "--game-dir", dir, src, out}) })
	if code != exitOK || !strings.Contains(stdout, `pmd15_robe999.mdl","sameShape":"pmd0"}`) || !strings.Contains(stdout, `"2 of 2 variants written"`) {
		t.Fatalf("exit %d, output %s: want pmd0 and pmd15, pmd15 as pmd0's shape", code, stdout)
	}
	a, err1 := os.ReadFile(filepath.Join(out, "pmd0_robe999.mdl"))
	b, err2 := os.ReadFile(filepath.Join(out, "pmd15_robe999.mdl"))
	if err1 != nil || err2 != nil {
		t.Fatal(err1, err2)
	}
	if strings.ReplaceAll(string(a), "pmd0", "pmd15") != string(b) {
		t.Error("pmd15's robe is not pmd0's renamed")
	}
}

// TestVariantsIgnoresEarlierOutput: the file a variant replaces is not its
// bone template. Here the template lacks a bone the robe uses (as a wemic's
// robes lack legs); a second run must still say so, not take the first
// run's output, which has the bone, as its template.
func TestVariantsIgnoresEarlierOutput(t *testing.T) {
	dir := t.TempDir()
	src := variantFixture(t, dir)
	tmplDir := filepath.Join(dir, "tmpl")
	if err := os.MkdirAll(tmplDir, 0o755); err != nil {
		t.Fatal(err)
	}
	legless := &mdl.Model{Name: "pmd0_robe999", SuperModel: "pmd0", Classification: "CHARACTER", AnimationScale: 1, Nodes: []*mdl.Node{
		{Name: "pmd0_robe999", Parent: "NULL", Orientation: mdl.Vec4{Z: 1}, Scale: 1},
		{Name: "pelvis_g", Parent: "pmd0_robe999", Position: mdl.Vec3{Z: 0.7}, Orientation: mdl.Vec4{Z: 1}, Scale: 1},
	}}
	if err := mdl.WriteFile(legless, filepath.Join(tmplDir, "pmd0_robe999.mdl")); err != nil {
		t.Fatal(err)
	}
	if err := os.Remove(filepath.Join(dir, "pmd0_robe999.mdl")); err != nil {
		t.Fatal(err)
	}
	for run := 1; run <= 2; run++ {
		var code int
		stdout, _ := captureStdio(t, func() {
			code = cmdVariants([]string{"--to", "pmd0", "--overwrite", "--resource-dir", tmplDir, "--game-dir", dir, src})
		})
		if code != exitOK || !strings.Contains(stdout, "bone lthigh_g is not in pmd0_robe999") {
			t.Errorf("run %d: exit %d, output %q: want the missing-bone warning", run, code, stdout)
		}
	}
}

// TestVariantsDryRun: --dry-run writes nothing and says what it would do.
func TestVariantsDryRun(t *testing.T) {
	dir := t.TempDir()
	src := variantFixture(t, dir)
	out := filepath.Join(dir, "out")
	var code int
	stdout, _ := captureStdio(t, func() {
		code = cmdVariants([]string{"--dry-run", "--json", "--to", "pmd0", "--game-dir", dir, src, out})
	})
	var results []map[string]any
	if err := json.Unmarshal([]byte(stdout), &results); code != exitOK || err != nil || len(results) != 1 || results[0]["action"] != "write" {
		t.Fatalf("exit %d, %v, output %s: want one result that would be written", code, err, stdout)
	}
	if _, err := os.Stat(out); !os.IsNotExist(err) {
		t.Errorf("--dry-run created %s", out)
	}
	captureStdio(t, func() { code = cmdVariants([]string{"--dry-run", "--json-lines", src}) })
	if code != exitUsage {
		t.Errorf("--dry-run --json-lines: exit %d, want a usage error", code)
	}
}
