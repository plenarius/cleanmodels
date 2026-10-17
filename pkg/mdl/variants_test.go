package mdl

import (
	"bytes"
	"fmt"
	"math"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"
)

// robeTestModel builds a robe-shaped model: root, rootdummy at hip height
// h, pelvis_g, lthigh_g (thigh offset) and lshin_g (shin offset) below it,
// plus a skin "robe" when withSkin is set: vertex 0 hangs 0.2 below the
// shin joint (shin weight), vertex 1 0.1 below the hip (thigh weight),
// vertex 2 at hip height (pelvis weight).
func robeTestModel(name string, h, thigh, shin float32, withSkin bool) *Model {
	prefix := name[:4]
	m := &Model{Name: name, SuperModel: prefix, Classification: "CHARACTER", AnimationScale: 1}
	add := func(n *Node) { n.Orientation = Vec4{0, 0, 1, 0}; n.Scale = 1; m.Nodes = append(m.Nodes, n) }
	add(&Node{Name: name, Parent: "NULL"})
	add(&Node{Name: "rootdummy", Parent: name, Position: Vec3{0, 0, h}})
	add(&Node{Name: "pelvis_g", Parent: "rootdummy"})
	add(&Node{Name: "lthigh_g", Parent: "pelvis_g", Position: Vec3{-0.1, 0, -thigh}})
	add(&Node{Name: "lshin_g", Parent: "lthigh_g", Position: Vec3{0, 0, -shin}})
	if withSkin {
		mesh := NewMeshData()
		mesh.Verts = []Vec3{{-0.1, 0, h - thigh - shin - 0.2}, {-0.1, 0, h - thigh - 0.1}, {0, 0.05, h}}
		mesh.Normals = []Vec3{{1, 0, 0}, {1, 0, 0}, {1, 0, 0}}
		mesh.TVerts = []Vec3{{0, 0, 0}, {1, 0, 0}, {0, 1, 0}}
		mesh.Faces = []Face{{Verts: [3]int32{0, 1, 2}, UVs: [3]int32{0, 1, 2}, SmoothGroup: 1}}
		mesh.Bitmap = "pmh0_robe999"
		add(&Node{Name: "robe", Parent: name, Mesh: mesh, Skin: &SkinData{Weights: []VertexWeight{
			{Bones: []string{"lshin_g"}, Weights: []float32{1}},
			{Bones: []string{"lthigh_g"}, Weights: []float32{1}},
			{Bones: []string{"pelvis_g"}, Weights: []float32{1}},
		}}})
	}
	return m
}

// skeletonTestModel is a race skeleton with a foot under the shin, so the
// shin, which has no child in a robe, can take its length from it.
func skeletonTestModel(name string, foot float32) *Model {
	m := robeTestModel(name+"_skel", 1, 0.4, 0.4, false)
	m.Name, m.Nodes[0].Name, m.Nodes[1].Parent = name, name, name
	m.Nodes = append(m.Nodes, &Node{Name: "lfoot_g", Parent: "lshin_g", Position: Vec3{0, 0, -foot}, Orientation: Vec4{0, 0, 1, 0}, Scale: 1})
	return m
}

func near(a, b Vec3) bool {
	return math.Abs(float64(a.X-b.X)) < 1e-5 && math.Abs(float64(a.Y-b.Y)) < 1e-5 && math.Abs(float64(a.Z-b.Z)) < 1e-5
}

func TestVariantRefitsToTargetBones(t *testing.T) {
	dir := t.TempDir()
	// The target body: hips at 0.7 instead of 1.0, thigh 0.3 instead of 0.4,
	// shin 0.3 instead of 0.4, and a foot half as far below the shin.
	for name, m := range map[string]*Model{
		"pmd0_robe999": robeTestModel("pmd0_robe999", 0.7, 0.3, 0.3, false),
		"pmh0":         skeletonTestModel("pmh0", 0.4),
		"pmd0":         skeletonTestModel("pmd0", 0.2),
	} {
		if err := WriteFile(m, filepath.Join(dir, name+".mdl")); err != nil {
			t.Fatal(err)
		}
	}
	src := robeTestModel("pmh0_robe999", 1, 0.4, 0.4, true)
	var warnings []string
	out, err := Variant(src, "pmd0", VariantOptions{Locs: []string{dir}, Warn: func(s string) { warnings = append(warnings, s) }})
	if err != nil {
		t.Fatal(err)
	}

	if out.Name != "pmd0_robe999" || out.SuperModel != "pmd0" || out.Nodes[0].Name != "pmd0_robe999" || out.Nodes[1].Parent != "pmd0_robe999" {
		t.Errorf("names not renamed: model %q supermodel %q root %q rootdummy parent %q", out.Name, out.SuperModel, out.Nodes[0].Name, out.Nodes[1].Parent)
	}
	if got := out.FindNode("lthigh_g").Position; !near(got, Vec3{-0.1, 0, -0.3}) {
		t.Errorf("lthigh_g not moved to the template's placement: %v", got)
	}
	if got := src.FindNode("lthigh_g").Position; got != (Vec3{-0.1, 0, -0.4}) {
		t.Errorf("source bone changed to %v", got)
	}

	verts := out.FindNode("robe").Mesh.Verts
	// Vertex 0: 0.2 below the shin joint; the shin is half as long (foot
	// 0.2 instead of 0.4), so 0.1 below the target's shin joint at
	// 0.7 - 0.3 - 0.3 = 0.1.
	if !near(verts[0], Vec3{-0.1, 0, 0.0}) {
		t.Errorf("shin vertex at %v, want (-0.1, 0, 0)", verts[0])
	}
	// Vertex 1: 0.1 below the hip on a thigh 0.75 as long: 0.075 below the
	// target's hip joint at 0.7 - 0.3 = 0.4.
	if !near(verts[1], Vec3{-0.1, 0, 0.325}) {
		t.Errorf("thigh vertex at %v, want (-0.1, 0, 0.325)", verts[1])
	}
	// Vertex 2 sits at the hip joint, level with the pelvis: it only moves
	// with it, to the target's hip height.
	if !near(verts[2], Vec3{0, 0.05, 0.7}) {
		t.Errorf("pelvis vertex at %v, want (0, 0.05, 0.7)", verts[2])
	}
	if n := out.FindNode("robe").Mesh.Normals[0]; !near(n, Vec3{1, 0, 0}) {
		t.Errorf("normal %v changed by a stretch along z", n)
	}
	if got := src.FindNode("robe").Mesh.Verts[0]; !near(got, Vec3{-0.1, 0, 1 - 0.4 - 0.4 - 0.2}) {
		t.Errorf("source vertex changed to %v", got)
	}
	if len(warnings) != 1 || !strings.Contains(warnings[0], "body parts") {
		t.Errorf("want one warning about the missing body parts, got %q", warnings)
	}
}

func TestVariantNeedsATemplate(t *testing.T) {
	src := robeTestModel("pmh0_robe999", 1, 0.4, 0.4, true)
	if _, err := Variant(src, "pmd0", VariantOptions{Locs: []string{t.TempDir()}}); err == nil || !strings.Contains(err.Error(), "no robe for pmd0") {
		t.Errorf("want a missing-template error, got %v", err)
	}
	if _, err := Variant(&Model{Name: "robe003"}, "pmd0", VariantOptions{}); err == nil {
		t.Error("want an error for a name without a body prefix")
	}
}

// TestVariantMatchesBioware generates the dwarf robe 3 from the human
// one and compares it with Bioware's own dwarf robe 3, vertex by vertex
// (the same mesh: vertices matched by the UVs their faces use). The
// prototype this was ported from measured 2.5 cm mean; generated robes look
// right in game. Needs the game install; skipped without it.
func TestVariantMatchesBioware(t *testing.T) {
	game := os.Getenv("NWN_ROOT")
	if game == "" {
		home, _ := os.UserHomeDir()
		game = filepath.Join(home, ".local", "share", "Steam", "steamapps", "common", "Neverwinter Nights")
	}
	locs := []string{game}
	if sk, _ := loadSkeleton("pmh0", locs); sk == nil {
		t.Skip("game install not available")
	}
	load := func(name string) *Model {
		ref := findModelResource(name, locs)
		if ref == nil {
			t.Fatalf("%s not found", name)
		}
		m, err := readModelRef(ref)
		if err != nil {
			t.Fatal(err)
		}
		return m
	}
	src, ref := load("pmh0_robe003"), load("pmd0_robe003")
	var warnings []string
	out, err := Variant(src, "pmd0", VariantOptions{Locs: locs, Warn: func(s string) { warnings = append(warnings, s) }})
	if err != nil {
		t.Fatal(err)
	}
	if len(warnings) > 0 {
		t.Errorf("unexpected warnings: %q", warnings)
	}
	if err := Compile(out, &bytes.Buffer{}); err != nil {
		t.Fatalf("compile: %v", err)
	}

	skinPoints := func(m *Model) (pts [][3]float64, uvs []string) {
		w := newWorldTransforms(m)
		for _, n := range m.Nodes {
			if n.Skin == nil || n.Mesh == nil {
				continue
			}
			t := w.of(n.Name)
			sets := make([]map[string]bool, len(n.Mesh.Verts))
			for i := range sets {
				sets[i] = map[string]bool{}
			}
			for _, f := range n.Mesh.Faces {
				for k := 0; k < 3; k++ {
					if v, u := f.Verts[k], f.UVs[k]; int(v) < len(sets) && int(u) < len(n.Mesh.TVerts) {
						tv := n.Mesh.TVerts[u]
						sets[v][fmt.Sprintf("%.4f,%.4f", tv.X, tv.Y)] = true
					}
				}
			}
			for i, v := range n.Mesh.Verts {
				pts = append(pts, t.apply(vec3d(v)))
				var keys []string
				for k := range sets[i] {
					keys = append(keys, k)
				}
				sort.Strings(keys)
				uvs = append(uvs, strings.Join(keys, ";"))
			}
		}
		return pts, uvs
	}
	gp, guv := skinPoints(out)
	rp, ruv := skinPoints(ref)
	byUV := map[string][]int{}
	for j, k := range ruv {
		byUV[k] = append(byUV[k], j)
	}
	var sum float64
	n := 0
	for i, k := range guv {
		best := math.Inf(1)
		for _, j := range byUV[k] {
			best = math.Min(best, len3([3]float64{gp[i][0] - rp[j][0], gp[i][1] - rp[j][1], gp[i][2] - rp[j][2]}))
		}
		if !math.IsInf(best, 1) {
			sum += best
			n++
		}
	}
	if n < len(gp)*9/10 {
		t.Fatalf("only %d of %d vertices matched Bioware's by UV", n, len(gp))
	}
	mean := sum / float64(n) * 100
	t.Logf("pmd0_robe003 generated vs Bioware's: %.2f cm mean over %d vertices", mean, n)
	if mean > 3.0 {
		t.Errorf("mean distance to Bioware's dwarf robe %.2f cm, want under 3", mean)
	}
}
