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
	head := robeTestModel("pmh0_head999", 1, 0.4, 0.4, true)
	if _, err := Variant(head, "pmd0", VariantOptions{}); err == nil || !strings.Contains(err.Error(), "not a robe or an armour body part") {
		t.Errorf("head: want a not-supported error, got %v", err)
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
	src, ref := loadGameModel(t, locs, "pmh0_robe003"), loadGameModel(t, locs, "pmd0_robe003")
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

	mean, n := meanDistanceByUV(t, out, ref, true)
	t.Logf("pmd0_robe003 generated vs Bioware's: %.2f cm mean over %d vertices", mean, n)
	if mean > 3.0 {
		t.Errorf("mean distance to Bioware's dwarf robe %.2f cm, want under 3", mean)
	}
}

// partTestModel is a rigid body part: one trimesh under the part root, its
// vertices in the frame of the node the part hangs from.
func partTestModel(name string, verts []Vec3) *Model {
	m := &Model{Name: name, Classification: "CHARACTER", AnimationScale: 1}
	m.Nodes = append(m.Nodes, &Node{Name: name, Parent: "NULL", Orientation: Vec4{0, 0, 1, 0}, Scale: 1})
	mesh := NewMeshData()
	mesh.Verts = verts
	mesh.TVerts = make([]Vec3, len(verts))
	for i := 0; i+2 < len(verts); i += 3 {
		mesh.Faces = append(mesh.Faces, Face{Verts: [3]int32{int32(i), int32(i + 1), int32(i + 2)}, UVs: [3]int32{int32(i), int32(i + 1), int32(i + 2)}, SmoothGroup: 1})
	}
	m.Nodes = append(m.Nodes, &Node{Name: name + "g", Parent: name, Mesh: mesh, Orientation: Vec4{0, 0, 1, 0}, Scale: 1})
	return m
}

func TestVariantRefitsBodyPart(t *testing.T) {
	dir := t.TempDir()
	// Skeletons: the thigh (lthigh_g -> lshin_g) is 0.4 long in pmh0, 0.3 in
	// pmd0. An effect node under the thigh must not count towards its length.
	write := func(m *Model) {
		if err := WriteFile(m, filepath.Join(dir, m.Name+".mdl")); err != nil {
			t.Fatal(err)
		}
	}
	for name, shin := range map[string]float32{"pmh0": 0.4, "pmd0": 0.3} {
		sk := skeletonTestModel(name, 0.4)
		sk.FindNode("lshin_g").Position = Vec3{0, 0, -shin}
		sk.Nodes = append(sk.Nodes, &Node{Name: "impact", Parent: "lthigh_g", Position: Vec3{0, 0.5, 0}, Orientation: Vec4{0, 0, 1, 0}, Scale: 1})
		write(sk)
	}
	// Base thighs: the dwarf's is 1.5 times as wide across x and y, and its
	// centre sits 0.01 further along x.
	sq := func(cx, half float32) []Vec3 {
		return []Vec3{{cx - half, -half, -0.1}, {cx + half, -half, -0.1}, {cx + half, half, -0.1}, {cx - half, half, -0.2}, {cx - half, -half, -0.2}, {cx + half, half, -0.2}}
	}
	write(partTestModel("pmh0_legl001", sq(0, 0.1)))
	write(partTestModel("pmd0_legl001", sq(0.01, 0.15)))

	src := partTestModel("pmh0_legl999", []Vec3{{0.1, 0, -0.2}, {0, 0.1, 0.05}, {0, 0, -0.4}})
	var warnings []string
	out, err := Variant(src, "pmd0", VariantOptions{Locs: []string{dir}, Warn: func(s string) { warnings = append(warnings, s) }})
	if err != nil {
		t.Fatal(err)
	}
	if len(warnings) > 0 {
		t.Errorf("unexpected warnings: %q", warnings)
	}
	if out.Name != "pmd0_legl999" || out.Nodes[1].Name != "pmd0_legl999g" || out.Nodes[1].Parent != "pmd0_legl999" {
		t.Errorf("names not renamed: %q, %q under %q", out.Name, out.Nodes[1].Name, out.Nodes[1].Parent)
	}
	got := out.Nodes[1].Mesh.Verts
	// Each vertex: z (along the thigh, downwards) times 0.75 below the hip
	// only, then x and y scaled 1.5 about the base thigh's centre (x, y) =
	// (0, 0) -> (0.01, 0).
	want := []Vec3{{0.1*1.5 + 0.01, 0, -0.15}, {0.01, 0.15, 0.05}, {0.01, 0, -0.3}}
	for i := range want {
		if !near(got[i], want[i]) {
			t.Errorf("vertex %d at %v, want %v", i, got[i], want[i])
		}
	}
	if src.Nodes[1].Mesh.Verts[0] != (Vec3{0.1, 0, -0.2}) {
		t.Error("source part was changed")
	}

	skinned := robeTestModel("pmh0_neck999", 1, 0.4, 0.4, true)
	if _, err := Variant(skinned, "pmd0", VariantOptions{Locs: []string{dir}}); err == nil || !strings.Contains(err.Error(), "skinned body parts are not supported yet") {
		t.Errorf("skinned part: want a not-supported error, got %v", err)
	}
}

// TestVariantPartsMatchBioware generates the dwarf thigh, chest and hand of
// stock armour from the human ones and compares them with Bioware's own
// dwarf versions (same meshes, vertices matched by UV). Prototype and Go
// measured 0.8 to 1.0 cm mean for these slots. Needs the game install.
func TestVariantPartsMatchBioware(t *testing.T) {
	game := os.Getenv("NWN_ROOT")
	if game == "" {
		home, _ := os.UserHomeDir()
		game = filepath.Join(home, ".local", "share", "Steam", "steamapps", "common", "Neverwinter Nights")
	}
	locs := []string{game}
	if sk, _ := loadSkeleton("pmh0", locs); sk == nil {
		t.Skip("game install not available")
	}
	for _, part := range []string{"legl005", "chest060", "handl005"} {
		src, ref := loadGameModel(t, locs, "pmh0_"+part), loadGameModel(t, locs, "pmd0_"+part)
		out, err := Variant(src, "pmd0", VariantOptions{Locs: locs, Warn: func(s string) { t.Errorf("%s: %s", part, s) }})
		if err != nil {
			t.Fatal(err)
		}
		if err := Compile(out, &bytes.Buffer{}); err != nil {
			t.Fatalf("%s: compile: %v", part, err)
		}
		mean, n := meanDistanceByUV(t, out, ref, false)
		t.Logf("pmd0_%s generated vs Bioware's: %.2f cm mean over %d vertices", part, mean, n)
		if mean > 1.5 {
			t.Errorf("pmd0_%s: %.2f cm from Bioware's, want under 1.5", part, mean)
		}
	}
}

func loadGameModel(t *testing.T, locs []string, name string) *Model {
	t.Helper()
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

// meanDistanceByUV matches the mesh vertices (with skinsOnly, the skins'
// only) of a and b (the same mesh, refitted) by the set of UVs their faces use and returns the mean distance
// in cm between matched vertices, and how many matched (at least 90% must).
func meanDistanceByUV(t *testing.T, a, b *Model, skinsOnly bool) (float64, int) {
	t.Helper()
	points := func(m *Model) (pts [][3]float64, uvs []string) {
		w := newWorldTransforms(m)
		for _, n := range m.Nodes {
			if n.Mesh == nil || n.Aabb != nil || len(n.Mesh.Verts) == 0 || (skinsOnly && n.Skin == nil) {
				continue
			}
			tr := w.of(n.Name)
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
				pts = append(pts, tr.apply(vec3d(v)))
				var keys []string
				for k := range sets[i] {
					keys = append(keys, k)
				}
				sort.Strings(keys)
				uvs = append(uvs, n.Name[strings.IndexByte(n.Name+"_", '_'):]+"|"+strings.Join(keys, ";"))
			}
		}
		return pts, uvs
	}
	ap, auv := points(a)
	bp, buv := points(b)
	byUV := map[string][]int{}
	for j, k := range buv {
		byUV[k] = append(byUV[k], j)
	}
	var sum float64
	n := 0
	for i, k := range auv {
		best := math.Inf(1)
		for _, j := range byUV[k] {
			best = math.Min(best, len3([3]float64{ap[i][0] - bp[j][0], ap[i][1] - bp[j][1], ap[i][2] - bp[j][2]}))
		}
		if !math.IsInf(best, 1) {
			sum += best
			n++
		}
	}
	if n < len(ap)*9/10 {
		t.Fatalf("only %d of %d vertices matched by UV", n, len(ap))
	}
	return sum / float64(n) * 100, n
}
