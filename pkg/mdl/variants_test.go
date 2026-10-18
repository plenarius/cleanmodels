package mdl

import (
	"bytes"
	"fmt"
	"math"
	"os"
	"path/filepath"
	"reflect"
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
	if _, err := Variant(src, "pmd0", VariantOptions{Locs: []string{t.TempDir()}}); err == nil || !strings.Contains(err.Error(), "no robe of pmd0") {
		t.Errorf("want a missing-template error, got %v", err)
	}
	if _, err := Variant(&Model{Name: "robe003"}, "pmd0", VariantOptions{}); err == nil {
		t.Error("want an error for a name without a body prefix")
	}
	head := robeTestModel("pmh0_head999", 1, 0.4, 0.4, true)
	if _, err := Variant(head, "pmd0", VariantOptions{}); err == nil || !strings.Contains(err.Error(), "not a robe, cloak or armour body part") {
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

	// Width and Length on top of the fit: x and y 1.1 times further from the
	// centre, z below the hip 1.2 times further.
	adj, err := Variant(src, "pmd0", VariantOptions{Locs: []string{dir}, Width: 1.1, Length: 1.2})
	if err != nil {
		t.Fatal(err)
	}
	wantAdj := []Vec3{{0.1*1.5*1.1 + 0.01, 0, -0.2 * 0.75 * 1.2}, {0.01, 0.15 * 1.1, 0.05}, {0.01, 0, -0.4 * 0.75 * 1.2}}
	for i, v := range adj.Nodes[1].Mesh.Verts {
		if !near(v, wantAdj[i]) {
			t.Errorf("Width/Length: vertex %d at %v, want %v", i, v, wantAdj[i])
		}
	}

	// The preview: the thigh where the dwarf skeleton hangs it (lthigh_g at
	// (-0.1, 0, 0.6)), and a body of the base parts found, without a thigh.
	write(partTestModel("pmd0_shinl001", []Vec3{{0, 0, 0}, {0, 0, -0.1}, {0.1, 0, 0}}))
	fit, body, notes, err := VariantPreview(out, "pmd0", []string{dir}, "")
	if err != nil {
		t.Fatal(err)
	}
	if len(fit.Nodes) != 2 || fit.Nodes[1].Skin != nil {
		t.Fatalf("fit has %d nodes, want the root and one plain mesh", len(fit.Nodes))
	}
	for i, v := range fit.Nodes[1].Mesh.Verts {
		w := Vec3{want[i].X - 0.1, want[i].Y, want[i].Z + 0.6}
		if !near(v, w) {
			t.Errorf("fit vertex %d at %v, want %v", i, v, w)
		}
	}
	if len(body.Nodes) != 2 || !near(body.Nodes[1].Mesh.Verts[1], Vec3{-0.1, 0, 0.2}) {
		t.Errorf("body: %d nodes, want the root and the shin at lshin_g (-0.1, 0, 0.3)", len(body.Nodes))
	}
	if len(notes) != 1 || strings.Contains(notes[0], "pmd0_legl001") || strings.Contains(notes[0], "pmd0_belt001") ||
		!strings.Contains(notes[0], "pmd0_footl001") || strings.Contains(notes[0], "pmd0_chest001") {
		t.Errorf("notes %q: want the missing base parts (the foot), without the thigh (the variant's slot), the belt (not in a bare body) or the chest (no torso_g in this skeleton)", notes)
	}

	// A skinned part needs its attach bone in both skeletons (these have no neck_g).
	skinned := robeTestModel("pmh0_neck999", 1, 0.4, 0.4, true)
	if _, err := Variant(skinned, "pmd0", VariantOptions{Locs: []string{dir}}); err == nil || !strings.Contains(err.Error(), "needs the skeletons pmh0 and pmd0 with neck_g") {
		t.Errorf("skinned part without its attach bone: want a clear error, got %v", err)
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
		// The measured fit by centres of mass; the slot choice (learned,
		// boxes, mass) is TestVariantLearnedMatchesBioware.
		out, err := Variant(src, "pmd0", VariantOptions{Locs: locs, noLearn: true, byMass: true, Warn: func(s string) { t.Errorf("%s: %s", part, s) }})
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

// TestVariantCloakMatchesBioware generates the dwarf cloak 1 from the human
// one and compares its cloth with Bioware's dwarf cloak 1 (the same mesh).
// Go and the prototype measured 2.9 cm mean over all stock cloaks in
// phenotype 0. Needs the game install.
func TestVariantCloakMatchesBioware(t *testing.T) {
	game := os.Getenv("NWN_ROOT")
	if game == "" {
		home, _ := os.UserHomeDir()
		game = filepath.Join(home, ".local", "share", "Steam", "steamapps", "common", "Neverwinter Nights")
	}
	locs := []string{game}
	if sk, _ := loadSkeleton("pmh0", locs); sk == nil {
		t.Skip("game install not available")
	}
	src, ref := loadGameModel(t, locs, "pmh0_cloak_001"), loadGameModel(t, locs, "pmd0_cloak_001")
	out, err := Variant(src, "pmd0", VariantOptions{Locs: locs, Warn: func(s string) { t.Error(s) }})
	if err != nil {
		t.Fatal(err)
	}
	if err := Compile(out, &bytes.Buffer{}); err != nil {
		t.Fatalf("compile: %v", err)
	}
	mean, n := meanDistanceByUV(t, out, ref, true)
	t.Logf("pmd0_cloak_001 generated vs Bioware's: %.2f cm mean over %d vertices", mean, n)
	if mean > 4 {
		t.Errorf("%.2f cm from Bioware's, want under 4", mean)
	}
}

// TestVariantRefitsSkinnedPart refits a skinned thigh piece bound to the
// skeleton: one vertex on the thigh, one on the shin. Each follows its bone
// from the pmh0 skeleton to the pmd0 one (thigh 0.4 -> 0.3, shin's foot 0.4
// -> 0.2), in the frame of the bone the part hangs from (lthigh_g).
func TestVariantRefitsSkinnedPart(t *testing.T) {
	dir := t.TempDir()
	for name, m := range map[string]*Model{"pmh0": skeletonTestModel("pmh0", 0.4), "pmd0": skeletonTestModel("pmd0", 0.2)} {
		if name == "pmd0" {
			m.FindNode("lshin_g").Position = Vec3{0, 0, -0.3}
		}
		if err := WriteFile(m, filepath.Join(dir, name+".mdl")); err != nil {
			t.Fatal(err)
		}
	}
	src := partTestModel("pmh0_legl999", []Vec3{{0, 0, -0.2}, {0, 0, -0.6}, {0.1, 0, -0.1}})
	g := src.Nodes[1]
	g.Skin = &SkinData{Weights: []VertexWeight{
		{Bones: []string{"lthigh_g"}, Weights: []float32{1}},
		{Bones: []string{"lshin_g"}, Weights: []float32{1}},
		{Bones: []string{"lthigh_g"}, Weights: []float32{1}},
	}}
	out, err := Variant(src, "pmd0", VariantOptions{Locs: []string{dir}, Warn: func(s string) {}})
	if err != nil {
		t.Fatal(err)
	}
	got := out.Nodes[1].Mesh.Verts
	// Vertex 0: 0.2 down the thigh, which is 0.75 as long: 0.15.
	// Vertex 1: 0.2 below the knee (0.4 down the source thigh); the shin is
	// half as long (foot 0.2 instead of 0.4) and the knee is at 0.3: 0.4.
	// Vertex 2: above the hip joint's far side stays (0.1, 0, -0.1) * 0.75 in z.
	want := []Vec3{{0, 0, -0.15}, {0, 0, -0.4}, {0.1, 0, -0.075}}
	for i := range want {
		if !near(got[i], want[i]) {
			t.Errorf("vertex %d at %v, want %v", i, got[i], want[i])
		}
	}
}

// TestVariantHDPartsMatchBeamdog generates the dwarf versions of Beamdog's
// skinned HD shin and chest from the human ones and compares them with
// Beamdog's own dwarf versions. Needs the game install and the HD pheno
// haks; skipped without them.
func TestVariantHDPartsMatchBeamdog(t *testing.T) {
	home, _ := os.UserHomeDir()
	game := os.Getenv("NWN_ROOT")
	if game == "" {
		game = filepath.Join(home, ".local", "share", "Steam", "steamapps", "common", "Neverwinter Nights")
	}
	patch := filepath.Join(home, ".local", "share", "Neverwinter Nights", "patch")
	other, chest := filepath.Join(patch, "bdhd_pheno_other.hak"), filepath.Join(patch, "bdhd_pheno_chest.hak")
	if findModelResource("pmh0_shinl003", []string{other}) == nil || findModelResource("pmh0_chest003", []string{chest}) == nil {
		t.Skip("HD pheno haks not available")
	}
	if sk, _ := loadSkeleton("pmh0", []string{game}); sk == nil {
		t.Skip("game install not available")
	}
	for _, c := range []struct{ part, hak string }{{"shinl003", other}, {"chest003", chest}} {
		locs := []string{c.hak, game}
		load := func(name string) *Model {
			ref := findModelResource(name, locs)
			data, err := ref.read()
			if err != nil {
				t.Fatal(err)
			}
			m, err := DecompileWithOptions(bytes.NewReader(data), int64(len(data)), DecompileOptions{SkeletonDirs: []string{game}})
			if err != nil {
				t.Fatal(err)
			}
			return m
		}
		src, ref := load("pmh0_"+c.part), load("pmd0_"+c.part)
		out, err := Variant(src, "pmd0", VariantOptions{Locs: []string{game}, Warn: func(s string) { t.Errorf("%s: %s", c.part, s) }})
		if err != nil {
			t.Fatal(err)
		}
		mean, n := meanDistanceByUV(t, out, ref, false)
		t.Logf("pmd0_%s (HD, skinned) generated vs Beamdog's: %.2f cm mean over %d vertices", c.part, mean, n)
		// The unchanged human parts are 7.3 (shin) and 6.2 cm (chest) off.
		if mean > 5 {
			t.Errorf("pmd0_%s: %.2f cm from Beamdog's, want under 5", c.part, mean)
		}
	}
}

func TestRobeHides(t *testing.T) {
	dir := t.TempDir()
	table := "2DA V2.0\n\n   HIDECHEST HIDEHANDR HIDEHEAD\n0  1 0 0\n3  1 1 ****\n"
	if err := os.WriteFile(filepath.Join(dir, "parts_robe.2da"), []byte(table), 0o644); err != nil {
		t.Fatal(err)
	}
	hidden, note := robeHides("pmd0_robe003", []string{dir})
	if note != "" || !hidden["chest"] || !hidden["handr"] || hidden["head"] || len(hidden) != 2 {
		t.Errorf("robe 3: hidden %v, note %q; want chest and right hand", hidden, note)
	}
	if hidden, note = robeHides("pmd0_robe039", []string{dir}); hidden != nil || !strings.Contains(note, "no row 39") {
		t.Errorf("robe 39 (no row): hidden %v, note %q", hidden, note)
	}
	if hidden, note = robeHides("pmd0_chest003", []string{dir}); hidden != nil || note != "" {
		t.Errorf("a chest is not a robe: hidden %v, note %q", hidden, note)
	}
}

func TestVariantBodies(t *testing.T) {
	dir := t.TempDir()
	write := func(m *Model) {
		if err := WriteFile(m, filepath.Join(dir, m.Name+".mdl")); err != nil {
			t.Fatal(err)
		}
	}
	writeFile := func(name, text string) {
		if err := os.WriteFile(filepath.Join(dir, name), []byte(text), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	skel := func(name string, foot float32) {
		sk := skeletonTestModel(name, foot)
		write(sk)
	}
	// Humans: phenotype 15 (a fighting style) has the same skeleton and only
	// robes; phenotype 2 the same skeleton but its own parts. Brownies (a
	// custom race, b) have parts in phenotype 2 but no skeleton for it.
	skel("pmh0", 0.4)
	skel("pmh2", 0.4)
	skel("pmh15", 0.4)
	skel("pmb0", 0.2)
	sq := []Vec3{{0, 0, 0}, {0.1, 0, 0}, {0, 0.1, 0}}
	for _, n := range []string{"pmh0_legl001", "pmh0_belt001", "pmh0_robe003", "pmh2_legl001", "pmh2_robe003", "pmh15_robe003", "pmh15_robe004", "pmb0_legl001", "pmb2_legl001", "pmb2_legl002", "pmh0_head001"} {
		write(partTestModel(n, sq))
	}
	writeFile("phenotype.2da", "2DA V2.0\n\n   Label DefaultPhenoType\n0  Normal 0\n2  Large 0\n15 Kensai 0\n")
	writeFile("appearance.2da", "2DA V2.0\n\n   LABEL RACE MODELTYPE\n0  Dwarf D P\n1  Ogre c_ogre F\n2  Brownie** B P\n")

	got := map[string]Body{}
	var order []string
	for _, b := range VariantBodies([]string{dir}) {
		got[b.Prefix] = b
		order = append(order, b.Prefix)
	}
	if want := []string{"pmh0", "pmh2", "pmh15", "pmb0", "pmb2"}; !reflect.DeepEqual(order, want) {
		t.Fatalf("bodies %v, want %v (stock races first, heads not counted)", order, want)
	}
	h15 := got["pmh15"]
	if h15.SameShape != "pmh0" || h15.PartsFrom != "pmh0" || h15.Slots["robe"] != 2 || h15.PhenotypeName != "Kensai" {
		t.Errorf("pmh15: %+v; want same shape as pmh0, pmh0's parts, 2 robes, Kensai", h15)
	}
	if h2 := got["pmh2"]; h2.SameShape != "" || h2.PartsFrom != "pmh2" {
		t.Errorf("pmh2 (same skeleton, its own parts, but no belt) is its own shape: %+v", h2)
	}
	b2 := got["pmb2"]
	if b2.Skeleton || b2.SkeletonFrom != "pmb0" || b2.RaceName != "Brownie" || b2.Slots["legl"] != 2 || b2.SameShape != "" {
		t.Errorf("pmb2: %+v; want no skeleton of its own (the game uses pmb0), Brownie, 2 thighs, its own shape", b2)
	}
	if n := basePart("pmh15", "legl", []string{dir}); n != "pmh0_legl001" {
		t.Errorf("basePart(pmh15, legl) = %s, want pmh0_legl001 (DefaultPhenoType)", n)
	}
	if k := VariantKind("pmh0_cloak_012"); k != "cloak" {
		t.Errorf("VariantKind(pmh0_cloak_012) = %q", k)
	}
	if k := VariantKind("pmh0_head001"); k != "" {
		t.Errorf("VariantKind(pmh0_head001) = %q, want none", k)
	}
}

func TestApplyShape(t *testing.T) {
	m := &Model{Name: "pmh0_robe999", Classification: "CHARACTER", AnimationScale: 1}
	add := func(n *Node) { n.Orientation = Vec4{0, 0, 1, 0}; n.Scale = 1; m.Nodes = append(m.Nodes, n) }
	add(&Node{Name: "pmh0_robe999", Parent: "NULL"})
	add(&Node{Name: "torso_g", Parent: "pmh0_robe999", Position: Vec3{0, 0, 1.2}})
	add(&Node{Name: "lbicep_g", Parent: "torso_g", Position: Vec3{-0.2, 0, 0.3}})
	add(&Node{Name: "rbicep_g", Parent: "torso_g", Position: Vec3{0.2, 0, 0.3}})
	add(&Node{Name: "lthigh_g", Parent: "torso_g", Position: Vec3{-0.1, 0, -0.2}})
	add(&Node{Name: "rthigh_g", Parent: "torso_g", Position: Vec3{0.1, 0, -0.2}})
	mesh := NewMeshData()
	mesh.Verts = []Vec3{
		{0.3, 0.1, 1.5}, // cape at the shoulders
		{0.2, 0, 1.35},  // chest
		{0.2, 0, 1.2},   // waist
		{0.35, 0, 1.3},  // sleeve
		{0.25, 0, 0.1},  // hem
		{0.25, 0, 0.55}, // halfway from hips (1.0) to hem (0.1)
	}
	mesh.TVerts = make([]Vec3, len(mesh.Verts))
	mesh.Faces = []Face{{Verts: [3]int32{0, 1, 2}}, {Verts: [3]int32{3, 4, 5}}}
	torso := VertexWeight{Bones: []string{"torso_g"}, Weights: []float32{1}}
	add(&Node{Name: "robe", Parent: "pmh0_robe999", Mesh: mesh, Skin: &SkinData{Weights: []VertexWeight{
		torso, torso, torso, {Bones: []string{"lbicep_g"}, Weights: []float32{1}}, torso, torso,
	}}})

	var warned []string
	applyShape(m, "pmh0", map[string]float64{"shoulders": 0.5, "waist": 0.5, "hem": 1.5}, nil, func(s string) { warned = append(warned, s) })
	if len(warned) > 0 {
		t.Fatalf("warnings: %q", warned)
	}
	got := m.Nodes[len(m.Nodes)-1].Mesh.Verts
	want := []Vec3{
		{0.15, 0.05, 1.5}, // halved about the shoulder centre (0, 0)
		{0.2, 0, 1.35},    // chest: its own region, factor 1
		{0.1, 0, 1.2},     // halved about torso_g
		{0.35, 0, 1.3},    // on the arm: unchanged
		{0.375, 0, 0.1},   // hem 1.5 about the hips' centre
		{0.3125, 0, 0.55}, // halfway: hips 1, hem 1.5 -> 1.25
	}
	for i := range want {
		if !near(got[i], want[i]) {
			t.Errorf("vertex %d at %v, want %v", i, got[i], want[i])
		}
	}
	if _, err := ParseShape("waist=0.8, hem=1.1"); err != nil {
		t.Error(err)
	}
	for _, bad := range []string{"belly=0.9", "waist=3", "waist"} {
		if _, err := ParseShape(bad); err == nil {
			t.Errorf("ParseShape(%q): want an error", bad)
		}
	}
}

func TestPreviewTexture(t *testing.T) {
	dir := t.TempDir()
	out := t.TempDir()
	// A 2x1 PLT: shade 255 of cloth 1, shade 0 of metal 1.
	plt := append([]byte("PLT V1  \x0a\x00\x00\x00\x00\x00\x00\x00"), 2, 0, 0, 0, 1, 0, 0, 0, 255, 4, 0, 2)
	if err := os.WriteFile(filepath.Join(dir, "pmh0_chest999.plt"), plt, 0o644); err != nil {
		t.Fatal(err)
	}
	// The cloth palette: row 52 (from the top) is red at every shade.
	pal := make([]byte, 18+256*176*4)
	pal[2], pal[12], pal[13], pal[14], pal[16], pal[17] = 2, 0, 1, 176, 32, 8
	for x := 0; x < 256; x++ {
		off := 18 + ((175-52)*256+x)*4 // bottom-left origin
		pal[off], pal[off+1], pal[off+2], pal[off+3] = 0, 0, 200, 255
	}
	if err := os.WriteFile(filepath.Join(dir, "pal_cloth01.tga"), pal, 0o644); err != nil {
		t.Fatal(err)
	}
	// pmd0_chest999 has no texture of its own: the game, and the preview,
	// take the human's.
	names := partTextureNames("pmd0_chest999", "somebitmap")
	if want := []string{"pmd0_chest999", "pmh0_chest999", "somebitmap"}; !reflect.DeepEqual(names, want) {
		t.Errorf("texture names %v, want %v", names, want)
	}
	if got := previewTexture(names, []string{dir}, out, map[string]bool{}); got != "pmh0_chest999" {
		t.Fatalf("previewTexture = %q", got)
	}
	tga, err := os.ReadFile(filepath.Join(out, "pmh0_chest999.tga"))
	if err != nil || len(tga) != 18+2*4 {
		t.Fatalf("TGA: %v, %d bytes", err, len(tga))
	}
	// BGRA: the cloth pixel red from the palette; metal 1's palette is not
	// there, so grey shade 0 (black).
	if got := tga[18:26]; !reflect.DeepEqual(got, []byte{0, 0, 200, 255, 0, 0, 0, 255}) {
		t.Errorf("pixels %v", got)
	}
}

// TestVariantLearnedMatchesBioware predicts Bioware's dwarf versions of stock
// parts as variants does, with each part's own pair left out of what is
// learned: the learned fit where it is chosen for the slot, the measured fit
// elsewhere. It must not be worse than the measured fit alone. Needs the
// game install.
func TestVariantLearnedMatchesBioware(t *testing.T) {
	game := os.Getenv("NWN_ROOT")
	if game == "" {
		home, _ := os.UserHomeDir()
		game = filepath.Join(home, ".local", "share", "Steam", "steamapps", "common", "Neverwinter Nights")
	}
	locs := []string{game}
	if sk, _ := loadSkeleton("pmh0", locs); sk == nil {
		t.Skip("game install not available")
	}
	var chosen, measured float64
	used := 0
	parts := []string{"legl005", "chest060", "handl005", "forel007", "shinl003", "neck004", "pelvis010", "bicepl005"}
	for _, part := range parts {
		slot, num := partKind("pmh0_"+part), strings.TrimLeft(part, "abcdefghijklmnopqrstuvwxyz")
		src, ref := loadGameModel(t, locs, "pmh0_"+part), loadGameModel(t, locs, "pmd0_"+part)
		meas, err := Variant(src, "pmd0", VariantOptions{Locs: locs, noLearn: true, byMass: true})
		if err != nil {
			t.Fatal(err)
		}
		choice := chooseSlot("pmh0", "pmd0", slot, locs, num)
		out, how := meas, "measured by mass"
		if choice.fit != nil {
			out, how = learnedVariant(src, "pmh0", "pmd0", choice.fit, 1, 1), fmt.Sprintf("learned from %d other pairs", choice.fit.pairs)
			used++
		} else if !choice.byMass {
			if out, err = Variant(src, "pmd0", VariantOptions{Locs: locs, noLearn: true}); err != nil {
				t.Fatal(err)
			}
			how = "measured by boxes"
		}
		if err := Compile(out, &bytes.Buffer{}); err != nil {
			t.Fatalf("%s: compile: %v", part, err)
		}
		c, m := pairDistance(out, ref, nil)*100, pairDistance(meas, ref, nil)*100
		t.Logf("pmd0_%s vs Bioware's: %.2f cm (%s), measured %.2f cm", part, c, how, m)
		chosen += c / float64(len(parts))
		measured += m / float64(len(parts))
	}
	t.Logf("mean: %.2f cm with learned fits for %d of %d slots, measured by mass %.2f cm", chosen, used, len(parts), measured)
	if used == 0 || chosen > measured {
		t.Errorf("learned fits used for %d slots, %.2f cm against %.2f cm measured", used, chosen, measured)
	}
}

// TestVariantLearnsFromPairs: where versions of a slot exist for both bodies
// and the target's are a scale and offset of the source's, a new part gets
// that fit, and Learned says how many pairs it came from.
func TestVariantLearnsFromPairs(t *testing.T) {
	dir := t.TempDir()
	write := func(m *Model) {
		if err := WriteFile(m, filepath.Join(dir, m.Name+".mdl")); err != nil {
			t.Fatal(err)
		}
	}
	for name, shin := range map[string]float32{"pmh0": 0.4, "pmd0": 0.3} {
		sk := skeletonTestModel(name, 0.4)
		sk.FindNode("lshin_g").Position = Vec3{0, 0, -shin}
		write(sk)
	}
	fit := func(v Vec3) Vec3 { return Vec3{v.X*1.3 + 0.02, v.Y * 1.1, v.Z*0.8 - 0.01} }
	part := func(name string, verts []Vec3) *Model {
		m := partTestModel(name, verts)
		mesh := m.Nodes[1].Mesh
		for i := range mesh.TVerts {
			mesh.TVerts[i] = Vec3{float32(i) * 0.01, float32(len(name)) * 0.001, 0}
		}
		return m
	}
	shapes := [][]Vec3{
		{{0.1, 0, -0.2}, {0, 0.1, -0.05}, {0, 0, -0.4}, {0.05, 0.05, -0.3}, {-0.05, 0.02, -0.1}, {0.02, -0.04, -0.25}},
		{{0.12, 0.01, -0.22}, {0.01, 0.11, -0.06}, {0.02, 0, -0.38}, {0.06, 0.04, -0.31}, {-0.04, 0.03, -0.12}, {0.03, -0.05, -0.2}},
		{{0.09, -0.01, -0.18}, {-0.01, 0.09, -0.04}, {0.01, 0.01, -0.42}, {0.04, 0.06, -0.29}, {-0.06, 0.01, -0.09}, {0.01, -0.03, -0.27}},
	}
	for i, verts := range shapes {
		num := fmt.Sprintf("%03d", i+2)
		write(part("pmh0_legl"+num, verts))
		moved := make([]Vec3, len(verts))
		for j, v := range verts {
			moved[j] = fit(v)
		}
		write(part("pmd0_legl"+num, moved))
	}
	src := part("pmh0_legl050", []Vec3{{0.08, 0.02, -0.15}, {0, 0.07, -0.3}, {0.03, 0, -0.35}})
	pairs := 0
	out, err := Variant(src, "pmd0", VariantOptions{Locs: []string{dir}, Learned: func(n int) { pairs = n }})
	if err != nil {
		t.Fatal(err)
	}
	if pairs != 3 {
		t.Errorf("learned from %d pairs, want 3", pairs)
	}
	for i, v := range out.Nodes[1].Mesh.Verts {
		if w := fit(src.Nodes[1].Mesh.Verts[i]); !near(v, w) {
			t.Errorf("vertex %d at %v, want %v (the pairs' fit)", i, v, w)
		}
	}
}
