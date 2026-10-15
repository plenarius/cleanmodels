package mdl

import (
	"bytes"
	"encoding/binary"
	"math"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// A small creature skeleton. Tree positions: ptx0 0, rootdummy 1, torso_g 2,
// lbicep_g 3, neck_g 4, head_g 5. neck_g is turned 90 degrees about z so the
// bind rotations are exercised.
const testSkeletonASCII = `newmodel ptx0
setsupermodel ptx0 NULL
classification CHARACTER
beginmodelgeom ptx0
  node dummy ptx0
    parent NULL
  endnode
  node dummy rootdummy
    parent ptx0
    position 0 0 0
  endnode
  node dummy torso_g
    parent rootdummy
    position 0 0 1
  endnode
  node dummy lbicep_g
    parent torso_g
    position 0.3 0 0.4
  endnode
  node dummy neck_g
    parent torso_g
    position 0 0 0.5
    orientation 0 0 1 1.5707963
  endnode
  node dummy head_g
    parent neck_g
    position 0 0 0.2
  endnode
endmodelgeom
donemodel ptx0
`

// A neck part for that skeleton; its own tree has two nodes.
const testNeckPartASCII = `newmodel ptx0_neck001
setsupermodel ptx0_neck001 NULL
classification CHARACTER
beginmodelgeom ptx0_neck001
  node dummy ptx0_neck001
    parent NULL
  endnode
  node skin ptx0_neck001g
    parent ptx0_neck001
    bitmap blank
    render 1
    verts 3
      0 0 0
      0.1 0 0
      0 0.1 0.1
    tverts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2  1 0 1 2  1
    weights 3
      neck_g 1.0
      neck_g 0.5 head_g 0.5
      head_g 0.6 torso_g 0.4
  endnode
endmodelgeom
donemodel ptx0_neck001
`

// skinHeaderBones reads, per skin node of a compiled binary, the bone map (64
// tree positions) and the boneindexarray length.
func skinHeaderBones(t *testing.T, data []byte) map[string]struct {
	boneMap   [64]int16
	bindCount int
} {
	t.Helper()
	u32 := func(o int) int { return int(binary.LittleEndian.Uint32(data[o:])) }
	out := map[string]struct {
		boneMap   [64]int16
		bindCount int
	}{}
	seen := map[int]bool{}
	var walk func(ptr int)
	walk = func(ptr int) {
		if ptr == 0 || seen[ptr] {
			return
		}
		seen[ptr] = true
		o := 12 + ptr
		name, _, _ := bytes.Cut(data[o+32:o+64], []byte{0})
		if u32(o+108)&0x40 != 0 {
			b := o + 112 + 512
			var e struct {
				boneMap   [64]int16
				bindCount int
			}
			e.bindCount = u32(b + 24)
			for i := range e.boneMap {
				e.boneMap[i] = int16(binary.LittleEndian.Uint16(data[b+64+2*i:]))
			}
			out[strings.ToLower(string(name))] = e
		}
		for i := 0; i < u32(o+76); i++ {
			walk(u32(12 + u32(o+72) + 4*i))
		}
	}
	walk(u32(12 + 72))
	return out
}

func writeTestSkeleton(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "ptx0.mdl"), []byte(testSkeletonASCII), 0o644); err != nil {
		t.Fatal(err)
	}
	return dir
}

// TestPartSkinBindsToSkeleton: a skinned body part is bound to its creature
// skeleton, as the engine binds it in game (NWMAIN_FINDINGS.md finding 22): the
// bone map holds skeleton tree positions and the bind lists have one entry per
// skeleton node, computed with the part hanging from its attach node (neck_g).
// We used to number the bones against the part's own two-node tree, which the
// game reads as skeleton positions, so the part rendered torn.
func TestPartSkinBindsToSkeleton(t *testing.T) {
	dir := writeTestSkeleton(t)
	part := mustParseASCII(t, testNeckPartASCII)
	var warns []string
	var buf bytes.Buffer
	opts := CompileOptions{SupermodelDirs: []string{dir}, Warn: func(m string) { warns = append(warns, m) }}
	if err := CompileWithOptions(part, &buf, opts); err != nil {
		t.Fatal(err)
	}
	if len(warns) > 0 {
		t.Errorf("unexpected warnings: %q", warns)
	}
	data := buf.Bytes()

	hdr, ok := skinHeaderBones(t, data)["ptx0_neck001g"]
	if !ok {
		t.Fatal("skin node missing")
	}
	if hdr.bindCount != 6 {
		t.Errorf("boneindexarray has %d entries, want 6 (one per skeleton node)", hdr.bindCount)
	}
	wantMap := []int16{4, 5, 2} // neck_g, head_g, torso_g, in order of first use
	for i, w := range wantMap {
		if hdr.boneMap[i] != w {
			t.Errorf("bone map[%d] = %d, want %d", i, hdr.boneMap[i], w)
		}
	}

	lists := skinBindLists(t, data)["ptx0_neck001g"]
	if len(lists[0]) != 6 || len(lists[1]) != 6 {
		t.Fatalf("bind lists have %d/%d entries, want 6", len(lists[0]), len(lists[1]))
	}
	// The skin sits at neck_g: (0, 0, 1.5), turned 90 degrees about z.
	s := float32(math.Sqrt(0.5))
	want := map[int][2][]float32{
		0: {{s, 0, 0, s}, {0, 0, 1.5}},  // ptx0: the skin's own turn, up 1.5
		2: {{s, 0, 0, s}, {0, 0, 0.5}},  // torso_g
		4: {{1, 0, 0, 0}, {0, 0, 0}},    // neck_g: the skin's own space
		5: {{1, 0, 0, 0}, {0, 0, -0.2}}, // head_g, 0.2 above the neck
	}
	const eps = 1e-5
	for i, w := range want {
		for li, label := range []string{"qbone_ref_inv", "tbone_ref_inv"} {
			got := lists[li][i]
			same, negated := true, true
			for k := range w[li] {
				if math.Abs(float64(got[k]-w[li][k])) > eps {
					same = false
				}
				if math.Abs(float64(got[k]+w[li][k])) > eps {
					negated = false
				}
			}
			if !same && !(li == 0 && negated) {
				t.Errorf("%s[%d] = %v, want %v", label, i, got, w[li])
			}
		}
	}

	// Decompiling names the bones from the skeleton again.
	m, err := DecompileWithOptions(bytes.NewReader(data), int64(len(data)), DecompileOptions{SkeletonDirs: []string{dir}})
	if err != nil {
		t.Fatal(err)
	}
	if len(m.Warnings) > 0 {
		t.Errorf("unexpected decompile warnings: %v", m.Warnings)
	}
	wantBones := [][]string{{"neck_g"}, {"neck_g", "head_g"}, {"head_g", "torso_g"}}
	got := map[string]bool{}
	for _, vw := range m.FindNode("ptx0_neck001g").Skin.Weights {
		got[strings.Join(vw.Bones, " ")] = true
	}
	for _, b := range wantBones {
		if !got[strings.Join(b, " ")] {
			t.Errorf("decompiled weights lack a vertex weighted to %v; got %v", b, got)
		}
	}
}

// TestPartSkinWithoutSkeleton: when the skeleton cannot be found the compile
// says the part will render torn, and decompiling a skeleton-bound skin
// without the skeleton leaves its bones unnamed and warns, rather than naming
// them after the part's own nodes (pmh0_neck001's torso_g used to come out as
// its shadow mesh).
func TestPartSkinWithoutSkeleton(t *testing.T) {
	dir := writeTestSkeleton(t)
	part := mustParseASCII(t, testNeckPartASCII)

	var warns []string
	var buf bytes.Buffer
	if err := CompileWithOptions(part, &buf, CompileOptions{SupermodelDirs: []string{t.TempDir()}, Warn: func(m string) { warns = append(warns, m) }}); err != nil {
		t.Fatal(err)
	}
	if len(warns) != 1 || !strings.Contains(warns[0], "render torn") {
		t.Errorf("compile without the skeleton: warnings %q, want one saying the part renders torn", warns)
	}

	buf.Reset()
	if err := CompileWithOptions(part, &buf, CompileOptions{SupermodelDirs: []string{dir}}); err != nil {
		t.Fatal(err)
	}
	m, err := Decompile(bytes.NewReader(buf.Bytes()), int64(buf.Len()))
	if err != nil {
		t.Fatal(err)
	}
	if len(m.Warnings) != 1 || !strings.Contains(m.Warnings[0].Message, "bound to a creature skeleton") {
		t.Errorf("decompile without the skeleton: warnings %v, want one about the creature skeleton", m.Warnings)
	}
	for _, vw := range m.FindNode("ptx0_neck001g").Skin.Weights {
		if len(vw.Bones) > 0 {
			t.Errorf("bones named without the skeleton: %v", vw.Bones)
		}
	}
}

// TestHDNeckBindMatchesEngine decompiles the engine's own compile of the
// Beamdog HD neck pmh0_neck001 (3 nodes, bound to the 56-node pmh0 skeleton)
// and compiles it again: the bone map, boneindexarray and all 56 entries of
// both bind lists must match the engine's. Needs the game install and the HD
// pheno hak; skipped without them.
func TestHDNeckBindMatchesEngine(t *testing.T) {
	game := os.Getenv("NWN_ROOT")
	home, _ := os.UserHomeDir()
	if game == "" {
		game = filepath.Join(home, ".local", "share", "Steam", "steamapps", "common", "Neverwinter Nights")
	}
	hak := filepath.Join(home, ".local", "share", "Neverwinter Nights", "patch", "bdhd_pheno_other.hak")
	ref := findModelResource("pmh0_neck001", []string{hak})
	if ref == nil {
		t.Skip("bdhd_pheno_other.hak not available")
	}
	if sk, _ := loadSkeleton("pmh0", []string{game}); sk == nil {
		t.Skip("game install not available")
	}
	engine, err := ref.read()
	if err != nil {
		t.Fatal(err)
	}
	m, err := DecompileWithOptions(bytes.NewReader(engine), int64(len(engine)), DecompileOptions{SkeletonDirs: []string{game}})
	if err != nil {
		t.Fatal(err)
	}
	if len(m.Warnings) > 0 {
		t.Errorf("decompile warnings: %v", m.Warnings)
	}
	var buf bytes.Buffer
	if err := CompileWithOptions(m, &buf, CompileOptions{SupermodelDirs: []string{game}}); err != nil {
		t.Fatal(err)
	}
	ours := buf.Bytes()

	const skin = "pmh0_neck001g"
	we, wo := skinHeaderBones(t, engine)[skin], skinHeaderBones(t, ours)[skin]
	if we.bindCount != 56 || wo.bindCount != we.bindCount {
		t.Errorf("boneindexarray: ours %d, engine %d (want 56)", wo.bindCount, we.bindCount)
	}
	for i := 0; i < 64 && we.boneMap[i] != 0; i++ {
		if wo.boneMap[i] != we.boneMap[i] {
			t.Errorf("bone map[%d]: ours %d, engine %d", i, wo.boneMap[i], we.boneMap[i])
		}
	}
	le, lo := skinBindLists(t, engine)[skin], skinBindLists(t, ours)[skin]
	const eps = 2e-3
	for li, label := range []string{"qbone_ref_inv", "tbone_ref_inv"} {
		if len(lo[li]) != len(le[li]) {
			t.Fatalf("%s: ours %d entries, engine %d", label, len(lo[li]), len(le[li]))
		}
		for i := range le[li] {
			same, negated := true, true
			for k := range le[li][i] {
				if math.Abs(float64(le[li][i][k]-lo[li][i][k])) > eps {
					same = false
				}
				if math.Abs(float64(le[li][i][k]+lo[li][i][k])) > eps {
					negated = false
				}
			}
			if !same && !(li == 0 && negated) {
				t.Errorf("%s[%d]: ours %v, engine %v", label, i, lo[li][i], le[li][i])
			}
		}
	}
}

// A custom skeleton can rename the attach nodes in its own capart.2da: the
// report that found this had FOOTR -> j_kao on a custom skeleton, and compile
// looked for the stock rfoot_g, warned and bound the foot to its own tree.
// Tree positions: pcx0 0, rootdummy 1, j_kao 2 (at 0.2 0 0.1).
func TestPartAttachNodeFromCapart(t *testing.T) {
	dir := t.TempDir()
	skel := `newmodel pcx0
setsupermodel pcx0 NULL
classification CHARACTER
beginmodelgeom pcx0
  node dummy pcx0
    parent NULL
  endnode
  node dummy rootdummy
    parent pcx0
  endnode
  node dummy j_kao
    parent rootdummy
    position 0.2 0 0.1
  endnode
endmodelgeom
donemodel pcx0
`
	capart := "2DA V2.0\n\n    NAME    MDLNAME    NODENAME\n0   0       FOOTR      j_kao\n1   0       \"NECK\"     neck_g\n"
	for name, body := range map[string]string{"pcx0.mdl": skel, "capart.2da": capart} {
		if err := os.WriteFile(filepath.Join(dir, name), []byte(body), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	part := mustParseASCII(t, strings.NewReplacer("ptx0_neck001", "pcx0_footr010", "neck_g 0.5 head_g 0.5", "j_kao 1.0", "head_g 0.6 torso_g 0.4", "j_kao 1.0", "neck_g 1.0", "j_kao 1.0").Replace(testNeckPartASCII))

	var warns []string
	var buf bytes.Buffer
	if err := CompileWithOptions(part, &buf, CompileOptions{SupermodelDirs: []string{dir}, Warn: func(m string) { warns = append(warns, m) }}); err != nil {
		t.Fatal(err)
	}
	if len(warns) > 0 {
		t.Fatalf("warnings: %q", warns)
	}
	data := buf.Bytes()
	hdr := skinHeaderBones(t, data)["pcx0_footr010g"]
	if hdr.bindCount != 3 || hdr.boneMap[0] != 2 {
		t.Errorf("boneindexarray %d, bone map[0] %d; want 3 and 2 (j_kao)", hdr.bindCount, hdr.boneMap[0])
	}
	// Hung from j_kao, the skin sits in j_kao's own space.
	tl := skinBindLists(t, data)["pcx0_footr010g"][1]
	if len(tl) != 3 || math.Abs(float64(tl[2][0])) > 1e-6 || math.Abs(float64(tl[2][2])) > 1e-6 {
		t.Errorf("tbone_ref_inv %v, want j_kao's entry (index 2) at the origin", tl)
	}

	// Without a capart.2da the stock table is used, and the warning names it.
	if err := os.Remove(filepath.Join(dir, "capart.2da")); err != nil {
		t.Fatal(err)
	}
	warns = nil
	if err := CompileWithOptions(part, &buf, CompileOptions{SupermodelDirs: []string{dir}, Warn: func(m string) { warns = append(warns, m) }}); err != nil {
		t.Fatal(err)
	}
	if len(warns) != 1 || !strings.Contains(warns[0], "rfoot_g") || !strings.Contains(warns[0], "stock capart.2da") {
		t.Errorf("warnings %q, want one naming rfoot_g and the stock capart.2da", warns)
	}
}

func TestParse2DA(t *testing.T) {
	rows, err := parse2DA([]byte("2DA V2.0\r\nDEFAULT: ****\r\n\r\n  NAME MDLNAME NODENAME\r\n0 0 FOOTR rfoot_g\r\n1 \"a b\" ROBE ****\r\n"))
	if err != nil {
		t.Fatal(err)
	}
	if len(rows) != 2 || rows[0]["nodename"] != "rfoot_g" || rows[1]["name"] != "a b" || rows[1]["nodename"] != "****" {
		t.Errorf("rows %v", rows)
	}
}

// TestHDPartsBindMatchEngine recompiles every skinned part in Beamdog's
// bdhd_pheno_*.hak (engine compiles, bound in game: chests, necks, arms, legs,
// hands, feet, robes, cloaks, every phenotype) from its decompile and requires both bind lists
// to match the engine's. It pins the attach table and that robes bind to their
// own tree (capart.2da's ROBE -> root notwithstanding). Needs the game install
// and the haks; skipped without them.
func TestHDPartsBindMatchEngine(t *testing.T) {
	if testing.Short() {
		t.Skip("slow: recompiles every skinned HD part")
	}
	game := os.Getenv("NWN_ROOT")
	home, _ := os.UserHomeDir()
	if game == "" {
		game = filepath.Join(home, ".local", "share", "Steam", "steamapps", "common", "Neverwinter Nights")
	}
	haks, _ := filepath.Glob(filepath.Join(home, ".local", "share", "Neverwinter Nights", "patch", "bdhd_pheno_*.hak"))
	if len(haks) == 0 {
		t.Skip("bdhd_pheno_*.hak not available")
	}
	if sk, _ := loadSkeleton("pmh0", []string{game}); sk == nil {
		t.Skip("game install not available")
	}
	checked, bad := 0, 0
	for _, hak := range haks {
		a := openArchive(hak)
		if a == nil {
			continue
		}
		for key, read := range a.entries {
			if key.typ != resTypeMDL {
				continue
			}
			engine, err := read()
			if err != nil {
				t.Fatal(err)
			}
			want := skinBindLists(t, engine)
			if len(want) == 0 {
				continue
			}
			m, err := DecompileWithOptions(bytes.NewReader(engine), int64(len(engine)), DecompileOptions{SkeletonDirs: []string{game}})
			if err != nil {
				t.Fatalf("%s: %v", key.name, err)
			}
			var buf bytes.Buffer
			if err := CompileWithOptions(m, &buf, CompileOptions{SupermodelDirs: []string{game}}); err != nil {
				t.Fatalf("%s: %v", key.name, err)
			}
			got := skinBindLists(t, buf.Bytes())
			checked++
			for skin, w := range want {
				g := got[skin]
				for li := 0; li < 2; li++ {
					ok := len(g[li]) == len(w[li])
					for i := 0; ok && i < len(w[li]); i++ {
						same, negated := true, li == 0
						for k := range w[li][i] {
							if math.Abs(float64(w[li][i][k]-g[li][i][k])) > 2e-3 {
								same = false
							}
							if math.Abs(float64(w[li][i][k]+g[li][i][k])) > 2e-3 {
								negated = false
							}
						}
						ok = same || negated
					}
					if !ok {
						bad++
						if bad <= 5 {
							t.Errorf("%s skin %s: list %d differs from the engine's (%d vs %d entries)", key.name, skin, li, len(g[li]), len(w[li]))
						}
					}
				}
			}
		}
	}
	if checked < 1000 {
		t.Errorf("only %d skinned parts checked", checked)
	}
	t.Logf("%d skinned parts match the engine's bind lists", checked-bad)
}

// TestHeadAndRobeBindToOwnTree: the engine loads a head (CNWCAnimBaseHead,
// attached at head_g) and a robe (capart row 18) as separate objects with
// AttachToObject, not appended into the skeleton, so their skins bind to their
// own tree even when the skeleton is at hand. v4.4.0 and v4.4.1 bound them to
// the skeleton.
func TestHeadAndRobeBindToOwnTree(t *testing.T) {
	dir := writeTestSkeleton(t)
	for _, name := range []string{"ptx0_head001", "ptx0_robe001"} {
		part := mustParseASCII(t, strings.ReplaceAll(testNeckPartASCII, "ptx0_neck001", name))
		var warns []string
		var buf bytes.Buffer
		if err := CompileWithOptions(part, &buf, CompileOptions{SupermodelDirs: []string{dir}, Warn: func(m string) { warns = append(warns, m) }}); err != nil {
			t.Fatal(err)
		}
		if len(warns) > 0 {
			t.Errorf("%s: warnings %q", name, warns)
		}
		if hdr := skinHeaderBones(t, buf.Bytes())[name+"g"]; hdr.bindCount != 2 {
			t.Errorf("%s: boneindexarray has %d entries, want 2 (the file's own nodes)", name, hdr.bindCount)
		}
	}
}

// The game install's ovr/ folder overrides its key files' resources.
func TestGameInstallOvrOverridesKeys(t *testing.T) {
	root := t.TempDir()
	if err := os.MkdirAll(filepath.Join(root, "data"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(filepath.Join(root, "ovr"), 0o755); err != nil {
		t.Fatal(err)
	}
	// An empty key file: no BIFs, no resources.
	key := make([]byte, 64)
	copy(key, "KEY V1  ")
	binary.LittleEndian.PutUint32(key[16:], 64)
	binary.LittleEndian.PutUint32(key[20:], 64)
	if err := os.WriteFile(filepath.Join(root, "data", "nwn_base.key"), key, 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(root, "ovr", "CAPART.2da"), []byte("2DA V2.0\n\n NAME MDLNAME NODENAME\n0 0 NECK my_neck\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	table, from := findCapart([]string{root})
	if table["neck"] != "my_neck" || !strings.Contains(from, "capart") {
		t.Errorf("capart from %q: %v", from, table)
	}
}

// A skin vertex with no bone influence is drawn at the origin in game; the
// decompiler says so (Beamdog's pmo0_neck003 has ten, left uninitialised by
// the engine's compile).
func TestDecompileWarnsOnUnboundSkinVertices(t *testing.T) {
	src := strings.NewReplacer("ptx0_neck001", "tst_skin", "neck_g 1.0", "_ 0").Replace(testNeckPartASCII)
	src = strings.Replace(src, "  endnode\n  node skin", "  endnode\n  node dummy neck_g\n    parent tst_skin\n  endnode\n  node dummy head_g\n    parent tst_skin\n  endnode\n  node dummy torso_g\n    parent tst_skin\n  endnode\n  node skin", 1)
	m := mustDecompile(t, mustCompile(t, mustParseASCII(t, src)))
	n := 0
	for _, w := range m.Warnings {
		if strings.Contains(w.Message, "1 of 3 vertices with no valid bone influence") {
			n++
		}
	}
	if n != 1 {
		t.Errorf("warnings %v, want one about 1 of 3 unbound vertices", m.Warnings)
	}
}

// TestUnknownBoneInfluenceDropped: a weight on a bone that is not a node is
// dropped when compiling, as the engine drops it when it binds the skin
// (NWMAIN_FINDINGS.md finding 26). It used to keep a bone slot with no tree
// position, which the renderer skips, leaving the vertex on another skin's
// leftover matrix. The remaining weights are not renormalised.
func TestUnknownBoneInfluenceDropped(t *testing.T) {
	src := strings.NewReplacer("ptx0_neck001", "tst_drop", "neck_g 0.5 head_g 0.5", "neck_g 0.5 ghost_g 0.5").Replace(testNeckPartASCII)
	src = strings.Replace(src, "  endnode\n  node skin", "  endnode\n  node dummy neck_g\n    parent tst_drop\n  endnode\n  node dummy head_g\n    parent tst_drop\n  endnode\n  node dummy torso_g\n    parent tst_drop\n  endnode\n  node skin", 1)
	data := mustCompile(t, mustParseASCII(t, src))
	hdr := skinHeaderBones(t, data)["tst_dropg"]
	for i, p := range hdr.boneMap[:4] {
		if p < 0 && i < 3 {
			t.Errorf("bone slot %d has no tree position: %v", i, hdr.boneMap[:4])
		}
	}
	m := mustDecompile(t, data)
	found := false
	for _, vw := range m.FindNode("tst_dropg").Skin.Weights {
		for i, b := range vw.Bones {
			if strings.EqualFold(b, "ghost_g") {
				t.Errorf("ghost_g influence kept: %v", vw)
			}
			if strings.EqualFold(b, "neck_g") && len(vw.Bones) == 1 && math.Abs(float64(vw.Weights[i])-0.5) < 1e-6 {
				found = true
			}
		}
	}
	if !found {
		t.Error("the vertex weighted neck_g 0.5 + ghost_g 0.5 should keep neck_g at 0.5, unnormalised")
	}
}
