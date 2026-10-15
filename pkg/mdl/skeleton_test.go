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
