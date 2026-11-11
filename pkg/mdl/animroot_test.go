package mdl

import (
	"bytes"
	"encoding/binary"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// animCountNodes returns an animation's stored count_nodes, found by its
// 64-byte name field in the animation header (MDL-BIN-070).
func animCountNodes(t *testing.T, data []byte, name string) uint32 {
	t.Helper()
	field := make([]byte, 64)
	copy(field, name)
	i := bytes.Index(data, field)
	if i < 0 || i+72 > len(data) {
		t.Fatalf("animation %q not found", name)
	}
	return binary.LittleEndian.Uint32(data[i+68:])
}

func animNodeNames(a Animation) string {
	var names []string
	for _, n := range a.Nodes {
		names = append(names, n.Name)
	}
	return strings.Join(names, " ")
}

func engineProbe(t *testing.T, name string) (ascii *Model, golden []byte) {
	t.Helper()
	dir := filepath.Join("..", "..", "tests", "fixtures", "engine")
	res, err := ParseFile(filepath.Join(dir, name+".ascii.mdl"))
	if err != nil {
		t.Fatal(err)
	}
	golden, err = os.ReadFile(filepath.Join(dir, name+".mdl"))
	if err != nil {
		t.Fatal(err)
	}
	return res.Model, golden
}

// zspec_animroot (nwn-mdl-spec MDL-NODE-045): the game's compiler cuts an
// animation's tree to the node its animroot names, case for case, among the
// animation's own nodes, and keeps the whole tree when none matches; the node
// count stays the ASCII's.
func TestCompileAnimRootPrunesAsTheEngine(t *testing.T) {
	m, golden := engineProbe(t, "zspec_animroot")
	var ours bytes.Buffer
	if err := Compile(m, &ours); err != nil {
		t.Fatal(err)
	}
	want, got := mustDecompile(t, golden), mustDecompile(t, ours.Bytes())
	if len(got.Animations) != len(want.Animations) {
		t.Fatalf("%d animations, engine %d", len(got.Animations), len(want.Animations))
	}
	for i, wa := range want.Animations {
		ga := got.Animations[i]
		if animNodeNames(ga) != animNodeNames(wa) {
			t.Errorf("%s: nodes %q, engine %q", wa.Name, animNodeNames(ga), animNodeNames(wa))
		}
		if g, w := animCountNodes(t, ours.Bytes(), wa.Name), animCountNodes(t, golden, wa.Name); g != w {
			t.Errorf("%s: count_nodes %d, engine %d", wa.Name, g, w)
		}
	}
	if n := animNodeNames(got.Animations[0]); strings.Contains(n, "leg") {
		t.Errorf("prune kept leg: %s", n)
	}
}

// The decompile writes an animroot that matches the same node as the stored
// one, case for case, or none: `case` stores ARM over a node arm, which the
// game did not match, so writing it as arm would make a recompile prune.
func TestDecompileAnimRootKeepsWhatItMatches(t *testing.T) {
	_, golden := engineProbe(t, "zspec_animroot")
	m := mustDecompile(t, golden)
	roots := map[string]string{}
	for _, a := range m.Animations {
		roots[a.Name] = a.Root
	}
	for anim, want := range map[string]string{"prune": "arm", "missing": "nosuchnode", "case": "ARM", "geomonly": "leg"} {
		if roots[anim] != want {
			t.Errorf("%s: animroot %q, want %q", anim, roots[anim], want)
		}
	}
	// A recompile of the decompile keeps every tree as the engine stored it.
	var again bytes.Buffer
	if err := Compile(m, &again); err != nil {
		t.Fatal(err)
	}
	re := mustDecompile(t, again.Bytes())
	for i, a := range m.Animations {
		if animNodeNames(re.Animations[i]) != animNodeNames(a) {
			t.Errorf("%s: recompiled nodes %q, stored %q", a.Name, animNodeNames(re.Animations[i]), animNodeNames(a))
		}
	}

	for _, c := range []struct {
		stored        string
		matched, wide bool
		want          string
	}{
		{"Arm", true, false, "arm"},  // matched a stored "Arm": written as the node is
		{"arm", false, false, "ARM"}, // stored node "Arm", animroot "arm": no match, and "arm" would match
		{"arm", true, true, "ARM"},   // matched, but the tree is wider than the cut: must match none
		{"nosuchnode", false, false, "nosuchnode"},
		{"NULL", false, false, "null"},
		{"", false, false, ""},
	} {
		nodes := []AnimNode{{Name: "root"}, {Name: "arm"}}
		if got := writtenAnimRoot(c.stored, c.matched, c.wide, nodes); got != c.want {
			t.Errorf("writtenAnimRoot(%q, %v) = %q, want %q", c.stored, c.matched, got, c.want)
		}
	}
}

// zspec_animdup (MDL-ASCII-062): animations that share a name are all kept,
// in file order, each with its own data.
func TestCompileKeepsDuplicateAnimationsInOrder(t *testing.T) {
	m, golden := engineProbe(t, "zspec_animdup")
	var ours bytes.Buffer
	if err := Compile(m, &ours); err != nil {
		t.Fatal(err)
	}
	describe := func(mm *Model) string {
		var s []string
		for _, a := range mm.Animations {
			s = append(s, strings.ToLower(a.Name)+":"+fmtFloat(a.Length))
		}
		return strings.Join(s, " ")
	}
	got, want := describe(mustDecompile(t, ours.Bytes())), describe(mustDecompile(t, golden))
	if got != want || !strings.HasPrefix(want, "swing:1 idle:") {
		t.Errorf("animations %q, engine %q", got, want)
	}
}

// a_ogre and seven more Layonara binaries were not compiled by the game: their
// animroot rootdummy names a node, but the stored tree also has headconjure
// and handconjure beside it, which the game's compile would have cut. The game
// plays the tree as stored, so the decompile must write an animroot that
// matches nothing, or a recompile drops their keys.
func TestStoredAnimRootMatch(t *testing.T) {
	// a_ogre: root, rootdummy (pelvis_g under it), headconjure, handconjure.
	names := []string{"a_ogre", "rootdummy", "pelvis_g", "headconjure", "handconjure"}
	parents := []int{-1, 0, 1, 0, 0}
	if m, w := storedAnimRootMatch("rootdummy", names, parents); !m || !w {
		t.Errorf("a_ogre: matched %v, wide %v; want true, true", m, w)
	}
	// As the game stores it after the cut: no wider.
	if m, w := storedAnimRootMatch("rootdummy", names[:3], parents[:3]); !m || w {
		t.Errorf("cut tree: matched %v, wide %v; want true, false", m, w)
	}
	if m, w := storedAnimRootMatch("ROOTDUMMY", names, parents); m || w {
		t.Errorf("another case: matched %v, wide %v", m, w)
	}
	if m, w := storedAnimRootMatch("a_ogre", names, parents); !m || w {
		t.Errorf("the root: matched %v, wide %v; want true, false", m, w)
	}
	nodes := []AnimNode{{Name: "a_ogre"}, {Name: "rootdummy"}, {Name: "headconjure"}}
	if got := writtenAnimRoot("rootdummy", true, true, nodes); got != "ROOTDUMMY" {
		t.Errorf("wide tree: animroot written %q, want one that matches no node", got)
	}
}
