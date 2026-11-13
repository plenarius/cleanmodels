package mdl

import (
	"bytes"
	"encoding/binary"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// Rules the game's compiler follows that a comparison of every Layonara hak
// model found cleanmodels breaking (nwn-mdl-spec MDL-NODE-002, MDL-NODE-003,
// MDL-MESH-013, MDL-ASCII-054, MDL-CTRL-037). The engine fixtures
// (tests/fixtures/engine/zspec_dupparent, ...) check the compiled output
// against the game's own compile; these check what the fixtures cannot see.

func engineFixture(t *testing.T, name string) []byte {
	t.Helper()
	data, err := os.ReadFile(filepath.Join("..", "..", "tests", "fixtures", "engine", name))
	if err != nil {
		t.Fatal(err)
	}
	return data
}

// TestTwoRootsCountEveryNode: the game writes the last root's tree and counts
// every node it numbered (zspec_root2: 2 written, 4 counted).
func TestTwoRootsCountEveryNode(t *testing.T) {
	out := mustCompile(t, mustParseASCII(t, string(engineFixture(t, "zspec_root2.ascii.mdl"))))
	if got := binary.LittleEndian.Uint32(out[12+76:]); got != 4 {
		t.Errorf("node count %d, want 4", got)
	}
	m := mustDecompile(t, out)
	if len(m.Nodes) != 2 || m.Nodes[0].Name != "second" {
		t.Errorf("written nodes %v, want second and second_kid", nodeNames(m.Nodes))
	}
}

// TestDecompileDuplicateParentsRoundTrip: the game's compile of
// zspec_dupparent puts c2 under the first of two grips and c3 under the
// outer of two heads. Decompiled and compiled again, every node keeps its
// stored parent, and no warning is needed.
func TestDecompileDuplicateParentsRoundTrip(t *testing.T) {
	golden := engineFixture(t, "zspec_dupparent.mdl")
	m := mustDecompile(t, golden)
	for _, w := range m.Warnings {
		if strings.Contains(w.Message, "MDL-NODE-002") {
			t.Errorf("unexpected warning: %s", w.Message)
		}
	}
	var buf bytes.Buffer
	if err := Write(m, &buf); err != nil {
		t.Fatal(err)
	}
	again := mustCompile(t, mustParseASCII(t, buf.String()))
	sameTree(t, storedTree(t, golden), storedTree(t, again), true)
}

// TestDecompileWarnsLostParent: a stored tree no declaration order gives
// back (a child under the second of two siblings of one name, which the
// game's lookup never reaches first) is decompiled with a warning naming the
// node.
func TestDecompileWarnsLostParent(t *testing.T) {
	src := `newmodel lp
setsupermodel lp NULL
classification PLACEABLE
beginmodelgeom lp
node dummy lp
  parent NULL
endnode
node dummy x
  parent lp
endnode
node dummy x
  parent lp
endnode
node dummy kid
  parent lp
endnode
endmodelgeom lp
donemodel lp
`
	data := mustCompile(t, mustParseASCII(t, src))
	m, _, err := decompileStored(bytes.NewReader(data), int64(len(data)), DecompileOptions{})
	if err != nil {
		t.Fatal(err)
	}
	// Hang kid under the second x, as a binary from another compiler could.
	kid, second := m.Nodes[3], m.Nodes[2]
	kid.Parent = "x"
	m.storedParent[kid] = second
	declarationOrder(m, nil)
	found := false
	for _, w := range m.Warnings {
		if w.Node == "kid" && strings.Contains(w.Message, "MDL-NODE-002") {
			found = true
		}
	}
	if !found {
		t.Errorf("no warning for kid; warnings: %v", m.Warnings)
	}
}

// TestDanglyConstraintsFollowReorderedVertices: zspec_dangord's faces use the
// vertices out of order, so the merge reorders them without splitting any;
// each compiled vertex keeps its own constraint.
func TestDanglyConstraintsFollowReorderedVertices(t *testing.T) {
	out := mustCompile(t, mustParseASCII(t, string(engineFixture(t, "zspec_dangord.ascii.mdl"))))
	m := mustDecompileStored(t, out)
	for _, n := range m.Nodes {
		if n.Name != "flap" {
			continue
		}
		want := []float32{140, 100, 10, 20, 180, 30}
		got := n.Dangly.Constraints
		if len(got) != len(want) {
			t.Fatalf("constraints %v, want %v", got, want)
		}
		for i := range want {
			if got[i] != want[i] {
				t.Fatalf("constraints %v, want %v", got, want)
			}
		}
		return
	}
	t.Fatal("no flap node")
}

// TestListsGivenTwice: a second verts or tverts list replaces the first, a
// second faces list is dropped (MDL-ASCII-054), each with a warning.
func TestListsGivenTwice(t *testing.T) {
	res, err := ParseFile(filepath.Join("..", "..", "tests", "fixtures", "engine", "zspec_twolists.ascii.mdl"))
	if err != nil {
		t.Fatal(err)
	}
	byName := map[string]*Node{}
	for _, n := range res.Model.Nodes {
		byName[n.Name] = n
	}
	tw := byName["twice"].Mesh
	if len(tw.Verts) != 4 || tw.Verts[1].X != 2 || len(tw.TVerts) != 4 || len(tw.Faces) != 1 || tw.Faces[0].Verts != [3]int32{0, 1, 2} {
		t.Errorf("twice: %d verts (%v), %d tverts, faces %v", len(tw.Verts), tw.Verts, len(tw.TVerts), tw.Faces)
	}
	if fs := byName["facestwice"].Mesh.Faces; len(fs) != 1 {
		t.Errorf("facestwice: %d faces, want the first list's 1", len(fs))
	}
	if vs := byName["vertstwice"].Mesh.Verts; len(vs) != 4 || vs[0].Z != 7 {
		t.Errorf("vertstwice: %v, want the second list", vs)
	}
	warned := 0
	for _, e := range res.Errors {
		if strings.Contains(e.Message, "MDL-ASCII-054") {
			warned++
		}
	}
	if warned != 5 {
		t.Errorf("%d MDL-ASCII-054 warnings, want 5 (twice: 3, facestwice, vertstwice)", warned)
	}
}

// TestKeyListsGivenTwice: a second key list of one controller replaces the
// first instead of running on from it (MDL-CTRL-037).
func TestKeyListsGivenTwice(t *testing.T) {
	m := mustParseASCII(t, string(engineFixture(t, "zspec_twokeys.ascii.mdl")))
	for _, an := range m.Animations[0].Nodes {
		if an.Name != "mover" {
			continue
		}
		if len(an.PositionKeys) != 3 || an.PositionKeys[0].Value.Y != 1 {
			t.Errorf("position keys %v, want the second list's 3", an.PositionKeys)
		}
		if len(an.OrientationKeys) != 2 {
			t.Errorf("%d orientation keys, want 2", len(an.OrientationKeys))
		}
		return
	}
	t.Fatal("no mover")
}
