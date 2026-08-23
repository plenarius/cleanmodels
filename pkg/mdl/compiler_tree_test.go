package mdl

import (
	"strings"
	"testing"
)

// nodeNamed is a terse dummy-node constructor for tree-shape tests.
func nodeNamed(name, parent string) *Node {
	return &Node{Name: name, Parent: parent, Scale: 1, Orientation: Vec4{W: 1}}
}

// TestResolveGeomTreeUniqueNames pins the no-duplicates case: resolution must
// agree with a plain name lookup, so ordinary models are unaffected.
func TestResolveGeomTreeUniqueNames(t *testing.T) {
	root := nodeNamed("root", "NULL")
	a := nodeNamed("a", "root")
	b := nodeNamed("b", "a")
	parentOf, childrenOf := resolveGeomTree([]*Node{root, a, b})

	if _, isRoot := parentOf[root]; isRoot {
		t.Error("root should have no parent")
	}
	if parentOf[a] != root {
		t.Error("a's parent should be root")
	}
	if parentOf[b] != a {
		t.Error("b's parent should be a")
	}
	if got := childrenOf[root]; len(got) != 1 || got[0] != a {
		t.Errorf("root children = %v, want [a]", got)
	}
}

// TestResolveGeomTreeBilateralDuplicates covers the creature-rig case: two
// "hand" dummies, one under each forearm. Each must attach to its own forearm
// rather than both binding to whichever forearm the name lookup happened to
// find.
func TestResolveGeomTreeBilateralDuplicates(t *testing.T) {
	root := nodeNamed("root", "NULL")
	lfore := nodeNamed("lforearm", "root")
	lhand := nodeNamed("hand", "lforearm")
	rfore := nodeNamed("rforearm", "root")
	rhand := nodeNamed("hand", "rforearm")
	// DFS pre-order, the order both the decompiler and ASCII sources produce.
	parentOf, childrenOf := resolveGeomTree([]*Node{root, lfore, lhand, rfore, rhand})

	if parentOf[lhand] != lfore {
		t.Error("left hand must attach to lforearm")
	}
	if parentOf[rhand] != rfore {
		t.Error("right hand must attach to rforearm")
	}
	if got := childrenOf[lfore]; len(got) != 1 || got[0] != lhand {
		t.Errorf("lforearm children = %v, want exactly the left hand", got)
	}
	if got := childrenOf[rfore]; len(got) != 1 || got[0] != rhand {
		t.Errorf("rforearm children = %v, want exactly the right hand", got)
	}
}

// TestResolveGeomTreeMeshHookIdiom covers the "head" danglymesh that contains a
// "head" dummy — a node whose child shares its own name. The child must not
// become its own parent, and must not be double-attached.
func TestResolveGeomTreeMeshHookIdiom(t *testing.T) {
	root := nodeNamed("root", "NULL")
	neck := nodeNamed("neck", "root")
	headMesh := nodeNamed("head", "neck")
	headHook := nodeNamed("head", "head")
	parentOf, childrenOf := resolveGeomTree([]*Node{root, neck, headMesh, headHook})

	if parentOf[headMesh] != neck {
		t.Error("head mesh must attach to neck")
	}
	if parentOf[headHook] != headMesh {
		t.Error("head hook dummy must attach to the head mesh")
	}
	if parentOf[headHook] == headHook {
		t.Fatal("head hook must not be its own parent")
	}
	if got := childrenOf[headHook]; len(got) != 0 {
		t.Errorf("head hook should be a leaf, got children %v", got)
	}
}

// TestResolveGeomTreeDuplicatesShareParent covers two same-named siblings under
// the SAME parent (c_asabmage has two "rbicep_g" under "torso_g"). Both must be
// retained as separate children, and each keeps its own subtree.
func TestResolveGeomTreeDuplicatesShareParent(t *testing.T) {
	root := nodeNamed("root", "NULL")
	torso := nodeNamed("torso_g", "root")
	bicep1 := nodeNamed("rbicep_g", "torso_g")
	child1 := nodeNamed("rfore1_g", "rbicep_g")
	bicep2 := nodeNamed("rbicep_g", "torso_g")
	child2 := nodeNamed("rfore2_g", "rbicep_g")
	parentOf, childrenOf := resolveGeomTree([]*Node{root, torso, bicep1, child1, bicep2, child2})

	if got := childrenOf[torso]; len(got) != 2 {
		t.Fatalf("torso should keep both biceps, got %d children", len(got))
	}
	// Nearest-preceding resolution: each child binds to the bicep declared
	// immediately above it, which is the DFS pre-order truth.
	if parentOf[child1] != bicep1 {
		t.Error("first forearm must attach to the first bicep")
	}
	if parentOf[child2] != bicep2 {
		t.Error("second forearm must attach to the second bicep")
	}
}

// TestPairAnimNodesToGeom checks that the Nth animation node of a duplicated
// name binds to the Nth geometry node of that name, so duplicate nodes keep
// their own controllers.
func TestPairAnimNodesToGeom(t *testing.T) {
	geom := []*Node{
		nodeNamed("root", "NULL"),
		nodeNamed("emit", "root"),
		nodeNamed("emit", "root"),
	}
	occ := nodeOccurrences(geom)
	anim := []AnimNode{
		{Name: "root", Parent: "NULL"},
		{Name: "emit", Parent: "root"},
		{Name: "emit", Parent: "root"},
	}
	pair := pairAnimNodesToGeom(anim, occ)

	if pair[&anim[1]] != geom[1] {
		t.Error("first emit anim node must pair with the first emit geom node")
	}
	if pair[&anim[2]] != geom[2] {
		t.Error("second emit anim node must pair with the second emit geom node")
	}
}

// TestRoundtripDuplicateNodeNamesPreserved is the end-to-end regression guard.
// The compiler used to key node identity by name, so the second node sharing a
// name was silently dropped along with its animation — costing real geometry on
// ~40 stock models (bilateral creature limbs, repeated VFX emitters). Both
// nodes must survive a compile, keep their own parent, and keep their own
// distinct animation data.
func TestRoundtripDuplicateNodeNamesPreserved(t *testing.T) {
	src := `newmodel duptest
setsupermodel duptest NULL
classification CHARACTER
setanimationscale 1.00
beginmodelgeom duptest
  node dummy duptest
    parent NULL
  endnode
  node dummy lforearm
    parent duptest
  endnode
  node dummy hand
    parent lforearm
  endnode
  node dummy rforearm
    parent duptest
  endnode
  node dummy hand
    parent rforearm
  endnode
endmodelgeom

newanim walk duptest
  animroot duptest
  length 1.0
  transtime 0.25
  node dummy duptest
    parent NULL
  endnode
  node dummy lforearm
    parent duptest
  endnode
  node dummy hand
    parent lforearm
    positionkey 2
      0.0 1 0 0
      1.0 2 0 0
    endlist
  endnode
  node dummy rforearm
    parent duptest
  endnode
  node dummy hand
    parent rforearm
    positionkey 3
      0.0 9 0 0
      0.5 9 1 0
      1.0 9 2 0
    endlist
  endnode
doneanim walk duptest

donemodel duptest
`
	m := mustParseASCII(t, src)
	if got := countNodesNamed(m.Nodes, "hand"); got != 2 {
		t.Fatalf("parser kept %d hand nodes, want 2", got)
	}
	m2 := mustDecompile(t, mustCompile(t, m))

	if got := countNodesNamed(m2.Nodes, "hand"); got != 2 {
		t.Fatalf("compile dropped a duplicate node: %d hand nodes survived, want 2", got)
	}

	// Each hand must still hang off its own forearm.
	parents := map[string]bool{}
	for _, n := range m2.Nodes {
		if strings.EqualFold(n.Name, "hand") {
			parents[strings.ToLower(n.Parent)] = true
		}
	}
	for _, want := range []string{"lforearm", "rforearm"} {
		if !parents[want] {
			t.Errorf("no hand node parented to %q; parents present: %v", want, parents)
		}
	}

	// Each hand must keep its own animation, identified by key count.
	var walk *Animation
	for i := range m2.Animations {
		if strings.EqualFold(m2.Animations[i].Name, "walk") {
			walk = &m2.Animations[i]
			break
		}
	}
	if walk == nil {
		t.Fatal("animation 'walk' missing after roundtrip")
	}
	byParent := map[string]int{}
	for i := range walk.Nodes {
		an := &walk.Nodes[i]
		if strings.EqualFold(an.Name, "hand") {
			byParent[strings.ToLower(an.Parent)] = len(an.PositionKeys)
		}
	}
	if byParent["lforearm"] != 2 {
		t.Errorf("hand under lforearm has %d position keys, want 2", byParent["lforearm"])
	}
	if byParent["rforearm"] != 3 {
		t.Errorf("hand under rforearm has %d position keys, want 3", byParent["rforearm"])
	}
}

func countNodesNamed(nodes []*Node, name string) int {
	n := 0
	for _, node := range nodes {
		if node != nil && strings.EqualFold(node.Name, name) {
			n++
		}
	}
	return n
}
