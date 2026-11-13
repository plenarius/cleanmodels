package checks

import (
	"strings"
	"testing"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

// bilateralHandsModel builds the canonical creature-rig shape that used to
// break renaming: two "hand" nodes, one under each forearm, each driven by its
// own animation node. The two animation nodes carry different key counts so a
// test can tell which hand ended up with which animation.
func bilateralHandsModel() *mdl.Model {
	node := func(name, parent string) *mdl.Node {
		return &mdl.Node{Name: name, Parent: parent, Scale: 1, Orientation: mdl.Vec4{W: 1}}
	}
	// Declaration order is DFS pre-order, as both the decompiler and ASCII
	// sources produce.
	return &mdl.Model{
		Name:           "rig",
		Classification: "CHARACTER",
		AnimationScale: 1,
		Nodes: []*mdl.Node{
			node("rig", "NULL"),
			node("lforearm", "rig"),
			node("hand", "lforearm"),
			node("rforearm", "rig"),
			node("hand", "rforearm"),
		},
		Animations: []mdl.Animation{{
			Name:   "walk",
			Length: 1,
			Root:   "rig",
			Nodes: []mdl.AnimNode{
				{Name: "rig", Parent: "NULL"},
				{Name: "lforearm", Parent: "rig"},
				{Name: "hand", Parent: "lforearm", PositionKeys: []mdl.PositionKey{
					{Time: 0}, {Time: 1},
				}},
				{Name: "rforearm", Parent: "rig"},
				{Name: "hand", Parent: "rforearm", PositionKeys: []mdl.PositionKey{
					{Time: 0}, {Time: 0.5}, {Time: 1},
				}},
			},
		}},
	}
}

// TestDuplicateNodeNamesRebindsMatchingAnimation is the regression guard for the
// rebinding bug. Renaming the second "hand" used to rename *every* animation
// node called "hand", so the first hand lost its animation entirely and the
// renamed one ended up with two. Each hand must keep its own animation.
func TestDuplicateNodeNamesRebindsMatchingAnimation(t *testing.T) {
	model := bilateralHandsModel()
	results := checkDuplicateNodeNames(model, "rig.mdl", true)
	if len(results) != 1 {
		t.Fatalf("expected exactly one rename, got %d", len(results))
	}

	// Geometry: one hand keeps the name, the other gets a unique one.
	var geomNames []string
	for _, n := range model.Nodes {
		if strings.HasPrefix(strings.ToLower(n.Name), "hand") {
			geomNames = append(geomNames, n.Name)
		}
	}
	if len(geomNames) != 2 || geomNames[0] == geomNames[1] {
		t.Fatalf("hand nodes should now have distinct names, got %v", geomNames)
	}

	// Every animation node name must map to exactly one geometry node name,
	// and carry the key count that belonged to that hand.
	animKeys := map[string]int{}
	for i := range model.Animations[0].Nodes {
		an := &model.Animations[0].Nodes[i]
		if strings.HasPrefix(strings.ToLower(an.Name), "hand") {
			if _, dup := animKeys[an.Name]; dup {
				t.Fatalf("two animation nodes still share the name %q", an.Name)
			}
			animKeys[an.Name] = len(an.PositionKeys)
		}
	}
	if len(animKeys) != 2 {
		t.Fatalf("expected two distinctly-named hand anim nodes, got %v", animKeys)
	}
	for _, name := range geomNames {
		if _, ok := animKeys[name]; !ok {
			t.Errorf("geometry node %q has no animation node after rename (animation was stolen); anim nodes: %v", name, animKeys)
		}
	}
	// The 2-key animation belonged to the left hand and the 3-key to the right;
	// both counts must still be present, i.e. neither was duplicated or lost.
	counts := map[int]bool{}
	for _, c := range animKeys {
		counts[c] = true
	}
	if !counts[2] || !counts[3] {
		t.Errorf("animation data was not preserved per hand, key counts = %v", animKeys)
	}
}

// TestDuplicateNodeNamesRepointsOnlyOwnChildren covers the mesh-plus-hook idiom
// and nested duplicates: a child must follow the specific duplicate it hung
// off, not whichever one happened to be renamed last.
func TestDuplicateNodeNamesRepointsOnlyOwnChildren(t *testing.T) {
	node := func(name, parent string) *mdl.Node {
		return &mdl.Node{Name: name, Parent: parent, Scale: 1, Orientation: mdl.Vec4{W: 1}}
	}
	first := node("bicep", "torso")
	firstChild := node("fore1", "bicep")
	second := node("bicep", "torso")
	secondChild := node("fore2", "bicep")
	model := &mdl.Model{
		Name:  "rig",
		Nodes: []*mdl.Node{node("rig", "NULL"), node("torso", "rig"), first, firstChild, second, secondChild},
	}

	checkDuplicateNodeNames(model, "rig.mdl", true)

	if first.Name != "bicep" {
		t.Errorf("first occurrence should keep its name, got %q", first.Name)
	}
	if second.Name == "bicep" {
		t.Fatal("second occurrence should have been renamed")
	}
	// Both children hang off the first bicep, as the game's compiler reads
	// them (nwn-mdl-spec MDL-NODE-002: the first node of the name in tree
	// order), so neither follows the rename.
	if firstChild.Parent != "bicep" {
		t.Errorf("child of the first bicep repointed to %q, want %q", firstChild.Parent, "bicep")
	}
	if secondChild.Parent != "bicep" {
		t.Errorf("second child repointed to %q, want %q: it hangs off the first bicep", secondChild.Parent, "bicep")
	}
}

// TestDuplicateNodeNamesRepointsNestedHook: the renamed copy's own children
// follow it. A hook dummy declared under the second bicep before any child
// names "bicep" again belongs to the second bicep only when that is the one
// the game finds first; here the second bicep is under an earlier arm.
func TestDuplicateNodeNamesRepointsNestedHook(t *testing.T) {
	node := func(name, parent string) *mdl.Node {
		return &mdl.Node{Name: name, Parent: parent, Scale: 1, Orientation: mdl.Vec4{W: 1}}
	}
	armL, armR := node("armL", "torso"), node("armR", "torso")
	first := node("bicep", "armR")
	second := node("bicep", "armL") // first in tree order: armL comes first
	child := node("fore", "bicep")
	model := &mdl.Model{
		Name:  "rig",
		Nodes: []*mdl.Node{node("rig", "NULL"), node("torso", "rig"), armL, armR, first, second, child},
	}
	checkDuplicateNodeNames(model, "rig.mdl", true)
	if second.Name == "bicep" {
		t.Fatal("second occurrence should have been renamed")
	}
	if child.Parent != second.Name {
		t.Errorf("child points at %q, want %q: the game attaches it to the bicep under armL", child.Parent, second.Name)
	}
}

// TestDuplicateNodeNamesLeavesSkinBonesAlone pins that bone references are not
// rewritten. Bones resolve by name to the first node carrying it, and that
// occurrence never gets renamed — rewriting the reference would silently
// repoint the skin at the other duplicate.
func TestDuplicateNodeNamesLeavesSkinBonesAlone(t *testing.T) {
	node := func(name, parent string) *mdl.Node {
		return &mdl.Node{Name: name, Parent: parent, Scale: 1, Orientation: mdl.Vec4{W: 1}}
	}
	skinned := node("body", "rig")
	skinned.Skin = &mdl.SkinData{Weights: []mdl.VertexWeight{
		{Bones: []string{"hand"}, Weights: []float32{1}},
	}}
	model := &mdl.Model{
		Name: "rig",
		Nodes: []*mdl.Node{
			node("rig", "NULL"),
			node("lforearm", "rig"),
			node("hand", "lforearm"),
			node("rforearm", "rig"),
			node("hand", "rforearm"),
			skinned,
		},
	}

	checkDuplicateNodeNames(model, "rig.mdl", true)

	if got := skinned.Skin.Weights[0].Bones[0]; got != "hand" {
		t.Errorf("skin bone reference was rewritten to %q; it must stay %q so the skin keeps pointing at the first hand", got, "hand")
	}
}
