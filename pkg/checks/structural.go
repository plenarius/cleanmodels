package checks

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"strings"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

func init() {
	Register("duplicate_node_names", "structural", true, "Fix duplicate node names by appending a unique suffix", checkDuplicateNodeNames)
	Register("missing_parents", "structural", true, "Reparent orphaned nodes to the model root", checkMissingParents)
	Register("base_dummy_wrong_type", "structural", false, "Detect a root node that is not a dummy (a warning: the compiler and the game take a mesh root)", checkBaseDummyWrongType)
	Register("base_dummy_parent_null", "structural", true, "Detect a model with no root node (none has parent NULL); the fix makes the node named after the model the root", checkBaseDummyParentNull)
	Register("duplicate_animations", "structural", true, "Detect animations that share a name (the game plays only the first); the fix drops the later copies", checkDuplicateAnimations)
	Register("aabb_has_child_nodes", "structural", false, "Detect child nodes under AABB nodes", checkAabbHasChildNodes)
	Register("light_has_child_nodes", "structural", false, "Detect child nodes under light nodes", checkLightHasChildNodes)
	Register("too_many_walkmeshes", "structural", false, "Detect multiple AABB walkmesh nodes", checkTooManyWalkmeshes)
	Register("node_name_length", "structural", true, "Detect node names over 31 characters, which the engine cuts to 31: a warning, an error when the cut makes two names equal; the fix cuts them as the engine does, or gives colliding ones unique names", checkNodeNameLength)
	Register("text_after_donemodel", "structural", false, "Detect lines after donemodel: a warning for animations or nodes there (the game reads the animations, not the nodes), info for junk the game ignores", checkTextAfterDoneModel)
}

// checkTextAfterDoneModel reports lines after an ASCII model's donemodel. The
// game reads on to the end of the file, but outside a block it runs only the
// top-level keywords: an animation there is read, a node block or anything
// else is not (mdl.AfterDoneModel). Lines the game reads there (an animation,
// a second newmodel or geometry block) or node blocks it drops are a warning:
// what the game makes of them is rarely what was meant. Junk the game ignores
// entirely (a stray byte or tag left by a tool) is information.
func checkTextAfterDoneModel(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil || model.AfterDoneModel == nil {
		return nil
	}
	a := model.AfterDoneModel
	if a.Read == 0 && len(a.Nodes) == 0 {
		return []mdl.CheckResult{{
			Check:    "text_after_donemodel",
			Severity: mdl.SevInfo,
			Message: fmt.Sprintf("%s: %d line(s) after donemodel, from line %d, none of which the game reads",
				file, a.Lines, a.Line),
		}}
	}
	msg := fmt.Sprintf("%s: %d line(s) after donemodel, from line %d; the game reads on but runs only top-level keywords there", file, a.Lines, a.Line)
	if len(a.Animations) > 0 {
		msg += fmt.Sprintf("; animations read: %s", nameList(a.Animations))
	}
	if len(a.Nodes) > 0 {
		msg += fmt.Sprintf("; nodes not read: %s", nameList(a.Nodes))
	}
	if a.Ignored > 0 {
		msg += fmt.Sprintf("; %d line(s) not read", a.Ignored)
	}
	return []mdl.CheckResult{{
		Check:    "text_after_donemodel",
		Severity: mdl.SevWarning,
		Message:  msg,
	}}
}

// nameList quotes up to five names and counts the rest.
func nameList(names []string) string {
	const show = 5
	var q []string
	for i, n := range names {
		if i == show {
			q = append(q, fmt.Sprintf("and %d more", len(names)-show))
			break
		}
		q = append(q, fmt.Sprintf("%q", n))
	}
	return strings.Join(q, ", ")
}

// checkDuplicateNodeNames gives every node a unique name by suffixing the
// second and later users of a name. The first occurrence always keeps the
// original name, so anything that resolves a name to its first match (skin
// bone references) is unaffected.
//
// Renaming has to be occurrence-aware. A blanket "rename every reference to
// this name" pass looks right but scrambles the model: renaming the second
// "hand" would also rename the *first* hand's animation node, leaving the
// first hand with no animation and the second with two. So parentage is
// resolved by tree position up front, and each rename only touches the
// subtree and the animation node that genuinely belong to that occurrence.
func checkDuplicateNodeNames(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}

	allNames := make(map[string]bool, len(model.Nodes))
	for _, n := range model.Nodes {
		if n != nil {
			allNames[strings.ToLower(n.Name)] = true
		}
	}

	// Resolve parentage, and index animation nodes by name, before any rename
	// lands — once a name changes, the name-based Parent fields no longer
	// describe the original tree.
	geomParent := mdl.ResolveNodeParents(model.Nodes)
	animParent := make([]map[*mdl.AnimNode]*mdl.AnimNode, len(model.Animations))
	animOccur := make([]map[string][]*mdl.AnimNode, len(model.Animations))
	for ai := range model.Animations {
		nodes := model.Animations[ai].Nodes
		animParent[ai] = mdl.ResolveAnimNodeParents(nodes)
		occ := make(map[string][]*mdl.AnimNode, len(nodes))
		for i := range nodes {
			an := &nodes[i]
			key := strings.ToLower(an.Name)
			occ[key] = append(occ[key], an)
		}
		animOccur[ai] = occ
	}

	seen := make(map[string]int)
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil {
			continue
		}
		// Every node still carries its original name here: renames below only
		// ever touch the node currently being visited, plus Parent fields.
		lowerName := strings.ToLower(n.Name)
		seen[lowerName]++
		occurrence := seen[lowerName]
		if occurrence < 2 {
			continue
		}

		suffix := occurrence
		newName := suffixedName(n.Name, suffix)
		for allNames[strings.ToLower(newName)] {
			suffix++
			newName = suffixedName(n.Name, suffix)
		}

		old := n.Name
		if fix {
			allNames[strings.ToLower(newName)] = true
			n.Name = newName

			// Geometry children that actually hang off this occurrence.
			for _, child := range model.Nodes {
				if child != nil && child != n && geomParent[child] == n {
					child.Parent = newName
				}
			}

			// The matching animation node in each animation — the Nth anim node
			// of this name drives the Nth geometry node of it — and that node's
			// own children.
			for ai := range model.Animations {
				occ := animOccur[ai][lowerName]
				if occurrence-1 >= len(occ) {
					continue
				}
				an := occ[occurrence-1]
				an.Name = newName
				nodes := model.Animations[ai].Nodes
				for i := range nodes {
					child := &nodes[i]
					if child != an && animParent[ai][child] == an {
						child.Parent = newName
					}
				}
			}
			// Skin bone references are intentionally left alone: they resolve
			// to the first node with the name, which never gets renamed.
		}
		out = append(out, mdl.CheckResult{
			Check:    "duplicate_node_names",
			Node:     old,
			Severity: mdl.SevWarning,
			Fixed:    fix,
			Message: fmt.Sprintf(
				"%s: duplicate node %q %s %q",
				file, old, fixVerb(fix, "renamed to", "would be renamed to"), newName,
			),
		})
	}
	return out
}

func checkMissingParents(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	names := make(map[string]struct{}, len(model.Nodes))
	for _, n := range model.Nodes {
		if n != nil && n.Name != "" {
			names[strings.ToLower(n.Name)] = struct{}{}
		}
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil {
			continue
		}
		p := n.Parent
		if strings.EqualFold(p, "NULL") {
			continue
		}
		if _, ok := names[strings.ToLower(p)]; !ok {
			oldParent := p
			if fix {
				n.Parent = model.Name
			}
			out = append(out, mdl.CheckResult{
				Check:    "missing_parents",
				Node:     n.Name,
				Severity: mdl.SevError,
				Fixed:    fix,
				Message: fmt.Sprintf(
					"%s: node %q references missing parent %q; %s %q",
					file, n.Name, oldParent, fixVerb(fix, "reparented to", "would be reparented to"), model.Name,
				),
			})
		}
	}
	return out
}

// checkBaseDummyWrongType reports a root node (parent NULL, named after the
// model) that is not a dummy. The usual export puts a dummy above the meshes,
// but nothing requires it: the engine takes as root whichever node has parent
// NULL (nwn-mdl-spec MDL-NODE-001), the compiler handles a mesh root, and the
// 2002 armour parts pmh0_chest201 and pmh0_head154, whose root is their mesh,
// draw correctly in game (checked 2026-11-11). So it is a warning, not fatal.
func checkBaseDummyWrongType(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || !strings.EqualFold(n.Parent, "NULL") || !strings.EqualFold(n.Name, model.Name) {
			continue
		}
		if n.NodeType() != "dummy" {
			out = append(out, mdl.CheckResult{
				Check:    "base_dummy_wrong_type",
				Node:     n.Name,
				Severity: mdl.SevWarning,
				Message: fmt.Sprintf(
					"%s: root node %q (parent NULL) has type %q, not dummy; the compiler and the game accept a mesh root, but --wrap-root can put a dummy above it",
					file, n.Name, n.NodeType(),
				),
				Fixed: false,
			})
		}
	}
	return out
}

// checkBaseDummyParentNull reports a model with no root: no node whose parent
// is NULL (nwn-mdl-spec MDL-NODE-001). The fix makes the node named after the
// model the root. The root is found by its NULL parent, not by its name: a
// model whose root is a dummy and whose mesh shares the model's name has a
// root, and the mesh must stay under it (duplicate_node_names reports the
// shared name).
func checkBaseDummyParentNull(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil || model.Name == "" {
		return nil
	}
	var named *mdl.Node
	for _, n := range model.Nodes {
		if n == nil {
			continue
		}
		if strings.EqualFold(n.Parent, "NULL") {
			return nil
		}
		if named == nil && strings.EqualFold(n.Name, model.Name) {
			named = n
		}
	}
	if named == nil {
		return nil
	}
	old := named.Parent
	if fix {
		named.Parent = "NULL"
	}
	return []mdl.CheckResult{{
		Check:    "base_dummy_parent_null",
		Node:     named.Name,
		Severity: mdl.SevError,
		Fixed:    fix,
		Message: fmt.Sprintf(
			"%s: no node has parent NULL, so the model has no root; node %q, named after the model, has parent %q, %s NULL",
			file, named.Name, old, fixVerb(fix, "set to", "would be set to"),
		),
	}}
}

// checkDuplicateAnimations reports animations that share a name, compared
// case-insensitively. The game's compiler keeps every copy in file order, but
// the engine finds an animation by name in list order (strcasecmp) and takes
// the first, so only the first copy ever plays (nwn-mdl-spec MDL-ASCII-062,
// E70). The fix keeps that first copy as it is and drops the later ones,
// which never play; it never merges them, which would change what plays.
func checkDuplicateAnimations(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	type dup struct {
		first     int      // index of the copy that plays
		positions []string // 1-based positions of the later copies
		names     []string // their names as written
	}
	byName := map[string]*dup{}
	var order []string
	for i, a := range model.Animations {
		key := strings.ToLower(a.Name)
		d, ok := byName[key]
		if !ok {
			byName[key] = &dup{first: i}
			continue
		}
		if len(d.positions) == 0 {
			order = append(order, key)
		}
		d.positions = append(d.positions, fmt.Sprint(i+1))
		d.names = append(d.names, fmt.Sprintf("%q", a.Name))
	}
	if len(order) == 0 {
		return nil
	}

	var out []mdl.CheckResult
	for _, key := range order {
		d := byName[key]
		first := model.Animations[d.first]
		out = append(out, mdl.CheckResult{
			Check:    "duplicate_animations",
			Node:     first.Name,
			Severity: mdl.SevWarning,
			Fixed:    fix,
			Message: fmt.Sprintf(
				"%s: animation %q (animation %d of %d) is repeated as %s (animation %s); the game plays only the first; the later copies %s",
				file, first.Name, d.first+1, len(model.Animations), strings.Join(d.names, ", "), strings.Join(d.positions, ", "),
				fixVerb(fix, "were dropped", "would be dropped"),
			),
		})
	}
	if fix {
		seen := map[string]bool{}
		kept := model.Animations[:0]
		for _, a := range model.Animations {
			key := strings.ToLower(a.Name)
			if seen[key] {
				continue
			}
			seen[key] = true
			kept = append(kept, a)
		}
		model.Animations = kept
	}
	return out
}

func checkChildlessNodeType(checkName, nodeType string) mdl.CheckFunc {
	return func(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
		if model == nil {
			return nil
		}
		restricted := make(map[string]struct{})
		for _, n := range model.Nodes {
			if n != nil && n.NodeType() == nodeType {
				restricted[strings.ToLower(n.Name)] = struct{}{}
			}
		}
		var out []mdl.CheckResult
		for _, n := range model.Nodes {
			if n == nil {
				continue
			}
			if _, ok := restricted[strings.ToLower(n.Parent)]; ok {
				out = append(out, mdl.CheckResult{
					Check:    checkName,
					Node:     n.Parent,
					Severity: mdl.SevWarning,
					Message: fmt.Sprintf(
						"%s: %s node %q has child node %q (use --reparent-children to fix)",
						file, nodeType, n.Parent, n.Name,
					),
					Fixed: false,
				})
			}
		}
		return out
	}
}

var checkAabbHasChildNodes = checkChildlessNodeType("aabb_has_child_nodes", "aabb")
var checkLightHasChildNodes = checkChildlessNodeType("light_has_child_nodes", "light")

func checkTooManyWalkmeshes(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var aabbs []string
	for _, n := range model.Nodes {
		if n != nil && n.NodeType() == "aabb" {
			aabbs = append(aabbs, n.Name)
		}
	}
	if len(aabbs) <= 1 {
		return nil
	}
	return []mdl.CheckResult{{
		Check:    "too_many_walkmeshes",
		Severity: mdl.SevError,
		Message: fmt.Sprintf(
			"%s: found %d AABB (walkmesh) nodes, expected at most 1: %v",
			file, len(aabbs), aabbs,
		),
		Fixed: false,
	}}
}

// maxNodeNameLen is how much of a node name the game's compiler keeps: a
// longer name is stored as its first 31 characters (nwn-mdl-spec MDL-BIN-007).
const maxNodeNameLen = 31

// checkNodeNameLength reports node names longer than maxNodeNameLen. The
// engine keeps the first 31 characters, so a name whose cut is still unique
// finds the same node and is only a warning; the fix cuts it the same way.
// When the cut equals another node's name (or cut name), case aside, the
// engine can find the wrong node: that is an error, and the fix gives the name
// a unique short form instead. A name the model already uses more than once
// collides with nothing new (duplicate_node_names reports it).
func checkNodeNameLength(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}

	// The distinct names (lower case) behind each cut name (lower case).
	byCut := make(map[string]map[string]string)
	isParent := make(map[string]bool)
	for _, n := range model.Nodes {
		if n == nil {
			continue
		}
		cut := strings.ToLower(cutNodeName(n.Name))
		if byCut[cut] == nil {
			byCut[cut] = make(map[string]string)
		}
		if _, ok := byCut[cut][strings.ToLower(n.Name)]; !ok {
			byCut[cut][strings.ToLower(n.Name)] = n.Name
		}
		if n.Parent != "" {
			isParent[strings.ToLower(n.Parent)] = true
		}
	}

	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || len(n.Name) <= maxNodeNameLen {
			continue
		}
		oldName := n.Name
		cut := cutNodeName(oldName)
		var other string
		for low, name := range byCut[strings.ToLower(cut)] {
			if low != strings.ToLower(oldName) && (other == "" || name < other) {
				other = name
			}
		}

		if other == "" {
			if fix {
				renameNodeEverywhere(model, oldName, cut)
			}
			msg := fmt.Sprintf(
				"%s: node name %q is %d characters; the engine keeps the first %d, %q, which no node of another name has; %s",
				file, oldName, len(oldName), maxNodeNameLen, cut, fixVerb(fix, "cut to that", "would be cut to that"),
			)
			if isParent[strings.ToLower(oldName)] {
				msg += " (the game's own compiler hangs on a parent name this long)"
			}
			out = append(out, mdl.CheckResult{
				Check:    "node_name_length",
				Node:     cut,
				Severity: mdl.SevWarning,
				Fixed:    fix,
				Message:  msg,
			})
			continue
		}

		newName := shortenNodeName(oldName)
		if fix {
			renameNodeEverywhere(model, oldName, newName)
		}
		out = append(out, mdl.CheckResult{
			Check:    "node_name_length",
			Node:     newName,
			Severity: mdl.SevError,
			Fixed:    fix,
			Message: fmt.Sprintf(
				"%s: node name %q is %d characters; the engine keeps the first %d, %q, which node %q also gives, so the engine can find the wrong node; %s %q",
				file, oldName, len(oldName), maxNodeNameLen, cut, other, fixVerb(fix, "renamed to", "would be renamed to"), newName,
			),
		})
	}
	return out
}

// cutNodeName is the name the engine keeps (MDL-BIN-007).
func cutNodeName(name string) string {
	if len(name) > maxNodeNameLen {
		return name[:maxNodeNameLen]
	}
	return name
}

// shortenNodeName produces a deterministic <=31 char name by keeping as much
// of the original prefix as possible and replacing the tail with a hash.
// Format: <prefix>_<4-char hex hash of full name> (total <= 31).
func shortenNodeName(name string) string {
	h := sha256.Sum256([]byte(name))
	hashSuffix := hex.EncodeToString(h[:])[:4]
	// "_" + 4 hex chars = 5 chars for suffix
	maxPrefix := 31 - 5
	prefix := name
	if len(prefix) > maxPrefix {
		prefix = prefix[:maxPrefix]
	}
	return prefix + "_" + hashSuffix
}

func suffixedName(name string, suffix int) string {
	s := fmt.Sprintf("_%d", suffix)
	base := name
	if len(base)+len(s) > 31 {
		base = base[:31-len(s)]
	}
	return base + s
}

// renameNodeEverywhere renames all occurrences of oldName to newName,
// including node Name fields, Parent references, and skin bone references.
func renameNodeEverywhere(model *mdl.Model, oldName, newName string) {
	renameNodeRefs(model, oldName, newName, true)
}

func renameNodeRefs(model *mdl.Model, oldName, newName string, includeNodeName bool) {
	for _, n := range model.Nodes {
		if n == nil {
			continue
		}
		if includeNodeName && strings.EqualFold(n.Name, oldName) {
			n.Name = newName
		}
		if strings.EqualFold(n.Parent, oldName) {
			n.Parent = newName
		}
		if n.Skin != nil {
			for i, vw := range n.Skin.Weights {
				for j, b := range vw.Bones {
					if strings.EqualFold(b, oldName) {
						n.Skin.Weights[i].Bones[j] = newName
					}
				}
			}
		}
	}
	for ai := range model.Animations {
		for ni := range model.Animations[ai].Nodes {
			an := &model.Animations[ai].Nodes[ni]
			if strings.EqualFold(an.Name, oldName) {
				an.Name = newName
			}
			if strings.EqualFold(an.Parent, oldName) {
				an.Parent = newName
			}
		}
	}
}
