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
	Register("duplicate_animations", "structural", true, "Merge duplicate animation blocks", checkDuplicateAnimations)
	Register("aabb_has_child_nodes", "structural", false, "Detect child nodes under AABB nodes", checkAabbHasChildNodes)
	Register("light_has_child_nodes", "structural", false, "Detect child nodes under light nodes", checkLightHasChildNodes)
	Register("too_many_walkmeshes", "structural", false, "Detect multiple AABB walkmesh nodes", checkTooManyWalkmeshes)
	Register("node_name_length", "structural", true, "Truncate node names exceeding 31 characters", checkNodeNameLength)
	Register("text_after_donemodel", "structural", false, "Detect lines after donemodel (the game reads only animations and other top-level keywords there; nodes there are not read)", checkTextAfterDoneModel)
}

// checkTextAfterDoneModel reports lines after an ASCII model's donemodel. The
// game reads on to the end of the file, but outside a block it runs only the
// top-level keywords: an animation there is read, a node block or anything
// else is not (mdl.AfterDoneModel). Such lines are usually a pasted tail or
// junk left by a tool, so what the game makes of them is rarely what was meant.
func checkTextAfterDoneModel(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil || model.AfterDoneModel == nil {
		return nil
	}
	a := model.AfterDoneModel
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

func checkDuplicateAnimations(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	seen := make(map[string]bool)
	var dupes []string
	for _, a := range model.Animations {
		key := strings.ToLower(a.Name)
		if seen[key] {
			if len(dupes) == 0 || !strings.EqualFold(dupes[len(dupes)-1], a.Name) {
				dupes = append(dupes, a.Name)
			}
		}
		seen[key] = true
	}

	if len(dupes) == 0 {
		return nil
	}

	if fix {
		firstIdx2 := make(map[string]int)
		for i, a := range model.Animations {
			key := strings.ToLower(a.Name)
			if fi, ok := firstIdx2[key]; ok {
				first := &model.Animations[fi]
				mergeAnimNodes(first, &model.Animations[i])
			} else {
				firstIdx2[key] = i
			}
		}
		kept := model.Animations[:0]
		seen := make(map[string]bool)
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

	var out []mdl.CheckResult
	for _, name := range dupes {
		out = append(out, mdl.CheckResult{
			Check:    "duplicate_animations",
			Severity: mdl.SevError,
			Fixed:    fix,
			Message: fmt.Sprintf(
				"%s: duplicate animation %q %s",
				file, name, fixVerb(fix, "merged", "would be merged"),
			),
		})
	}
	return out
}

func mergeAnimNodes(dst, src *mdl.Animation) {
	if src.Length > dst.Length {
		dst.Length = src.Length
	}
	dst.Events = append(dst.Events, src.Events...)

	existing := make(map[string]int)
	for i, n := range dst.Nodes {
		existing[strings.ToLower(n.Name)] = i
	}
	for _, sn := range src.Nodes {
		if idx, ok := existing[strings.ToLower(sn.Name)]; ok {
			mergeAnimNodeKeys(&dst.Nodes[idx], &sn)
		} else {
			dst.Nodes = append(dst.Nodes, sn)
		}
	}
}

func mergeAnimNodeKeys(dst, src *mdl.AnimNode) {
	if dst.Type == "" {
		dst.Type = src.Type
	}
	if len(dst.PositionKeys) == 0 {
		dst.PositionKeys = src.PositionKeys
		dst.PositionBezier = src.PositionBezier
	}
	if len(dst.OrientationKeys) == 0 {
		dst.OrientationKeys = src.OrientationKeys
	}
	if len(dst.ScaleKeys) == 0 {
		dst.ScaleKeys = src.ScaleKeys
		takeBezier(dst, src, "scale")
	}
	if len(dst.AlphaKeys) == 0 {
		dst.AlphaKeys = src.AlphaKeys
		takeBezier(dst, src, "alpha")
	}
	if len(dst.SelfIllumColorKeys) == 0 {
		dst.SelfIllumColorKeys = src.SelfIllumColorKeys
		takeBezier(dst, src, "selfillumcolor")
	}
	if len(dst.ColorKeys) == 0 {
		dst.ColorKeys = src.ColorKeys
		takeBezier(dst, src, "color")
	}
	if len(dst.RadiusKeys) == 0 {
		dst.RadiusKeys = src.RadiusKeys
		takeBezier(dst, src, "radius")
	}
	if len(dst.MultiplierKeys) == 0 {
		dst.MultiplierKeys = src.MultiplierKeys
		takeBezier(dst, src, "multiplier")
	}
	if len(dst.ShadowRadiusKeys) == 0 {
		dst.ShadowRadiusKeys = src.ShadowRadiusKeys
		takeBezier(dst, src, "shadowradius")
	}
	if len(dst.VerticalDisplacementKeys) == 0 {
		dst.VerticalDisplacementKeys = src.VerticalDisplacementKeys
		takeBezier(dst, src, "verticaldisplacement")
	}
	if len(dst.BirthRateKeys) == 0 {
		dst.BirthRateKeys = src.BirthRateKeys
		takeBezier(dst, src, "birthrate")
	}
	if len(dst.DetonateKeys) == 0 {
		dst.DetonateKeys = src.DetonateKeys
	}
	if dst.Mesh == nil && src.Mesh != nil {
		dst.Mesh = src.Mesh
	}
	if dst.AnimMesh == nil && src.AnimMesh != nil {
		dst.AnimMesh = src.AnimMesh
	}
}

// takeBezier carries a controller's bezier mark with the keys taken from src.
func takeBezier(dst, src *mdl.AnimNode, name string) {
	if !src.Bezier[name] {
		return
	}
	if dst.Bezier == nil {
		dst.Bezier = make(map[string]bool)
	}
	dst.Bezier[name] = true
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

func checkNodeNameLength(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}

	allNames := make(map[string]bool)
	for _, n := range model.Nodes {
		if n != nil {
			allNames[strings.ToLower(n.Name)] = true
		}
	}

	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || len(n.Name) <= 31 {
			continue
		}
		oldName := n.Name
		newName := shortenNodeName(oldName)

		if fix {
			allNames[strings.ToLower(newName)] = true
			renameNodeEverywhere(model, oldName, newName)
		}

		out = append(out, mdl.CheckResult{
			Check:    "node_name_length",
			Node:     newName,
			Severity: mdl.SevError,
			Fixed:    fix,
			Message: fmt.Sprintf(
				"%s: node name %q is %d chars (max 31); %s %q",
				file, oldName, len(oldName), fixVerb(fix, "truncated to", "would be truncated to"), newName,
			),
		})
	}
	return out
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
