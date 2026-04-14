package checks

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"strings"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

func init() {
	Register("duplicate_node_names", "structural", checkDuplicateNodeNames)
	Register("missing_parents", "structural", checkMissingParents)
	Register("base_dummy_wrong_type", "structural", checkBaseDummyWrongType)
	Register("base_dummy_parent_null", "structural", checkBaseDummyParentNull)
	Register("duplicate_animations", "structural", checkDuplicateAnimations)
	Register("aabb_has_child_nodes", "structural", checkAabbHasChildNodes)
	Register("light_has_child_nodes", "structural", checkLightHasChildNodes)
	Register("too_many_walkmeshes", "structural", checkTooManyWalkmeshes)
	Register("node_name_length", "structural", checkNodeNameLength)
}


func checkDuplicateNodeNames(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}

	allNames := make(map[string]bool)
	for _, n := range model.Nodes {
		if n != nil {
			allNames[strings.ToLower(n.Name)] = true
		}
	}

	seen := make(map[string]int)
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil {
			continue
		}
		lowerName := strings.ToLower(n.Name)
		seen[lowerName]++
		if seen[lowerName] < 2 {
			continue
		}

		suffix := seen[lowerName]
		newName := suffixedName(n.Name, suffix)
		for allNames[strings.ToLower(newName)] {
			suffix++
			newName = suffixedName(n.Name, suffix)
		}

		old := n.Name
		if fix {
			allNames[strings.ToLower(newName)] = true
			n.Name = newName
			renameNodeParentRefs(model, old, newName)
		}
		out = append(out, mdl.CheckResult{
			Check:    "duplicate_node_names",
			Severity: mdl.SevWarning,
			Fixed:    fix,
			Message: fmt.Sprintf(
				"%s: duplicate node %q renamed to %q",
				file, old, newName,
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
					"%s: node %q referenced missing parent %q, reparented to %q",
					file, n.Name, oldParent, model.Name,
				),
			})
		}
	}
	return out
}

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
				Severity: mdl.SevFatal,
				Message: fmt.Sprintf(
					"%s: root node %q (parent NULL) has type %q, expected dummy (use --wrap-root to insert dummy wrapper)",
					file, n.Name, n.NodeType(),
				),
				Fixed: false,
			})
		}
	}
	return out
}

func checkBaseDummyParentNull(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil || model.Name == "" {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || !strings.EqualFold(n.Name, model.Name) {
			continue
		}
		if !strings.EqualFold(n.Parent, "NULL") {
			old := n.Parent
			if fix {
				n.Parent = "NULL"
			}
			out = append(out, mdl.CheckResult{
				Check:    "base_dummy_parent_null",
				Node:     n.Name,
				Severity: mdl.SevError,
				Fixed:    fix,
				Message: fmt.Sprintf(
					"%s: model root node %q had parent %q, set to NULL",
					file, n.Name, old,
				),
			})
		}
	}
	return out
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
				"%s: duplicate animation %q merged",
				file, name,
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
	if len(dst.PositionKeys) == 0 {
		dst.PositionKeys = src.PositionKeys
	}
	if len(dst.OrientationKeys) == 0 {
		dst.OrientationKeys = src.OrientationKeys
	}
	if len(dst.ScaleKeys) == 0 {
		dst.ScaleKeys = src.ScaleKeys
	}
	if len(dst.AlphaKeys) == 0 {
		dst.AlphaKeys = src.AlphaKeys
	}
	if len(dst.SelfIllumColorKeys) == 0 {
		dst.SelfIllumColorKeys = src.SelfIllumColorKeys
	}
	if len(dst.ColorKeys) == 0 {
		dst.ColorKeys = src.ColorKeys
	}
	if len(dst.RadiusKeys) == 0 {
		dst.RadiusKeys = src.RadiusKeys
	}
	if len(dst.MultiplierKeys) == 0 {
		dst.MultiplierKeys = src.MultiplierKeys
	}
	if len(dst.ShadowRadiusKeys) == 0 {
		dst.ShadowRadiusKeys = src.ShadowRadiusKeys
	}
	if len(dst.VerticalDisplacementKeys) == 0 {
		dst.VerticalDisplacementKeys = src.VerticalDisplacementKeys
	}
	if len(dst.BirthRateKeys) == 0 {
		dst.BirthRateKeys = src.BirthRateKeys
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
				"%s: node name %q was %d chars (max 31), truncated to %q",
				file, oldName, len(oldName), newName,
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

// renameNodeParentRefs renames only Parent, skin bone, and animation references
// (not geometry-node Name fields). Used when the caller has already renamed
// the specific node's Name and only needs reference updates.
func renameNodeParentRefs(model *mdl.Model, oldName, newName string) {
	renameNodeRefs(model, oldName, newName, false)
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
