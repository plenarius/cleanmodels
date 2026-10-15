package checks

import (
	"fmt"
	"sort"
	"strings"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

const (
	skinInfluencesPerVertex = 4  // what a skin vertex can store
	skinSummaryBoneNames    = 16 // bones listed in the summary before "+N more"
	skinMissingBoneNames    = 8  // missing bones listed before "+N more"
)

func init() {
	Register("skin_unknown_bone", "geometry", false, "Detect skin weights that name bones the model does not contain", checkSkinUnknownBone)
	Register("skin_influence_count", "geometry", false, "Detect skin vertices with more than four bone influences", checkSkinInfluenceCount)
	Register("skin_summary", "geometry", false, "Summarise each skin: its bones, their tree positions and the node chain above it", checkSkinSummary)
	Register("skin_part_binding", "geometry", false, "Detect skinned body parts whose weights name nodes called after the part, which the game cannot bind", checkSkinPartBinding)
}

// skinBones counts, per bone name (lower case), the vertices of skin whose
// weights name it with a positive weight, keeping the first spelling seen.
func skinBones(skin *mdl.SkinData) (count map[string]int, spelling map[string]string) {
	count = map[string]int{}
	spelling = map[string]string{}
	for _, vw := range skin.Weights {
		seen := map[string]bool{}
		for i, b := range vw.Bones {
			if b == "" || (i < len(vw.Weights) && vw.Weights[i] <= 0) {
				continue
			}
			k := strings.ToLower(b)
			if seen[k] {
				continue
			}
			seen[k] = true
			count[k]++
			if _, ok := spelling[k]; !ok {
				spelling[k] = b
			}
		}
	}
	return count, spelling
}

func nodeNameSet(model *mdl.Model) map[string]bool {
	set := make(map[string]bool, len(model.Nodes))
	for _, n := range model.Nodes {
		if n != nil && n.Name != "" {
			set[strings.ToLower(n.Name)] = true
		}
	}
	return set
}

func checkSkinUnknownBone(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	if skel, _ := mdl.BodyPartSkeleton(model.Name); skel != "" {
		// A body part's bones are the creature skeleton's nodes, which the
		// part file does not contain; skin_part_binding covers parts.
		return nil
	}
	nodes := nodeNameSet(model)
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Skin == nil {
			continue
		}
		count, spelling := skinBones(n.Skin)
		var missing []string
		affected := 0
		for k, c := range count {
			if !nodes[k] {
				missing = append(missing, k)
				affected += c
			}
		}
		if len(missing) == 0 {
			continue
		}
		sort.Slice(missing, func(i, j int) bool {
			if count[missing[i]] != count[missing[j]] {
				return count[missing[i]] > count[missing[j]]
			}
			return missing[i] < missing[j]
		})
		var names []string
		for i, k := range missing {
			if i == skinMissingBoneNames {
				names = append(names, fmt.Sprintf("+%d more", len(missing)-i))
				break
			}
			names = append(names, fmt.Sprintf("%s (%d vertices)", spelling[k], count[k]))
		}
		out = append(out, mdl.CheckResult{
			Check:    "skin_unknown_bone",
			Node:     n.Name,
			Severity: mdl.SevWarning,
			Message: fmt.Sprintf(
				"%s: skin %q has weights for %d bone(s) that are not nodes of this model: %s. The engine's compiler discards these influences, so a vertex weighted to them loses that share of its weight",
				file, n.Name, len(missing), strings.Join(names, ", ")),
		})
	}
	return out
}

func checkSkinInfluenceCount(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Skin == nil {
			continue
		}
		over, most := 0, 0
		for _, vw := range n.Skin.Weights {
			k := 0
			for i, b := range vw.Bones {
				if b != "" && (i >= len(vw.Weights) || vw.Weights[i] > 0) {
					k++
				}
			}
			if k > skinInfluencesPerVertex {
				over++
			}
			if k > most {
				most = k
			}
		}
		if over == 0 {
			continue
		}
		out = append(out, mdl.CheckResult{
			Check:    "skin_influence_count",
			Node:     n.Name,
			Severity: mdl.SevWarning,
			Message: fmt.Sprintf(
				"%s: skin %q has %d of %d vertices with more than %d bone influences (most: %d); a skin vertex stores %d, so the compiler keeps the strongest %d and renormalises them",
				file, n.Name, over, len(n.Skin.Weights), skinInfluencesPerVertex, most, skinInfluencesPerVertex, skinInfluencesPerVertex),
		})
	}
	return out
}

// treePositions numbers the nodes in depth-first order from the roots, the
// order skin bones are addressed by.
func treePositions(model *mdl.Model) map[string]int {
	byName := map[string]*mdl.Node{}
	children := map[string][]string{}
	var roots []string
	for _, n := range model.Nodes {
		if n == nil || n.Name == "" {
			continue
		}
		k := strings.ToLower(n.Name)
		if _, dup := byName[k]; dup {
			continue
		}
		byName[k] = n
	}
	for _, n := range model.Nodes {
		if n == nil || n.Name == "" {
			continue
		}
		k := strings.ToLower(n.Name)
		if byName[k] != n {
			continue
		}
		p := strings.ToLower(n.Parent)
		if _, ok := byName[p]; ok && p != k {
			children[p] = append(children[p], k)
		} else {
			roots = append(roots, k)
		}
	}
	pos := map[string]int{}
	var walk func(k string)
	walk = func(k string) {
		if _, done := pos[k]; done {
			return
		}
		pos[k] = len(pos)
		for _, c := range children[k] {
			walk(c)
		}
	}
	for _, r := range roots {
		walk(r)
	}
	return pos
}

// checkSkinSummary reports the structure around each skin node and nothing
// else: no coordinates, only names, counts and tree positions, so the output
// can be shared without sharing the model.
func checkSkinSummary(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	byName := map[string]*mdl.Node{}
	for _, n := range model.Nodes {
		if n != nil && n.Name != "" {
			if _, dup := byName[strings.ToLower(n.Name)]; !dup {
				byName[strings.ToLower(n.Name)] = n
			}
		}
	}
	pos := treePositions(model)
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Skin == nil {
			continue
		}
		count, spelling := skinBones(n.Skin)

		// Chain of ancestors from the root down to the skin node's parent.
		var chain []string
		ancestors := map[string]bool{}
		seen := map[string]bool{strings.ToLower(n.Name): true}
		for p := strings.ToLower(n.Parent); p != ""; {
			pn, ok := byName[p]
			if !ok || seen[p] {
				break
			}
			seen[p] = true
			ancestors[p] = true
			chain = append([]string{pn.Name}, chain...)
			p = strings.ToLower(pn.Parent)
		}

		keys := make([]string, 0, len(count))
		for k := range count {
			keys = append(keys, k)
		}
		sort.Slice(keys, func(i, j int) bool {
			pi, iok := pos[keys[i]]
			pj, jok := pos[keys[j]]
			if iok != jok {
				return iok
			}
			if pi != pj {
				return pi < pj
			}
			return keys[i] < keys[j]
		})
		var bones []string
		for i, k := range keys {
			if i == skinSummaryBoneNames {
				bones = append(bones, fmt.Sprintf("+%d more", len(keys)-i))
				break
			}
			label := spelling[k]
			if p, ok := pos[k]; ok {
				label += fmt.Sprintf("@%d", p)
				if ancestors[k] {
					label += " (ancestor)"
				}
			} else {
				label += " (NOT IN MODEL)"
			}
			bones = append(bones, label)
		}

		minSum, maxSum := 0.0, 0.0
		minInf, maxInf, none := 0, 0, 0
		for vi, vw := range n.Skin.Weights {
			var sum float64
			k := 0
			for i, b := range vw.Bones {
				if i < len(vw.Weights) {
					sum += float64(vw.Weights[i])
				}
				if b != "" && (i >= len(vw.Weights) || vw.Weights[i] > 0) {
					k++
				}
			}
			if k == 0 {
				none++
			}
			if vi == 0 || sum < minSum {
				minSum = sum
			}
			if vi == 0 || sum > maxSum {
				maxSum = sum
			}
			if vi == 0 || k < minInf {
				minInf = k
			}
			if k > maxInf {
				maxInf = k
			}
		}

		verts := 0
		if n.Mesh != nil {
			verts = len(n.Mesh.Verts)
		}
		skinPos := "?"
		if p, ok := pos[strings.ToLower(n.Name)]; ok {
			skinPos = fmt.Sprint(p)
		}
		msg := fmt.Sprintf("%s: skin %q @%s under [%s]: %d vertices, %d weight entries, %d bone(s): %s; influences per vertex %d-%d, weight sum %.3f-%.3f",
			file, n.Name, skinPos, strings.Join(chain, " > "), verts, len(n.Skin.Weights), len(keys), strings.Join(bones, ", "), minInf, maxInf, minSum, maxSum)
		if none > 0 {
			msg += fmt.Sprintf("; %d vertices with no bone influence", none)
		}
		if verts != len(n.Skin.Weights) {
			msg += fmt.Sprintf("; WEIGHT COUNT DIFFERS FROM VERTEX COUNT (%d vs %d)", len(n.Skin.Weights), verts)
		}
		out = append(out, mdl.CheckResult{
			Check:    "skin_summary",
			Node:     n.Name,
			Severity: mdl.SevInfo,
			Message:  msg,
		})
	}
	return out
}

// checkSkinPartBinding covers skinned body parts (pmh0_neck001, pfe0_head003,
// ...). The game binds such a skin to the creature skeleton (pmh0), not to the
// part's own nodes: bones are looked up by name among the skeleton's nodes, and
// a binary stores them as skeleton tree positions (NWMAIN_FINDINGS.md findings
// 20-22). A weight that names a node called after the part itself (its root
// pmh0_neck001, its meshes pmh0_neck001g, pmh0_neck001gs) can never be a
// skeleton node, so in game that influence is lost; a vertex left with none is
// drawn at the origin of clip space and tears its triangles into sheets. Nodes
// named like skeleton nodes are fine whatever their type: robes carry trimeshes
// called torso_g and pelvis_g, and the game binds bones by name.
func checkSkinPartBinding(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	skel, _ := mdl.BodyPartSkeleton(model.Name)
	if skel == "" {
		return nil
	}
	prefix := strings.ToLower(strings.TrimSpace(model.Name))
	own := map[string]bool{} // lower-case names of the part's nodes no skeleton has
	for _, n := range model.Nodes {
		if n != nil && strings.HasPrefix(strings.ToLower(n.Name), prefix) {
			own[strings.ToLower(n.Name)] = true
		}
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Skin == nil {
			continue
		}
		count, spelling := skinBones(n.Skin)
		var bad []string
		for k := range count {
			if own[k] {
				bad = append(bad, k)
			}
		}
		if len(bad) == 0 {
			continue
		}
		sort.Strings(bad)
		stranded := 0
		for _, vw := range n.Skin.Weights {
			kept := false
			for i, b := range vw.Bones {
				if b != "" && !own[strings.ToLower(b)] && (i >= len(vw.Weights) || vw.Weights[i] > 0) {
					kept = true
				}
			}
			if !kept {
				stranded++
			}
		}
		names := make([]string, len(bad))
		for i, k := range bad {
			names[i] = fmt.Sprintf("%s (%d vertices)", spelling[k], count[k])
		}
		msg := fmt.Sprintf(
			"%s: body part skin %q has weights for %s, which are this part's own nodes. In game a body part's skin is bound to the creature skeleton %s, so only skeleton nodes can be bones and these influences are lost",
			file, n.Name, strings.Join(names, ", "), skel)
		if stranded > 0 {
			msg += fmt.Sprintf("; %d vertices have no other bone and will tear the mesh", stranded)
		}
		out = append(out, mdl.CheckResult{
			Check:    "skin_part_binding",
			Node:     n.Name,
			Severity: mdl.SevWarning,
			Message:  msg,
		})
	}
	return out
}
