package mdl

import (
	"fmt"
	"strings"
)

// ReparentFromRestrictedNodes moves children of AABB/light nodes to their
// grandparent. Returns a list of human-readable repair messages.
func ReparentFromRestrictedNodes(model *Model) []string {
	noChildTypes := make(map[string]string)
	for _, n := range model.Nodes {
		if n == nil {
			continue
		}
		nt := n.NodeType()
		if nt == "aabb" || nt == "light" {
			noChildTypes[strings.ToLower(n.Name)] = nt
		}
	}

	var msgs []string
	for _, n := range model.Nodes {
		if n == nil {
			continue
		}
		parentType, isRestricted := noChildTypes[strings.ToLower(n.Parent)]
		if !isRestricted {
			continue
		}
		var grandparent string
		for _, p := range model.Nodes {
			if p != nil && strings.EqualFold(p.Name, n.Parent) {
				grandparent = p.Parent
				break
			}
		}
		if grandparent == "" {
			grandparent = model.Name
		}
		oldParent := n.Parent
		n.Parent = grandparent
		msgs = append(msgs, "reparented \""+n.Name+"\" from "+parentType+" node \""+oldParent+"\" to \""+grandparent+"\"")
	}
	return msgs
}

// WrapRootInDummy inserts a dummy wrapper node when the model root is a
// geometry node type (trimesh, danglymesh, etc.) instead of a dummy.
// Returns a repair message, or empty string if no wrapping was needed.
func WrapRootInDummy(model *Model) string {
	for i, n := range model.Nodes {
		if n == nil || !strings.EqualFold(n.Parent, "NULL") || !strings.EqualFold(n.Name, model.Name) {
			continue
		}
		if n.NodeType() == "dummy" {
			continue
		}
		wrapper := &Node{
			Name:   model.Name,
			Parent: "NULL",
		}
		oldType := n.NodeType()
		oldName := n.Name
		n.Parent = model.Name
		n.Name = model.Name + "_mesh"
		newNodes := make([]*Node, 0, len(model.Nodes)+1)
		newNodes = append(newNodes, model.Nodes[:i]...)
		newNodes = append(newNodes, wrapper)
		newNodes = append(newNodes, model.Nodes[i:]...)
		model.Nodes = newNodes
		for _, gn := range model.Nodes {
			if gn != nil && strings.EqualFold(gn.Parent, oldName) && gn != n && gn != wrapper {
				gn.Parent = n.Name
			}
		}
		for ai := range model.Animations {
			for ni := range model.Animations[ai].Nodes {
				an := &model.Animations[ai].Nodes[ni]
				if strings.EqualFold(an.Name, oldName) {
					an.Name = n.Name
				}
				if strings.EqualFold(an.Parent, oldName) {
					an.Parent = n.Name
				}
			}
		}
		return "wrapped " + oldType + " root \"" + model.Name + "\" in dummy node, mesh renamed to \"" + n.Name + "\""
	}
	return ""
}

// FixAnimationLengths clamps negative animation lengths to 0 and ensures
// TILE transition animations meet the minimum length.
// Returns a list of repair messages.
func FixAnimationLengths(model *Model) []string {
	var msgs []string
	isTile := strings.EqualFold(model.Classification, "TILE")
	for i := range model.Animations {
		a := &model.Animations[i]
		if a.Length < 0 {
			old := a.Length
			a.Length = 0
			msgs = append(msgs, fmt.Sprintf("animation %q had negative length %g, set to 0", a.Name, old))
		}
		if isTile && (strings.EqualFold(a.Name, "day2night") || strings.EqualFold(a.Name, "night2day")) && a.Length < TileAnimMinLength {
			old := a.Length
			a.Length = TileAnimMinLength
			msgs = append(msgs, fmt.Sprintf("TILE animation %q had length %g, set to minimum %g", a.Name, old, TileAnimMinLength))
		}
	}
	return msgs
}
