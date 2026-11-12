package mdl

import "strings"

// pruneAnimTree applies an animation's animroot as the game's compiler does
// when it reads the ASCII (nwn-mdl-spec MDL-NODE-045; E71:
// Animation::PostProcess 0x453600, findnode 0x461600). The animroot is looked
// up among the animation's own nodes, depth first from its root, by exact
// name: case counts, and the geometry is not searched. When a node matches,
// the tree is cut to the path from the root to it and its own subtree: every
// other child of each node on the path is dropped, with its subtree. When none
// matches (a name that exists nowhere, another case, a node only the geometry
// has), or the animroot is empty or NULL, the whole tree is kept.
//
// It returns the children to write the tree with (children itself when
// nothing is pruned) and the nodes pruned. The game's node count still counts
// the pruned nodes (the ASCII's count, MDL-NODE-042), and they are not
// "dropped" in the sense of compiler_dropped.go: the game drops them by
// design.
func pruneAnimTree(root *AnimNode, children map[*AnimNode][]*AnimNode, animRoot string) (map[*AnimNode][]*AnimNode, []*AnimNode) {
	if root == nil || animRoot == "" || strings.EqualFold(animRoot, "NULL") {
		return children, nil
	}
	parent := map[*AnimNode]*AnimNode{}
	seen := map[*AnimNode]bool{}
	var find func(an *AnimNode) *AnimNode
	find = func(an *AnimNode) *AnimNode {
		if seen[an] {
			return nil
		}
		seen[an] = true
		if an.Name == animRoot {
			return an
		}
		for _, ch := range children[an] {
			if seen[ch] {
				continue
			}
			parent[ch] = an
			if f := find(ch); f != nil {
				return f
			}
		}
		return nil
	}
	found := find(root)
	if found == nil || found == root {
		return children, nil
	}
	kept := make(map[*AnimNode][]*AnimNode, len(children))
	for k, v := range children {
		kept[k] = v
	}
	for n := found; n != root; n = parent[n] {
		kept[parent[n]] = []*AnimNode{n}
	}
	inKept := reachableAnimNodes(root, kept)
	var pruned []*AnimNode
	for _, an := range orderedReachable(root, children) {
		if !inKept[an] {
			pruned = append(pruned, an)
		}
	}
	return kept, pruned
}

// reachableAnimNodes is the set of nodes reachable from root through children.
func reachableAnimNodes(root *AnimNode, children map[*AnimNode][]*AnimNode) map[*AnimNode]bool {
	out := map[*AnimNode]bool{}
	for _, an := range orderedReachable(root, children) {
		out[an] = true
	}
	return out
}

// orderedReachable lists the nodes reachable from root through children,
// depth first, each once.
func orderedReachable(root *AnimNode, children map[*AnimNode][]*AnimNode) []*AnimNode {
	var out []*AnimNode
	seen := map[*AnimNode]bool{}
	var walk func(an *AnimNode)
	walk = func(an *AnimNode) {
		if seen[an] {
			return
		}
		seen[an] = true
		out = append(out, an)
		for _, ch := range children[an] {
			walk(ch)
		}
	}
	walk(root)
	return out
}
