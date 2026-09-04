// compiler_tree.go — node-identity tree resolution for the binary MDL compiler.
//
// MDL stores parentage by NAME, but node names are not unique in practice.
// BioWare ships models that reuse a name deliberately:
//
//   - bilateral creature rig parts — two "hand" dummies (one under each
//     forearm), four "foot" dummies on many-legged creatures;
//   - the mesh-plus-hook idiom, where a mesh node has a child dummy of the
//     same name ("head" danglymesh containing a "head" dummy);
//   - repeated emitters (vdr_sanctuary has two "omenemitter05").
//
// Those names are load-bearing: the engine attaches items and inherits
// supermodel animations by node name, so renaming a duplicate breaks the rig.
// The compiler therefore has to keep duplicates intact, which means it cannot
// use the name as a node's identity.
//
// Resolution rule: a node's parent is the NEAREST PRECEDING node whose name
// matches its Parent field. Both producers of a Model emit nodes in DFS
// pre-order — the binary decompiler appends each node before recursing into its
// children, and ASCII sources declare parents before children — and in
// pre-order the nearest preceding match is always the correct ancestor:
//
//	torso_g
//	  rbicep_g   <- #1
//	    <child>        parent "rbicep_g" resolves to #1 (nearest preceding)
//	  rbicep_g   <- #2
//	    <child>        parent "rbicep_g" resolves to #2
//
// When every name is unique this rule degenerates to a plain name lookup, so
// well-formed models compile exactly as they did before.
package mdl

import "strings"

// resolveGeomTree returns the parent of each node and each node's children,
// both keyed by node identity rather than name. Child order follows the
// declaration order of nodes so output stays deterministic.
//
// A node whose Parent does not resolve (the root, "NULL", or a dangling
// reference) is absent from parentOf.
func resolveGeomTree(nodes []*Node) (parentOf map[*Node]*Node, childrenOf map[*Node][]*Node) {
	parentOf = make(map[*Node]*Node, len(nodes))
	childrenOf = make(map[*Node][]*Node, len(nodes))

	for i, n := range nodes {
		if n == nil || isRootParent(n.Parent) {
			continue
		}
		var parent *Node
		// Nearest preceding match — the correct ancestor in DFS pre-order.
		for j := i - 1; j >= 0; j-- {
			if nodes[j] != nil && nodes[j] != n && strings.EqualFold(nodes[j].Name, n.Parent) {
				parent = nodes[j]
				break
			}
		}
		if parent == nil {
			// Fall back to any match, so sources that list a child before its
			// parent still link up the way the old name-keyed lookup did.
			for _, cand := range nodes {
				if cand != nil && cand != n && strings.EqualFold(cand.Name, n.Parent) {
					parent = cand
					break
				}
			}
		}
		if parent == nil {
			continue
		}
		parentOf[n] = parent
		childrenOf[parent] = append(childrenOf[parent], n)
	}
	return parentOf, childrenOf
}

// resolveAnimTree is resolveGeomTree for an animation's node list. Returned
// pointers alias elements of nodes, so the caller must not append to the slice
// while the maps are in use.
func resolveAnimTree(nodes []AnimNode) (parentOf map[*AnimNode]*AnimNode, childrenOf map[*AnimNode][]*AnimNode) {
	parentOf = make(map[*AnimNode]*AnimNode, len(nodes))
	childrenOf = make(map[*AnimNode][]*AnimNode, len(nodes))

	for i := range nodes {
		n := &nodes[i]
		if isRootParent(n.Parent) {
			continue
		}
		var parent *AnimNode
		for j := i - 1; j >= 0; j-- {
			if cand := &nodes[j]; cand != n && strings.EqualFold(cand.Name, n.Parent) {
				parent = cand
				break
			}
		}
		if parent == nil {
			for j := range nodes {
				if cand := &nodes[j]; cand != n && strings.EqualFold(cand.Name, n.Parent) {
					parent = cand
					break
				}
			}
		}
		if parent == nil {
			continue
		}
		parentOf[n] = parent
		childrenOf[parent] = append(childrenOf[parent], n)
	}
	return parentOf, childrenOf
}

// nodeOccurrences groups nodes by lowercased name, preserving declaration
// order, so the Nth animation node named X can be paired with the Nth geometry
// node named X.
func nodeOccurrences(nodes []*Node) map[string][]*Node {
	out := make(map[string][]*Node, len(nodes))
	for _, n := range nodes {
		if n == nil {
			continue
		}
		key := strings.ToLower(n.Name)
		out[key] = append(out[key], n)
	}
	return out
}

// pairAnimNodesToGeom maps each animation node to the geometry node it drives.
// Names are matched by occurrence: the Nth anim node named X pairs with the Nth
// geometry node named X. Surplus anim nodes fall back to the last geometry
// occurrence, which is what a single-geometry-node model always did.
func pairAnimNodesToGeom(anim []AnimNode, geomOccur map[string][]*Node) map[*AnimNode]*Node {
	out := make(map[*AnimNode]*Node, len(anim))
	seen := make(map[string]int, len(anim))
	for i := range anim {
		an := &anim[i]
		key := strings.ToLower(an.Name)
		occ := geomOccur[key]
		if len(occ) == 0 {
			continue
		}
		idx := seen[key]
		seen[key]++
		if idx >= len(occ) {
			idx = len(occ) - 1
		}
		out[an] = occ[idx]
	}
	return out
}

// isRootParent reports whether a Parent field denotes "no parent".
func isRootParent(parent string) bool {
	return parent == "" || strings.EqualFold(parent, "NULL")
}

// ResolveNodeParents returns each geometry node's parent instance, resolved by
// tree position rather than by name. Nodes with no resolvable parent (the root,
// or a dangling Parent reference) are absent from the result.
//
// Exported for the checks package: anything that renames one of several
// same-named nodes first has to know which of them a given child actually
// hangs off, or it will repoint the wrong subtree.
func ResolveNodeParents(nodes []*Node) map[*Node]*Node {
	parentOf, _ := resolveGeomTree(nodes)
	return parentOf
}

// ResolveAnimNodeParents is ResolveNodeParents for an animation's node list.
// Returned pointers alias elements of nodes, so the caller must not append to
// the slice while the map is in use.
func ResolveAnimNodeParents(nodes []AnimNode) map[*AnimNode]*AnimNode {
	parentOf, _ := resolveAnimTree(nodes)
	return parentOf
}
