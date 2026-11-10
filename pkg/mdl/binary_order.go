package mdl

import (
	"slices"
	"strings"
)

// Declaration order of a decompiled model's geometry nodes.
//
// A binary stores its geometry tree depth-first, and the ASCII is compiled
// back by the game's rules (compiler_order.go): numbers follow declaration
// order, and where one of a node's children is a mesh with transparencyhint
// > 0, the children are sorted with std::sort, which is not stable past 16
// elements. Declaring the nodes in stored order therefore shuffles the equal
// keys of a node with more than 16 such children (the draw order changes),
// and gives part numbers in stored order even where the original's numbers
// came from another declaration order.
//
// declarationOrder picks, in this order of preference:
//
//  1. ascending part number, when the model has no supermodel and that order
//     rebuilds both the stored numbers and the stored tree (the original was
//     declared out of tree order, nwn-mdl-spec MDL-NODE-010);
//  2. for each node whose children get sorted, the order the sort turns back
//     into the stored one (inverse sort; MDL-NODE-012). std::sort's result
//     depends only on its comparisons, so sorting the positions 0..n-1 by the
//     stored children's keys gives out[j] = i, and declaring the stored child
//     j at position i makes the sort put it back at j;
//  3. the stored order.
//
// A candidate is only taken when compiling it rebuilds the stored tree, node
// for node, so a model the compiler cannot rebuild keeps its stored order. The
// writer keeps a part-number order as listed (declaredInNumberOrder) and
// writes any other depth first, which is how the inverse sort lists it.
func declarationOrder(m *Model) {
	stored := m.Nodes
	if len(stored) < 2 || slices.Contains(stored, nil) {
		return
	}
	// The writer and the compiler find a parent by name, so duplicate names
	// keep the stored order.
	names := make(map[string]bool, len(stored))
	for _, n := range stored {
		key := strings.ToLower(n.Name)
		if names[key] {
			return
		}
		names[key] = true
	}
	parentOf, children := resolveGeomTree(stored)
	if !walksAs(stored, children, stored) {
		return
	}
	if order := partNumberOrder(m); order != nil && compilesTo(order, stored, parentOf, true) {
		m.Nodes = order
		return
	}
	if order := inverseSortOrder(stored, children); order != nil && compilesTo(order, stored, parentOf, false) {
		m.Nodes = order
	}
}

// partNumberOrder returns the nodes in ascending part number, or nil when the
// model has a supermodel or its numbers are not exactly 0..n-1.
func partNumberOrder(m *Model) []*Node {
	if sm := strings.TrimSpace(m.SuperModel); sm != "" && !strings.EqualFold(sm, "NULL") {
		return nil
	}
	order := make([]*Node, len(m.Nodes))
	for _, n := range m.Nodes {
		if n.PartNumber < 0 || int(n.PartNumber) >= len(order) || order[n.PartNumber] != nil {
			return nil
		}
		order[n.PartNumber] = n
	}
	if slices.Equal(order, m.Nodes) {
		return nil
	}
	return order
}

// inverseSortOrder returns the depth-first declaration order whose sort gives
// the stored children of every node, or nil when that is the stored order or
// some sorted node's stored children are not in the comparison's order (no
// declaration order reaches them).
func inverseSortOrder(stored []*Node, children map[*Node][]*Node) []*Node {
	decl := make(map[*Node][]*Node)
	changed := false
	for _, p := range stored {
		kids := children[p]
		if !slices.ContainsFunc(kids, func(k *Node) bool { return k.Mesh != nil && k.Mesh.TransparencyHint > 0 }) {
			continue
		}
		for j := 1; j < len(kids); j++ {
			if transparencyLess(kids[j], kids[j-1]) {
				return nil
			}
		}
		// Sort stand-ins for the positions 0..n-1: position i compares as the
		// stored child i. Their order afterwards is out[j] = i.
		pos := make([]*Node, len(kids))
		index := make(map[*Node]int, len(kids))
		for i, k := range kids {
			pos[i] = &Node{Mesh: k.Mesh}
			index[pos[i]] = i
		}
		engineSort(pos, transparencyLess)
		d := make([]*Node, len(kids))
		for j, s := range pos {
			d[index[s]] = kids[j]
		}
		if !slices.Equal(d, kids) {
			decl[p] = d
			changed = true
		}
	}
	if !changed {
		return nil
	}
	order := make([]*Node, 0, len(stored))
	stack := []*Node{stored[0]}
	for len(stack) > 0 {
		n := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		order = append(order, n)
		kids := children[n]
		if d, ok := decl[n]; ok {
			kids = d
		}
		for i := len(kids) - 1; i >= 0; i-- {
			stack = append(stack, kids[i])
		}
	}
	if len(order) != len(stored) {
		return nil
	}
	return order
}

// compilesTo reports whether compiling the nodes declared in order writes the
// stored tree: the same nodes depth-first, each under its stored parent, and
// with numbers set, each numbered with its stored part number.
func compilesTo(order, stored []*Node, storedParent map[*Node]*Node, numbers bool) bool {
	if !strings.EqualFold(order[0].Parent, "NULL") {
		return false
	}
	parentOf, children := resolveGeomTree(order)
	for _, n := range order {
		if parentOf[n] != storedParent[n] {
			return false
		}
	}
	reorderChildren(order[0], children)
	if !walksAs(order, children, stored) {
		return false
	}
	if numbers {
		for i, n := range order {
			if n.PartNumber != int32(i) {
				return false
			}
		}
	}
	return true
}

// walksAs reports whether the depth-first walk from nodes[0] through children
// visits exactly want, in order (walkTree's order).
func walksAs(nodes []*Node, children map[*Node][]*Node, want []*Node) bool {
	visited := make(map[*Node]bool, len(nodes))
	stack := []*Node{nodes[0]}
	i := 0
	for len(stack) > 0 {
		n := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		if visited[n] {
			continue
		}
		visited[n] = true
		if i >= len(want) || want[i] != n {
			return false
		}
		i++
		kids := children[n]
		for k := len(kids) - 1; k >= 0; k-- {
			if !visited[kids[k]] {
				stack = append(stack, kids[k])
			}
		}
	}
	return i == len(want)
}
