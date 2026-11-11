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
//     j at position i makes the sort put it back at j. A node whose stored
//     children are not in the comparison's order (no declaration order
//     gives them) keeps them declared as stored; the others are still
//     inverse-sorted;
//  3. the stored order.
//
// A candidate is only taken when compiling it rebuilds the stored tree, node
// for node (apart from the children of such an unsorted node, which come back
// as a compile of the stored order sorts them), so a model the compiler cannot
// rebuild keeps its stored order. The writer keeps a part-number order as
// listed (declaredInNumberOrder) and writes any other depth first, which is
// how the inverse sort lists it.
//
// The candidates are planned and checked with the transparencyhints the
// ASCII carries. storedHints holds the stored hint of each mesh the decompile
// gave another (a restored donor takes its receiver's, unmergeRenderLists);
// when no candidate rebuilds the stored tree with the new hints, they are
// planned again with the stored ones, which the stored tree was sorted by.
func declarationOrder(m *Model, storedHints map[*Node]int32) {
	stored := m.Nodes
	if len(stored) < 2 || slices.Contains(stored, nil) {
		return
	}
	// The writer and the compiler find a parent by name: the nearest node of
	// that name declared before it (resolveGeomTree). Where a duplicate name
	// is some node's parent, which one that is depends on the declaration
	// order, and the writer keeps a part-number order only when no duplicate
	// is a parent (declaredInNumberOrder): such a model gets no part-number
	// order, and the inverse sort is checked against the stored parents.
	dupParent := duplicateIsParent(stored)
	parentOf, children := resolveGeomTree(stored)
	if !walksAs(stored, children, stored) {
		return
	}
	stale := func(n *Node) int32 {
		if h, ok := storedHints[n]; ok {
			return h
		}
		return n.Mesh.TransparencyHint
	}
	// Candidates are checked with the hints the ASCII carries, which the
	// compile sorts by, so one taken rebuilds the stored tree as compiled.
	// A restored donor's stored hint (storedHints) is what the stored tree
	// was sorted by: an order planned with it can give the stored numbers
	// where none planned with the new hints does, but the compile sorts the
	// donor by its new hint. Before an order with the new hints, such an order
	// is taken only when that compile leaves every other node where it is
	// stored (keepsOthers, plc_tn2_13): past 16 children std::sort can
	// otherwise reshuffle the donor's equal-hint siblings where an order with
	// the new hints keeps them, a draw order change (trlk1_o08_01,
	// tcn01_s05_01).
	order := []*Node(nil)
	if !dupParent {
		if part := partNumberOrder(m); part != nil {
			switch {
			case compilesTo(part, stored, parentOf, children, nil, true, meshHint):
				order = part
			case len(storedHints) > 0 && compilesTo(part, stored, parentOf, children, nil, true, stale) &&
				keepsOthers(part, stored, parentOf, storedHints):
				order = part
			}
		}
	}
	if order == nil {
		if inv, unsorted := inverseSortOrder(stored, children, meshHint); inv != nil && compilesTo(inv, stored, parentOf, children, unsorted, false, meshHint) {
			order = inv
		}
	}
	// When no order rebuilds the stored tree with the new hints, and the
	// stored order does not either, the compile moves the donor's siblings
	// whatever is declared: an order planned with the stored hints at least
	// gives the stored tree up to the donors' new places (plc_tn1_44,
	// ttu01_o14_01).
	if order == nil && len(storedHints) > 0 && !compilesTo(stored, stored, parentOf, children, nil, false, meshHint) {
		if part := partNumberOrder(m); !dupParent && part != nil && compilesTo(part, stored, parentOf, children, nil, true, stale) {
			order = part
		} else if inv, unsorted := inverseSortOrder(stored, children, stale); inv != nil && compilesTo(inv, stored, parentOf, children, unsorted, false, stale) {
			order = inv
		}
	}
	if order != nil {
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
// the stored children of every node, or nil when that is the stored order.
// A sorted node whose stored children are not in the comparison's order (no
// declaration order reaches them) keeps them declared as stored, and is
// returned in unsorted: the compile sorts them as it sorts the stored order.
func inverseSortOrder(stored []*Node, children map[*Node][]*Node, hint func(*Node) int32) (order []*Node, unsorted map[*Node]bool) {
	less := hintLess(hint)
	decl := make(map[*Node][]*Node)
	changed := false
	for _, p := range stored {
		kids := children[p]
		if !slices.ContainsFunc(kids, func(k *Node) bool { return k.Mesh != nil && hint(k) > 0 }) {
			continue
		}
		sorted := true
		for j := 1; j < len(kids); j++ {
			if less(kids[j], kids[j-1]) {
				sorted = false
				break
			}
		}
		if !sorted {
			if unsorted == nil {
				unsorted = make(map[*Node]bool)
			}
			unsorted[p] = true
			continue
		}
		// Sort stand-ins for the positions 0..n-1: position i compares as the
		// stored child i. Their order afterwards is out[j] = i.
		pos := make([]*Node, len(kids))
		index := make(map[*Node]int, len(kids))
		for i := range kids {
			pos[i] = &Node{}
			index[pos[i]] = i
		}
		engineSort(pos, func(a, b *Node) bool { return less(kids[index[a]], kids[index[b]]) })
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
		return nil, nil
	}
	order = make([]*Node, 0, len(stored))
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
		return nil, nil
	}
	return order, unsorted
}

// compilesTo reports whether compiling the nodes declared in order writes the
// stored tree: the same nodes depth-first, each under its stored parent, and
// with numbers set, each numbered with its stored part number, with the
// meshes sorted by hint. The children
// of a node in unsorted may come back in the order the compile sorts them
// into (no declaration order gives their stored order back).
func compilesTo(order, stored []*Node, storedParent map[*Node]*Node, storedChildren map[*Node][]*Node, unsorted map[*Node]bool, numbers bool, hint func(*Node) int32) bool {
	if !strings.EqualFold(order[0].Parent, "NULL") {
		return false
	}
	parentOf, children := resolveGeomTree(order)
	for _, n := range order {
		if parentOf[n] != storedParent[n] {
			return false
		}
	}
	reorderChildrenBy(order[0], children, hint)
	want := stored
	if len(unsorted) > 0 {
		kids := make(map[*Node][]*Node, len(storedChildren))
		for p, k := range storedChildren {
			kids[p] = k
			if unsorted[p] {
				kids[p] = children[p]
			}
		}
		want = depthFirst(stored[0], kids)
	}
	if !walksAs(order, children, want) {
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

// depthFirst lists the tree under root through children, each node once.
func depthFirst(root *Node, children map[*Node][]*Node) []*Node {
	var out []*Node
	visited := make(map[*Node]bool)
	stack := []*Node{root}
	for len(stack) > 0 {
		n := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		if visited[n] {
			continue
		}
		visited[n] = true
		out = append(out, n)
		kids := children[n]
		for k := len(kids) - 1; k >= 0; k-- {
			if !visited[kids[k]] {
				stack = append(stack, kids[k])
			}
		}
	}
	return out
}

// keepsOthers reports whether compiling the nodes declared in order, sorted
// by the hints they carry, puts every node but the restored donors (the keys
// of moved) under its stored parent and in its stored order: only the donors
// move, sorted by their new hints.
func keepsOthers(order, stored []*Node, storedParent map[*Node]*Node, moved map[*Node]int32) bool {
	parentOf, children := resolveGeomTree(order)
	for _, n := range order {
		if parentOf[n] != storedParent[n] {
			return false
		}
	}
	reorderChildren(order[0], children)
	got := depthFirst(order[0], children)
	i := 0
	for _, n := range got {
		if _, ok := moved[n]; ok {
			continue
		}
		for i < len(stored) {
			if _, ok := moved[stored[i]]; !ok {
				break
			}
			i++
		}
		if i >= len(stored) || stored[i] != n {
			return false
		}
		i++
	}
	return true
}
