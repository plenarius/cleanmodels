package mdl

// Node order and numbering as the game's compiler does them.
//
// Numbers follow declaration order: ParseNode (0x447540) gives each node the
// tree's next number at its endnode, so a node takes its position among the
// source's node blocks, a node left out of the tree included (nwn-mdl-spec E43;
// goldens zspec_nodeorder, zspec_numbers).
//
// Child order is declaration order, except where MdlNode::ReorderChildren
// (0x458120) sorts it: after parsing, it walks the geometry tree from the root
// (ImportMaxTree 0x447ef0; animation trees are not sorted), and where one of a
// node's children is a mesh with transparencyhint > 0 it sorts that node's
// children with std::sort and the comparison at 0x453470: a non-mesh is less
// than a mesh, two meshes compare by transparencyhint, two non-meshes are
// equal. std::sort is not stable past 16 elements, so engineSort is
// libstdc++'s algorithm step for step (engine goldens zcm_tsort24, zcm_tsort50
// and zcm_tsort90 in tests/fixtures/engine; a stable sort matches none of
// them). The file is written depth-first in the sorted order, and the skin
// bone tree positions (treeIndex) follow it.

// reorderChildren applies ReorderChildren to the geometry tree under root.
func reorderChildren(root *Node, children map[*Node][]*Node) {
	visited := make(map[*Node]bool)
	stack := []*Node{root}
	for len(stack) > 0 {
		n := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		if n == nil || visited[n] {
			continue
		}
		visited[n] = true
		kids := children[n]
		for _, k := range kids {
			if k.Mesh != nil && k.Mesh.TransparencyHint > 0 {
				sorted := append([]*Node(nil), kids...)
				engineSort(sorted, transparencyLess)
				children[n] = sorted
				kids = sorted
				break
			}
		}
		stack = append(stack, kids...)
	}
}

// transparencyLess is the comparison at 0x453470. Every mesh type counts as a
// mesh (MdlNode::AsMdlNodeTriMesh tests node flags 0x21).
func transparencyLess(a, b *Node) bool {
	switch {
	case a.Mesh == nil:
		return b.Mesh != nil
	case b.Mesh == nil:
		return false
	}
	return a.Mesh.TransparencyHint < b.Mesh.TransparencyHint
}

// engineSort is libstdc++'s std::sort: introsort to runs of 16 or fewer
// elements (median of three pivots, heap sort past 2*floor(log2 n) levels),
// then an insertion sort.
func engineSort(a []*Node, less func(a, b *Node) bool) {
	n := len(a)
	if n < 2 {
		return
	}
	depth := 0
	for k := n; k > 1; k >>= 1 {
		depth++
	}
	introsortLoop(a, 0, n, 2*depth, less)
	if n > 16 {
		insertionSort(a, 0, 16, less)
		for i := 16; i < n; i++ {
			unguardedLinearInsert(a, i, less)
		}
	} else {
		insertionSort(a, 0, n, less)
	}
}

func introsortLoop(a []*Node, first, last, depth int, less func(a, b *Node) bool) {
	for last-first > 16 {
		if depth == 0 {
			heapSort(a, first, last, less)
			return
		}
		depth--
		mid := first + (last-first)/2
		moveMedianToFirst(a, first, first+1, mid, last-1, less)
		cut := unguardedPartition(a, first+1, last, first, less)
		introsortLoop(a, cut, last, depth, less)
		last = cut
	}
}

func moveMedianToFirst(a []*Node, result, x, y, z int, less func(a, b *Node) bool) {
	swap := func(i int) { a[result], a[i] = a[i], a[result] }
	switch {
	case less(a[x], a[y]):
		switch {
		case less(a[y], a[z]):
			swap(y)
		case less(a[x], a[z]):
			swap(z)
		default:
			swap(x)
		}
	case less(a[x], a[z]):
		swap(x)
	case less(a[y], a[z]):
		swap(z)
	default:
		swap(y)
	}
}

func unguardedPartition(a []*Node, first, last, pivot int, less func(a, b *Node) bool) int {
	for {
		for less(a[first], a[pivot]) {
			first++
		}
		last--
		for less(a[pivot], a[last]) {
			last--
		}
		if first >= last {
			return first
		}
		a[first], a[last] = a[last], a[first]
		first++
	}
}

func insertionSort(a []*Node, first, last int, less func(a, b *Node) bool) {
	for i := first + 1; i < last; i++ {
		if less(a[i], a[first]) {
			v := a[i]
			copy(a[first+1:i+1], a[first:i])
			a[first] = v
		} else {
			unguardedLinearInsert(a, i, less)
		}
	}
}

func unguardedLinearInsert(a []*Node, i int, less func(a, b *Node) bool) {
	v := a[i]
	j := i - 1
	for less(v, a[j]) {
		a[i] = a[j]
		i = j
		j--
	}
	a[i] = v
}

func heapSort(a []*Node, first, last int, less func(a, b *Node) bool) {
	if n := last - first; n >= 2 {
		for parent := (n - 2) / 2; ; parent-- {
			adjustHeap(a, first, parent, n, a[first+parent], less)
			if parent == 0 {
				break
			}
		}
	}
	for last-first > 1 {
		last--
		v := a[last]
		a[last] = a[first]
		adjustHeap(a, first, 0, last-first, v, less)
	}
}

func adjustHeap(a []*Node, first, hole, n int, v *Node, less func(a, b *Node) bool) {
	top := hole
	second := hole
	for second < (n-1)/2 {
		second = 2 * (second + 1)
		if less(a[first+second], a[first+second-1]) {
			second--
		}
		a[first+hole] = a[first+second]
		hole = second
	}
	if n%2 == 0 && second == (n-2)/2 {
		second = 2 * (second + 1)
		a[first+hole] = a[first+second-1]
		hole = second - 1
	}
	parent := (hole - 1) / 2
	for hole > top && less(a[first+parent], v) {
		a[first+hole] = a[first+parent]
		hole = parent
		parent = (hole - 1) / 2
	}
	a[first+hole] = v
}

// declarationIndex maps each geometry node to its position among the model's
// node blocks: the number the game's compiler gives it.
func (c *compiler) declarationIndex() map[*Node]int32 {
	decl := make(map[*Node]int32, len(c.model.Nodes))
	for i, n := range c.model.Nodes {
		if n != nil {
			decl[n] = int32(i)
		}
	}
	return decl
}

// walkTree calls visit for each node of the geometry tree under root,
// depth-first in child order (the order the file is written in), with the
// node's parent and its position in that walk. A visited set guards against
// cyclic parent references.
func (c *compiler) walkTree(root *Node, visit func(n, parent *Node, index int32)) {
	type item struct{ n, parent *Node }
	visited := make(map[*Node]bool, len(c.model.Nodes))
	stack := []item{{root, nil}}
	var index int32
	for len(stack) > 0 {
		it := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		if it.n == nil || visited[it.n] {
			continue
		}
		visited[it.n] = true
		visit(it.n, it.parent, index)
		index++
		children := c.childrenOf(it.n)
		for i := len(children) - 1; i >= 0; i-- {
			if !visited[children[i]] {
				stack = append(stack, item{children[i], it.n})
			}
		}
	}
}
