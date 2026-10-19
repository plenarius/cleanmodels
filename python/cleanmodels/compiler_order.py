"""Node order and numbering as the game's compiler does them: a port of
pkg/mdl/compiler_order.go.

Numbers follow declaration order. Where one of a node's children is a mesh with
transparencyhint > 0, the geometry tree's children are sorted with libstdc++'s
std::sort (not stable past 16 elements), which engine_sort reproduces step for
step.
"""


def reorder_children(root, children):
    """reorderChildren: MdlNode::ReorderChildren over the geometry tree."""
    visited = set()
    stack = [root]
    while stack:
        n = stack.pop()
        if n is None or n in visited:
            continue
        visited.add(n)
        kids = children.get(n, [])
        for k in kids:
            if k.mesh is not None and k.mesh.transparency_hint > 0:
                kids = list(kids)
                engine_sort(kids, transparency_less)
                children[n] = kids
                break
        stack.extend(kids)


def transparency_less(a, b):
    """transparencyLess: the comparison at 0x453470."""
    if a.mesh is None:
        return b.mesh is not None
    if b.mesh is None:
        return False
    return a.mesh.transparency_hint < b.mesh.transparency_hint


def engine_sort(a, less):
    """engineSort: libstdc++'s std::sort."""
    n = len(a)
    if n < 2:
        return
    depth = 0
    k = n
    while k > 1:
        depth += 1
        k >>= 1
    _introsort_loop(a, 0, n, 2 * depth, less)
    if n > 16:
        _insertion_sort(a, 0, 16, less)
        for i in range(16, n):
            _unguarded_linear_insert(a, i, less)
    else:
        _insertion_sort(a, 0, n, less)


def _introsort_loop(a, first, last, depth, less):
    while last - first > 16:
        if depth == 0:
            _heap_sort(a, first, last, less)
            return
        depth -= 1
        mid = first + (last - first) // 2
        _move_median_to_first(a, first, first + 1, mid, last - 1, less)
        cut = _unguarded_partition(a, first + 1, last, first, less)
        _introsort_loop(a, cut, last, depth, less)
        last = cut


def _move_median_to_first(a, result, x, y, z, less):
    if less(a[x], a[y]):
        if less(a[y], a[z]):
            i = y
        elif less(a[x], a[z]):
            i = z
        else:
            i = x
    elif less(a[x], a[z]):
        i = x
    elif less(a[y], a[z]):
        i = z
    else:
        i = y
    a[result], a[i] = a[i], a[result]


def _unguarded_partition(a, first, last, pivot, less):
    while True:
        while less(a[first], a[pivot]):
            first += 1
        last -= 1
        while less(a[pivot], a[last]):
            last -= 1
        if first >= last:
            return first
        a[first], a[last] = a[last], a[first]
        first += 1


def _insertion_sort(a, first, last, less):
    for i in range(first + 1, last):
        if less(a[i], a[first]):
            v = a[i]
            a[first + 1:i + 1] = a[first:i]
            a[first] = v
        else:
            _unguarded_linear_insert(a, i, less)


def _unguarded_linear_insert(a, i, less):
    v = a[i]
    j = i - 1
    while less(v, a[j]):
        a[i] = a[j]
        i = j
        j -= 1
    a[i] = v


def _heap_sort(a, first, last, less):
    n = last - first
    if n >= 2:
        parent = (n - 2) // 2
        while True:
            _adjust_heap(a, first, parent, n, a[first + parent], less)
            if parent == 0:
                break
            parent -= 1
    while last - first > 1:
        last -= 1
        v = a[last]
        a[last] = a[first]
        _adjust_heap(a, first, 0, last - first, v, less)


def _adjust_heap(a, first, hole, n, v, less):
    top = hole
    second = hole
    while second < (n - 1) // 2:
        second = 2 * (second + 1)
        if less(a[first + second], a[first + second - 1]):
            second -= 1
        a[first + hole] = a[first + second]
        hole = second
    if n % 2 == 0 and second == (n - 2) // 2:
        second = 2 * (second + 1)
        a[first + hole] = a[first + second - 1]
        hole = second - 1
    parent = (hole - 1) // 2
    while hole > top and less(a[first + parent], v):
        a[first + hole] = a[first + parent]
        hole = parent
        parent = (hole - 1) // 2
    a[first + hole] = v
