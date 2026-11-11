"""Declaration order of a decompiled model's geometry nodes: a port of
pkg/mdl/binary_order.go.

Prefers ascending part number (no supermodel, numbers 0..n-1, no name used
twice a parent, and compiling that order rebuilds the stored numbers and tree),
then for each node whose children get sorted the order the game's std::sort
turns back into the stored one (a node whose stored children no order gives
keeps them), then the stored order. A candidate is only taken when compiling it
rebuilds the stored tree node for node, apart from such a node's children. The
candidates are planned with the transparencyhints the ASCII carries, and only
when none is taken with the stored ones (stored_hints: those of meshes the
decompile gave another).
"""

from .compiler_order import engine_sort, hint_less, mesh_hint, reorder_children
from .compiler_tree import resolve_geom_tree
from .gostr import equal_fold, trim_space
from .types import Node
from .writer import duplicate_is_parent


def declaration_order(m, stored_hints=None):
    """declarationOrder: reorder m.nodes in place."""
    stored = m.nodes
    if len(stored) < 2 or any(n is None for n in stored):
        return
    dup_parent = duplicate_is_parent(stored)
    parent_of, children = resolve_geom_tree(stored)
    if not _walks_as(stored, children, stored):
        return

    def plan(hint):
        if not dup_parent:
            order = _part_number_order(m)
            if order is not None and _compiles_to(order, stored, parent_of, children, None, True, hint):
                return order
        order, unsorted = _inverse_sort_order(stored, children, hint)
        if order is not None and _compiles_to(order, stored, parent_of, children, unsorted, False, hint):
            return order
        return None

    # first with the hints the ASCII carries; the stored hints of restored
    # donors only when no order rebuilds the stored tree with them
    order = plan(mesh_hint)
    if order is None and stored_hints and not _compiles_to(stored, stored, parent_of, children, None, False, mesh_hint):
        def stored_hint(n):
            h = stored_hints.get(n)
            return n.mesh.transparency_hint if h is None else h
        order = plan(stored_hint)
    if order is not None:
        m.nodes = order


def _part_number_order(m):
    sm = trim_space(m.super_model)
    if sm != "" and not equal_fold(sm, "NULL"):
        return None
    order = [None] * len(m.nodes)
    for n in m.nodes:
        p = n.part_number
        if p < 0 or p >= len(order) or order[p] is not None:
            return None
        order[p] = n
    if all(a is b for a, b in zip(order, m.nodes)):
        return None
    return order


def _inverse_sort_order(stored, children, hint):
    """inverseSortOrder: (order, unsorted parents), (None, None) when unchanged."""
    less = hint_less(hint)
    decl = {}
    unsorted = None
    for p in stored:
        kids = children.get(p, [])
        if not any(k.mesh is not None and hint(k) > 0 for k in kids):
            continue
        if any(less(kids[j], kids[j - 1]) for j in range(1, len(kids))):
            if unsorted is None:
                unsorted = set()
            unsorted.add(p)
            continue
        pos = []
        index = {}
        for i in range(len(kids)):
            s = Node()
            pos.append(s)
            index[s] = i
        engine_sort(pos, lambda a, b, kids=kids: less(kids[index[a]], kids[index[b]]))
        d = [None] * len(kids)
        for j, s in enumerate(pos):
            d[index[s]] = kids[j]
        if any(a is not b for a, b in zip(d, kids)):
            decl[p] = d
    if not decl:
        return None, None
    order = []
    stack = [stored[0]]
    while stack:
        n = stack.pop()
        order.append(n)
        kids = decl.get(n, children.get(n, []))
        for i in range(len(kids) - 1, -1, -1):
            stack.append(kids[i])
    if len(order) != len(stored):
        return None, None
    return order, unsorted


def _compiles_to(order, stored, stored_parent, stored_children, unsorted, numbers, hint):
    if not equal_fold(order[0].parent, "NULL"):
        return False
    parent_of, children = resolve_geom_tree(order)
    for n in order:
        if parent_of.get(n) is not stored_parent.get(n):
            return False
    reorder_children(order[0], children, hint)
    want = stored
    if unsorted:
        kids = dict(stored_children)
        for p in unsorted:
            if p in kids:
                kids[p] = children.get(p, [])
        want = _depth_first(stored[0], kids)
    if not _walks_as(order, children, want):
        return False
    if numbers:
        for i, n in enumerate(order):
            if n.part_number != i:
                return False
    return True


def _walks_as(nodes, children, want):
    visited = set()
    stack = [nodes[0]]
    i = 0
    while stack:
        n = stack.pop()
        if n in visited:
            continue
        visited.add(n)
        if i >= len(want) or want[i] is not n:
            return False
        i += 1
        kids = children.get(n, [])
        for k in range(len(kids) - 1, -1, -1):
            if kids[k] not in visited:
                stack.append(kids[k])
    return i == len(want)


def _depth_first(root, children):
    """depthFirst: the tree under root through children, each node once."""
    out = []
    visited = set()
    stack = [root]
    while stack:
        n = stack.pop()
        if n in visited:
            continue
        visited.add(n)
        out.append(n)
        kids = children.get(n, [])
        for k in range(len(kids) - 1, -1, -1):
            if kids[k] not in visited:
                stack.append(kids[k])
    return out
