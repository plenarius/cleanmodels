"""Declaration order of a decompiled model's geometry nodes: a port of
pkg/mdl/binary_order.go.

Prefers ascending part number (no supermodel, numbers 0..n-1, and compiling
that order rebuilds the stored numbers and tree, parents found the game's way,
MDL-NODE-002),
then for each node whose children get sorted the order the game's std::sort
turns back into the stored one (a node whose stored children no order gives
keeps them), then the stored order. A candidate is only taken when compiling it
rebuilds the stored tree node for node, apart from such a node's children. The
candidates are planned with the transparencyhints the ASCII carries, and only
when none is taken with the stored ones (stored_hints: those of meshes the
decompile gave another). The stored tree is the binary's own (m.stored_parent);
a node no order taken gives its stored parent is warned about.
"""

from .compiler_order import engine_sort, hint_less, mesh_hint, reorder_children
from .compiler_tree import resolve_geom_tree
from .gostr import equal_fold, trim_space
from .types import DecompileWarning, Node, WARN_GENERAL
from .gostr import quote


def declaration_order(m, stored_hints=None):
    """declarationOrder: reorder m.nodes in place."""
    stored = m.nodes
    if len(stored) < 2 or any(n is None for n in stored):
        return
    parent_of, children = _stored_parent_tree(stored, m.stored_parent)
    if not _walks_as(stored, children, stored):
        return
    try:
        _plan(m, stored, parent_of, children, stored_hints)
    finally:
        _warn_lost_parents(m, parent_of)


def _plan(m, stored, parent_of, children, stored_hints):
    def stale(n):
        h = stored_hints.get(n)
        return n.mesh.transparency_hint if h is None else h

    # checked with the hints the ASCII carries; an order planned with a restored
    # donor's stored hint only when its compile moves no other node (keepsOthers)
    order = None
    part = _part_number_order(m)
    if part is not None:
        if _compiles_to(part, stored, parent_of, children, None, True, mesh_hint):
            order = part
        elif stored_hints and _compiles_to(part, stored, parent_of, children, None, True, stale) and \
                _keeps_others(part, stored, parent_of, stored_hints):
            order = part
    if order is None:
        inv, unsorted = _inverse_sort_order(stored, children, mesh_hint)
        if inv is not None and _compiles_to(inv, stored, parent_of, children, unsorted, False, mesh_hint):
            order = inv
    # no order rebuilds the stored tree with the new hints, nor the stored order:
    # an order planned with the stored hints, whatever else the compile moves
    if order is None and stored_hints and not _compiles_to(stored, stored, parent_of, children, None, False, mesh_hint):
        part = _part_number_order(m)
        if part is not None and _compiles_to(part, stored, parent_of, children, None, True, stale):
            order = part
        else:
            inv, unsorted = _inverse_sort_order(stored, children, stale)
            if inv is not None and _compiles_to(inv, stored, parent_of, children, unsorted, False, stale):
                order = inv
    if order is not None:
        m.nodes = order


def _stored_parent_tree(stored, stored_parent):
    """storedParentTree: (parent_of, children) of the stored tree, from
    stored_parent, or by name (the nearest node of the parent's name listed
    before) without it."""
    parent_of, children = {}, {}
    for i, n in enumerate(stored):
        p = None
        if stored_parent is not None:
            p = stored_parent.get(n)
        elif not (n.parent == "" or equal_fold(n.parent, "NULL")):
            for j in range(i - 1, -1, -1):
                if equal_fold(stored[j].name, n.parent):
                    p = stored[j]
                    break
        if p is not None:
            parent_of[n] = p
            children.setdefault(p, []).append(n)
    return parent_of, children


def _warn_lost_parents(m, stored_parent):
    """warnLostParents: a node the game's lookup puts under another node of its
    parent's name in m.nodes as declared."""
    got = resolve_geom_tree(m.nodes)[0]
    for n in m.nodes:
        want = stored_parent.get(n)
        if want is None or got.get(n) is want:
            continue
        m.warnings.append(DecompileWarning(
            WARN_GENERAL, 0, n.name,
            "node %s: its parent %s is one of several nodes of that name, and the game's compiler attaches a child "
            "to the first of them in tree order (nwn-mdl-spec MDL-NODE-002), not the one the binary has it under; "
            "compiled from this ASCII, the node moves. Rename one of the parents to keep it"
            % (quote(n.name), quote(n.parent))))


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


def _keeps_others(order, stored, stored_parent, moved):
    """keepsOthers: compiling order (sorted by the hints the nodes carry) puts
    every node but the donors in moved under its stored parent and in its
    stored order."""
    parent_of, children = resolve_geom_tree(order)
    for n in order:
        if parent_of.get(n) is not stored_parent.get(n):
            return False
    reorder_children(order[0], children)
    i = 0
    for n in _depth_first(order[0], children):
        if n in moved:
            continue
        while i < len(stored) and stored[i] in moved:
            i += 1
        if i >= len(stored) or stored[i] is not n:
            return False
        i += 1
    return True
