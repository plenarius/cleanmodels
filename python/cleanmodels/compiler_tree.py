"""Node-identity tree resolution: a port of pkg/mdl/compiler_tree.go.

MDL stores parentage by name, but names are not unique (two "hand" dummies, a
mesh with a same-named hook child). A node's parent is the nearest preceding
node whose name matches its Parent field (nodes are in DFS pre-order), else any
node of that name. Results are keyed by node identity, not name.
"""

from bisect import bisect_left

from .gostr import equal_fold, fold_key
from .gomath import go_lower


def is_root_parent(parent):
    return parent == "" or equal_fold(parent, "NULL")


def _resolve(nodes, name_of, parent_of):
    """Shared by the geometry and animation trees: (parent_of, children_of) by identity."""
    by_name = {}
    for i, n in enumerate(nodes):
        if n is not None:
            by_name.setdefault(fold_key(name_of(n)), []).append(i)
    parents = {}
    children = {}
    for i, n in enumerate(nodes):
        if n is None or is_root_parent(parent_of(n)):
            continue
        idxs = by_name.get(fold_key(parent_of(n)))
        if not idxs:
            continue
        parent = None
        k = bisect_left(idxs, i) - 1
        while k >= 0:
            cand = nodes[idxs[k]]
            if cand is not n:
                parent = cand
                break
            k -= 1
        if parent is None:
            for j in idxs:
                if nodes[j] is not n:
                    parent = nodes[j]
                    break
        if parent is None:
            continue
        parents[n] = parent
        children.setdefault(parent, []).append(n)
    return parents, children


def resolve_geom_tree(nodes):
    """resolveGeomTree: (parent_of, children_of), keyed by node."""
    return _resolve(nodes, lambda n: n.name, lambda n: n.parent)


def resolve_anim_tree(nodes):
    """resolveAnimTree, for an animation's node list."""
    return _resolve(nodes, lambda n: n.name, lambda n: n.parent)


def node_occurrences(nodes):
    """Geometry nodes grouped by lower-case name, in declaration order."""
    out = {}
    for n in nodes:
        if n is not None:
            out.setdefault(go_lower(n.name), []).append(n)
    return out


def pair_anim_nodes_to_geom(anim_nodes, geom_occur):
    """The Nth animation node named X drives the Nth geometry node named X;
    surplus animation nodes take the last one."""
    out = {}
    seen = {}
    for an in anim_nodes:
        key = go_lower(an.name)
        occ = geom_occur.get(key)
        if not occ:
            continue
        idx = seen.get(key, 0)
        seen[key] = idx + 1
        if idx >= len(occ):
            idx = len(occ) - 1
        out[an] = occ[idx]
    return out
