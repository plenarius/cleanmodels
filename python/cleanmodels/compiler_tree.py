"""Node-identity tree resolution: a port of pkg/mdl/compiler_tree.go.

MDL stores parentage by name, but names are not unique (two "hand" dummies, a
mesh with a same-named hook child). A node's parent is found the game's way
(nwn-mdl-spec MDL-NODE-002): the first node of its parent's name in a depth-
first walk of the tree built so far, children in the order they were added.
`parent NULL` makes a node the root, a later one replacing it (MDL-NODE-003).
A parent the walk does not find (the game's compiler crashes) falls back to
the nearest node of that name declared before, then to any. Results are keyed
by node identity, not name.
"""

from .gostr import equal_fold, fold_key
from .gomath import go_lower


def is_root_parent(parent):
    return parent == "" or equal_fold(parent, "NULL")


def _resolve(nodes, name_of, parent_of):
    """Shared by the geometry and animation trees: (parent_of, children_of) by identity."""
    parents = {}
    children = {}
    attached = {}  # fold key -> nodes of that name added so far
    root = None
    moved = False

    def attach(n):
        attached.setdefault(fold_key(name_of(n)), []).append(n)

    def find(name):
        if root is None:
            return None
        cands = attached.get(fold_key(name))
        if not cands:
            return None
        if len(cands) == 1 and not moved:
            return cands[0]
        stack = [root]
        while stack:
            x = stack.pop()
            if equal_fold(name_of(x), name):
                return x
            stack.extend(reversed(children.get(x, ())))
        return None

    for i, n in enumerate(nodes):
        if n is None or parent_of(n) == "":
            continue
        if is_root_parent(parent_of(n)):
            if root is not None:
                moved = True
            root = n
            attach(n)
            continue
        want = parent_of(n)
        parent = find(want)
        if parent is None:
            for j in range(i - 1, -1, -1):
                cand = nodes[j]
                if cand is not None and cand is not n and equal_fold(name_of(cand), want):
                    parent = cand
                    break
        if parent is None:
            for cand in nodes:
                if cand is not None and cand is not n and equal_fold(name_of(cand), want):
                    parent = cand
                    break
        if parent is None:
            continue
        parents[n] = parent
        children.setdefault(parent, []).append(n)
        attach(n)
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


def anim_node_numbers(nodes, root, geom_root, children_of, node_ids):
    """animNodeNumbers (nwn-mdl-spec MDL-NODE-040): each animation node's part
    number, found by walking its path down the geometry tree. A matched node
    takes the geometry node's number; one the walk does not find gets -1 under
    a matched parent, otherwise its declaration index in the animation."""
    parent_of, _ = resolve_anim_tree(nodes)
    decl = {an: i for i, an in reversed(list(enumerate(nodes)))}
    out = {}
    match = {}

    def number_one(an):
        out[an] = decl[an]
        g = None
        parent_matched = False
        if an is root:
            g = geom_root
        else:
            p = parent_of.get(an)
            pg = match.get(p) if p is not None else None
            if pg is not None:
                parent_matched = True
                for ch in children_of(pg):
                    if equal_fold(ch.name, an.name):
                        g = ch
                        break
        if g is not None and g in node_ids:
            match[an] = g
            out[an] = node_ids[g]
        elif parent_matched:
            out[an] = -1

    for an in nodes:
        # Number the unnumbered ancestors first, root-most first; a parent
        # cycle stops at a node already on the chain.
        chain = []
        on_chain = set()
        cur = an
        while cur is not None and cur not in out and cur not in on_chain:
            chain.append(cur)
            on_chain.add(cur)
            cur = parent_of.get(cur) if cur is not root else None
        for n in reversed(chain):
            number_one(n)
    return out


def _reachable(root, children):
    """orderedReachable: the nodes reachable from root, depth first, each once."""
    out = []
    seen = set()
    stack = [root]
    while stack:
        an = stack.pop()
        if an in seen:
            continue
        seen.add(an)
        out.append(an)
        stack.extend(reversed(children.get(an, ())))
    return out


def prune_anim_tree(root, children, anim_root):
    """pruneAnimTree: cut an animation's tree to its animroot as the game's
    compiler does (MDL-NODE-045). Returns (children to write with, pruned)."""
    if root is None or not anim_root or equal_fold(anim_root, "NULL"):
        return children, []
    parent = {}
    seen = set()
    found = None
    stack = [root]
    while stack:
        an = stack.pop()
        if an in seen:
            continue
        seen.add(an)
        if an.name == anim_root:
            found = an
            break
        for ch in reversed(children.get(an, ())):
            if ch not in seen:
                parent[ch] = an
                stack.append(ch)
    if found is None or found is root:
        return children, []
    kept = dict(children)
    n = found
    while n is not root:
        kept[parent[n]] = [n]
        n = parent[n]
    in_kept = set(_reachable(root, kept))
    return kept, [an for an in _reachable(root, children) if an not in in_kept]
