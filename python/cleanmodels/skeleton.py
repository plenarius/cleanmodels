"""Creature skeletons for skinned body parts: the decompile side of pkg/mdl/skeleton.go.

A skinned body part (pmh0_neck001) stores its bones as tree positions in the
creature skeleton (pmh0), numbered depth first from the root, children in file
order, with a node's parent being the nearest preceding node of that name
(compiler_tree.go resolveGeomTree, compiler.go assignNodeIDs).
"""

import threading

from .resources import find_model_resource
from .types import Model, Node
from .gostr import equal_fold


class Skeleton:
    __slots__ = ("path", "model", "order")

    def __init__(self, path, model, order):
        self.path = path
        self.model = model
        self.order = order


def part_skeleton_name(model_name):
    name = model_name.strip().lower()
    i = name.find("_")
    if i < 3 or name[0] != "p" or i == len(name) - 1:
        return ""
    return name[:i]


_cache = {}
_lock = threading.Lock()


def load_skeleton(name, locs):
    """Find <name>.mdl in locs and number its nodes in tree order; None if not found."""
    ref = find_model_resource(name, locs)
    if ref is None:
        return None
    key = ref.id + "\0" + ref.stamp
    with _lock:
        if key in _cache:
            return _cache[key]
    data = ref.read()
    model = _read_model_bytes(data)
    if model is None or model.root_node() is None:
        raise ValueError("%s: no node tree" % ref.id)
    order = tree_order(model.nodes, model.root_node())
    sk = Skeleton(ref.id, model, order)
    with _lock:
        _cache[key] = sk
    return sk


def resolve_children(nodes):
    """compiler_tree.go resolveGeomTree: each node's children by identity."""
    children = {}
    for i, n in enumerate(nodes):
        if n is None or n.parent == "" or equal_fold(n.parent, "NULL"):
            continue
        parent = None
        for j in range(i - 1, -1, -1):
            c = nodes[j]
            if c is not None and c is not n and equal_fold(c.name, n.parent):
                parent = c
                break
        if parent is None:
            for c in nodes:
                if c is not None and c is not n and equal_fold(c.name, n.parent):
                    parent = c
                    break
        if parent is None:
            continue
        children.setdefault(id(parent), []).append(n)
    return children


def tree_order(nodes, root):
    """compiler.go assignNodeIDs: pre-order from root, children in declaration order."""
    children = resolve_children(nodes)
    visited = set()
    order = []
    stack = [root]
    while stack:
        n = stack.pop()
        if n is None or id(n) in visited:
            continue
        visited.add(id(n))
        order.append(n)
        kids = children.get(id(n), [])
        for c in reversed(kids):
            if id(c) not in visited:
                stack.append(c)
    return order


def _read_model_bytes(data):
    from .binary import decompile, is_binary_mdl
    if is_binary_mdl(data):
        return decompile(data)
    return _read_ascii_tree(data)


def _read_ascii_tree(data):
    """The node names and parents of an ASCII model's geometry: all the
    skeleton numbering needs."""
    m = Model()
    m.file_type = "ascii"
    in_geom = False
    node = None
    for raw in data.decode("latin-1").splitlines():
        line = raw.split("#", 1)[0].strip()
        if not line:
            continue
        w = line.split()
        k = w[0].lower()
        if k == "beginmodelgeom":
            in_geom = True
        elif k == "endmodelgeom":
            in_geom = False
        elif k == "newmodel" and len(w) > 1:
            m.name = w[1].lower()
        elif in_geom and k == "node" and len(w) >= 3:
            node = Node(w[2].lower(), "")
            m.nodes.append(node)
        elif in_geom and k == "parent" and node is not None and len(w) >= 2:
            node.parent = w[1].lower()
        elif k == "endnode":
            node = None
    for n in m.nodes:
        if n.parent == "":
            n.parent = "NULL"
    return m
