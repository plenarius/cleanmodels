"""Supermodel numbering: a port of pkg/mdl/supermodel.go.

A model compiled against its supermodel takes the supermodel's node numbers
for the nodes they share, so the supermodel's animations address the right
node. A binary supermodel carries its numbers; an ASCII one is numbered the
way the engine would number it, following its own supermodel chain.
"""

import struct
import threading

from .gomath import go_lower
from .resources import find_model_resource


class SuperNode:
    __slots__ = ("name", "number", "children")

    def __init__(self, name, number):
        self.name = name      # lower case
        self.number = number  # -1 when the engine left the node unnumbered
        self.children = []

    def child(self, name):
        """superNode.child: the first child named name (any letter case), or None."""
        name = go_lower(name)
        for k in self.children:
            if k.name == name:
                return k
        return None


class SupermodelInfo:
    def __init__(self, path):
        self.path = path
        self.root = None  # SuperNode tree
        self.count = 0


def super_tree(model, number):
    """supermodel.go superTree: the supermodel's tree from its root, numbered by number(node)."""
    from .compiler_tree import resolve_geom_tree
    root = model.root_node()
    if root is None:
        return None
    _, children = resolve_geom_tree(model.nodes)
    visited = set()

    def build(n):
        visited.add(n)
        sn = SuperNode(go_lower(n.name), number(n))
        for k in children.get(n, []):
            if k not in visited:
                sn.children.append(build(k))
        return sn
    return build(root)


_cache = {}
_lock = threading.Lock()


def read_model_bytes(data):
    """skeleton.go readModelBytes: a model from memory, binary or ASCII."""
    from .binary import decompile, is_binary_mdl
    if is_binary_mdl(data):
        return decompile(data)
    return parse_ascii(data)


def parse_ascii(data):
    """The ASCII parser's model, or ValueError when it cannot parse."""
    from . import parser
    return parser.parse(data).model


def load_supermodel(ref, dirs, seen):
    key = ref.id + "\0" + ref.stamp + "\0" + "\0".join(dirs)
    with _lock:
        if key in _cache:
            return _cache[key]
    info = _read_supermodel(ref, dirs, seen)
    with _lock:
        _cache[key] = info
    return info


def _read_supermodel(ref, dirs, seen):
    from .binary import decompile, is_binary_mdl
    from .compiler import Compiler
    data = ref.read()
    info = SupermodelInfo(ref.id)
    if len(data) >= 12 and struct.unpack_from("<I", data, 0)[0] == 0:
        model = decompile(data)
        off = 12 + 8 + 64 + 4
        if len(data) < off + 4:
            raise ValueError("%s: truncated binary header" % ref.id)
        info.count = struct.unpack_from("<i", data, off)[0]
        info.root = super_tree(model, lambda n: n.part_number)
        return info

    model = parse_ascii(data)
    root = model.root_node() if model is not None else None
    if root is None:
        raise ValueError("%s: supermodel has no root node" % ref.id)
    c = Compiler(model)
    parent = find_supermodel_for(model, dirs, seen)
    if parent is not None:
        try:
            sm = load_supermodel(parent, dirs, seen)
        except Exception:  # noqa: BLE001 - Go ignores the error and numbers without it
            sm = None
        if sm is not None:
            c.assign_node_ids_from_supermodel(root, sm)
    if not c.node_ids:
        c.assign_node_ids(root)
    info.root = super_tree(model, lambda n: c.node_ids.get(n, 0))
    info.count = len(c.node_ids)
    if c.node_count_override > 0:
        info.count = c.node_count_override
    return info


def find_supermodel_for(model, dirs, seen):
    name = go_lower(model.super_model.strip())
    if name == "" or name == "null" or name == go_lower(model.name) or name in seen:
        return None
    seen.add(name)
    return find_model_resource(name, dirs)
