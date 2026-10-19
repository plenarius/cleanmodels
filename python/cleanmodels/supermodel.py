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
    __slots__ = ("number", "parent")

    def __init__(self, number, parent):
        self.number = number  # -1 when the engine left the node unnumbered
        self.parent = parent


class SupermodelInfo:
    def __init__(self, path):
        self.path = path
        self.nodes = {}   # lower-case node name -> SuperNode
        self.root = ""
        self.count = 0


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
        for n in model.nodes:
            info.nodes[go_lower(n.name)] = SuperNode(n.part_number, go_lower(n.parent))
        root = model.root_node()
        if root is not None:
            info.root = go_lower(root.name)
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
    # In file order, as for a binary supermodel: with a name used twice the
    # later node's number wins.
    for n in model.nodes:
        nid = c.node_ids.get(n)
        if nid is not None:
            info.nodes[go_lower(n.name)] = SuperNode(nid, go_lower(n.parent))
    info.root = go_lower(root.name)
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
