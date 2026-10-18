"""Body parts bound to a creature skeleton: the compile side of pkg/mdl/skeleton.go,
with twoda.go's capart.2da reader.

A skinned body part (pmh0_neck001) is bound in game to the creature skeleton
(pmh0), hung from the node capart.2da names for its part type (neck_g); its
bone map and bind lists are numbered in that skeleton's tree.
"""

import threading

from .compiler_skinbind import world_transforms
from .compiler_tree import resolve_geom_tree
from .gomath import go_lower
from .gostr import equal_fold_ascii
from .resources import RES_TYPE_2DA, find_model_resource, find_resource
from .skeleton import part_skeleton_name

PART_ATTACH_NODES = {
    "footr": "rfoot_g", "footl": "lfoot_g",
    "shinr": "rshin_g", "shinl": "lshin_g",
    "legl": "lthigh_g", "legr": "rthigh_g",
    "pelvis": "pelvis_g", "chest": "torso_g", "belt": "belt_g",
    "neck": "neck_g",
    "forer": "rforearm_g", "forel": "lforearm_g",
    "bicepr": "rbicep_g", "bicepl": "lbicep_g",
    "shor": "rshoulder_g", "shol": "lshoulder_g",
    "handr": "rhand_g", "handl": "lhand_g",
}


def part_kind(model_name):
    name = go_lower(model_name.strip())
    return name[name.find("_") + 1:].rstrip("0123456789")


def part_attach_node(model_name):
    if part_skeleton_name(model_name) == "":
        return ""
    return PART_ATTACH_NODES.get(part_kind(model_name), "")


class CompileSkeleton:
    """A creature skeleton numbered in tree order, with rest transforms."""

    def __init__(self, path, model, order):
        self.path = path
        self.model = model
        self.order = order
        self.index = {}
        self.pos = []
        self.rot = []

    def attach_index(self, node):
        node = go_lower(node)
        if node == "root":
            return 0, len(self.order) > 0
        i = self.index.get(node)
        return (i, True) if i is not None else (0, False)


_cache = {}
_capart = {}
_lock = threading.Lock()


def load_skeleton(name, locs):
    """loadSkeleton: None when not found; raises ValueError when unreadable."""
    from .compiler import Compiler
    from .supermodel import read_model_bytes
    ref = find_model_resource(name, locs)
    if ref is None:
        return None
    key = ref.id + "\0" + ref.stamp
    with _lock:
        if key in _cache:
            return _cache[key]
    data = ref.read()
    try:
        model = read_model_bytes(data)
    except Exception as e:  # noqa: BLE001 - reported like Go's error
        raise ValueError("%s: %s" % (ref.id, e))
    if model is None or model.root_node() is None:
        raise ValueError("%s: no node tree" % ref.id)
    c = Compiler(model)
    c.assign_node_ids(model.root_node())
    order = [None] * len(c.tree_index)
    for n, i in c.tree_index.items():
        order[i] = n
    sk = CompileSkeleton(ref.id, model, order)
    _, children = resolve_geom_tree(model.nodes)
    wpos, wrot = world_transforms(model.root_node(), children)
    for i, n in enumerate(order):
        sk.index.setdefault(go_lower(n.name), i)
        sk.pos.append(wpos.get(n, (0.0, 0.0, 0.0)))
        sk.rot.append(wrot.get(n, (0.0, 0.0, 0.0, 0.0)))
    with _lock:
        _cache[key] = sk
    return sk


def parse_2da(data):
    """parse2DA: rows as {lower-case column: value}, row number under "# row"."""
    lines = data.decode("latin-1").replace("\r\n", "\n").split("\n")
    if not lines[0].strip().startswith("2DA"):
        raise ValueError("not a 2DA table")
    cols = None
    rows = []
    for line in lines[1:]:
        f = _split_2da_line(line)
        if not f or equal_fold_ascii(f[0], "default:"):
            continue
        if cols is None:
            cols = [go_lower(c) for c in f]
            continue
        row = {"# row": f[0]}
        for i, c in enumerate(cols):
            if i + 1 < len(f):
                row[c] = f[i + 1]
        rows.append(row)
    return rows


def _split_2da_line(line):
    out = []
    i, n = 0, len(line)
    while i < n:
        while i < n and line[i] in " \t":
            i += 1
        if i >= n:
            break
        if line[i] == '"':
            j = line.find('"', i + 1)
            if j < 0:
                out.append(line[i + 1:])
                break
            out.append(line[i + 1:j])
            i = j + 1
            continue
        j = i
        while j < n and line[j] not in " \t":
            j += 1
        out.append(line[i:j])
        i = j
    return out


def find_capart(locs):
    """findCapart: ({part type: attach node}, where it was read) or (None, "")."""
    ref = find_resource("capart", RES_TYPE_2DA, ".2da", locs)
    if ref is None:
        return None, ""
    key = ref.id + "\0" + ref.stamp
    with _lock:
        if key in _capart:
            return _capart[key], ref.id
    try:
        rows = parse_2da(ref.read())
    except (OSError, ValueError):
        return None, ""
    nodes = {}
    for r in rows:
        kind, node = go_lower(r.get("mdlname", "")), r.get("nodename", "")
        if kind not in ("", "****") and node not in ("", "****"):
            nodes[kind] = node
    with _lock:
        _capart[key] = nodes
    return nodes, ref.id
