"""Materials (.mtr) for tangent generation: a port of pkg/mdl/materials.go.

A mesh gets tangents when its effective render hint is NormalAndSpecMapped or
NormalTangents: its own renderhint, else the one in <materialname>.mtr, or
<texture0>.mtr when it has no material name.
"""

import os
import re
import threading

from . import resources
from .compiler_tangents import hint_wants_tangents
from .gomath import go_lower, go_quote

# Go's (?im)^\s*renderhint\s+(\S+); RE2's \s is [\t\n\f\r ].
_HINT_RE = re.compile(rb"^[\t\n\f\r ]*renderhint[\t\n\f\r ]+([^\t\n\f\r ]+)", re.I | re.M)

_lock = threading.Lock()
_indexes = {}   # dir -> {lower-case material name: path}
_hints = {}     # resource id -> lower-case renderhint


def _walk(d, out):
    """filepath.WalkDir order: entries sorted by name, depth first."""
    try:
        names = sorted(os.listdir(d), key=os.fsencode)
    except OSError:
        return
    for name in names:
        p = os.path.join(d, name)
        if os.path.isdir(p) and not os.path.islink(p):
            _walk(p, out)
        else:
            raw = os.fsencode(name).decode("latin-1")  # Go sees the name's bytes
            if len(raw) <= 4 or raw[-4:].lower() != ".mtr":
                continue
            key = go_lower(raw[:-4])
            if key not in out:
                out[key] = p


def _dir_index(d):
    with _lock:
        idx = _indexes.get(d)
    if idx is None:
        idx = {}
        if os.path.isdir(d):
            _walk(d, idx)
        with _lock:
            _indexes[d] = idx
    return idx


def find_material(name, locs):
    """<name>.mtr from the first location that has it, or None."""
    name = go_lower(name.strip())
    if name == "" or name == "null":
        return None
    for loc in locs:
        a = resources._open_archive(loc)
        if a is not None:
            read = a.get((name, resources.RES_TYPE_MTR))
            if read is not None:
                return resources.ResourceRef(loc + "#" + name + ".mtr", "", read)
            continue
        p = _dir_index(loc).get(name)
        if p is not None:
            return resources.ResourceRef(p, "", (lambda p=p: open(p, "rb").read()))
    return None


def material_render_hint(ref):
    with _lock:
        h = _hints.get(ref.id)
    if h is not None:
        return h
    h = ""
    try:
        m = _HINT_RE.search(ref.read())
        if m:
            h = go_lower(m.group(1).decode("latin-1"))
    except (OSError, ValueError):
        pass
    with _lock:
        _hints[ref.id] = h
    return h


def material_looked_up(name):
    """Whether find_material searches for name at all: empty or NULL is no material."""
    name = go_lower(name.strip())
    return name != "" and name != "null"


def is_normal_mapped(mesh, dirs, found=None):
    """found, if given, is called with each material looked up and its ref."""
    if mesh is None:
        return False
    if hint_wants_tangents(mesh.render_hint):
        return True
    if not dirs:
        return False
    for name in (mesh.material_name, mesh.bitmap):
        ref = find_material(name, dirs)
        if found is not None and material_looked_up(name):
            found(name, ref)
        if ref is not None:
            return hint_wants_tangents(material_render_hint(ref))
    return False


def mesh_normal_mapped(mesh, dirs, node_name, warn):
    """compiler.meshNormalMapped: is_normal_mapped, warning (through warn, when
    node_name is not None) when the mesh's materialname names a material none
    of dirs has. A bitmap with no .mtr is not warned about, and nothing is with
    no dirs."""
    if mesh is None:
        return False
    named = material_looked_up(mesh.material_name)
    seen = []  # refs in lookup order
    normal_mapped = is_normal_mapped(mesh, dirs, lambda name, ref: seen.append(ref))
    if not named or not dirs:
        return normal_mapped
    if seen:
        mat = seen[0]
        via_bitmap = len(seen) > 1 and seen[1] is not None
    else:
        mat = find_material(mesh.material_name, dirs)
        via_bitmap = False
    if mat is None and warn is not None and node_name is not None:
        warn(missing_material_warning(node_name, mesh, via_bitmap, dirs))
    return normal_mapped


def missing_material_warning(node, mesh, via_bitmap, dirs):
    if hint_wants_tangents(mesh.render_hint):
        consequence = "its own renderhint still gives it tangents"
    elif via_bitmap:
        consequence = "its render hint is taken from %s.mtr, named by its bitmap, instead" % mesh.bitmap.strip()
    else:
        consequence = "it gets no tangents, which the game generates when that material is normal-mapped"
    return "mesh %s: material %s.mtr not found in %s; %s" % (
        go_quote(node), mesh.material_name.strip(), ", ".join(dirs), consequence)
