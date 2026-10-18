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
from .gomath import go_lower

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


def is_normal_mapped(mesh, dirs):
    if mesh is None:
        return False
    if hint_wants_tangents(mesh.render_hint):
        return True
    if not dirs:
        return False
    for name in (mesh.material_name, mesh.bitmap):
        ref = find_material(name, dirs)
        if ref is not None:
            return hint_wants_tangents(material_render_hint(ref))
    return False
