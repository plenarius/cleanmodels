"""share_positions.go: give binary vertices one position again where the source shared it
(nwn-mdl-spec MDL-MESH-025). See the Go file for the reasoning."""

import math

from .gostr import go_lower
from .share_neighbours import stored_neighbours


def _feq(a, b):
    """Floats compared as the engine compares them: ==, so -0 equals +0 and NaN never equals."""
    return all(x == y for x, y in zip(a, b))


def share_positions(n):
    m = n.mesh
    if m is None or n.aabb is not None or n.anim_mesh is not None:
        return
    nv = len(m.verts)
    if (nv == 0 or len(m.stored_adjacency) != len(m.faces)
            or len(m.tverts1) + len(m.tverts2) + len(m.tverts3) > 0
            or len(m.tex_indices0) + len(m.tex_indices1) + len(m.tex_indices2) + len(m.tex_indices3) > 0):
        return
    weights = n.skin.weights if n.skin is not None else []
    constraints = n.dangly.constraints if n.dangly is not None else []

    def full(lst):
        return len(lst) == 0 or len(lst) == nv

    if (not full(m.normals) or not full(m.tangents) or not full(m.colors)
            or not full(weights) or not full(constraints) or len(m.tverts) != nv):
        return
    for f in m.faces:
        for c in range(3):
            if f.verts[c] != f.uvs[c] or f.verts[c] < 0 or f.verts[c] >= nv:
                return

    keys = []
    nans = []
    for i in range(nv):
        fl = list(m.verts[i])
        if m.normals:
            fl += list(m.normals[i])
        if m.tangents:
            fl += list(m.tangents[i])
        if m.colors:
            fl += list(m.colors[i])
        if constraints:
            fl.append(constraints[i])
        bones = ()
        if weights:
            w = weights[i]
            for k, bone in enumerate(w.bones):
                fl.append(w.weights[k] if k < len(w.weights) else None)
            bones = tuple(go_lower(b) for b in w.bones)
        nans.append(any(isinstance(x, float) and math.isnan(x) for x in fl))
        keys.append((bones, fl))

    def same(a, b):
        ka, kb = keys[a], keys[b]
        if ka[0] != kb[0] or len(ka[1]) != len(kb[1]):
            return False
        for x, y in zip(ka[1], kb[1]):
            if x is None or y is None:
                if x is not y:
                    return False
            elif x != y:
                return False
        return True

    parent = list(range(nv))

    def find(x):
        while parent[x] != x:
            parent[x] = parent[parent[x]]
            x = parent[x]
        return x

    def join(a, b):
        if nans[a] or nans[b] or not same(a, b):
            return
        ra, rb = find(a), find(b)
        if ra < rb:
            parent[rb] = ra
        elif rb < ra:
            parent[ra] = rb

    faces = m.faces
    adj = stored_neighbours(m)
    for fi, f in enumerate(faces):
        for k in range(3):
            j = adj[fi][k]
            if j < 0 or j >= len(faces) or j == fi:
                continue
            a, c = f.verts[k], f.verts[(k + 1) % 3]
            g = faces[j].verts
            for t in range(3):
                x, y = g[t], g[(t + 1) % 3]
                pa, pc, px, py = m.verts[a], m.verts[c], m.verts[x], m.verts[y]
                if _feq(pa, px) and _feq(pc, py):
                    join(a, x)
                    join(c, y)
                    break
                if _feq(pa, py) and _feq(pc, px):
                    join(a, y)
                    join(c, x)
                    break

    remap = [0] * nv
    new_index = {}
    keep = []
    for i in range(nv):
        r = find(i)
        if r in new_index:
            remap[i] = new_index[r]
            continue
        new_index[r] = len(keep)
        remap[i] = len(keep)
        keep.append(i)
    if len(keep) == nv:
        return
    m.verts = [m.verts[i] for i in keep]
    if m.normals:
        m.normals = [m.normals[i] for i in keep]
    if m.tangents:
        m.tangents = [m.tangents[i] for i in keep]
    if m.colors:
        m.colors = [m.colors[i] for i in keep]
    if constraints:
        n.dangly.constraints = [constraints[i] for i in keep]
    if weights:
        n.skin.weights = [weights[i] for i in keep]
    for f in faces:
        f.verts = [remap[v] for v in f.verts]  # uvs keep indexing tverts
