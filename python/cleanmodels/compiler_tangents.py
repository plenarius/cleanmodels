"""Tangent generation: a port of pkg/mdl/compiler_tangents.go.

The game's MdlNodeTriMesh::CalculateTangentSpaceBasis, run on the merged GPU
vertices: per-triangle dP/du, accumulated per vertex, a cloned vertex where a
triangle's handedness disagrees, then Gram-Schmidt against the normal.
"""

import math

from .compiler_normals import game_normalize
from .f32 import f32
from .gostr import equal_fold_ascii

_THIRD = f32(0.3333333432674408)


def generate_tangents(exp, anim_mesh):
    """generateTangents: (tangents, handedness) per GPU vertex, or (None, None)."""
    if exp is None:
        return None, None
    n = len(exp.positions)
    if n == 0 or len(exp.normals) < n or len(exp.uvs) < n:
        return None, None
    pos, uvs, nors = exp.positions, exp.uvs, exp.normals

    tris = []
    for fv in exp.face_verts:
        i0, i1, i2 = fv[0], fv[1], fv[2]
        if i0 >= n or i1 >= n or i2 >= n:
            tris.append(((0.0, 0.0, 0.0), 0.0))
            continue
        p0, p1, p2 = pos[i0], pos[i1], pos[i2]
        u0, u1, u2 = uvs[i0], uvs[i1], uvs[i2]
        dv2 = f32(u2[1] - u0[1])
        dv1 = f32(u1[1] - u0[1])
        det = f32(f32(f32(u1[0] - u0[0]) * dv2) - f32(f32(u2[0] - u0[0]) * dv1))
        w = 1.0 if det >= 0 else -1.0
        if det != 0:
            a, b = f32(dv1 / det), f32(dv2 / det)
            t = (f32(f32(b * f32(p1[0] - p0[0])) - f32(a * f32(p2[0] - p0[0]))),
                 f32(f32(f32(p1[1] - p0[1]) * b) - f32(f32(p2[1] - p0[1]) * a)),
                 f32(f32(f32(p1[2] - p0[2]) * b) - f32(f32(p2[2] - p0[2]) * a)))
        else:
            n0, n1, n2 = nors[i0], nors[i1], nors[i2]
            t = (f32(f32(f32(n0[1] + n1[1]) + n2[1]) * _THIRD),
                 f32(f32(f32(n0[2] + n1[2]) + n2[2]) * _THIRD),
                 f32(f32(f32(n0[0] + n1[0]) + n2[0]) * _THIRD))
        tris.append((t, w))

    tan = [(0.0, 0.0, 0.0)] * n
    hand = [0.0] * n
    clones = []      # [src, w, t]
    clone_at = {}    # (src, w) -> index into clones
    rewrites = []    # (face, corner, to)
    for f, fv in enumerate(exp.face_verts):
        t, w = tris[f]
        for c in range(3):
            v = fv[c]
            if v >= n:
                continue
            h = hand[v]
            if h == 0 or anim_mesh:
                hand[v] = w
                tan[v] = t
            elif h == w:
                o = tan[v]
                tan[v] = (f32(o[0] + t[0]), f32(o[1] + t[1]), f32(o[2] + t[2]))
            else:
                k = clone_at.get((v, w))
                if k is None:
                    clones.append([v, w, (0.0, 0.0, 0.0)])
                    k = clone_at[(v, w)] = len(clones) - 1
                cl = clones[k]
                o = cl[2]
                cl[2] = (f32(o[0] + t[0]), f32(o[1] + t[1]), f32(o[2] + t[2]))
                rewrites.append((f, c, (n + k) & 0xFFFF))

    if clones:
        for src, w, t in clones:
            exp.positions.append(exp.positions[src])
            exp.orig_vert.append(exp.orig_vert[src])
            for arr in (exp.uvs, exp.uvs1, exp.uvs2, exp.uvs3, exp.normals, exp.colors):
                if len(arr) > src:
                    arr.append(arr[src])
            tan.append(t)
            hand.append(w)
        for f, c, to in rewrites:
            exp.face_verts[f][c] = to

    nors = exp.normals
    for i in range(len(tan)):
        nx, ny, nz = nors[i]
        tx, ty, tz = tan[i]
        d = f32(f32(f32(nx * tx) + f32(ny * ty)) + f32(nz * tz))
        tan[i] = game_normalize((f32(tx - f32(nx * d)), f32(ty - f32(ny * d)), f32(tz - f32(nz * d))))
    return tan, hand


def resolve_tangents(mesh, exp, normal_mapped, anim_mesh):
    """resolveTangents: authored tangents when they cover every GPU vertex,
    else generated ones for a normal-mapped mesh, else (None, None)."""
    if exp is None or not exp.positions:
        return None, None
    if _use_existing_tangents(mesh, exp):
        return _expand_existing_tangents(mesh, exp)
    if mesh is None or not normal_mapped:
        return None, None
    return generate_tangents(exp, anim_mesh)


def zero_length_tangents(mesh):
    """ZeroLengthTangents: tangents with no direction (length under 1e-6)."""
    if mesh is None:
        return 0
    z = 0
    for t in mesh.tangents:
        if math.sqrt(t[0] * t[0] + t[1] * t[1] + t[2] * t[2]) < 1e-6:
            z += 1
    return z


def use_existing_tangents(mesh, exp):
    return _use_existing_tangents(mesh, exp)


def _use_existing_tangents(mesh, exp):
    if mesh is None or not mesh.tangents:
        return False
    if len(exp.normals) < len(exp.positions):
        return False
    nt = len(mesh.tangents)
    for ov in exp.orig_vert:
        if ov < 0 or ov >= nt:
            return False
    return True


def _expand_existing_tangents(mesh, exp):
    tangents = []
    handed = []
    for i in range(len(exp.positions)):
        t = mesh.tangents[exp.orig_vert[i]]
        tangents.append((t[0], t[1], t[2]))
        handed.append(-1.0 if t[3] < 0 else 1.0)
    return tangents, handed


def render_hint_value(hint):
    """renderHintValue: SharedMaterial::ExtractRenderHint's number for a renderhint."""
    if equal_fold_ascii(hint, "normalandspecmapped"):
        return 2
    if equal_fold_ascii(hint, "normaltangents"):
        return 3
    if equal_fold_ascii(hint, "none"):
        return 1
    return 0


def hint_wants_tangents(hint):
    return equal_fold_ascii(hint, "normalandspecmapped") or equal_fold_ascii(hint, "normaltangents")
