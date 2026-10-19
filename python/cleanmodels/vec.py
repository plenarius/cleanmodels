"""Vector helpers the compiler uses: a port of parts of pkg/mdl/vec.go and transforms.go.

float32 operations are rounded one by one, as Go does them on amd64; float64
ones are plain Python floats. Sin and Cos are Go's (gomath).
"""

import math
import struct

from .compiler_tree import resolve_geom_tree
from .f32 import f32
from .gomath import go_cos, go_sin


def axis_angle_to_quat(aa):
    """vec.go axisAngleToQuat: axis-angle (x, y, z, angle) -> quaternion (x, y, z, w)."""
    ax, ay, az = engine_normalize(aa[0], aa[1], aa[2])
    half = f32(aa[3] * 0.5)
    s = f32(go_sin(half))
    c = f32(go_cos(half))
    return (f32(ax * s), f32(ay * s), f32(az * s), c)


_NORMALIZE_LOW = struct.unpack("<f", struct.pack("<I", 0x3F7FFF58))[0]
_NORMALIZE_HIGH = struct.unpack("<f", struct.pack("<I", 0x3F800054))[0]


def engine_normalize(x, y, z):
    """vec.go engineNormalize: Vector::Normalize (0x5b3700) in float32."""
    l2 = f32(f32(f32(x * x) + f32(y * y)) + f32(z * z))
    if l2 == 1 or _NORMALIZE_LOW <= l2 <= _NORMALIZE_HIGH:
        return x, y, z
    if l2 == 0:
        return 1.0, 0.0, 0.0
    k = f32(math.sqrt(f32(1 / l2)))
    return f32(x * k), f32(y * k), f32(z * k)


def abs_f32(x):
    return -x if x < 0 else x


def min_f(a, b):
    return a if a < b else b


def max_f(a, b):
    return a if a > b else b


def rotation_matrix(axis):
    """The Rodrigues matrix RotateVectorAxisAngle builds, or None when it
    returns its input unchanged (zero angle). A zero axis is +X."""
    angle = axis[3]
    if angle == 0:
        return None
    ax, ay, az = axis[0], axis[1], axis[2]
    r = math.sqrt(ax * ax + ay * ay + az * az)
    if r == 0:
        ax, r = 1.0, 1.0
    x, y, z = ax / r, ay / r, az / r
    c = go_cos(angle)
    s = go_sin(angle)
    t = 1.0 - c
    return (
        t * x * x + c, t * x * y - z * s, t * x * z + y * s,
        t * x * y + z * s, t * y * y + c, t * y * z - x * s,
        t * x * z - y * s, t * y * z + x * s, t * z * z + c,
    )


def rotate_with(m, v):
    """RotateVectorAxisAngle's final step, with a rotation_matrix result."""
    if m is None:
        return v
    ux, uy, uz = v
    return (f32(m[0] * ux + m[1] * uy + m[2] * uz),
            f32(m[3] * ux + m[4] * uy + m[5] * uz),
            f32(m[6] * ux + m[7] * uy + m[8] * uz))


def rotate_vector_axis_angle(v, axis):
    return rotate_with(rotation_matrix(axis), v)


def parent_chain(parent_of, n):
    """transforms.go parentChain: n, its parent, ... (stops at a cycle)."""
    chain = []
    seen = set()
    cur = n
    while cur is not None:
        if cur in seen:
            break
        seen.add(cur)
        chain.append(cur)
        cur = parent_of.get(cur)
    return chain


class _NodeXform:
    __slots__ = ("s", "m", "px", "py", "pz")

    def __init__(self, n):
        s = n.scale
        self.s = 1.0 if s == 0 else s
        self.m = rotation_matrix(n.orientation)
        self.px, self.py, self.pz = n.position


def _apply(t, v):
    """applyNodeTransform: scale, rotate, translate."""
    s = t.s
    sv = (f32(v[0] * s), f32(v[1] * s), f32(v[2] * s))
    rx, ry, rz = rotate_with(t.m, sv)
    return (f32(rx + t.px), f32(ry + t.py), f32(rz + t.pz))


def model_bounds(m):
    """transforms.go modelBounds: (bmin, bmax, radius) of the model in world space."""
    if m is None or not m.nodes:
        return (0.0, 0.0, 0.0), (0.0, 0.0, 0.0), 0.0
    parent_of, _ = resolve_geom_tree(m.nodes)
    xf = {}

    def chain_of(n):
        out = []
        for nd in parent_chain(parent_of, n):
            t = xf.get(nd)
            if t is None:
                t = xf[nd] = _NodeXform(nd)
            out.append(t)
        return out

    first = True
    x0 = y0 = z0 = x1 = y1 = z1 = 0.0
    for n in m.nodes:
        if n is None:
            continue
        if n.mesh is not None and n.mesh.verts:
            pts = n.mesh.verts
            chain = chain_of(n)
        else:
            pts = ((0.0, 0.0, 0.0),)
            chain = chain_of(n)
        for p in pts:
            for t in chain:
                p = _apply(t, p)
            x, y, z = p
            if first:
                x0, y0, z0 = x, y, z
                x1, y1, z1 = x, y, z
                first = False
                continue
            x0 = x0 if x0 < x else x
            y0 = y0 if y0 < y else y
            z0 = z0 if z0 < z else z
            x1 = x1 if x1 > x else x
            y1 = y1 if y1 > y else y
            z1 = z1 if z1 > z else z
    if first:
        return (0.0, 0.0, 0.0), (0.0, 0.0, 0.0), 0.0
    cx, cy, cz = f32(f32(x0 + x1) / 2), f32(f32(y0 + y1) / 2), f32(f32(z0 + z1) / 2)
    kx, ky, kz = f32(x1 - cx), f32(y1 - cy), f32(z1 - cz)
    radius = f32(math.sqrt(f32(f32(f32(kx * kx) + f32(ky * ky)) + f32(kz * kz))))
    return (x0, y0, z0), (x1, y1, z1), radius
