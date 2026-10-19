"""Normal generation: a port of pkg/mdl/compiler_normals.go.

Per-face-corner normals from smoothing groups, as the game's
MdlNodeTriMesh::ProcessVerts computes them, in float32 (every product and sum
rounded on its own, no fused multiply-add).
"""

import math

from .f32 import f32

_NEAR1_LO = f32(0.9999899864196777)
_NEAR1_HI = f32(1.0000100135803223)


def game_normalize(v):
    """The game's Vector::Normalize: nearly-unit vectors are left alone, zero
    becomes (1, 0, 0), anything else is scaled by sqrt(1/lengthSquared)."""
    x, y, z = v
    l = f32(f32(f32(x * x) + f32(y * y)) + f32(z * z))
    if l == 1:
        return v
    if _NEAR1_LO <= l <= _NEAR1_HI:
        return v
    if l == 0:
        return (1.0, 0.0, 0.0)
    s = f32(math.sqrt(f32(1.0 / l)))
    return (f32(x * s), f32(y * s), f32(z * s))


def face_cross(p1, p2, p3):
    """(p2-p1) x (p3-p2), as compiler_normals.go and writeMeshFaceData compute it."""
    e1x, e1y, e1z = f32(p2[0] - p1[0]), f32(p2[1] - p1[1]), f32(p2[2] - p1[2])
    e2x, e2y, e2z = f32(p3[0] - p2[0]), f32(p3[1] - p2[1]), f32(p3[2] - p2[2])
    return (f32(f32(e1y * e2z) - f32(e1z * e2y)),
            f32(f32(e1z * e2x) - f32(e1x * e2z)),
            f32(f32(e1x * e2y) - f32(e1y * e2x)))


def _corner_at(f, v):
    if f.verts[2] == v:
        return 2
    if f.verts[1] == v:
        return 1
    return 0


def generate_normals(mesh):
    """generateNormals: fills mesh.normals and mesh.corner_normals unless the
    mesh already has a normal per vertex."""
    if mesh is None or not mesh.verts or not mesh.faces:
        return
    if len(mesh.normals) >= len(mesh.verts):
        return
    verts = mesh.verts
    faces = mesh.faces
    n_verts = len(verts)
    n_faces = len(faces)

    face_normals = [(0.0, 0.0, 0.0)] * n_faces
    for fi, f in enumerate(faces):
        a, b, c = f.verts[0], f.verts[1], f.verts[2]
        if 0 <= a < n_verts and 0 <= b < n_verts and 0 <= c < n_verts:
            face_normals[fi] = face_cross(verts[a], verts[b], verts[c])

    vert_faces = [[] for _ in range(n_verts)]
    for fi, f in enumerate(faces):
        for vi in range(3):
            idx = f.verts[vi]
            if 0 <= idx < n_verts:
                vert_faces[idx].append(fi)

    zero = (0.0, 0.0, 0.0)
    corner = [[zero, zero, zero] for _ in range(n_faces)]
    for fi, f in enumerate(faces):
        sg = f.smooth_group
        own = game_normalize(face_normals[fi])
        cn_fi = corner[fi]
        for vi in range(3):
            vert_idx = f.verts[vi]
            if vert_idx < 0 or vert_idx >= n_verts:
                continue
            ax, ay, az = face_normals[fi]
            reused = False
            if sg != 0:
                for fj in vert_faces[vert_idx]:
                    if fj == fi or faces[fj].smooth_group & sg == 0:
                        continue
                    if fj < fi:
                        cn = corner[fj][_corner_at(faces[fj], vert_idx)]
                        dot = f32(f32(f32(own[0] * cn[0]) + f32(own[1] * cn[1])) + f32(own[2] * cn[2]))
                        if dot >= -0.99:
                            cn_fi[vi] = cn
                            reused = True
                            break
                        continue
                    fn = face_normals[fj]
                    ax, ay, az = f32(ax + fn[0]), f32(ay + fn[1]), f32(az + fn[2])
            if reused:
                continue
            if f32(f32(f32(ax * ax) + f32(ay * ay)) + f32(az * az)) != 0:
                cn_fi[vi] = game_normalize((ax, ay, az))
            else:
                cn_fi[vi] = own

    normals = [zero] * n_verts
    seen = [False] * n_verts
    for fi, f in enumerate(faces):
        for vi in range(3):
            idx = f.verts[vi]
            if 0 <= idx < n_verts and not seen[idx]:
                normals[idx] = corner[fi][vi]
                seen[idx] = True
    mesh.normals = normals
    mesh.corner_normals = [tuple(c) for c in corner]
