"""The 512-byte mesh header and its MDX vertex data: a port of pkg/mdl/compiler_mesh.go.

Face corners are merged into GPU vertices the way the game's
InternalGenVertices merges them (position, UV0-3, normal, colour, mirrorlist
value, and the bone influences / constraint of skin and dangly meshes, floats
compared by value), degenerate faces are dropped, and the vertex streams go to
the MDX block in the order real binaries use.
"""

import math
import struct

from .compiler_normals import face_cross, generate_normals
from .compiler_tangents import render_hint_value, resolve_tangents
from .f32 import f32, f32_bits
from .gomath import go_lower
from .materials import is_normal_mapped

_MAX_F32 = 3.4028234663852886e38
_FACE = struct.Struct("<4fI6H")  # normal, plane distance, material, adjacency, corners


class CompileError(Exception):
    pass


class ExpandedMesh:
    """expandedMesh: the merged GPU vertex arrays and per-face corner indices."""

    def __init__(self):
        self.positions = []
        self.uvs = []
        self.uvs1 = []
        self.uvs2 = []
        self.uvs3 = []
        self.normals = []
        self.colors = []
        self.tex_stages = 0
        self.face_verts = []   # [a, b, c] GPU indices per kept face
        self.face_src = []     # face_verts[k] came from mesh.faces[face_src[k]]
        self.orig_vert = []    # GPU vertex -> source vertex index
        self.orig_tvert = []   # GPU vertex -> source UV0 index, -1 without one
        self.faces_ptr_pos = 0
        self.index_count_list_ptr_pos = -1
        self.index_offset_list_ptr_pos = -1
        self.index_buf_off = -1


def _bits(v):
    """f32bits as a dict key: -0 equals +0 already; a NaN is keyed by its bits."""
    if v != v:
        return ("nan", f32_bits(v))
    return v


def skin_merge_keys(skin):
    """skinMergeKeys: per-vertex bone count, names (lower case) and weight bits."""
    if skin is None:
        return None
    from .compiler_node import pick_top_bone_influences
    keys = []
    for vw in skin.weights:
        bones, weights = pick_top_bone_influences(vw.bones, vw.weights, 4)
        parts = [str(len(bones))]
        for bi, bone in enumerate(bones):
            parts.append("\0" + go_lower(bone))
            if bi < len(weights):
                w = weights[bi]
                parts.append(":%x" % (0 if w == 0 else f32_bits(w)))
        keys.append("".join(parts))
    return keys


def _has_nan(seq):
    for v in seq:
        for x in v:
            if x != x:
                return True
    return False


def _zbits(v):
    """f32bits: the float's bits, with -0 folded into +0."""
    return 0 if v == 0 else f32_bits(v)


def anim_sets(lst, n):
    """animSets: how many sets lst holds for a mesh with n source entries."""
    return len(lst) // n if n > 0 else 0


def anim_merge_key(mesh, am, vert_idx, tv_idx):
    """animMergeKey: the corner's vertex in every animverts set, then its tvert in every animtverts set."""
    nv, nt = len(mesh.verts), len(mesh.tverts)
    key = []
    if 0 <= vert_idx < nv:
        for s in range(anim_sets(am.anim_verts, nv)):
            v = am.anim_verts[s * nv + vert_idx]
            key.append((_zbits(v[0]), _zbits(v[1]), _zbits(v[2])))
    key.append("|")
    if 0 <= tv_idx < nt:
        for s in range(anim_sets(am.anim_tverts, nt)):
            v = am.anim_tverts[s * nt + tv_idx]
            key.append((_zbits(v[0]), _zbits(v[1])))
    return tuple(key)


def anim_vertex_major(lst, n, orig):
    """animVertexMajor: for each GPU vertex, its source entry in every set."""
    sets = anim_sets(lst, n)
    zero = (0.0, 0.0, 0.0)
    out = []
    for o in orig:
        for s in range(sets):
            out.append(lst[s * n + o] if 0 <= o < n else zero)
    return out


def build_expanded_mesh(mesh, strip_degenerate=False, skin_keys=None, dangly=None, aabb_merge=False, anim=None):
    """buildExpandedMeshOpts. Raises CompileError past 65535 GPU vertices."""
    verts = mesh.verts
    tverts, tverts1, tverts2, tverts3 = mesh.tverts, mesh.tverts1, mesh.tverts2, mesh.tverts3
    has_uv, has_uv1, has_uv2, has_uv3 = bool(tverts), bool(tverts1), bool(tverts2), bool(tverts3)
    has_tex_idx0 = bool(mesh.tex_indices0)
    has_normals = bool(mesh.normals)
    has_colors = bool(mesh.colors)
    corner_normals = mesh.corner_normals
    normals = mesh.normals
    colors = mesh.colors
    ti0, ti1, ti2, ti3 = mesh.tex_indices0, mesh.tex_indices1, mesh.tex_indices2, mesh.tex_indices3
    mirror_list = mesh.mirror_list
    constraints = dangly.constraints if dangly is not None else None
    nv, nt, nt1, nt2, nt3 = len(verts), len(tverts), len(tverts1), len(tverts2), len(tverts3)
    nn, nc, ncn = len(normals), len(colors), len(corner_normals)
    nsk = len(skin_keys) if skin_keys is not None else 0
    ncons = len(constraints) if constraints is not None else 0

    exp = ExpandedMesh()
    if has_uv:
        exp.tex_stages = 1
    if has_uv1:
        exp.tex_stages = 2
    if has_uv2:
        exp.tex_stages = 3
    if has_uv3:
        exp.tex_stages = 4

    nan = (_has_nan(verts) or _has_nan(tverts) or _has_nan(tverts1) or _has_nan(tverts2)
           or _has_nan(tverts3) or _has_nan(normals) or _has_nan(colors)
           or any(_has_nan(c) for c in corner_normals)
           or (constraints is not None and any(x != x for x in constraints)))
    kb = _bits if nan else (lambda v: v)

    positions, origv, origt = exp.positions, exp.orig_vert, exp.orig_tvert
    uvs, uvs1, uvs2, uvs3 = exp.uvs, exp.uvs1, exp.uvs2, exp.uvs3
    enor, ecol = exp.normals, exp.colors
    vkeys = []
    zero = (0.0, 0.0, 0.0)

    def add_vertex(c):
        if len(positions) >= 65535:
            raise CompileError("mesh exceeds 65535 unique GPU vertices (%d); split the mesh" % len(positions))
        key, vert_idx, tv_idx, pos, uv, uv1, uv2, uv3, nor, col = c
        idx = len(positions)
        vkeys.append(key)
        positions.append(pos)
        origv.append(vert_idx)
        origt.append(tv_idx)
        if has_uv:
            uvs.append(uv)
        if has_uv1:
            uvs1.append(uv1)
        if has_uv2:
            uvs2.append(uv2)
        if has_uv3:
            uvs3.append(uv3)
        if has_normals:
            enor.append(nor)
        if has_colors:
            ecol.append(col)
        return idx

    cache = {}
    all_faces = []
    pos_faces = {} if aabb_merge else None
    face_verts, face_src = exp.face_verts, exp.face_src
    n_mirror = len(mirror_list)
    mirror = 0
    for fi, face in enumerate(mesh.faces):
        if fi < n_mirror:
            mirror = mirror_list[fi] & 0xFF
        fverts = face.verts
        fuvs = face.uvs
        cs = []
        for vi in range(3):
            vert_idx = fverts[vi]
            uv_idx = fuvs[vi]
            pos = verts[vert_idx] if 0 <= vert_idx < nv else zero
            uv = uv1 = uv2 = uv3 = nor = col = zero
            tv_idx = -1
            if has_uv:
                ti = uv_idx
                if has_tex_idx0 and fi < len(ti0):
                    ti = ti0[fi][vi]
                if 0 <= ti < nt:
                    uv = tverts[ti]
                    tv_idx = ti
            if has_uv1:
                ti = ti1[fi][vi] if fi < len(ti1) else uv_idx
                if 0 <= ti < nt1:
                    uv1 = tverts1[ti]
            if has_uv2:
                ti = ti2[fi][vi] if fi < len(ti2) else uv_idx
                if 0 <= ti < nt2:
                    uv2 = tverts2[ti]
            if has_uv3:
                ti = ti3[fi][vi] if fi < len(ti3) else uv_idx
                if 0 <= ti < nt3:
                    uv3 = tverts3[ti]
            if has_normals:
                if fi < ncn:
                    nor = corner_normals[fi][vi]
                elif 0 <= vert_idx < nn:
                    nor = normals[vert_idx]
            if has_colors and 0 <= vert_idx < nc:
                col = colors[vert_idx]
            key = (kb(pos[0]), kb(pos[1]), kb(pos[2]), kb(uv[0]), kb(uv[1]), kb(uv1[0]), kb(uv1[1]),
                   kb(uv2[0]), kb(uv2[1]), kb(uv3[0]), kb(uv3[1]), kb(nor[0]), kb(nor[1]), kb(nor[2]),
                   kb(col[0]), kb(col[1]), kb(col[2]), mirror,
                   skin_keys[vert_idx] if 0 <= vert_idx < nsk else "",
                   kb(constraints[vert_idx]) if 0 <= vert_idx < ncons else 0.0,
                   anim_merge_key(mesh, anim, vert_idx, tv_idx) if anim is not None else "")
            cs.append((key, vert_idx, tv_idx, pos, uv, uv1, uv2, uv3, nor, col))

        if aabb_merge:
            cand = []
            pkeys = [cs[vi][0][:3] for vi in range(3)]
            for pk in pkeys:
                cand.extend(pos_faces.get(pk, ()))
            cand.sort()
            corners = None
            prev = -1
            for j in cand:
                if j == prev:
                    continue
                prev = j
                fj = all_faces[j]
                match = [-1, -1, -1]
                count = 0
                for k in range(3):
                    key = cs[k][0]
                    for c in range(3):
                        if vkeys[fj[c]] == key:
                            match[k] = c
                            count += 1
                            break
                if count > 1:
                    corners = [add_vertex(cs[k]) if match[k] < 0 else fj[match[k]] for k in range(3)]
                    break
            if corners is None:
                corners = [add_vertex(cs[0]), add_vertex(cs[1]), add_vertex(cs[2])]
            all_faces.append(corners)
            for pk in pkeys:
                lst = pos_faces.get(pk)
                if lst is None:
                    pos_faces[pk] = [fi]
                elif lst[-1] != fi:
                    lst.append(fi)
        else:
            corners = []
            for c in cs:
                g = cache.get(c[0])
                if g is None:
                    g = cache[c[0]] = add_vertex(c)
                corners.append(g)
        if strip_degenerate and (corners[0] == corners[1] or corners[1] == corners[2] or corners[0] == corners[2]):
            continue
        face_verts.append(list(corners))
        face_src.append(fi)
    return exp


def compute_bounds(verts):
    """computeBounds: (bmin, bmax, center, radius); the centre is the vertex mean."""
    if not verts:
        z = (0.0, 0.0, 0.0)
        return z, z, z, 0.0
    x0 = y0 = z0 = _MAX_F32
    x1 = y1 = z1 = -_MAX_F32
    sx = sy = sz = 0.0
    for x, y, z in verts:
        if x < x0:
            x0 = x
        if y < y0:
            y0 = y
        if z < z0:
            z0 = z
        if x > x1:
            x1 = x
        if y > y1:
            y1 = y
        if z > z1:
            z1 = z
        sx += x
        sy += y
        sz += z
    n = float(len(verts))
    cx, cy, cz = f32(sx / n), f32(sy / n), f32(sz / n)
    radius = 0.0
    for x, y, z in verts:
        dx, dy, dz = f32(x - cx), f32(y - cy), f32(z - cz)
        r = f32(math.sqrt(f32(f32(f32(dx * dx) + f32(dy * dy)) + f32(dz * dz))))
        if r > radius:
            radius = r
    return (x0, y0, z0), (x1, y1, z1), (cx, cy, cz), radius


def build_face_adjacency(src, n):
    """buildFaceAdjacency: the game's BuildNeighborLists over the first n
    source faces (by position, as the game reads them after stripping)."""
    if n > len(src):
        n = len(src)
    by_edge = {}
    for i in range(n):
        v = src[i].verts
        for k in range(3):
            a, b = v[k], v[(k + 1) % 3]
            e = (a, b) if a <= b else (b, a)
            lst = by_edge.get(e)
            if lst is None:
                by_edge[e] = [i]
            elif lst[-1] != i:
                lst.append(i)
    out = []
    for i in range(n):
        v = src[i].verts
        row = [0xFFFF, 0xFFFF, 0xFFFF]
        for k in range(3):
            a, b = v[k], v[(k + 1) % 3]
            for j in by_edge[(a, b) if a <= b else (b, a)]:
                if j != i:
                    row[k] = j & 0xFFFF
                    break
        out.append(row)
    return out


def clamp_byte(f):
    v = f32(f32(f * 255.0) + 0.5)
    if not v >= 0:  # negative or NaN
        return 0
    if v >= 256:
        return 255
    return int(v)


class MeshMixin:
    """The compiler's mesh writers (compiler_mesh.go)."""

    def write_mesh_header_full(self, n):
        mesh = n.mesh
        if mesh is None:
            self.write_empty_mesh_header()
            return None
        self.write_mesh_header_inner(mesh, n)
        return self.last_expanded

    def write_mesh_header_inner(self, mesh, n):
        core, vol = self.core, self.vol
        generate_normals(mesh)
        try:
            exp = build_expanded_mesh(
                mesh,
                strip_degenerate=n is None or n.aabb is None,
                skin_keys=skin_merge_keys(n.skin) if n is not None else None,
                dangly=n.dangly if n is not None else None,
                aabb_merge=n is not None and n.aabb is not None,
                anim=self.merge_anim)
        except CompileError as e:
            self.err = e
            self.write_empty_mesh_header()
            return
        self.last_expanded = exp

        tangents, handedness = resolve_tangents(
            mesh, exp, is_normal_mapped(mesh, self.resource_dirs), n is not None and n.anim_mesh is not None)

        core.zeros(8)
        faces_ptr_pos = core.placeholder()
        core.u32(len(exp.face_verts))
        core.u32(len(exp.face_verts))
        bmin, bmax, center, radius = compute_bounds(exp.positions)
        core.vec3(bmin)
        core.vec3(bmax)
        core.f32(radius)
        core.vec3(center)
        core.vec3(mesh.diffuse)
        core.vec3(mesh.ambient)
        core.vec3(mesh.specular)
        core.f32(mesh.shininess)
        core.i32(mesh.shadow)
        core.i32(mesh.beaming)
        core.i32(mesh.render)
        core.i32(mesh.transparency_hint)
        core.u32(render_hint_value(mesh.render_hint))
        core.fixed_str(mesh.bitmap, 64)
        core.fixed_str(mesh.texture1, 64)
        core.fixed_str(mesh.texture2, 64)
        core.fixed_str(mesh.material_name, 64)
        core.i32(mesh.tile_fade)
        core.proxy_list_empty()
        core.proxy_list_empty()
        if exp.face_verts:
            exp.index_count_list_ptr_pos = core.placeholder()
            core.u32(1)
            core.u32(1)
            exp.index_offset_list_ptr_pos = core.placeholder()
            core.u32(1)
            core.u32(1)
        else:
            exp.index_count_list_ptr_pos = -1
            exp.index_offset_list_ptr_pos = -1
            core.proxy_list_empty()
            core.proxy_list_empty()
        core.u32(0xFFFFFFFF)
        core.u32(0)
        core.u32(3)  # mesh_type: triangle list
        core.i32(0)
        mdx_vert_ptr_pos = core.len()
        core.i32(0)
        core.u16(len(exp.positions))
        core.u16(exp.tex_stages)

        tex_ptr = []
        for stage in range(4):
            if exp.tex_stages > stage:
                tex_ptr.append(core.len())
                core.i32(0)
            else:
                tex_ptr.append(-1)
                core.i32(-1)
        normal_ptr_pos = -1
        if exp.normals:
            normal_ptr_pos = core.len()
            core.i32(0)
        else:
            core.i32(-1)
        color_ptr_pos = -1
        if exp.colors:
            color_ptr_pos = core.len()
            core.i32(0)
        else:
            core.i32(-1)
        core.i32(-1)
        core.i32(-1)
        core.i32(-1)
        tangent_ptr_pos = -1
        if tangents:
            tangent_ptr_pos = core.len()
            core.i32(0)
        else:
            core.i32(-1)
        core.i32(-1)
        hand_ptr_pos = -1
        if tangents:
            hand_ptr_pos = core.len()
            core.i32(0)
        else:
            core.i32(-1)
        core.u8(mesh.light_mapped & 0xFF)
        core.u8(mesh.rotate_texture & 0xFF)
        core.zeros(2)
        core.f32(0.0)
        core.u32(0)
        exp.faces_ptr_pos = faces_ptr_pos

        # MDX: positions, UVs, normals, colours, index buffer, tangents, handedness.
        core.patch_u32(mdx_vert_ptr_pos, vol.len())
        vol.vec3s(exp.positions)
        for uvs, ptr in zip((exp.uvs, exp.uvs1, exp.uvs2, exp.uvs3), tex_ptr):
            if uvs and ptr >= 0:
                core.patch_u32(ptr, vol.len())
                vol.vec2s(uvs)
        if exp.normals and normal_ptr_pos >= 0:
            core.patch_u32(normal_ptr_pos, vol.len())
            vol.vec3s(exp.normals)
        if exp.colors and color_ptr_pos >= 0:
            core.patch_u32(color_ptr_pos, vol.len())
            vol.raw(bytes(b for c in exp.colors for b in (clamp_byte(c[0]), clamp_byte(c[1]), clamp_byte(c[2]), 0xFF)))
        exp.index_buf_off = -1
        if exp.index_count_list_ptr_pos >= 0:
            exp.index_buf_off = vol.len()
            vol.u16s([i for fv in exp.face_verts for i in fv])
        if tangents and tangent_ptr_pos >= 0:
            core.patch_u32(tangent_ptr_pos, vol.len())
            vol.vec3s(tangents)
        if handedness and hand_ptr_pos >= 0:
            core.patch_u32(hand_ptr_pos, vol.len())
            vol.f32s(handedness)

    def write_mesh_face_data(self, mesh, exp):
        if mesh is None or exp is None:
            return
        core = self.core
        core.patch_u32(exp.faces_ptr_pos, core.len())
        adjacency = build_face_adjacency(mesh.faces, len(exp.face_verts))
        verts = mesh.verts
        nv = len(verts)
        out = bytearray()
        pack = _FACE.pack
        for k, fi in enumerate(exp.face_src):
            face = mesh.faces[fi]
            nx, ny, nz = face.normal
            dist = 0.0
            a, b, c = face.verts[0], face.verts[1], face.verts[2]
            if 0 <= a < nv and 0 <= b < nv and 0 <= c < nv:
                p1 = verts[a]
                cx, cy, cz = face_cross(p1, verts[b], verts[c])
                length = f32(math.sqrt(f32(f32(f32(cx * cx) + f32(cy * cy)) + f32(cz * cz))))
                if length > 1e-10:
                    nx, ny, nz = f32(cx / length), f32(cy / length), f32(cz / length)
                dist = -f32(f32(f32(nx * p1[0]) + f32(ny * p1[1])) + f32(nz * p1[2]))
            adj = adjacency[k]
            fv = exp.face_verts[k]
            out += pack(nx, ny, nz, dist, face.material & 0xFFFFFFFF, adj[0], adj[1], adj[2], fv[0], fv[1], fv[2])
        core.raw(out)
        if exp.index_count_list_ptr_pos >= 0 and exp.index_buf_off >= 0:
            at = core.len()
            core.u32(len(exp.face_verts) * 3)
            core.patch_u32(exp.index_count_list_ptr_pos, at)
            at = core.len()
            core.u32(exp.index_buf_off)
            core.patch_u32(exp.index_offset_list_ptr_pos, at)

