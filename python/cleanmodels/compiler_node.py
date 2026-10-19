"""Geometry node writing: a port of pkg/mdl/compiler_node.go.

Every write mirrors the read order of binary.go readNode and its sub-readers.
"""

from .f32 import f32
from .gomath import go_lower, slice_stable

_MAX_F32 = 3.4028234663852886e38


def bound_influences(bones, weights, slots):
    """boundInfluences: the influences whose bone has a bone slot, in order."""
    ob, ow = [], []
    for i, b in enumerate(bones):
        if b == "" or go_lower(b) not in slots:
            continue
        ob.append(b)
        ow.append(weights[i] if i < len(weights) else 0.0)
    return ob, ow


def pick_top_bone_influences(bones, weights, max_bones):
    """pickTopBoneInfluences: at most max_bones influences, heaviest first,
    renormalised to sum to 1; (bones, weights) unchanged when within the cap."""
    if max_bones <= 0:
        return bones, weights
    n = min(len(bones), len(weights))
    if n == 0 or n <= max_bones:
        return bones, weights
    pairs = [(bones[i], weights[i]) for i in range(n)]
    slice_stable(pairs, lambda a, b: a[1] > b[1])
    pairs = pairs[:max_bones]
    total = 0.0
    for _, w in pairs:
        total += w
    if total > 0:
        scale = f32(1.0 / total)
        return [p[0] for p in pairs], [f32(p[1] * scale) for p in pairs]
    return [p[0] for p in pairs], [p[1] for p in pairs]


def _bool_bit(v, bit):
    return 1 << bit if v != 0 else 0


class _AabbTreeEntry:
    __slots__ = ("bmin", "bmax", "leaf_face", "plane", "left", "right")

    def __init__(self, bmin, bmax, leaf_face):
        self.bmin = bmin
        self.bmax = bmax
        self.leaf_face = leaf_face
        self.plane = 0
        self.left = -1
        self.right = -1


def authored_aabb_tree(entries, face_count):
    """authoredAABBTree: the preorder entries linked into a tree, or None when
    they are not one complete tree over face_count faces."""
    if not entries:
        return None
    tree = []
    stack = []   # internal nodes still waiting for a child
    for i, e in enumerate(entries):
        if i > 0 and not stack:
            return None  # the root's tree ended before the entries did
        t = _AabbTreeEntry(e.bound_min, e.bound_max, e.leaf_face)
        tree.append(t)
        if stack:
            parent = tree[stack[-1]]
            if parent.left < 0:
                parent.left = i
            else:
                parent.right = i
                stack.pop()
        if e.leaf_face >= 0:
            if e.leaf_face >= face_count:
                return None
        else:
            stack.append(i)
    if stack:
        return None
    for i, e in enumerate(entries):
        t = tree[i]
        if e.leaf_face < 0:
            t.plane = e.plane
            if t.plane == 0:
                t.plane = aabb_split_plane(tree[t.left], tree[t.right])
    return tree


def aabb_split_plane(l, r):
    """The engine's plane for an internal node: the axis on which the
    children's box centres are furthest apart, signed by the right child."""
    dx = f32(f32(r.bmin[0] + r.bmax[0]) - f32(l.bmin[0] + l.bmax[0]))
    dy = f32(f32(r.bmin[1] + r.bmax[1]) - f32(l.bmin[1] + l.bmax[1]))
    dz = f32(f32(r.bmin[2] + r.bmax[2]) - f32(l.bmin[2] + l.bmax[2]))
    ax, ay, az = (-dx if dx < 0 else dx), (-dy if dy < 0 else dy), (-dz if dz < 0 else dz)
    if ax > ay and ax > az:
        return 1 if dx > 0 else 8
    if ay > az:
        return 2 if dy > 0 else 16
    return 4 if dz > 0 else 32


def build_aabb_tree(mesh):
    """buildAABBTree: a median-split tree over the mesh's faces."""
    if mesh is None or not mesh.faces:
        return None
    out = []
    _build_aabb_node(mesh, list(range(len(mesh.faces))), out)
    return out


def _faces_aabb(mesh, face_idxs):
    x0 = y0 = z0 = _MAX_F32
    x1 = y1 = z1 = -_MAX_F32
    verts = mesh.verts
    nv = len(verts)
    for fi in face_idxs:
        for vi in mesh.faces[fi].verts:
            if vi < 0 or vi >= nv:
                continue
            x, y, z = verts[vi]
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
    return (x0, y0, z0), (x1, y1, z1)


def _longest_axis(bmin, bmax):
    dx = f32(bmax[0] - bmin[0])
    dy = f32(bmax[1] - bmin[1])
    dz = f32(bmax[2] - bmin[2])
    if dx >= dy and dx >= dz:
        return 0
    if dy >= dz:
        return 1
    return 2


def _face_centroid_axis(mesh, fi, axis):
    s = 0.0
    count = 0
    verts = mesh.verts
    for vi in mesh.faces[fi].verts:
        if 0 <= vi < len(verts):
            s = f32(s + verts[vi][axis])
            count += 1
    if count == 0:
        return 0.0
    return f32(s / count)


def _sort_faces_by_axis(mesh, faces, axis):
    """sortFacesByAxis: a stable insertion sort on the face centroids."""
    keys = {fi: _face_centroid_axis(mesh, fi, axis) for fi in faces}
    if all(k == k for k in keys.values()):
        faces.sort(key=keys.__getitem__)
        return
    for i in range(1, len(faces)):
        key = faces[i]
        kc = keys[key]
        j = i - 1
        while j >= 0 and keys[faces[j]] > kc:
            faces[j + 1] = faces[j]
            j -= 1
        faces[j + 1] = key


def _build_aabb_node(mesh, faces, out):
    bmin, bmax = _faces_aabb(mesh, faces)
    idx = len(out)
    e = _AabbTreeEntry(bmin, bmax, -1)
    out.append(e)
    if len(faces) == 1:
        e.leaf_face = faces[0]
        e.plane = 1 << _longest_axis(bmin, bmax)
        return idx
    axis = _longest_axis(bmin, bmax)
    e.plane = 1 << axis
    mid = len(faces) // 2
    _sort_faces_by_axis(mesh, faces, axis)
    e.left = _build_aabb_node(mesh, faces[:mid], out)
    e.right = _build_aabb_node(mesh, faces[mid:], out)
    return idx


class NodeMixin:
    """The compiler's geometry node writers (compiler_node.go)."""

    def write_node(self, root):
        """writeNode, iteratively: the same pre-order as Go's recursion."""
        count = 0
        root_off = 0
        stack = [(root, 0, -1)]   # node, parent offset, where to patch its offset
        core = self.core
        while stack:
            n, parent_off, patch_at = stack.pop()
            off = self.node_offsets.get(n)
            if off is None:
                off = core.len()
                self.node_offsets[n] = off
                count += 1
                children = self._write_one_node(n, parent_off)
                first = core.len() - 4 * len(children)
                for i in range(len(children) - 1, -1, -1):
                    stack.append((children[i], off, first + 4 * i))
            if patch_at >= 0:
                core.patch_u32(patch_at, off)
            else:
                root_off = off
        return root_off, count

    def _write_one_node(self, n, parent_off):
        """One node up to and including its child pointer array; returns its children."""
        core = self.core
        core.zeros(24)
        core.i32(n.inherit_color)
        core.i32(self.node_ids.get(n, 0))
        core.fixed_str(n.name, 32)
        core.i32(0)
        core.i32(parent_off)
        child_ptr_pos = core.placeholder()
        child_num_pos = core.placeholder()
        child_alc_pos = core.placeholder()
        ctrl = [core.placeholder() for _ in range(6)]

        content = n.node_type_flag()
        core.u32(content)
        has_light = content & 0x02 != 0
        has_emitter = content & 0x04 != 0
        has_reference = content & 0x10 != 0
        has_mesh = content & 0x20 != 0
        has_skin = content & 0x40 != 0
        has_anim_mesh = content & 0x80 != 0
        has_dangly = content & 0x100 != 0
        has_aabb = content & 0x200 != 0

        if has_light:
            self.write_light_header(n)
        if has_emitter:
            self.write_emitter_header(n)
        if has_reference:
            self.write_reference_header(n)
        expanded = None
        av_pos = atv_pos = 0
        n_vsets = n_tvsets = 0
        if has_mesh:
            expanded = self.write_mesh_header_full(n)
        if has_skin:
            self.write_skin_header(n, expanded)
        if has_anim_mesh:
            av_pos, atv_pos, n_vsets, n_tvsets = self.write_anim_mesh_header(n, None)
        if has_dangly:
            self.write_dangly_header(n)
        if has_aabb:
            self.write_aabb_header(n)

        if has_mesh:
            self.write_mesh_face_data(n.mesh, expanded)
        if has_skin:
            self.write_skin_bind_data(n)
        if has_dangly:
            self.write_dangly_constraints(n)
        if has_anim_mesh and n.anim_mesh is not None:
            if n_vsets > 0:
                core.patch_u32(av_pos, core.len())
                core.f32s([c for v in n.anim_mesh.anim_verts for c in (v[0], v[1], v[2])])
            if n_tvsets > 0:
                core.patch_u32(atv_pos, core.len())
                core.f32s([c for v in n.anim_mesh.anim_tverts for c in (v[0], v[1])])

        keys, data = self.encode_geom_node_controllers(n)
        self.write_ctrl_block(keys, data, *ctrl)

        children = self.children_of(n)
        if children:
            core.patch_u32(child_ptr_pos, core.len())
        core.patch_u32(child_num_pos, len(children))
        core.patch_u32(child_alc_pos, len(children))
        core.zeros(4 * len(children))
        return children

    def write_light_header(self, n):
        core = self.core
        l = n.light
        if l is None:
            core.zeros(92)
            return
        core.f32(l.flare_radius)
        core.proxy_list_empty()
        lists = (l.flare_sizes, l.flare_positions, l.flare_color_shifts, l.texture_names)
        ptrs = []
        for lst in lists:
            ptrs.append(core.placeholder())
            core.u32(len(lst))
            core.u32(len(lst))
        for v in (l.light_priority, l.ambient_only, l.n_dynamic_type, l.affect_dynamic, l.shadow,
                  l.generate_flare, l.fading_light):
            core.i32(v)
        if l.flare_sizes:
            core.patch_u32(ptrs[0], core.len())
            core.f32s(l.flare_sizes)
        if l.flare_positions:
            core.patch_u32(ptrs[1], core.len())
            core.f32s(l.flare_positions)
        if l.flare_color_shifts:
            core.patch_u32(ptrs[2], core.len())
            core.vec3s(l.flare_color_shifts)
        if l.texture_names:
            core.patch_u32(ptrs[3], core.len())
            name_ptrs = [core.placeholder() for _ in l.texture_names]
            for p, name in zip(name_ptrs, l.texture_names):
                core.patch_u32(p, core.len())
                core.fixed_str(name, 64)

    def write_emitter_header(self, n):
        core = self.core
        em = n.emitter
        if em is None:
            core.zeros(216)
            return
        core.f32(em.dead_space)
        core.f32(em.blast_radius)
        core.f32(em.blast_length)
        core.u32(em.x_grid)
        core.u32(em.y_grid)
        core.u32(em.spawn_type)
        core.fixed_str(em.update, 32)
        core.fixed_str(em.render, 32)
        core.fixed_str(em.blend, 32)
        core.fixed_str(em.texture, 64)
        core.fixed_str(em.chunk_name, 16)
        core.u32(em.two_sided_tex)
        core.u32(em.loop)
        core.u16(em.render_order)
        core.zeros(2)
        flags = 0
        for bit, v in enumerate((em.p2p, em.p2p_sel, em.affected_by_wind, em.is_tinted, em.bounce, em.random,
                                 em.inherit, em.inherit_vel, em.inherit_local, em.splat, em.inherit_part)):
            flags |= _bool_bit(v, bit)
        core.u32(flags)

    def write_reference_header(self, n):
        ref = n.reference
        if ref is None:
            self.core.zeros(68)
            return
        self.core.fixed_str(ref.ref_model, 64)
        self.core.i32(ref.reattachable)

    def write_skin_header(self, n, exp):
        core, vol = self.core, self.vol
        skin, mesh = n.skin, n.mesh
        if skin is None or mesh is None:
            core.zeros(192)
            return

        part_skel = self.part_skel

        def tree_pos(name):
            if part_skel is not None:
                p = part_skel.index.get(go_lower(name))
                if p is None:
                    return 0, False
                return _i16(p), True
            bone = self.geom_node_by_name(name)
            if bone is not None:
                i = self.tree_index.get(bone)
                if i is not None:
                    return _i16(i), True
            return -1, False

        bone_index = {}
        bone_names = []
        for vw in skin.weights:
            for b in vw.bones:
                if b == "" or not tree_pos(b)[1]:
                    continue
                key = go_lower(b)
                if key in bone_index or len(bone_names) >= 64:
                    continue
                bone_index[key] = len(bone_names)
                bone_names.append(b)

        part_nums = [-1] * 64
        for i, name in enumerate(bone_names):
            part_nums[i] = tree_pos(name)[0]

        n_gpu = len(exp.positions) if exp is not None else len(mesh.faces) * 3
        wgt_off = vol.len()
        bref_off = vol.len() + n_gpu * 16

        core.proxy_list_empty()
        core.i32(wgt_off)
        core.i32(bref_off)
        sb = self.skin_bind
        sb.bone_index_ptr = core.placeholder()
        sb.bone_index_size = core.placeholder()
        sb.lists = [[core.placeholder(), core.placeholder(), core.placeholder()] for _ in range(3)]
        sb.bone_tree = part_nums[:len(bone_names)]
        core.u16s(part_nums)

        weights = skin.weights
        nw = len(weights)
        cache = {}
        wgts = []
        brefs = []
        orig = exp.orig_vert if exp is not None else None
        faces = mesh.faces
        for g in range(n_gpu):
            oi = -1
            if orig is not None and g < len(orig):
                oi = orig[g]
            elif g // 3 < len(faces):
                oi = faces[g // 3].verts[g % 3]
            row = cache.get(oi)
            if row is None:
                w4 = [0.0, 0.0, 0.0, 0.0]
                b4 = [0, 0, 0, 0]
                if 0 <= oi < nw:
                    vw = weights[oi]
                    bones, ws = pick_top_bone_influences(*bound_influences(vw.bones, vw.weights, bone_index), 4)
                    for bi in range(min(4, len(bones))):
                        w4[bi] = ws[bi] if bi < len(ws) else 0.0
                        idx = -1
                        if bones[bi] != "":
                            idx = bone_index.get(go_lower(bones[bi]), -1)
                        b4[bi] = idx
                row = cache[oi] = (w4, b4)
            wgts.extend(row[0])
            brefs.extend(row[1])
        vol.f32s(wgts)
        vol.u16s(brefs)

    def expanded_constraints(self, dangly):
        if dangly is None or not dangly.constraints:
            return None
        exp = self.last_expanded
        cons = dangly.constraints
        if exp is None or len(exp.orig_vert) == len(cons):
            return cons
        nc = len(cons)
        return [cons[oi] if 0 <= oi < nc else 0.0 for oi in exp.orig_vert]

    def write_dangly_header(self, n):
        core = self.core
        dangly = n.dangly
        if dangly is None:
            core.zeros(24)
            return
        cons = self.expanded_constraints(dangly) or ()
        self.dangly_constraints_ptr_pos = core.placeholder()
        core.u32(len(cons))
        core.u32(len(cons))
        core.f32(dangly.displacement)
        core.f32(dangly.tightness)
        core.f32(dangly.period)

    def write_dangly_constraints(self, n):
        dangly = n.dangly
        if dangly is None or not dangly.constraints:
            return
        cons = self.expanded_constraints(dangly)
        self.core.patch_u32(self.dangly_constraints_ptr_pos, self.core.len())
        self.core.f32s(cons)

    def write_aabb_header(self, n):
        core = self.core
        aabb = n.aabb
        if aabb is None or n.mesh is None or not n.mesh.faces:
            core.zeros(4)
            return 0
        root_ptr_pos = core.placeholder()
        tree = authored_aabb_tree(aabb.entries, len(n.mesh.faces))
        if tree is None:
            tree = build_aabb_tree(n.mesh)
        if not tree:
            return 0
        base = core.len()
        nt = len(tree)
        core.patch_u32(root_ptr_pos, base)
        for e in tree:
            core.vec3(e.bmin)
            core.vec3(e.bmax)
            core.u32(base + 40 * e.left if 0 <= e.left < nt else 0)
            core.u32(base + 40 * e.right if 0 <= e.right < nt else 0)
            core.i32(e.leaf_face)
            core.u32(e.plane)
        return base


def _i16(v):
    v &= 0xFFFF
    return v - 0x10000 if v >= 0x8000 else v
