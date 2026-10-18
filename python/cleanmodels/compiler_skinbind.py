"""Skin bind-pose data: a port of pkg/mdl/compiler_skinbind.go.

One entry per node of the model in tree order (or per skeleton node for a body
part): boneindexarray, the inverse bind rotation and translation of the skin in
that node's space, and zeroed boneconstantindices. float64 throughout.
"""

import math

from .vec import axis_angle_to_quat


class SkinBindPatch:
    """Where a skin header's bind-pose lists are patched in."""

    def __init__(self):
        self.bone_index_ptr = 0
        self.bone_index_size = 0
        self.lists = [[0, 0, 0], [0, 0, 0], [0, 0, 0]]  # qbone_ref_inv, tbone_ref_inv, boneconstantindices
        self.bone_tree = []                              # bone index -> tree position


def quat_mul(a, b):
    return (a[3] * b[0] + a[0] * b[3] + a[1] * b[2] - a[2] * b[1],
            a[3] * b[1] - a[0] * b[2] + a[1] * b[3] + a[2] * b[0],
            a[3] * b[2] + a[0] * b[1] - a[1] * b[0] + a[2] * b[3],
            a[3] * b[3] - a[0] * b[0] - a[1] * b[1] - a[2] * b[2])


def quat_conj(q):
    return (-q[0], -q[1], -q[2], q[3])


def quat_rotate(q, v):
    r = quat_mul(quat_mul(q, (v[0], v[1], v[2], 0.0)), quat_conj(q))
    return (r[0], r[1], r[2])


def quat_normalize(q):
    l = math.sqrt(q[0] * q[0] + q[1] * q[1] + q[2] * q[2] + q[3] * q[3])
    if l == 0:
        return (0.0, 0.0, 0.0, 1.0)
    return (q[0] / l, q[1] / l, q[2] / l, q[3] / l)


def world_transforms(root, children_of):
    """computeWorldTransforms: ({node: pos}, {node: rot}) accumulated down the tree."""
    wpos, wrot = {}, {}
    if root is None:
        return wpos, wrot
    stack = [(root, None)]
    while stack:
        n, parent = stack.pop()
        if n in wrot:
            continue
        local = axis_angle_to_quat(n.orientation)
        lp = (n.position[0], n.position[1], n.position[2])
        if parent is None:
            wpos[n], wrot[n] = lp, quat_normalize(local)
        else:
            pp, pq = wpos[parent], wrot[parent]
            r = quat_rotate(pq, lp)
            wpos[n] = (pp[0] + r[0], pp[1] + r[1], pp[2] + r[2])
            wrot[n] = quat_normalize(quat_mul(pq, local))
        for child in children_of.get(n, ()):
            stack.append((child, n))
    return wpos, wrot


class SkinBindMixin:
    """writeSkinBindData and its transforms (compiler_skinbind.go)."""

    def compute_world_transforms(self):
        self.world_pos, self.world_rot = world_transforms(self.model.root_node(), self.children_by_node)

    def write_skin_bind_data(self, n):
        if self.world_rot is None:
            self.compute_world_transforms()
        core = self.core
        skin_pos, skin_rot = self.world_pos.get(n), self.world_rot.get(n)
        if skin_pos is None:
            skin_pos, skin_rot = (0.0, 0.0, 0.0), (0.0, 0.0, 0.0, 0.0)
        sk = self.part_skel
        if sk is not None:
            bone_pos, bone_rot = sk.pos, sk.rot
            ap, aq = sk.pos[self.part_attach], sk.rot[self.part_attach]
            r = quat_rotate(aq, skin_pos)
            skin_pos = (ap[0] + r[0], ap[1] + r[1], ap[2] + r[2])
            skin_rot = quat_normalize(quat_mul(aq, skin_rot))
        else:
            k = len(self.tree_index)
            bone_pos = [(0.0, 0.0, 0.0)] * k
            bone_rot = [(0.0, 0.0, 0.0, 0.0)] * k
            for node, idx in self.tree_index.items():
                if idx < k:
                    bone_pos[idx] = self.world_pos.get(node, (0.0, 0.0, 0.0))
                    bone_rot[idx] = self.world_rot.get(node, (0.0, 0.0, 0.0, 0.0))
        count = len(bone_pos)
        if count == 0:
            return

        bone_of = [-1] * count
        sb = self.skin_bind
        for bone, tree in enumerate(sb.bone_tree):
            if 0 <= tree < count:
                bone_of[tree] = bone
        core.patch_u32(sb.bone_index_ptr, core.len())
        core.patch_u32(sb.bone_index_size, count)
        core.u16s(bone_of)
        if count % 2 == 1:
            core.u16(0)

        def start_list(i):
            core.patch_u32(sb.lists[i][0], core.len())
            core.patch_u32(sb.lists[i][1], count)
            core.patch_u32(sb.lists[i][2], count)

        start_list(0)
        out = []
        for br in bone_rot:
            q = quat_mul(quat_conj(br), skin_rot)
            out += (q[3], q[0], q[1], q[2])
        core.f32s(out)
        start_list(1)
        out = []
        for i, p in enumerate(bone_pos):
            t = quat_rotate(quat_conj(bone_rot[i]), (skin_pos[0] - p[0], skin_pos[1] - p[1], skin_pos[2] - p[2]))
            out += t
        core.f32s(out)
        start_list(2)
        core.zeros(4 * count)
