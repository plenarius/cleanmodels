"""Binary MDL decompiler: a port of pkg/mdl/binary.go.

Reads NWN binary (compiled) .mdl files into a Model. Pointer conventions:
  goToPointer(p)    = 12 + p                (core block)
  goToMDXPointer(p) = 12 + mdx_offset + p   (per-vertex streams)
The decompiler is non-fatal: truncated data, out-of-bounds pointers and
unknown controllers become warnings on the model.

Read semantics follow Go's bytes.Reader and encoding/binary: a read past the
end returns 0 and consumes what remained; skipping past the end is allowed.
"""

import math
import struct

from . import controllers
from .controllers import controller_id
from .compiler_tangents import hint_wants_tangents
from .f32 import f32
from .gostr import equal_fold, go_lower, quote
from .types import (
    AabbData, AabbEntry, AnimMeshData, AnimNode, Animation, DanglyData, DecompileWarning,
    EmitterData, Face, LightData, MeshData, Model, Node, ReferenceData, SkinData, VertexWeight,
    WARN_GENERAL, WARN_POINTER_OUT_OF_BOUNDS, WARN_TRUNCATED_DATA, WARN_UNKNOWN_CONTROLLER,
    ANIM_FLOAT_CONTROLLERS, ANIM_COLOR_CONTROLLERS, VEC3_ZERO, classification_from_code,
)

MESH_HEADER_SIZE = 512
MESH_HEADER_P_MDX_VERTEX = 444
MAX_READ_NODE_DEPTH = 256

_U32 = struct.Struct("<I")
_I32 = struct.Struct("<i")
_U16 = struct.Struct("<H")
_I16 = struct.Struct("<h")
_F32 = struct.Struct("<f")
_ARRAY_DEF = struct.Struct("<III")

_F32_1E6 = f32(1e-6)
_F32_255 = 255.0

_ANIM_FLOAT = frozenset(ANIM_FLOAT_CONTROLLERS) - {"detonate"}
_ANIM_COLOR = frozenset(ANIM_COLOR_CONTROLLERS)
_EMITTER_FLOATS = None  # filled lazily from types


class DecompileError(Exception):
    pass


class _ReadError(Exception):
    pass


def is_binary_mdl(data):
    """A binary MDL starts with a zero uint32 (the file header's first field)."""
    return len(data) >= 12 and _U32.unpack_from(data, 0)[0] == 0


def decompile_file(path, skeleton_dirs=()):
    with open(path, "rb") as f:
        data = f.read()
    return decompile(data, skeleton_dirs)


def decompile(data, skeleton_dirs=()):
    """Decompile binary MDL bytes into a Model (binary.go DecompileWithOptions)."""
    d = _Decompiler(data, skeleton_dirs)
    try:
        d.read_file_header()
    except _ReadError as e:
        raise DecompileError("file header: %s" % e)
    try:
        d.read_model_header()
    except _ReadError as e:
        raise DecompileError("model header: %s" % e)
    if d.root_node_ptr != 0:
        err = d.read_node(d.root_node_ptr, "NULL", 0)
        if err:
            d.warn(WARN_TRUNCATED_DATA, 0, "", "geometry read error: %s" % err)
    seen_anims = set()
    for ptr in d.anim_ptrs:
        if ptr == 0 or ptr in seen_anims:
            continue
        seen_anims.add(ptr)
        d.visited_ptrs = set()
        err = d.read_animation(ptr)
        if err:
            d.warn(WARN_TRUNCATED_DATA, ptr, "", "animation read error: %s" % err)
    for ps in d.pending_skins:
        d.resolve_skin_weights(ps)
    unmerge_render_lists(d)
    return d.model


class _PendingSkin:
    __slots__ = ("node", "bone_part_nums", "bind_count", "map_pos", "skin_weights", "skin_bone_refs")

    def __init__(self, node, bone_part_nums, bind_count, map_pos, skin_weights, skin_bone_refs):
        self.node = node
        self.bone_part_nums = bone_part_nums
        self.bind_count = bind_count
        self.map_pos = map_pos  # bone index -> first tree position in the boneindexarray
        self.skin_weights = skin_weights
        self.skin_bone_refs = skin_bone_refs

    def bone_pos(self, ref):
        """pendingSkin.bonePos: (tree position, taken from the boneindexarray)."""
        p = self.bone_part_nums[ref]
        if (p < 0 or p >= self.bind_count) and self.map_pos is not None and ref in self.map_pos:
            return self.map_pos[ref], True
        return p, False


class _Decompiler:
    def __init__(self, data, skeleton_dirs):
        self.data = data
        self.size = len(data)
        self.p = 0
        self.model = Model()
        self.model.animation_scale = 1.0
        self.model.file_type = "binary"
        self.compiler = controllers.COMPILER_UNKNOWN
        self.mdx_offset = 0
        self.mdx_size = 0
        self.root_node_ptr = 0
        self.anim_ptrs = []
        self.node_order = []   # (name, part number)
        self.pending_skins = []
        self.visited_ptrs = set()
        self.total_alloc = 0
        self.max_alloc = self.size * 16
        self.read_err = False
        self.skeleton_dirs = list(skeleton_dirs)
        # readPlace (anim, node, list) being read, for warn_nan; one warning per place
        self.where = ("", "", "")
        self.nan_warns = {}
        self.render_lists = {}  # node -> render index list (binary_merged.go)

    # --- low-level reads (bytes.Reader + encoding/binary semantics) ---

    def track_alloc(self, n):
        self.total_alloc += n
        return self.total_alloc <= self.max_alloc

    def warn(self, kind, offset, node, message):
        self.model.warnings.append(DecompileWarning(kind, offset, node, message))

    def note(self, kind, offset, node, message):
        self.model.notes.append(DecompileWarning(kind, offset, node, message))

    def at(self, lst):
        self.where = (self.where[0], self.where[1], lst)

    def warn_nan(self, offset, bits):
        """warnNaN: one warning per list of a node, with the count and the first's offset and bits."""
        w = self.where
        what = w[2] or "the model"
        if w[0]:
            what += " in animation %s" % quote(w[0])
        n = self.nan_warns.get(w)
        if n is not None:
            n[1] += 1
            self.model.warnings[n[0]].message = (
                "%d NaN/Inf floats in %s replaced with 0 (the first at this offset, raw bits: 0x%08X)" % (n[1], what, n[2]))
            return
        self.nan_warns[w] = [len(self.model.warnings), 1, bits]
        self.warn(WARN_GENERAL, offset, w[1], "NaN/Inf float in %s replaced with 0 (raw bits: 0x%08X)" % (what, bits))

    @staticmethod
    def go_to_pointer(p):
        return 12 + p

    def go_to_mdx_pointer(self, p):
        if p < 0:
            return -1
        return 12 + self.mdx_offset + p

    def seek(self, offset):
        """Returns True on success (Go: err == nil)."""
        if offset < 0 or offset > self.size:
            return False
        self.p = offset
        return True

    def _take(self, n):
        """Read n bytes; on a short read consume the rest and return None."""
        p = self.p
        if p + n <= self.size:
            self.p = p + n
            return p
        if p < self.size:
            self.p = self.size
        return None

    def _read(self, st, n, track=True):
        at = self._take(n)
        if at is None:
            if track:
                self.read_err = True
            return 0, False
        return st.unpack_from(self.data, at)[0], True

    def read_u32(self):
        return self._read(_U32, 4)[0]

    def read_u32_ok(self):
        return self._read(_U32, 4)

    def read_i32(self):
        return self._read(_I32, 4)[0]

    def read_u16(self):
        return self._read(_U16, 2)[0]

    def read_i16(self):
        return self._read(_I16, 2)[0]

    def read_byte(self):
        at = self._take(1)
        if at is None:
            self.read_err = True
            return 0, False
        return self.data[at], True

    def read_f32(self):
        at = self._take(4)
        if at is None:
            self.read_err = True
            return 0.0, False
        v = _F32.unpack_from(self.data, at)[0]
        if v != v or v in (math.inf, -math.inf):
            self.warn_nan(self.p - 4, _U32.unpack_from(self.data, at)[0])
            return 0.0, True
        return v, True

    def read_vec3(self):
        return (self.read_f32()[0], self.read_f32()[0], self.read_f32()[0])

    def skip(self, n):
        self.p += n

    def read_fixed_string(self, n):
        """Returns (string, ok). A short read returns ("", False)."""
        at = self._take(n)
        if at is None:
            return "", False
        buf = self.data[at:at + n]
        end = buf.find(b"\0")
        if end >= 0:
            buf = buf[:end]
        return bytes(c for c in buf if 0x20 <= c < 0x7F).decode("ascii").lower(), True

    def read_array_def(self):
        """Returns (ptr, count, alloc); zeros on a short read (no read_err)."""
        at = self._take(12)
        if at is None:
            return (0, 0, 0), False
        return _ARRAY_DEF.unpack_from(self.data, at), True

    def read_pointer_array(self, a):
        """Returns (ptrs, error message or None)."""
        ptr, count = a[0], a[1]
        if count == 0 or ptr == 0:
            return [], None
        pos = self.p
        abs_ptr = self.go_to_pointer(ptr)
        if abs_ptr + count * 4 > self.size:
            return [], "array pointer out of bounds"
        if not self.track_alloc(count * 4):
            return [], "pointer array allocation exceeds limit"
        if not self.seek(abs_ptr):
            return [], "seek out of bounds"
        ptrs = list(struct.unpack_from("<%dI" % count, self.data, abs_ptr))
        self.p = abs_ptr + count * 4
        self.seek(pos)
        return ptrs, None

    # --- headers ---

    def read_file_header(self):
        if not self.seek(0):
            raise _ReadError("seek")
        for _ in range(3):
            v, ok = self._read(_U32, 4, track=False)
            if not ok:
                raise _ReadError("unexpected EOF")
            if _ == 1:
                self.mdx_offset = v
            elif _ == 2:
                self.mdx_size = v

    def read_model_header(self):
        if not self.seek(12):
            raise _ReadError("seek out of bounds")
        self.where = ("", "", "the model header")
        self.skip(8)
        model_name, ok = self.read_fixed_string(64)
        if not ok:
            raise _ReadError("unexpected EOF")
        root_ptr, ok = self.read_u32_ok()
        if not ok:
            raise _ReadError("unexpected EOF")
        count_nodes = self.read_u32()
        if root_ptr == 0 and count_nodes == 0 and self.size > 256:
            recovered = self.try_recover_name(model_name)
            if recovered:
                self.model.name = recovered
                self.warn(WARN_TRUNCATED_DATA, 12, "",
                          "non-standard header layout, recovered name '%s'; geometry unavailable" % recovered)
                self.model.super_model = "NULL"
                self.model.animation_scale = 1.0
                self.root_node_ptr = 0
                return
        self.model.name = model_name
        self.root_node_ptr = root_ptr
        self.skip(12 + 12 + 4 + 4)
        self.skip(2)
        class_code, ok = self.read_byte()
        if not ok:
            raise _ReadError("unexpected EOF")
        self.model.classification = classification_from_code(class_code)
        fog, ok = self.read_byte()
        if not ok:
            raise _ReadError("unexpected EOF")
        if fog == 0:
            self.model.ignore_fog = 1
        self.skip(4)
        anim_def, ok = self.read_array_def()
        if not ok:
            raise _ReadError("unexpected EOF")
        anim_ptrs, err = self.read_pointer_array(anim_def)
        if err:
            self.warn(WARN_POINTER_OUT_OF_BOUNDS, anim_def[0], model_name,
                      "animation array out of bounds, skipping animations")
            anim_ptrs = []
        self.anim_ptrs = anim_ptrs
        self.skip(4 + 24 + 4)
        anim_scale, ok = self.read_f32()
        if not ok:
            raise _ReadError("unexpected EOF")
        self.model.animation_scale = anim_scale
        super_model, ok = self.read_fixed_string(64)
        if not ok:
            raise _ReadError("unexpected EOF")
        self.model.super_model = super_model or "NULL"

    def try_recover_name(self, standard_name):
        if len(standard_name) > 3 and "\0" not in standard_name:
            return ""
        if self.size > 80 and self.seek(16):
            name, ok = self.read_fixed_string(64)
            if ok and len(name) > 3 and all(32 <= ord(c) <= 126 for c in name):
                return name
        return ""

    # --- geometry nodes ---

    def read_node(self, node_ptr, parent_name, depth):
        """Returns an error message or None."""
        if node_ptr == 0:
            return None
        if depth > MAX_READ_NODE_DEPTH:
            self.warn(WARN_GENERAL, 0, parent_name, "node tree exceeds max depth %d; truncating" % MAX_READ_NODE_DEPTH)
            return None
        if node_ptr in self.visited_ptrs:
            self.warn(WARN_POINTER_OUT_OF_BOUNDS, node_ptr, "", "cycle detected at node pointer")
            return None
        self.visited_ptrs.add(node_ptr)
        abs_ptr = self.go_to_pointer(node_ptr)
        if abs_ptr >= self.size:
            return "node pointer %d out of bounds" % node_ptr
        if not self.seek(abs_ptr):
            return "seek out of bounds: %d (file size %d)" % (abs_ptr, self.size)

        self.skip(24)
        inherit_color = self.read_i32()
        node_number = self.read_i32()
        node_name, _ = self.read_fixed_string(32)
        self.where = ("", node_name, "the node header")
        self.skip(8)
        children_def, _ = self.read_array_def()
        ctrl_keys_def, _ = self.read_array_def()
        ctrl_data_def, _ = self.read_array_def()
        content = self.read_u32()

        node = Node(node_name, parent_name)
        node.part_number = node_number
        node.inherit_color = inherit_color
        self.node_order.append((node_name, node_number))

        if content & 0x02:
            node.light = LightData()
            self.read_light_header(node)
        if content & 0x04:
            node.emitter = EmitterData()
            self.read_emitter_header(node)
        if content & 0x08:
            node.camera = True
        if content & 0x10:
            node.reference = ReferenceData()
            self.read_reference_header(node)
        if content & 0x20:
            node.mesh = MeshData()
            self.read_mesh_header(node)
        if content & 0x40:
            if node.mesh is None:
                node.mesh = MeshData()
            node.skin = SkinData()
            self.read_skin_header(node)
        if content & 0x80:
            if node.mesh is None:
                node.mesh = MeshData()
            node.anim_mesh = AnimMeshData()
            self.read_anim_mesh_header(node)
        if content & 0x100:
            if node.mesh is None:
                node.mesh = MeshData()
            node.dangly = DanglyData()
            self.read_dangly_header(node)
        if content & 0x200:
            if node.mesh is None:
                node.mesh = MeshData()
            node.aabb = AabbData()
            self.read_aabb_header(node)

        if self.read_err:
            self.warn(WARN_TRUNCATED_DATA, self.p, node_name, "truncated binary: some fields may be zeroed")
            self.read_err = False

        self.model.nodes.append(node)

        if ctrl_keys_def[1] > 0:
            self.read_controllers(node, ctrl_keys_def, ctrl_data_def)

        child_ptrs, _ = self.read_pointer_array(children_def)
        for cp in child_ptrs:
            if cp != 0:
                err = self.read_node(cp, node_name, depth + 1)
                if err:
                    self.warn(WARN_TRUNCATED_DATA, cp, node_name, "child read: %s" % err)
        return None

    def read_light_header(self, node):
        l = node.light
        self.at("the light header")
        l.flare_radius = self.read_f32()[0]
        self.skip(12)
        sizes_def, _ = self.read_array_def()
        pos_def, _ = self.read_array_def()
        color_def, _ = self.read_array_def()
        tex_def, _ = self.read_array_def()
        l.light_priority = self.read_i32()
        l.ambient_only = self.read_i32()
        l.n_dynamic_type = self.read_i32()
        l.affect_dynamic = self.read_i32()
        l.shadow = self.read_i32()
        l.generate_flare = self.read_i32()
        l.fading_light = self.read_i32()

        pos = self.p
        if sizes_def[1] > 0 and sizes_def[0] != 0:
            l.flare_sizes = self.read_float_array_at(sizes_def, l.flare_sizes)
        if pos_def[1] > 0 and pos_def[0] != 0:
            l.flare_positions = self.read_float_array_at(pos_def, l.flare_positions)
        if color_def[1] > 0 and color_def[0] != 0:
            abs_ptr = self.go_to_pointer(color_def[0])
            if (abs_ptr + color_def[1] * 12 <= self.size and self.track_alloc(color_def[1] * 12)
                    and self.seek(abs_ptr)):
                l.flare_color_shifts = [self.read_vec3() for _ in range(color_def[1])]
        if tex_def[1] > 0 and tex_def[0] != 0:
            tex_ptrs, _ = self.read_pointer_array(tex_def)
            for tp in tex_ptrs:
                if tp == 0:
                    continue
                if self.seek(self.go_to_pointer(tp)):
                    name, _ = self.read_fixed_string(64)
                    l.texture_names.append(name)
        if l.texture_names:
            l.lens_flares = len(l.texture_names)
        self.seek(pos)

    def read_emitter_header(self, node):
        em = node.emitter
        self.at("the emitter header")
        em.dead_space = self.read_f32()[0]
        em.blast_radius = self.read_f32()[0]
        em.blast_length = self.read_f32()[0]
        em.x_grid = _as_i32(self.read_u32())
        em.y_grid = _as_i32(self.read_u32())
        em.spawn_type = _as_i32(self.read_u32())
        em.update = self.read_fixed_string(32)[0]
        em.render = self.read_fixed_string(32)[0]
        em.blend = self.read_fixed_string(32)[0]
        em.texture = self.read_fixed_string(64)[0]
        chunk = self.read_fixed_string(16)[0]
        if chunk:
            em.chunk_name = chunk
        em.two_sided_tex = _as_i32(self.read_u32())
        em.loop = _as_i32(self.read_u32())
        em.render_order = self.read_u16()
        self.skip(2)
        flags = self.read_u32()
        em.p2p = flags & 1
        em.p2p_sel = (flags >> 1) & 1
        em.affected_by_wind = (flags >> 2) & 1
        em.is_tinted = (flags >> 3) & 1
        em.bounce = (flags >> 4) & 1
        em.random = (flags >> 5) & 1
        em.inherit = (flags >> 6) & 1
        em.inherit_vel = (flags >> 7) & 1
        em.inherit_local = (flags >> 8) & 1
        em.splat = (flags >> 9) & 1
        em.inherit_part = (flags >> 10) & 1

    def read_reference_header(self, node):
        node.reference.ref_model = self.read_fixed_string(64)[0]
        node.reference.reattachable = self.read_i32()

    def read_mesh_header(self, node):
        mesh = node.mesh
        self.at("the mesh header")
        self.skip(8)
        faces_def, _ = self.read_array_def()
        self.skip(24 + 4 + 12)
        mesh.diffuse = self.read_vec3()
        mesh.ambient = self.read_vec3()
        mesh.specular = self.read_vec3()
        mesh.shininess = self.read_f32()[0]
        mesh.shadow = _as_i32(self.read_u32())
        mesh.beaming = _as_i32(self.read_u32())
        mesh.render = _as_i32(self.read_u32())
        mesh.transparency_hint = _as_i32(self.read_u32())
        render_hint = self.read_u32()  # SharedMaterial::ExtractRenderHint's value
        if render_hint == 2:
            mesh.render_hint = "NormalAndSpecMapped"
        elif render_hint == 3:
            mesh.render_hint = "NormalTangents"
        elif render_hint != 0:
            mesh.render_hint = "None"
        bitmap = self.read_fixed_string(64)[0]
        if bitmap:
            mesh.bitmap = bitmap
        tex1 = self.read_fixed_string(64)[0]
        if tex1:
            mesh.texture1 = tex1
        tex2 = self.read_fixed_string(64)[0]
        if tex2:
            mesh.texture2 = tex2
        mat = self.read_fixed_string(64)[0]
        if mat:
            mesh.material_name = mat
        mesh.tile_fade = _as_i32(self.read_u32())
        self.skip(12 * 2)
        idx_count_def, _ = self.read_array_def()   # the render index list (MDL-BIN-049)
        idx_offset_def, _ = self.read_array_def()
        self.skip(4 + 4 + 4 + 4)
        p_vertex = self.read_i32()
        count_verts = self.read_u16()
        count_textures = self.read_u16()
        p_tex0 = self.read_i32()
        p_tex1 = self.read_i32()
        p_tex2 = self.read_i32()
        p_tex3 = self.read_i32()
        p_normals = self.read_i32()
        p_colors = self.read_i32()
        self.skip(12)
        p_tangent = self.read_i32()
        self.skip(4)
        p_handedness = self.read_i32()
        self.skip(1)  # lightmapped: uninitialised in the game's output, ignored (binary.go)
        mesh.rotate_texture = self.read_byte()[0]
        self.skip(2 + 4 + 4)

        header_end = self.p
        try:
            n = count_verts
            if n > 0 and p_vertex >= 0:
                self.at("verts")
                mesh.verts = self.read_vec3_array_at(self.go_to_mdx_pointer(p_vertex), n, mesh.verts)
            if n > 0 and p_tex0 >= 0:
                self.at("tverts")
                mesh.tverts = self.read_vec2_array_at(self.go_to_mdx_pointer(p_tex0), n, mesh.tverts)
            if count_textures > 1 and p_tex1 >= 0:
                self.at("tverts1")
                mesh.tverts1 = self.read_vec2_array_at(self.go_to_mdx_pointer(p_tex1), n, mesh.tverts1)
            if count_textures > 2 and p_tex2 >= 0:
                self.at("tverts2")
                mesh.tverts2 = self.read_vec2_array_at(self.go_to_mdx_pointer(p_tex2), n, mesh.tverts2)
            if count_textures > 3 and p_tex3 >= 0:
                self.at("tverts3")
                mesh.tverts3 = self.read_vec2_array_at(self.go_to_mdx_pointer(p_tex3), n, mesh.tverts3)
            if n > 0 and p_normals >= 0:
                self.at("normals")
                mesh.normals = self.read_vec3_array_at(self.go_to_mdx_pointer(p_normals), n, mesh.normals)
            if n > 0 and p_colors >= 0:
                self.at("colors")
                self.read_mdx_colors(mesh, p_colors, n)
            if n > 0 and p_tangent >= 0:
                self.read_mdx_tangents(mesh, p_tangent, p_handedness, n)
            if faces_def[1] > 0 and faces_def[0] != 0:
                self.read_faces(mesh, faces_def)
            rl = self.read_render_list(idx_count_def, idx_offset_def)
            if rl is not None:
                self.render_lists[node] = rl
        finally:
            self.seek(header_end)

    def read_render_list(self, counts, offsets):
        """readRenderList: the u16 vertex indices of each entry, or None."""
        n = min(counts[1], offsets[1])
        if n == 0 or n > 16 or counts[0] == 0 or offsets[0] == 0:
            return None
        out = []
        for i in range(n):
            if not self.seek(self.go_to_pointer(counts[0]) + i * 4):
                return None
            cnt, ok = self.read_u32_ok()
            if not ok:
                return None
            if not self.seek(self.go_to_pointer(offsets[0]) + i * 4):
                return None
            off, ok = self.read_u32_ok()
            if not ok:
                return None
            at = self.go_to_mdx_pointer(_as_i32(off))
            if at < 0 or at + cnt * 2 > self.size or not self.track_alloc(cnt * 2) or not self.seek(at):
                return None
            for _ in range(cnt):
                v, ok = self._read(_U16, 2)
                if not ok:
                    return None
                out.append(v)
        return out

    def read_skin_header(self, node):
        self.skip(12)
        p_weight_vertex = self.read_i32()
        p_bone_ref_index = self.read_i32()
        p_bone_map = self.read_i32()
        bind_count = self.read_i32()
        self.skip(36)
        bone_part_nums = [self.read_i16() for _ in range(64)]
        header_end = self.p
        try:
            map_pos = None
            if p_bone_map > 0 and bind_count > 0:
                abs_ptr = self.go_to_pointer(p_bone_map)
                if abs_ptr + bind_count * 2 <= self.size and self.track_alloc(bind_count * 2) and self.seek(abs_ptr):
                    map_pos = {}
                    for p in range(bind_count):
                        b = self.read_i16()
                        if b >= 0 and b not in map_pos:
                            map_pos[b] = p
            n = len(node.mesh.verts)
            if n == 0:
                return
            if not self.track_alloc(n * 24):
                return
            weights = [[0.0, 0.0, 0.0, 0.0] for _ in range(n)]
            refs = [[0, 0, 0, 0] for _ in range(n)]
            if p_weight_vertex >= 0:
                self.at("weights")
                abs_ptr = self.go_to_mdx_pointer(p_weight_vertex)
                if abs_ptr >= 0 and abs_ptr + n * 16 <= self.size and self.seek(abs_ptr):
                    for i in range(n):
                        w = weights[i]
                        w[0] = self.read_f32()[0]
                        w[1] = self.read_f32()[0]
                        w[2] = self.read_f32()[0]
                        w[3] = self.read_f32()[0]
            if p_bone_ref_index >= 0:
                abs_ptr = self.go_to_mdx_pointer(p_bone_ref_index)
                if abs_ptr >= 0 and abs_ptr + n * 8 <= self.size and self.seek(abs_ptr):
                    for i in range(n):
                        r = refs[i]
                        r[0] = self.read_i16()
                        r[1] = self.read_i16()
                        r[2] = self.read_i16()
                        r[3] = self.read_i16()
            self.pending_skins.append(_PendingSkin(node, bone_part_nums, bind_count, map_pos, weights, refs))
        finally:
            self.seek(header_end)

    def read_anim_mesh_header(self, node):
        am = node.anim_mesh
        self.at("the animmesh header")
        am.sample_period = self.read_f32()[0]
        self.skip(36)
        p_anim_verts = self.read_i32()
        p_anim_tverts = self.read_i32()
        n_vertex_sets = self.read_i32()
        n_tvert_sets = self.read_i32()
        header_end = self.p
        n = len(node.mesh.verts)
        total = n_vertex_sets * n
        if 0 < total <= self.size and p_anim_verts > 0 and n > 0:
            self.at("animverts")
            am.anim_verts = self.read_vec3_array_at(self.go_to_pointer(p_anim_verts), total, am.anim_verts)
        total = n_tvert_sets * n
        if 0 < total <= self.size and p_anim_tverts > 0 and n > 0:
            self.at("animtverts")
            am.anim_tverts = self.read_vec2_array_at(self.go_to_pointer(p_anim_tverts), total, am.anim_tverts)
        am.anim_verts = anim_set_major(am.anim_verts, n)
        am.anim_tverts = anim_set_major(am.anim_tverts, n)
        self.seek(header_end)

    def read_dangly_header(self, node):
        constraints_def, _ = self.read_array_def()
        self.at("the danglymesh header")
        node.dangly.displacement = self.read_f32()[0]
        node.dangly.tightness = self.read_f32()[0]
        node.dangly.period = self.read_f32()[0]
        header_end = self.p
        if constraints_def[1] > 0 and constraints_def[0] != 0:
            self.at("constraints")
            node.dangly.constraints = self.read_float_array_at(constraints_def, node.dangly.constraints)
        self.seek(header_end)

    def read_aabb_header(self, node):
        p_aabb = self.read_u32()
        if p_aabb != 0:
            self.read_aabb_tree(node.aabb, p_aabb)

    def read_aabb_tree(self, aabb, root_ptr):
        self.at("aabb")
        visited = set()
        stack = [root_ptr]
        while stack:
            ptr = stack.pop()
            if ptr == 0 or ptr in visited:
                continue
            visited.add(ptr)
            abs_ptr = self.go_to_pointer(ptr)
            if abs_ptr + 40 > self.size or not self.seek(abs_ptr):
                continue
            bmin = self.read_vec3()
            bmax = self.read_vec3()
            p_left = self.read_u32()
            p_right = self.read_u32()
            leaf = self.read_i32()
            plane = self.read_u32()
            aabb.entries.append(AabbEntry(bmin, bmax, leaf, plane))
            if p_right > 0:
                stack.append(p_right)
            if p_left > 0:
                stack.append(p_left)

    def read_faces(self, mesh, faces_def):
        abs_ptr = self.go_to_pointer(faces_def[0])
        if abs_ptr >= self.size or not self.seek(abs_ptr):
            return
        count = faces_def[1]
        if abs_ptr + count * 32 > self.size:
            return
        if not self.track_alloc(count * 32):
            return
        self.at("faces")
        faces = []
        for _ in range(count):
            normal = self.read_vec3()
            self.skip(4)
            surface = self.read_i32()
            self.skip(6)
            v = (self.read_u16(), self.read_u16(), self.read_u16())
            faces.append(Face(v, 0, v, surface, normal))
        n = len(mesh.verts)
        if n > 0:
            for f in faces:
                for i in range(3):
                    if f.verts[i] < 0 or f.verts[i] >= n:
                        f.verts[i] = 0
                    if f.uvs[i] < 0 or f.uvs[i] >= n:
                        f.uvs[i] = 0
        mesh.faces = faces

    # --- arrays ---

    def read_vec3_array_at(self, abs_ptr, count, current):
        if abs_ptr < 0 or abs_ptr + count * 12 > self.size:
            return current
        if not self.track_alloc(count * 12) or not self.seek(abs_ptr):
            return current
        return [self.read_vec3() for _ in range(count)]

    def read_vec2_array_at(self, abs_ptr, count, current):
        if abs_ptr < 0 or abs_ptr + count * 8 > self.size:
            return current
        if not self.track_alloc(count * 12) or not self.seek(abs_ptr):
            return current
        return [(self.read_f32()[0], self.read_f32()[0], 0.0) for _ in range(count)]

    def read_mdx_colors(self, mesh, ptr, count):
        abs_ptr = self.go_to_mdx_pointer(ptr)
        if abs_ptr < 0 or abs_ptr + count * 4 > self.size:
            return
        if not self.track_alloc(count * 12) or not self.seek(abs_ptr):
            return
        out = []
        for _ in range(count):
            r = self.read_byte()[0]
            g = self.read_byte()[0]
            b = self.read_byte()[0]
            self.skip(1)
            out.append((f32(r / _F32_255), f32(g / _F32_255), f32(b / _F32_255)))
        mesh.colors = out

    def read_mdx_tangents(self, mesh, tangent_ptr, handedness_ptr, count):
        tangent_abs = self.go_to_mdx_pointer(tangent_ptr)
        if tangent_abs < 0 or tangent_abs + count * 12 > self.size:
            return
        if not self.track_alloc(count * 16):
            return
        handedness = []
        self.at("tangent handedness")
        if handedness_ptr >= 0:
            h_abs = self.go_to_mdx_pointer(handedness_ptr)
            if h_abs >= 0 and h_abs + count * 4 <= self.size and self.track_alloc(count * 4):
                if self.seek(h_abs):
                    handedness = [self.read_f32()[0] for _ in range(count)]
        if not self.seek(tangent_abs):
            return
        self.at("tangents")
        out = []
        unit = True
        for i in range(count):
            t = self.read_vec3()
            w = -1.0 if i < len(handedness) and handedness[i] < 0 else 1.0
            out.append((t[0], t[1], t[2], w))
            unit = unit and _is_unit_tangent(t)
        # 1.69 water's bump-map stream shares the pointer; without a normal-mapped
        # hint it is kept only if every vector is unit length (binary.go).
        if not hint_wants_tangents(mesh.render_hint) and not unit:
            return
        mesh.tangents = out

    def read_float_array_at(self, a, current):
        abs_ptr = self.go_to_pointer(a[0])
        count = a[1]
        if abs_ptr + count * 4 > self.size or not self.seek(abs_ptr):
            return current
        if not self.track_alloc(count * 4):
            return current
        return [self.read_f32()[0] for _ in range(count)]

    # --- skins ---

    def resolve_skin_weights(self, ps):
        n = len(ps.skin_weights)
        if not self.track_alloc(n * 64):
            return
        names = [name for name, _ in self.node_order]
        bound, used = self.skin_bound_elsewhere(ps)
        if bound:
            names = self.skeleton_bone_names(ps, used)
        out = []
        unbound = 0
        remapped = {}
        nparts = len(ps.bone_part_nums)
        for i in range(n):
            vw = VertexWeight()
            ws, rs = ps.skin_weights[i], ps.skin_bone_refs[i]
            for j in range(4):
                weight, ref = ws[j], rs[j]
                if weight == 0 or ref < 0 or ref >= nparts:
                    continue
                part, from_map = ps.bone_pos(ref)
                if part < 0:
                    continue
                if from_map:
                    remapped[ref] = part
                if names is not None and part < len(names) and names[part] != "":
                    vw.bones.append(names[part])
                    vw.weights.append(weight)
            out.append(vw)
            if not vw.bones:
                unbound += 1
        ps.node.skin.weights = out
        if remapped:
            parts = ", ".join("bone %d (list entry %d, tree position %d)" % (b, ps.bone_part_nums[b], remapped[b])
                              for b in sorted(remapped))
            self.warn(WARN_GENERAL, 0, ps.node.name,
                      "skin %s: %s: the bone list entry is outside the %d-node tree, read past the end of a short "
                      "skin header, so the bone was named from the boneindexarray instead. The game skips such a "
                      "bone, so the vertices it weights lose that influence in game"
                      % (_go_quote(ps.node.name), parts, ps.bind_count))
        if unbound > 0 and names is not None:
            self.warn(WARN_GENERAL, 0, ps.node.name,
                      "skin %s has %d of %d vertices with no valid bone influence (all weights zero, or bone "
                      "references the file does not map, such as the uninitialised slots an engine compile can "
                      "leave). In game such a vertex is drawn at the origin and tears the mesh; give it weights "
                      "before compiling" % (_go_quote(ps.node.name), unbound, n))

    def skin_bound_elsewhere(self, ps):
        seen = set()
        used = []
        nparts = len(ps.bone_part_nums)
        for i in range(len(ps.skin_weights)):
            ws, rs = ps.skin_weights[i], ps.skin_bone_refs[i]
            for j in range(4):
                ref = rs[j]
                if ws[j] == 0 or ref < 0 or ref >= nparts:
                    continue
                p, _ = ps.bone_pos(ref)
                if p >= 0 and p not in seen:
                    seen.add(p)
                    used.append(p)
        used.sort()
        own = len(self.node_order)
        bound = ps.bind_count > own or (len(used) > 0 and used[-1] >= own)
        return bound, used

    def skeleton_bone_names(self, ps, used):
        from .skeleton import load_skeleton, part_skeleton_name
        node = ps.node.name
        where = ("skin %s is bound to a creature skeleton: its bones are tree positions %s of a %d-node tree, "
                 "and this file has %d nodes" % (_go_quote(node), ", ".join(str(p) for p in used),
                                                 ps.bind_count, len(self.node_order)))
        skel_name = part_skeleton_name(self.model.name)
        if not skel_name:
            self.warn(WARN_GENERAL, 0, node, "%s. The model name %s has no body-part prefix (such as pmh0_) to "
                      "find the skeleton by, so the bones are left unnamed" % (where, _go_quote(self.model.name)))
            return None
        try:
            sk = load_skeleton(skel_name, self.skeleton_dirs)
        except Exception as e:  # noqa: BLE001 - reported as a warning, like Go's error
            self.warn(WARN_GENERAL, 0, node, "%s. Reading the skeleton %s.mdl failed (%s), so the bones are "
                      "left unnamed" % (where, skel_name, e))
            return None
        if sk is None:
            self.warn(WARN_GENERAL, 0, node, "%s. Its bone names need the skeleton %s.mdl: give --game-dir "
                      "(stock skeletons) or a --supermodel-dir or --resource-dir that holds it. The bones are "
                      "left unnamed" % (where, skel_name))
            return None
        names = [n.name for n in sk.order]
        if ps.bind_count != len(names):
            self.warn(WARN_GENERAL, 0, node, "%s, but the skeleton %s has %d nodes; the bone names read from "
                      "it may be wrong" % (where, sk.path, len(names)))
        return names

    # --- controllers ---

    def read_controller_keys(self, keys_def):
        abs_ptr = self.go_to_pointer(keys_def[0])
        count = keys_def[1]
        if abs_ptr >= self.size or abs_ptr + count * 12 > self.size or not self.seek(abs_ptr):
            return []
        if not self.track_alloc(count * 12):
            return []
        keys = []
        for _ in range(count):
            off = self.p  # of the key, for warnings
            t = self.read_u32()
            vc = self.read_u16()
            ts = self.read_u16()
            ds = self.read_u16()
            cc = self.read_byte()[0]
            self.skip(1)
            keys.append((t, vc, ts, ds, cc, off))
        return keys

    def read_controller_rows(self, name, data_ptr, time_start, data_start, num_rows, num_cols):
        if num_rows <= 0:
            return []
        if not self.track_alloc(num_rows * (32 + num_cols * 4)):
            return []
        base = self.go_to_pointer(data_ptr)
        rows = []
        self.at("the " + name + " controller")
        for row in range(num_rows):
            time_ptr = base + (time_start + row) * 4
            if time_ptr < 0 or time_ptr + 4 > self.size or not self.seek(time_ptr):
                break
            t = self.read_f32()[0]
            val_ptr = base + (data_start + row * num_cols) * 4
            if val_ptr < 0 or val_ptr + num_cols * 4 > self.size or not self.seek(val_ptr):
                break
            vals = [self.read_f32()[0] for _ in range(num_cols)]
            if num_cols == 4 and name == "orientation":
                vals = quaternion_to_angle_axis(vals)
            rows.append((t, vals))
        return rows

    def resolve_controller_defs(self, keys, node_flag, node_name):
        out = []
        if node_flag == 5:
            for k in keys:
                self.detect_compiler(k[0], k[4])
        for k in keys:
            type_id, column_count = k[0], k[4]
            d = controller_id(type_id, node_flag, self.compiler)
            if d is None and type_id == 144 and node_flag == 3:
                # older EE compilers wrote it on lights; ASCII cannot carry it (binary.go, nwn-mdl-spec E51)
                self.note(WARN_UNKNOWN_CONTROLLER, k[5], node_name,
                          "light controller 144 dropped: an older EE compiler wrote it, the game's compiler writes it "
                          "for no keyword, and ASCII has none (nwn-mdl-spec E51)")
                continue
            if d is None:
                self.warn(WARN_UNKNOWN_CONTROLLER, k[5], node_name,
                          "unknown controller type %d for node flag %d" % (type_id, node_flag))
                continue
            name, def_cols = d
            # bezier: the value then two handles, each as wide as the value; the
            # column count comes from the controller (binary.go)
            # detonate keys are times only, whatever the byte says (binary.go)
            if name == "detonate":
                out.append((name, 0, k, False, def_cols))
                continue
            if column_count == 0xFF:  # no values (sscanf's -1), not bezier; skipped by the game
                self.warn(WARN_UNKNOWN_CONTROLLER, k[5], node_name,
                          "controller %s has no values (numfloats=0xff); the game skips such a controller, so it was "
                          "dropped" % name)
                continue
            bezier = column_count & 0x10 != 0
            if bezier and (name == "orientation" or def_cols <= 0):
                self.warn(WARN_UNKNOWN_CONTROLLER, k[5], node_name,
                          "controller %s is a bezier controller (numfloats=0x%02x), which has no readable bezier "
                          "form; it was dropped" % (name, column_count))
                continue
            # applied by the low nibble: 1, 3 or 4 columns, else skipped (binary.go)
            num_cols = column_count & 0x0F
            applied = num_cols in (1, 3, 4)
            if bezier:
                if not applied:
                    self.warn(WARN_UNKNOWN_CONTROLLER, k[5], node_name,
                              "controller %s (numfloats=0x%02x) is read with the width of its keyword; the game skips "
                              "it, since it applies only 1, 3 or 4 columns" % (name, column_count))
                if def_cols > 0:
                    num_cols = def_cols
                num_cols *= 3
            elif not applied:
                self.warn(WARN_UNKNOWN_CONTROLLER, k[5], node_name,
                          "controller %s has %d value columns (numfloats=0x%02x); the game skips such a controller, "
                          "so it was dropped" % (name, num_cols, column_count))
                continue
            elif name == "orientation" and num_cols != 4:
                self.warn(WARN_UNKNOWN_CONTROLLER, k[5], node_name,
                          "orientation controller with %d value columns: the game writes them into its quaternion w "
                          "first, which ASCII cannot express, so it was dropped" % num_cols)
                continue
            elif 0 < def_cols < num_cols:
                self.warn(WARN_UNKNOWN_CONTROLLER, k[5], node_name,
                          "controller %s has %d value columns for a %d-wide value: the game writes past the value; "
                          "only the first %d are kept" % (name, num_cols, def_cols, def_cols))
            out.append((name, num_cols, k, bezier, def_cols))
        return out

    def read_controllers(self, node, keys_def, data_def):
        keys = self.read_controller_keys(keys_def)
        if not keys:
            return
        node_flag = node.node_type_flag()
        for name, num_cols, k, bezier, def_cols in self.resolve_controller_defs(keys, node_flag, node.name):
            rows = self.read_controller_rows(name, data_def[0], k[2], k[3], k[1], num_cols)
            if not rows:
                continue
            vals = rows[0][1]
            if bezier:
                vals = vals[:len(vals) // 3]  # a geometry node keeps one value
            else:
                vals = runtime_width(def_cols, vals)
            set_static_controller(node, name, vals)
            if len(rows) > 1:
                self.warn(WARN_GENERAL, 0, node.name, "controller %s has %d keyframes on geometry node; using "
                          "first value only" % (name, len(rows)))

    def detect_compiler(self, type_id, column_count):
        """Run on every key of an emitter before any is resolved (binary.go)."""
        if self.compiler != controllers.COMPILER_UNKNOWN:
            return
        if type_id in (448, 452):
            self.compiler = controllers.COMPILER_BIOWARE
        elif type_id in (480, 481, 482, 484, 488):
            self.compiler = controllers.COMPILER_NWNMDLCOMP
        elif type_id == 468 and column_count & 0x0F == 3:
            self.compiler = controllers.COMPILER_NWNMDLCOMP

    # --- animations ---

    def read_animation(self, ptr):
        abs_ptr = self.go_to_pointer(ptr)
        if abs_ptr >= self.size:
            return "animation pointer out of bounds: %d" % ptr
        if not self.seek(abs_ptr):
            return "seek out of bounds: %d (file size %d)" % (abs_ptr, self.size)
        self.skip(8)
        name, _ = self.read_fixed_string(64)
        self.where = (name, "", "the animation header")
        root_node_ptr = self.read_u32()
        self.skip(4 + 12 + 12 + 4 + 4)
        length = self.read_f32()[0]
        trans_time = self.read_f32()[0]
        root, _ = self.read_fixed_string(64)
        events_def, _ = self.read_array_def()
        anim = Animation(name, length, trans_time, root)
        if events_def[1] > 0 and events_def[0] != 0:
            self.at("the events")
            evt_abs = self.go_to_pointer(events_def[0])
            if evt_abs + events_def[1] * 36 <= self.size and self.track_alloc(events_def[1] * 36):
                if self.seek(evt_abs):
                    for _ in range(events_def[1]):
                        t = self.read_f32()[0]
                        ename, _ = self.read_fixed_string(32)
                        anim.events.append((t, ename))
        if root_node_ptr != 0:
            self.read_anim_node(anim, root_node_ptr, "NULL", 0)
        self.model.animations.append(anim)
        return None

    def read_anim_node(self, anim, node_ptr, parent_name, depth):
        if node_ptr == 0:
            return
        if depth > MAX_READ_NODE_DEPTH:
            self.warn(WARN_GENERAL, 0, parent_name, "anim node tree exceeds max depth %d; truncating" % MAX_READ_NODE_DEPTH)
            return
        if node_ptr in self.visited_ptrs:
            self.warn(WARN_POINTER_OUT_OF_BOUNDS, node_ptr, "", "cycle detected at anim node pointer")
            return
        self.visited_ptrs.add(node_ptr)
        abs_ptr = self.go_to_pointer(node_ptr)
        if abs_ptr >= self.size or not self.seek(abs_ptr):
            return

        self.skip(24 + 4 + 4)
        node_name, _ = self.read_fixed_string(32)
        self.where = (anim.name, node_name, "the node header")
        self.skip(8)
        children_def, _ = self.read_array_def()
        ctrl_keys_def, _ = self.read_array_def()
        ctrl_data_def, _ = self.read_array_def()
        content = self.read_u32()

        has_light = content & 0x02 != 0
        has_emitter = content & 0x04 != 0
        has_mesh = content & 0x20 != 0
        has_anim = content & 0x80 != 0
        node_flag = 3 if has_light else 5 if has_emitter else 33 if has_mesh else 1

        if has_light:
            self.skip(4 + 5 * 12 + 7 * 4)
        if has_emitter:
            self.skip(4 * 3 + 4 * 3 + 32 * 3 + 64 + 16 + 4 * 2 + 2 + 2 + 4)
        if content & 0x10:
            self.skip(64 + 4)

        anim_mesh_verts = 0
        anim_faces_def = (0, 0, 0)
        anim_p_vertex = anim_p_tex0 = 0
        if has_mesh and has_anim:
            mesh_start = self.p
            self.skip(8)
            anim_faces_def, _ = self.read_array_def()
            self.skip(MESH_HEADER_P_MDX_VERTEX - 20)
            anim_p_vertex = self.read_i32()
            count_verts = self.read_u16()
            self.skip(2)
            anim_p_tex0 = self.read_i32()
            anim_mesh_verts = count_verts
            self.seek(mesh_start + MESH_HEADER_SIZE)
        elif has_mesh:
            self.skip(MESH_HEADER_SIZE)
        if content & 0x40:
            self.skip(12 + 4 * 2 + 4 * 2 + 12 * 3 + 64 * 2)

        anim_mesh_data = None
        if has_anim:
            anim_mesh_data = AnimMeshData()
            self.at("the animmesh header")
            anim_mesh_data.sample_period = self.read_f32()[0]
            self.skip(36)
            p_anim_verts = self.read_i32()
            p_anim_tverts = self.read_i32()
            n_vertex_sets = self.read_i32()
            n_tvert_sets = self.read_i32()
            if anim_mesh_verts > 0:
                total = n_vertex_sets * anim_mesh_verts
                if 0 < total <= self.size and p_anim_verts > 0:
                    self.at("animverts")
                    anim_mesh_data.anim_verts = self.read_vec3_array_at(
                        self.go_to_pointer(p_anim_verts), total, anim_mesh_data.anim_verts)
                total = n_tvert_sets * anim_mesh_verts
                if 0 < total <= self.size and p_anim_tverts > 0:
                    self.at("animtverts")
                    anim_mesh_data.anim_tverts = self.read_vec2_array_at(
                        self.go_to_pointer(p_anim_tverts), total, anim_mesh_data.anim_tverts)
                anim_mesh_data.anim_verts = anim_set_major(anim_mesh_data.anim_verts, anim_mesh_verts)
                anim_mesh_data.anim_tverts = anim_set_major(anim_mesh_data.anim_tverts, anim_mesh_verts)
        if content & 0x100:
            self.skip(12 + 4 * 3)
        if content & 0x200:
            self.skip(4)

        an = AnimNode(node_name, parent_name)

        if has_mesh and has_anim and anim_mesh_verts > 0:
            header_end = self.p
            mesh = MeshData()
            if anim_p_vertex >= 0:
                self.at("verts")
                mesh.verts = self.read_vec3_array_at(self.go_to_mdx_pointer(anim_p_vertex), anim_mesh_verts, mesh.verts)
            if anim_p_tex0 >= 0:
                self.at("tverts")
                mesh.tverts = self.read_vec2_array_at(self.go_to_mdx_pointer(anim_p_tex0), anim_mesh_verts, mesh.tverts)
            if anim_faces_def[1] > 0 and anim_faces_def[0] != 0:
                self.read_faces(mesh, anim_faces_def)
            an.mesh = mesh
            self.seek(header_end)

        if anim_mesh_data is not None and (anim_mesh_data.sample_period != 0 or anim_mesh_data.anim_verts
                                           or anim_mesh_data.anim_tverts):
            an.anim_mesh = anim_mesh_data

        if ctrl_keys_def[1] > 0:
            self.read_anim_controllers(an, node_flag, ctrl_keys_def, ctrl_data_def)

        anim.nodes.append(an)

        child_ptrs, _ = self.read_pointer_array(children_def)
        for cp in child_ptrs:
            if cp != 0:
                self.read_anim_node(anim, cp, node_name, depth + 1)

    def read_anim_controllers(self, an, node_flag, keys_def, data_def):
        keys = self.read_controller_keys(keys_def)
        if not keys:
            return
        for name, num_cols, k, bezier, def_cols in self.resolve_controller_defs(keys, node_flag, an.name):
            rows = self.read_controller_rows(name, data_def[0], k[2], k[3], k[1], num_cols)
            if bezier and name != "position":
                add_anim_bezier_keys(an, name, rows)
                continue
            if bezier:
                for t, v in rows:
                    if len(v) >= 9:
                        an.position_keys.append((t, (v[0], v[1], v[2]), (v[3], v[4], v[5]), (v[6], v[7], v[8])))
                an.position_bezier = True
                continue
            for t, v in rows:
                add_anim_key(an, name, t, runtime_width(def_cols, v))


def _as_i32(v):
    return v - 0x100000000 if v >= 0x80000000 else v


def _go_quote(s):
    """Go's %q for the plain ASCII names used here."""
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'


def quaternion_to_angle_axis(q):
    """binary.go quaternionToAngleAxis, with Go's float32 rounding at each step."""
    if len(q) < 4:
        return q
    x, y, z, w = q
    s = f32(f32(f32(x * x) + f32(y * y)) + f32(z * z))
    sin_half = f32(math.sqrt(s))
    if sin_half < _F32_1E6:
        return [0.0, 0.0, 1.0, 0.0]
    angle = f32(2.0 * math.atan2(sin_half, w))
    return [f32(x / sin_half), f32(y / sin_half), f32(z / sin_half), angle]


def runtime_width(def_cols, vals):
    """binary.go runtimeWidth: a linear controller's values fitted to its width, short ones padded with 0."""
    if def_cols <= 0 or len(vals) == def_cols:
        return vals
    if len(vals) > def_cols:
        return vals[:def_cols]
    return list(vals) + [0.0] * (def_cols - len(vals))


def anim_set_major(lst, n):
    """binary.go animSetMajor: the binary's vertex-major sets as the ASCII's set-major list."""
    if n <= 0 or len(lst) % n != 0:
        return lst
    sets = len(lst) // n
    out = [None] * len(lst)
    for v in range(n):
        for s in range(sets):
            out[s * n + v] = lst[v * sets + s]
    return out


def set_static_controller(node, name, vals):
    node.declared.add(name)
    if name == "position":
        if len(vals) >= 3:
            node.position = (vals[0], vals[1], vals[2])
    elif name == "orientation":
        if len(vals) >= 4:
            node.orientation = (vals[0], vals[1], vals[2], vals[3])
    elif name == "scale":
        if len(vals) >= 1:
            node.scale = vals[0]
    elif name == "selfillumcolor":
        if len(vals) >= 3 and node.mesh is not None:
            node.mesh.self_illum_color = (vals[0], vals[1], vals[2])
    elif name == "alpha":
        if len(vals) >= 1 and node.mesh is not None:
            node.mesh.alpha = vals[0]
    elif name == "color":
        if len(vals) >= 3 and node.light is not None:
            node.light.color = (vals[0], vals[1], vals[2])
    elif name in ("radius", "multiplier", "shadowradius", "verticaldisplacement"):
        if len(vals) >= 1 and node.light is not None:
            attr = {"radius": "radius", "multiplier": "multiplier", "shadowradius": "shadow_radius",
                    "verticaldisplacement": "vertical_displacement"}[name]
            setattr(node.light, attr, vals[0])
    elif node.emitter is not None and vals:
        em = node.emitter
        if name in em.floats:
            em.floats[name] = vals[0]
        elif name in em.colors and len(vals) >= 3:
            em.colors[name] = (vals[0], vals[1], vals[2])


def add_anim_bezier_keys(an, name, rows):
    """binary.go addAnimBezierKeys: rows of value, handle in, handle out."""
    if name in _ANIM_FLOAT:
        keys = an.float_keys.setdefault(name, [])
        for t, v in rows:
            if len(v) >= 3:
                keys.append((t, v[0], v[1], v[2]))
        if not keys:
            del an.float_keys[name]
        an.bezier.add(name)
        return
    if name in _ANIM_COLOR:
        keys = an.color_keys.setdefault(name, [])
        for t, v in rows:
            if len(v) >= 9:
                keys.append((t, (v[0], v[1], v[2]), (v[3], v[4], v[5]), (v[6], v[7], v[8])))
        if not keys:
            del an.color_keys[name]
        an.bezier.add(name)


def add_anim_key(an, name, t, vals):
    if name == "detonate":
        an.float_keys.setdefault("detonate", []).append((t, 1.0))
        return
    if name in _ANIM_FLOAT:
        if vals:
            an.float_keys.setdefault(name, []).append((t, vals[0]))
        return
    if name in _ANIM_COLOR:
        if len(vals) >= 3:
            an.color_keys.setdefault(name, []).append((t, (vals[0], vals[1], vals[2])))
        return
    if name == "position":
        if len(vals) >= 3:
            # Linear keys carry zero tangents, as Go's PositionKey does: they are
            # written as bezier rows if a bezier position controller follows.
            an.position_keys.append((t, (vals[0], vals[1], vals[2]), VEC3_ZERO, VEC3_ZERO))
    elif name == "orientation":
        if len(vals) >= 4:
            an.orientation_keys.append((t, (vals[0], vals[1], vals[2], vals[3])))


def _is_unit_tangent(t):
    """binary.go isUnitTangent: squared length within 0.02 of 1, in float64."""
    l2 = t[0] * t[0] + t[1] * t[1] + t[2] * t[2]
    return 0.98 < l2 < 1.02


# --- merged meshes (binary_merged.go) ---

MERGE_TOLERANCE = 1e-3  # m


def _mesh_tri(mesh, a, b, c):
    """meshTri: (uv key, sorted side lengths), or None when an index is out of range."""
    idx = (a, b, c)
    for i in idx:
        if i < 0 or i >= len(mesh.verts):
            return None
    uv = []
    for i in idx:
        if i < len(mesh.tverts):
            uv.append((mesh.tverts[i][0], mesh.tverts[i][1]))
        else:
            uv.append((0.0, 0.0))
    uv.sort()
    side = []
    for j in range(3):
        p, q = mesh.verts[idx[j]], mesh.verts[idx[(j + 1) % 3]]
        dx, dy, dz = p[0] - q[0], p[1] - q[1], p[2] - q[2]
        side.append(math.sqrt(dx * dx + dy * dy + dz * dz))
    side.sort()
    return tuple(uv), side


class _TriPool:
    """triPool: triangles by their uv key, each taken once by one of the same shape."""
    __slots__ = ("by_uv", "n")

    def __init__(self):
        self.by_uv = {}
        self.n = 0

    def add(self, t):
        self.by_uv.setdefault(t[0], []).append(t)
        self.n += 1

    def find(self, t):
        for i, c in enumerate(self.by_uv.get(t[0], ())):
            if (abs(c[1][0] - t[1][0]) <= MERGE_TOLERANCE and abs(c[1][1] - t[1][1]) <= MERGE_TOLERANCE
                    and abs(c[1][2] - t[1][2]) <= MERGE_TOLERANCE):
                return i
        return -1

    def take(self, t, i):
        del self.by_uv[t[0]][i]
        self.n -= 1

    def copy(self):
        p = _TriPool()
        p.by_uv = {k: list(v) for k, v in self.by_uv.items()}
        p.n = self.n
        return p


def unmerge_render_lists(d):
    """unmergeRenderLists: put merged donors back (render 1), note or warn per receiver."""
    if not d.render_lists:
        return
    moves = set()  # nodes an animation moves: a donor among them was drawn still
    for a in d.model.animations:
        for an in a.nodes:
            if len(an.position_keys) > 1 or len(an.orientation_keys) > 1:
                moves.add(go_lower(an.name))
    used = set()
    for r in d.model.nodes:
        rl = d.render_lists.get(r)
        if r.mesh is None or rl is None or len(rl) <= 3 * len(r.mesh.faces):
            continue
        extra = _TriPool()
        unreadable = 0
        for k in range(0, len(rl) - 2, 3):
            t = _mesh_tri(r.mesh, rl[k], rl[k + 1], rl[k + 2])
            if t is not None:
                extra.add(t)
            else:
                unreadable += 1
        for f in r.mesh.faces:
            t = _mesh_tri(r.mesh, f.verts[0], f.verts[1], f.verts[2])
            if t is not None:
                i = extra.find(t)
                if i >= 0:
                    extra.take(t, i)
        if extra.n == 0 and unreadable == 0:
            continue
        donors = []
        for n in d.model.nodes:
            m = n.mesh
            if (n is r or m is None or id(n) in used or m.render != 0 or d.render_lists.get(n) or not m.faces
                    or not equal_fold(m.bitmap, r.mesh.bitmap)):
                continue
            trial = extra.copy()
            ok = True
            for f in m.faces:
                t = _mesh_tri(m, f.verts[0], f.verts[1], f.verts[2])
                i = trial.find(t) if t is not None else -1
                if i < 0:
                    ok = False
                    break
                trial.take(t, i)
            if not ok:
                continue
            extra = trial
            used.add(id(n))
            m.render = 1
            donors.append(n.name)
        lost = extra.n + unreadable
        d.model.render_merges.append((r.name, donors, lost))
        if donors:
            d.note(WARN_GENERAL, 0, r.name,
                   "its render list also draws %s, merged into it by the tool that compiled the model (render 0 and no "
                   "render list of their own): written with render 1, which draws the same as separate meshes"
                   % ", ".join(donors))
        moving = [n for n in donors if go_lower(n) in moves]
        if moving:
            d.note(WARN_GENERAL, 0, r.name,
                   "this model's animations move %s, which the game drew still, as part of this mesh's render list: with "
                   "render 1, the animations move it" % ", ".join(moving))
        if lost > 0:
            d.warn(WARN_GENERAL, 0, r.name,
                   "%d of the triangles its render list draws are in no mesh's faces: ASCII has no place for them, so a "
                   "decompile, repair or recompile loses them" % lost)

