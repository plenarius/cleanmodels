"""Binary MDL compiler: a port of pkg/mdl/compiler.go.

Converts a Model to NWN binary (compiled) .mdl bytes, exactly as
`cleanmodels compile` writes them. Every write mirrors the corresponding read
in binary.go. Pointers are core-relative (file offset 12 + p) or MDX-relative
(file offset 12 + core length + p).

Layout:
    [0-11]            file header {0, core_len, mdx_len}
    [12 ...]          core block (model header, node tree, animations)
    [12+core_len ...] MDX block (vertex data)
"""

import struct

from .compiler_buf import PatchBuf
from .compiler_controllers import ControllersMixin
from .compiler_mesh import CompileError, MeshMixin, anim_sets, anim_vertex_major
from .compiler_node import NodeMixin
from .compiler_order import reorder_children
from .compiler_skinbind import SkinBindMixin, SkinBindPatch
from .compiler_tree import is_root_parent, node_occurrences, pair_anim_nodes_to_geom, resolve_anim_tree, \
    resolve_geom_tree
from .f32 import f32
from .gomath import go_lower, go_quote
from .gostr import equal_fold, go_upper
from .vec import model_bounds

MESH_HEADER_SIZE = 512
MAX_DROPPED_NAMED = 5

__all__ = ["CompileError", "compile", "compile_file"]


def compile(model, supermodel_dirs=(), resource_dirs=(), warn=None):  # noqa: A001 - mirrors Go's Compile
    """The binary MDL for model (compiler.go CompileWithOptions).

    supermodel_dirs are searched for the supermodel (node numbering) and a body
    part's creature skeleton; resource_dirs for materials (.mtr) that decide
    whether a mesh gets tangents. `cleanmodels compile` passes the model's
    folder then the game install for both (resources.lookup_dirs). warn, if
    given, is called with each non-fatal note. Raises CompileError.

    Note: like Go, compiling fills in generated normals on the model's meshes.
    """
    if model is None:
        raise CompileError("compile: nil model")
    c = Compiler(model)
    c.resource_dirs = list(resource_dirs)
    c.warn = warn
    supermodel_dirs = list(supermodel_dirs)

    root = model.root_node()
    if root is not None:
        sm = c.resolve_supermodel(supermodel_dirs, warn)
        if sm is not None:
            c.assign_node_ids_from_supermodel(root, sm)
        else:
            c.assign_node_ids(root)
        c.warn_dropped_geometry()
        c.resolve_part_skeleton(supermodel_dirs, warn)

    c.write_model()
    if c.err is not None:
        raise c.err if isinstance(c.err, CompileError) else CompileError(str(c.err))
    return struct.pack("<III", 0, c.core.len(), c.vol.len()) + bytes(c.core.b) + bytes(c.vol.b)


def compile_file(model, path, supermodel_dirs=(), resource_dirs=(), warn=None):
    data = compile(model, supermodel_dirs, resource_dirs, warn)
    with open(path, "wb") as f:
        f.write(data)


def _summarise_dropped(kind, names):
    shown = names[:MAX_DROPPED_NAMED]
    more = " and %d more" % (len(names) - MAX_DROPPED_NAMED) if len(names) > MAX_DROPPED_NAMED else ""
    return "%d %s node(s) are not connected to the root and were left out of the binary: %s%s" % (
        len(names), kind, ", ".join(shown), more)


class Compiler(NodeMixin, MeshMixin, ControllersMixin, SkinBindMixin):
    """State for one compilation (compiler.go compiler)."""

    def __init__(self, model):
        self.core = PatchBuf()
        self.vol = PatchBuf()
        self.model = model
        self.node_ids = {}        # node -> part number
        self.tree_index = {}      # node -> tree position (skin bones are addressed by it)
        self.skin_bind = SkinBindPatch()
        self.part_skel = None
        self.part_attach = 0
        self.world_pos = None
        self.world_rot = None
        self.resource_dirs = []
        self.warn = None
        self.node_count_override = 0
        self.node_offsets = {}
        _, self.children_by_node = resolve_geom_tree(model.nodes)
        root = model.root_node()
        if root is not None:
            reorder_children(root, self.children_by_node)
        gi = {}
        for n in model.nodes:
            if n is not None:
                gi.setdefault(go_lower(n.name), n)
        self.geom_node_index = gi
        self.geom_occur = node_occurrences(model.nodes)
        self.anim_geom_pair = {}
        self.last_expanded = None
        self.merge_anim = None    # animmesh sets taking part in the vertex merge
        self.dangly_constraints_ptr_pos = 0
        self.err = None

    def children_of(self, n):
        return self.children_by_node.get(n, [])

    def geom_node_by_name(self, name):
        return self.geom_node_index.get(go_lower(name))

    # --- node numbering (compiler.go, supermodel.go) ---

    def declaration_index(self):
        """compiler_order.go declarationIndex: node -> position among the node blocks."""
        return {n: i for i, n in enumerate(self.model.nodes) if n is not None}

    def walk_tree(self, root, visit):
        """compiler_order.go walkTree: visit(n, parent, index), depth-first in child order."""
        visited = set()
        stack = [(root, None)]
        index = 0
        while stack:
            n, parent = stack.pop()
            if n is None or n in visited:
                continue
            visited.add(n)
            visit(n, parent, index)
            index += 1
            children = self.children_of(n)
            for i in range(len(children) - 1, -1, -1):
                if children[i] not in visited:
                    stack.append((children[i], n))

    def assign_node_ids(self, root):
        """assignNodeIDs: declaration index; tree_index the position in the written tree."""
        decl = self.declaration_index()

        def visit(n, _parent, index):
            self.node_ids[n] = decl[n]
            self.tree_index[n] = index
        self.walk_tree(root, visit)

    def assign_node_ids_from_supermodel(self, root, sm):
        """assignNodeIDsFromSupermodel: a node at a path the supermodel has takes its
        number, any other child of a matched node -1, the rest count_nodes + 1 +
        declaration index."""
        base = sm.count + 1
        decl = self.declaration_index()
        match = {}
        count = [0]

        def visit(n, parent, index):
            if n is root:
                self.node_ids[n] = 0
                match[n] = sm.root
            else:
                p = match.get(parent)
                sn = p.child(n.name) if p is not None else None
                if sn is not None and sn.number >= 0:
                    self.node_ids[n] = sn.number
                    match[n] = sn
                elif p is not None:
                    self.node_ids[n] = -1
                else:
                    self.node_ids[n] = _i32(base + decl[n])
            self.tree_index[n] = index
            count[0] = index + 1
        self.walk_tree(root, visit)
        self.node_count_override = _i32(base + count[0])

    def resolve_supermodel(self, dirs, warn):
        from .resources import find_model_resource
        from .supermodel import load_supermodel
        name = self.model.super_model.strip()
        if name == "" or go_lower(name) == "null" or go_lower(name) == go_lower(self.model.name):
            return None
        if not dirs:
            return None

        def note(msg):
            if warn is not None:
                warn(msg)

        ref = find_model_resource(name, dirs)
        if ref is None:
            note("supermodel %s not found in %s; node numbers assume no supermodel and may not match its "
                 "animations" % (go_quote(name), ", ".join(dirs)))
            return None
        try:
            return load_supermodel(ref, dirs, {go_lower(self.model.name), go_lower(name)})
        except Exception as e:  # noqa: BLE001 - any read failure is reported, as in Go
            note("supermodel %s could not be read (%s); node numbers assume no supermodel" % (go_quote(name), e))
            return None

    def resolve_part_skeleton(self, dirs, warn):
        from .compiler_skeleton import find_capart, load_skeleton, part_attach_node, part_kind
        from .skeleton import part_skeleton_name
        skin = None
        for n in self.model.nodes:
            if n is not None and n.skin is not None:
                skin = n
                break
        attach = part_attach_node(self.model.name)
        if skin is None or attach == "":
            return

        def note(msg):
            if warn is not None:
                warn(msg)

        skel_name = part_skeleton_name(self.model.name)
        consequence = ("its skins are bound to the part's own node tree, but the game binds a body part's skin to "
                       "the creature skeleton by tree position, so the compiled part will render torn in game. "
                       "Give --game-dir (stock skeletons) or a --supermodel-dir holding the skeleton, or ship the "
                       "part as ASCII")
        try:
            sk = load_skeleton(skel_name, dirs)
        except Exception as e:  # noqa: BLE001 - reported like Go's error
            note("body part %s: skeleton %s.mdl could not be read (%s); %s"
                 % (go_quote(self.model.name), skel_name, e, consequence))
            return
        if sk is None:
            note("body part %s: skeleton %s.mdl not found; %s" % (go_quote(self.model.name), skel_name, consequence))
            return
        source = ("the stock capart.2da (no capart.2da was found; put yours next to the model or give the hak or "
                  "folder holding it with --resource-dir)")
        table, path = find_capart(dirs)
        if table is not None:
            node = table.get(part_kind(self.model.name))
            if node is not None:
                attach, source = node, path
        at, ok = sk.attach_index(attach)
        if not ok:
            note("body part %s: skeleton %s has no node %s to attach the part to (attach node from %s); %s"
                 % (go_quote(self.model.name), sk.path, attach, source, consequence))
            return
        self.part_skel, self.part_attach = sk, at

        missing = []
        seen = set()
        for n in self.model.nodes:
            if n is None or n.skin is None:
                continue
            for vw in n.skin.weights:
                for b in vw.bones:
                    k = go_lower(b)
                    if b == "" or k in seen:
                        continue
                    seen.add(k)
                    if k not in sk.index:
                        missing.append(b)
        if missing:
            missing.sort(key=lambda s: s.encode("latin-1", "replace"))
            note("body part %s: skin bones %s are not nodes of the skeleton %s; the game binds a part's skin to "
                 "the skeleton only, so their influence is lost in game"
                 % (go_quote(self.model.name), ", ".join(missing), sk.path))

    # --- dropped nodes (compiler_dropped.go) ---

    def warn_dropped_geometry(self):
        if self.warn is None:
            return
        names = []
        for n in self.model.nodes:
            if n is None or n in self.node_ids:
                continue
            if n.parent == "":
                reason = "no parent"
            elif is_root_parent(n.parent):
                reason = "a second root"
            elif self.geom_node_by_name(n.parent) is None:
                reason = "parent %s does not exist" % go_quote(n.parent)
            else:
                reason = "parent %s is itself not connected" % go_quote(n.parent)
            names.append("%s (%s)" % (n.name, reason))
        if names:
            self.warn(_summarise_dropped("geometry", names))

    def warn_dropped_anim(self, anim, visited):
        if self.warn is None:
            return
        names = [an.name for an in anim.nodes if an not in visited]
        if names:
            self.warn("animation %s: %s" % (go_quote(anim.name), _summarise_dropped("animation", names)))

    # --- model and animations ---

    def write_model(self):
        m = self.model
        core = self.core
        core.zeros(8)
        core.fixed_str(m.name, 64)
        root_ptr_off = core.placeholder()
        count_nodes_off = core.placeholder()
        core.zeros(28)
        core.u32(2)  # type: geometry

        core.zeros(2)
        w = _plural_classification_warning(m.classification)
        if w is not None and self.warn is not None:
            self.warn(w)
        core.u8(_classification_code(m.classification))
        core.u8(0 if m.ignore_fog != 0 else 1)  # affected by fog
        core.zeros(4)
        anim_list = [core.placeholder(), core.placeholder(), core.placeholder()]
        core.zeros(4)
        bmin, bmax, radius = model_bounds(m)
        core.vec3(bmin)
        core.vec3(bmax)
        core.f32(radius)
        core.f32(m.animation_scale)
        sup = m.super_model
        if sup == "" or equal_fold(sup, "NULL"):
            sup = "NULL"
        core.fixed_str(sup, 64)

        anims = m.animations
        anim_array_off = core.len()
        anim_ptrs = [core.placeholder() for _ in anims]
        if anims:
            core.patch_u32(anim_list[0], anim_array_off)
            core.patch_u32(anim_list[1], len(anims))
            core.patch_u32(anim_list[2], len(anims))

        node_count = 0
        root = m.root_node()
        if root is not None:
            root_off, node_count = self.write_node(root)
            core.patch_u32(root_ptr_off, root_off)
        if self.node_count_override > 0:
            node_count = self.node_count_override
        core.patch_u32(count_nodes_off, node_count)

        for i, anim in enumerate(anims):
            core.patch_u32(anim_ptrs[i], self.write_animation(anim))

    def write_animation(self, anim):
        core = self.core
        anim_off = core.len()
        core.zeros(8)
        core.fixed_str(anim.name, 64)
        root_ptr_off = core.placeholder()
        count_nodes_off = core.placeholder()
        core.zeros(28)
        core.u32(1)  # type: animation

        core.f32(anim.length)
        core.f32(anim.trans_time)
        core.fixed_str(anim.root if anim.root != "" else self.model.name, 64)
        evt = [core.placeholder(), core.placeholder(), core.placeholder()]
        if anim.events:
            core.patch_u32(evt[0], core.len())
            core.patch_u32(evt[1], len(anim.events))
            core.patch_u32(evt[2], len(anim.events))
            for t, name in anim.events:
                core.f32(t)
                core.fixed_str(name, 32)

        _, anim_children = resolve_anim_tree(anim.nodes)
        self.anim_geom_pair = pair_anim_nodes_to_geom(anim.nodes, self.geom_occur)

        count = 0
        root_off = 0
        root_an = None
        for an in anim.nodes:
            if is_root_parent(an.parent):
                root_an = an
                break
        if root_an is None and anim.nodes:
            root_an = anim.nodes[0]
        if root_an is not None:
            visited = set()
            root_off, count = self.write_anim_tree(root_an, anim_children, visited)
            self.warn_dropped_anim(anim, visited)
        core.patch_u32(root_ptr_off, root_off)
        core.patch_u32(count_nodes_off, count)
        return anim_off

    def write_anim_tree(self, root, anim_children, visited):
        """writeAnimNode over the tree, iteratively in Go's recursion order."""
        core = self.core
        count = 0
        root_off = 0
        stack = [(root, 0, -1)]
        while stack:
            an, parent_off, patch_at = stack.pop()
            if an in visited:
                off = 0
            else:
                visited.add(an)
                off = core.len()
                count += 1
                children = self._write_one_anim_node(an, anim_children, parent_off)
                first = core.len() - 4 * len(children)
                for i in range(len(children) - 1, -1, -1):
                    stack.append((children[i], off, first + 4 * i))
            if patch_at >= 0:
                core.patch_u32(patch_at, off)
            else:
                root_off = off
        return root_off, count

    def _write_one_anim_node(self, an, anim_children, parent_off):
        core = self.core
        geom = self.anim_geom_pair.get(an)
        if geom is None:
            geom = self.geom_node_by_name(an.name)
        content = 1
        has_light = has_emitter = has_ref = has_mesh = has_skin = has_anim_mesh = False
        has_dangly = has_aabb = mesh_ctrl_only = False
        if geom is not None:
            gf = geom.node_type_flag()
            if gf & 0x02:
                has_light = True
                content |= 0x02
            if gf & 0x04:
                has_emitter = True
                content |= 0x04
            if gf & 0x10:
                has_ref = True
                content |= 0x10
            if gf & 0x80 or an.anim_mesh is not None:
                if gf & 0x20:
                    has_mesh = True
                    content |= 0x20
                if gf & 0x40:
                    has_skin = True
                    content |= 0x40
                if gf & 0x80:
                    has_anim_mesh = True
                    content |= 0x80
                if gf & 0x100:
                    has_dangly = True
                    content |= 0x100
                if gf & 0x200:
                    has_aabb = True
                    content |= 0x200
            elif gf & 0x20 and (an.float_keys.get("alpha") or an.color_keys.get("selfillumcolor")):
                content |= 0x20
                mesh_ctrl_only = True

        core.zeros(24)
        core.i32(0)
        core.i32(self.node_ids.get(geom, 0) if geom is not None else 0)
        core.fixed_str(an.name, 32)
        core.i32(0)
        core.i32(parent_off)
        child_ptr_pos = core.placeholder()
        child_num_pos = core.placeholder()
        child_alc_pos = core.placeholder()
        ctrl = [core.placeholder() for _ in range(6)]
        core.u32(content)

        if has_light and geom is not None:
            self.write_light_header(geom)
        elif has_light:
            core.zeros(92)
        if has_emitter and geom is not None:
            self.write_emitter_header(geom)
        elif has_emitter:
            core.zeros(216)
        if has_ref and geom is not None:
            self.write_reference_header(geom)
        elif has_ref:
            core.zeros(68)

        expanded = None
        if has_mesh and geom is not None:
            self.write_mesh_header_for_anim_node(geom, an)
            expanded = self.last_expanded
        elif has_mesh or mesh_ctrl_only:
            self.write_empty_mesh_header()

        if has_skin and geom is not None:
            self.write_skin_header(geom, expanded)
        elif has_skin:
            core.zeros(100)

        av_pos = atv_pos = 0
        n_vsets = n_tvsets = 0
        if has_anim_mesh:
            av_pos, atv_pos, n_vsets, n_tvsets = self.write_anim_mesh_header(geom, an, expanded)

        if has_dangly and geom is not None:
            self.write_dangly_header(geom)
        elif has_dangly:
            core.zeros(24)
        if has_aabb:
            core.zeros(4)

        if has_mesh and expanded is not None and geom is not None:
            self.write_mesh_face_data(an.mesh if an.mesh is not None else geom.mesh, expanded)
        if has_dangly and geom is not None:
            self.write_dangly_constraints(geom)

        if has_anim_mesh and expanded is not None:
            mesh = an.mesh if an.mesh is not None else geom.mesh
            am = anim_mesh_source(geom, an)
            if am is not None:
                self.write_anim_mesh_data(mesh, am, expanded, av_pos, atv_pos, n_vsets, n_tvsets)

        node_flag = content
        if has_light:
            node_flag = 3
        elif has_emitter:
            node_flag = 5
        elif has_mesh or mesh_ctrl_only:
            node_flag = 33
        keys, data = self.encode_anim_node_controllers(an, node_flag)
        self.write_ctrl_block(keys, data, *ctrl)

        children = anim_children.get(an, [])
        if children:
            core.patch_u32(child_ptr_pos, core.len())
        core.patch_u32(child_num_pos, len(children))
        core.patch_u32(child_alc_pos, len(children))
        core.zeros(4 * len(children))
        return children

    def write_mesh_header_for_anim_node(self, geom, an):
        """Only animmesh nodes carry geometry in an animation; others get a
        well-formed empty mesh header."""
        if (an is not None and an.anim_mesh is not None) or (geom is not None and geom.anim_mesh is not None):
            mesh = geom.mesh
            if an is not None and an.mesh is not None:
                mesh = an.mesh
            if mesh is None:
                self.write_empty_mesh_header()
                return
            self.merge_anim = anim_mesh_source(geom, an)
            self.write_mesh_header_inner(mesh, geom)
            self.merge_anim = None
            return
        self.write_empty_mesh_header()

    def write_empty_mesh_header(self):
        """A geometry-free but well-formed 512-byte mesh header: -1 in every
        optional MDX pointer (a zeroed header points every stream at MDX 0)."""
        core = self.core
        start = core.len()
        core.zeros(8)
        core.proxy_list_empty()
        core.zeros(40)  # bmin, bmax, radius, centre
        core.vec3((f32(0.8), f32(0.8), f32(0.8)))
        core.vec3((f32(0.2), f32(0.2), f32(0.2)))
        core.zeros(12)
        core.f32(1.0)
        core.i32(1)
        core.i32(0)
        core.i32(1)
        core.i32(0)
        core.u32(0)
        core.zeros(256)
        core.i32(0)
        for _ in range(4):
            core.proxy_list_empty()
        core.i32(-1)
        core.u32(0)
        core.u32(0)
        core.i32(0)
        core.i32(0)
        core.u16(0)
        core.u16(0)
        for v in (-1, -1, -1, -1, 0, -1, -1, -1, -1, -1, -1, -1):
            core.i32(v)
        core.u8(0)
        core.u8(0)
        core.zeros(2)
        core.f32(0.0)
        core.u32(0)
        got = core.len() - start
        if got != MESH_HEADER_SIZE and self.err is None:
            self.err = CompileError("empty mesh header wrote %d bytes, want %d" % (got, MESH_HEADER_SIZE))

    def write_anim_mesh_header(self, geom, an, exp):
        """The 56-byte animmesh sub-header; returns (verts ptr pos, tverts ptr
        pos, vertex sets, tvertex sets). The sets are counted over the mesh the
        node is written with, and are 0 without exp."""
        core = self.core
        am = anim_mesh_source(geom, an)
        sp = am.sample_period if am is not None else 0.0
        if an is not None and an.mesh is not None:
            mesh = an.mesh
        else:
            mesh = geom.mesh if geom is not None else None
        n_vsets = n_tvsets = 0
        if am is not None and mesh is not None and exp is not None:
            n_vsets = anim_sets(am.anim_verts, len(mesh.verts))
            n_tvsets = anim_sets(am.anim_tverts, len(mesh.tverts))
        core.f32(sp)
        core.zeros(36)
        av_pos = core.placeholder()
        atv_pos = core.placeholder()
        core.i32(n_vsets)
        core.i32(n_tvsets)
        return av_pos, atv_pos, n_vsets, n_tvsets

    def write_anim_mesh_data(self, mesh, am, exp, av_pos, atv_pos, n_vsets, n_tvsets):
        """writeAnimMeshData: the sets, vertex-major over exp's GPU vertices."""
        core = self.core
        if n_vsets > 0:
            core.patch_u32(av_pos, core.len())
            core.f32s([c for v in anim_vertex_major(am.anim_verts, len(mesh.verts), exp.orig_vert)
                       for c in (v[0], v[1], v[2])])
        if n_tvsets > 0:
            core.patch_u32(atv_pos, core.len())
            core.f32s([c for v in anim_vertex_major(am.anim_tverts, len(mesh.tverts), exp.orig_tvert)
                       for c in (v[0], v[1])])


def anim_mesh_source(geom, an):
    """animMeshSource: the animation node's sets, else the geometry node's."""
    if an is not None and an.anim_mesh is not None:
        return an.anim_mesh
    return geom.anim_mesh if geom is not None else None


_CLASSIFICATION_CODES = {"EFFECT": 1, "EFFECTS": 1, "TILE": 2, "CHARACTER": 4, "DOOR": 8}


def _classification_code(cls):
    """types.go ClassificationToCode: the game's words, compared without case."""
    return _CLASSIFICATION_CODES.get(go_upper(cls), 0)


def _plural_classification_warning(cls):
    """types.go pluralClassificationWarning, or None."""
    u = go_upper(cls)
    if u == "EFFECTS" or not u.endswith("S") or _classification_code(u[:-1]) == 0:
        return None
    return "classification %s is not one of the game's words; it compiles as no classification (0), not %s" % (
        go_quote(cls), u[:-1])


def _i32(v):
    v &= 0xFFFFFFFF
    return v - 0x100000000 if v >= 0x80000000 else v
