"""ASCII MDL writer: a port of pkg/mdl/writer.go (output_models.pl ordering)."""

from .controllers import EMITTER_DECLARED_NAMES
from .f32 import fmt_float, fmt_vec3
from .types import ANIM_COLOR_CONTROLLERS, ANIM_FLOAT_CONTROLLERS, VEC3_ZERO

MAX_NODE_DEPTH = 10000

_CAMEL = {
    "fountain": "Fountain", "single": "Single", "explosion": "Explosion", "lightning": "Lightning",
    "normal": "Normal", "linked": "Linked", "billboard_to_local_z": "Billboard_to_Local_Z",
    "billboard_to_world_z": "Billboard_to_World_Z", "aligned_to_world_z": "Aligned_to_World_Z",
    "aligned_to_particle_dir": "Aligned_to_Particle_Dir", "motion_blur": "Motion_Blur",
    "punch-through": "Punch-Through", "lighten": "Lighten", "trail": "Trail", "bezier": "Bezier",
    "gravity": "Gravity", "dummy": "Dummy",
}


def camel_case(s):
    return _CAMEL.get(s.lower(), s)


def write(model):
    """Return the model as ASCII MDL text (writer.go Write)."""
    w = _Writer(model.use_texture0)
    w.raw("# Rewritten by cleanmodels\n")
    if model.file_dependancy:
        w.raw("filedependancy %s\n" % model.file_dependancy)
    w.raw("#MAXMODEL ASCII\n")
    w.raw("newmodel %s\n" % model.name)
    w.write_model_header(model)
    w.raw("#MAXGEOM ASCII\n")
    w.raw("beginmodelgeom %s\n" % model.name)
    w.write_node_tree(model)
    w.raw("endmodelgeom %s\n" % model.name)
    for anim in model.animations:
        w.raw("\n")
        w.write_animation(model, anim)
    w.raw("donemodel %s\n" % model.name)
    return "".join(w.out)


def write_file(model, path):
    with open(path, "w", encoding="latin-1", newline="\n") as f:
        f.write(write(model))


def _uint64(v):
    return v + (1 << 64) if v < 0 else v


class _Writer:
    def __init__(self, use_texture0):
        self.out = []
        self.use_texture0 = use_texture0

    def raw(self, s):
        self.out.append(s)

    def indent(self, level, s):
        self.out.append("  " * level + s + "\n")

    def write_model_header(self, m):
        if m.classification:
            self.indent(4, "classification %s" % m.classification)
        self.indent(4, "setsupermodel %s %s" % (m.name, m.super_model))
        if m.ignore_fog != 0:
            self.indent(4, "ignorefog %d" % m.ignore_fog)
        self.indent(4, "setanimationscale %s" % fmt_float(m.animation_scale))

    def write_node_tree(self, m):
        root = m.root_node()
        if root is None and m.nodes:
            root = m.nodes[0]
        if root is None:
            return
        children = {}
        for n in m.nodes:
            if n is None:
                continue
            children.setdefault(n.parent.lower(), []).append(n)
        visited = set()
        # Iterative pre-order, children in declaration order (writeNodeRecursive)
        stack = [(root, 0)]
        while stack:
            node, depth = stack.pop()
            if id(node) in visited or depth > MAX_NODE_DEPTH:
                continue
            visited.add(id(node))
            self.write_node(node)
            lname = node.name.lower()
            kids = [c for c in children.get(lname, ()) if c is not node and c.parent.lower() == lname]
            for c in reversed(kids):
                stack.append((c, depth + 1))

    def write_node(self, n):
        ind = self.indent
        ind(2, "node %s %s" % (n.node_type(), n.name))
        ind(4, "parent %s" % (n.parent if n.parent != "" else "NULL"))
        if n.part_number != 0:
            ind(4, "#part-number %d" % n.part_number)
        if n.position != VEC3_ZERO:
            ind(4, "position %s" % fmt_vec3(n.position))
        o = n.orientation
        if o[0] != 0 or o[1] != 0 or o[2] != 0 or (o[3] != 0 and o[3] != 1):
            ind(4, "orientation %s %s %s %s" % (fmt_float(o[0]), fmt_float(o[1]), fmt_float(o[2]), fmt_float(o[3])))
        if n.scale != 1.0 or "scale" in n.declared:
            ind(4, "scale %s" % fmt_float(n.scale))
        if n.inherit_color != 0:
            ind(4, "inheritcolor %d" % n.inherit_color)
        if n.wire_color != VEC3_ZERO:
            ind(4, "wirecolor %s" % fmt_vec3(n.wire_color))
        if n.mesh is not None:
            self.write_mesh_data(n, n.mesh)
        if n.anim_mesh is not None:
            self.write_anim_mesh_data(n.anim_mesh)
        if n.dangly is not None:
            self.write_dangly_data(n.dangly)
        if n.skin is not None:
            self.write_skin_data(n.skin)
        if n.aabb is not None:
            self.write_aabb_data(n.aabb)
        if n.light is not None:
            self.write_light_data(n, n.light)
        if n.emitter is not None:
            self.write_emitter_data(n, n.emitter)
        if n.reference is not None:
            ind(4, "refModel %s" % n.reference.ref_model)
            ind(4, "reattachable %d" % n.reference.reattachable)
        ind(2, "endnode")

    def write_mesh_data(self, n, m):
        ind = self.indent
        ind(4, "ambient %s" % fmt_vec3(m.ambient))
        ind(4, "diffuse %s" % fmt_vec3(m.diffuse))
        ind(4, "specular %s" % fmt_vec3(m.specular))
        ind(4, "shininess %s" % fmt_float(m.shininess))
        if m.bitmap:
            ind(4, ("texture0 %s" if self.use_texture0 else "bitmap %s") % m.bitmap)
        if m.texture1:
            ind(4, "texture1 %s" % m.texture1)
        if m.texture2:
            ind(4, "texture2 %s" % m.texture2)
        if m.material_name:
            ind(4, "materialname %s" % m.material_name)
        if m.render_hint:
            ind(4, "renderhint %s" % m.render_hint)
        ind(4, "render %d" % m.render)
        ind(4, "shadow %d" % m.shadow)
        ind(4, "beaming %d" % m.beaming)
        ind(4, "transparencyhint %d" % m.transparency_hint)
        if m.alpha != 1.0 or "alpha" in n.declared:
            ind(4, "alpha %s" % fmt_float(m.alpha))
        if m.self_illum_color != VEC3_ZERO or "selfillumcolor" in n.declared:
            ind(4, "selfillumcolor %s" % fmt_vec3(m.self_illum_color))
        if m.tile_fade != 0:
            ind(4, "tilefade %d" % m.tile_fade)
        ind(4, "rotatetexture %d" % m.rotate_texture)
        if m.light_mapped != 0:
            ind(4, "lightmapped %d" % m.light_mapped)
        if m.multimaterial:
            ind(4, "multimaterial %d" % len(m.multimaterial))
            for mat in m.multimaterial:
                ind(6, mat)
        if m.verts:
            ind(4, "verts %d" % len(m.verts))
            self.vec3_lines(6, m.verts)
        self.write_faces(4, m.faces)
        if m.mirror_list:
            ind(4, "mirrorlist %d" % len(m.mirror_list))
            for v in m.mirror_list:
                ind(6, "%d" % v)
        self.vec3_list(4, "tverts", m.tverts)
        self.vec3_list(4, "tverts1", m.tverts1)
        self.vec3_list(4, "tverts2", m.tverts2)
        self.vec3_list(4, "tverts3", m.tverts3)
        for i, ti in enumerate((m.tex_indices0, m.tex_indices1, m.tex_indices2, m.tex_indices3)):
            if ti:
                ind(4, "texindices%d %d" % (i, len(ti)))
                for v in ti:
                    ind(6, "%d %d %d" % (v[0], v[1], v[2]))
        if m.colors:
            ind(4, "colors %d" % len(m.colors))
            self.vec3_lines(6, m.colors)
        self.vec3_list(4, "normals", m.normals)
        if m.tangents:
            ind(4, "tangents %d" % len(m.tangents))
            pre = "  " * 6
            self.out.extend("%s%s %s %s %s\n" % (pre, fmt_float(t[0]), fmt_float(t[1]), fmt_float(t[2]),
                                                  fmt_float(t[3])) for t in m.tangents)

    def write_skin_data(self, s):
        if s.weights:
            self.indent(4, "weights %d" % len(s.weights))
            for wt in s.weights:
                parts = []
                for bone, weight in zip(wt.bones, wt.weights):
                    parts.append(bone)
                    parts.append(fmt_float(weight))
                self.indent(6, " ".join(parts) if parts else "_ 0")

    def write_dangly_data(self, d):
        ind = self.indent
        ind(4, "displacement %s" % fmt_float(d.displacement))
        ind(4, "tightness %s" % fmt_float(d.tightness))
        ind(4, "period %s" % fmt_float(d.period))
        if d.displ_type != 0:
            ind(4, "displtype %d" % d.displ_type)
        if d.constraints:
            ind(4, "constraints %d" % len(d.constraints))
            for c in d.constraints:
                ind(6, fmt_float(c))

    def write_anim_mesh_data(self, am):
        ind = self.indent
        ind(4, "sampleperiod %s" % fmt_float(am.sample_period))
        if am.clip_u != 0 or am.clip_v != 0 or am.clip_w != 0 or am.clip_h != 0:
            ind(4, "clipu %s" % fmt_float(am.clip_u))
            ind(4, "clipv %s" % fmt_float(am.clip_v))
            ind(4, "clipw %s" % fmt_float(am.clip_w))
            ind(4, "cliph %s" % fmt_float(am.clip_h))
        self.vec3_list(4, "animverts", am.anim_verts)
        self.vec3_list(4, "animtverts", am.anim_tverts)

    def write_aabb_data(self, a):
        if a.entries:
            self.out.append(self._aabb_line(4, "aabb ", a.entries[0]))
            self.out.extend(self._aabb_line(6, "", e) for e in a.entries[1:])

    @staticmethod
    def _aabb_line(level, lead, e):
        return "%s%s%s %s %d\n" % ("  " * level, lead, fmt_vec3(e.bound_min), fmt_vec3(e.bound_max), e.leaf_face)

    def write_light_data(self, n, l):
        ind = self.indent
        if l.color != VEC3_ZERO or "color" in n.declared:
            ind(4, "color %s" % fmt_vec3(l.color))
        if l.radius != 0 or "radius" in n.declared:
            ind(4, "radius %s" % fmt_float(l.radius))
        if l.multiplier != 0 or "multiplier" in n.declared:
            ind(4, "multiplier %s" % fmt_float(l.multiplier))
        if l.shadow_radius != 0 or "shadowradius" in n.declared:
            ind(4, "shadowradius %s" % fmt_float(l.shadow_radius))
        if l.vertical_displacement != 0 or "verticaldisplacement" in n.declared:
            ind(4, "verticaldisplacement %s" % fmt_float(l.vertical_displacement))
        ind(4, "ambientonly %d" % l.ambient_only)
        ind(4, "nDynamicType %d" % l.n_dynamic_type)
        ind(4, "affectdynamic %d" % l.affect_dynamic)
        ind(4, "shadow %d" % l.shadow)
        ind(4, "lightpriority %d" % l.light_priority)
        ind(4, "fadinglight %d" % l.fading_light)
        if l.negative_light != 0:
            ind(4, "negativelight %d" % l.negative_light)
        if l.generate_flare != 0:
            ind(4, "generateflare %d" % l.generate_flare)
        if l.flare_radius != 0:
            ind(4, "flareradius %s" % fmt_float(l.flare_radius))
        if l.lens_flares != 0:
            ind(4, "lensflares %d" % l.lens_flares)
        if l.texture_names:
            ind(4, "texturenames %d" % len(l.texture_names))
            for t in l.texture_names:
                ind(6, t)
        if l.flare_sizes:
            ind(4, "flaresizes %d" % len(l.flare_sizes))
            for s in l.flare_sizes:
                ind(6, fmt_float(s))
        if l.flare_positions:
            ind(4, "flarepositions %d" % len(l.flare_positions))
            for p in l.flare_positions:
                ind(6, fmt_float(p))
        if l.flare_color_shifts:
            ind(4, "flarecolorshifts %d" % len(l.flare_color_shifts))
            for c in l.flare_color_shifts:
                ind(6, fmt_vec3(c))
        if l.shadow_radius != 0:
            ind(4, "shadowradius %s" % fmt_float(l.shadow_radius))
        if l.vertical_displacement != 0:
            ind(4, "verticaldisplacement %s" % fmt_float(l.vertical_displacement))

    def write_emitter_data(self, n, e):
        ind = self.indent
        ind(4, "deadspace %s" % fmt_float(e.dead_space))
        ind(4, "blastRadius %s" % fmt_float(e.blast_radius))
        ind(4, "blastLength %s" % fmt_float(e.blast_length))
        ind(4, "xgrid %d" % e.x_grid)
        ind(4, "ygrid %d" % e.y_grid)
        ind(4, "spawntype %d" % e.spawn_type)
        ind(4, "update %s" % camel_case(e.update))
        ind(4, "render %s" % camel_case(e.render))
        ind(4, "blend %s" % camel_case(e.blend))
        if e.texture:
            ind(4, "texture %s" % e.texture)
        if e.chunk_name:
            ind(4, "chunkName %s" % e.chunk_name)
        ind(4, "twosidedtex %d" % e.two_sided_tex)
        ind(4, "loop %d" % e.loop)
        ind(4, "renderorder %d" % e.render_order)
        ind(4, "p2p %d" % e.p2p)
        ind(4, "p2p_sel %d" % e.p2p_sel)
        ind(4, "affectedByWind %d" % e.affected_by_wind)
        ind(4, "m_isTinted %d" % e.is_tinted)
        ind(4, "bounce %d" % e.bounce)
        ind(4, "random %d" % e.random)
        ind(4, "inherit %d" % e.inherit)
        ind(4, "inheritvel %d" % e.inherit_vel)
        ind(4, "inherit_local %d" % e.inherit_local)
        ind(4, "splat %d" % e.splat)
        ind(4, "inherit_part %d" % e.inherit_part)

        def cf(label):
            key = label.lower()
            v = e.floats[key]
            if key in EMITTER_DECLARED_NAMES and v == 0 and key not in n.declared:
                return
            ind(4, "%s %s" % (label, fmt_float(v)))

        def cv(label):
            key = label.lower()
            v = e.colors[key]
            if key in EMITTER_DECLARED_NAMES and v == VEC3_ZERO and key not in n.declared:
                return
            ind(4, "%s %s" % (label, fmt_vec3(v)))

        for label in ("alphaStart", "alphaMid", "alphaEnd"):
            cf(label)
        for label in ("colorStart", "colorMid", "colorEnd"):
            cv(label)
        for label in ("sizeStart", "sizeMid", "sizeEnd", "sizeStart_y", "sizeMid_y", "sizeEnd_y", "birthrate",
                      "lifeExp", "mass", "spread", "particleRot", "velocity", "randvel", "bounce_co",
                      "blurlength", "fps", "frameStart", "frameEnd", "grav", "drag", "threshold", "combinetime",
                      "percentStart", "percentMid", "percentEnd", "lightningDelay", "lightningRadius",
                      "lightningScale"):
            cf(label)
        if e.floats["lightningsubdiv"] != 0:
            cf("lightningSubDiv")
        for label in ("p2p_bezier2", "p2p_bezier3", "xsize", "ysize"):
            cf(label)

    def write_animation(self, m, anim):
        self.raw("#MAXANIM ASCII\n")
        self.raw("newanim %s %s\n" % (anim.name, m.name))
        self.indent(4, "length %s" % fmt_float(anim.length))
        self.indent(4, "transtime %s" % fmt_float(anim.trans_time))
        for t, name in anim.events:
            self.indent(4, "event %s %s" % (fmt_float(t), name))
        if anim.root:
            self.indent(4, "animroot %s" % anim.root)
        for an in anim.nodes:
            self.write_anim_node(m, an)
        self.raw("doneanim %s %s\n" % (anim.name, m.name))

    def write_anim_node(self, m, an):
        ind = self.indent
        geom = m.find_node(an.name)
        ind(4, "node %s %s" % (geom.node_type() if geom is not None else "dummy", an.name))
        ind(6, "parent %s" % an.parent)
        if an.mesh is not None:
            self.vec3_list(6, "verts", an.mesh.verts)
            self.write_faces(6, an.mesh.faces)
            self.vec3_list(6, "tverts", an.mesh.tverts)
        if an.anim_mesh is not None:
            am = an.anim_mesh
            if am.clip_u != 0 or am.clip_v != 0 or am.clip_w != 0 or am.clip_h != 0:
                ind(6, "clipu %s" % fmt_float(am.clip_u))
                ind(6, "clipv %s" % fmt_float(am.clip_v))
                ind(6, "clipw %s" % fmt_float(am.clip_w))
                ind(6, "cliph %s" % fmt_float(am.clip_h))
            if am.sample_period != 0:
                ind(6, "sampleperiod %s" % fmt_float(am.sample_period))
            self.vec3_list(6, "animverts", am.anim_verts)
            self.vec3_list(6, "animtverts", am.anim_tverts)
        self.write_position_keys(an.position_keys, an.position_bezier)
        if an.orientation_keys:
            ind(6, "orientationkey %d" % len(an.orientation_keys))
            pre = "  " * 3
            self.out.extend("%s%s %s %s %s %s\n" % (pre, fmt_float(t), fmt_float(v[0]), fmt_float(v[1]),
                                                     fmt_float(v[2]), fmt_float(v[3]))
                            for t, v in an.orientation_keys)
            ind(6, "endlist")
        for name in ANIM_FLOAT_CONTROLLERS:
            keys = an.float_keys.get(name)
            if not keys:
                continue
            if name == "detonate":
                ind(6, "detonatekey %d" % len(keys))
                for t, _ in keys:
                    ind(3, fmt_float(t))
            else:
                ind(6, "%skey %d" % (name, len(keys)))
                pre = "  " * 3
                self.out.extend("%s%s %s\n" % (pre, fmt_float(t), fmt_float(v)) for t, v in keys)
            ind(6, "endlist")
        for name in ANIM_COLOR_CONTROLLERS:
            keys = an.color_keys.get(name)
            if not keys:
                continue
            ind(6, "%skey %d" % (name, len(keys)))
            pre = "  " * 3
            self.out.extend("%s%s %s\n" % (pre, fmt_float(t), fmt_vec3(v)) for t, v in keys)
            ind(6, "endlist")
        ind(4, "endnode")

    def write_position_keys(self, keys, bezier):
        if not keys:
            return
        pre = "  " * 3
        if bezier:
            self.indent(6, "positionbezierkey %d" % len(keys))
            self.out.extend("%s%s %s %s %s\n" % (pre, fmt_float(k[0]), fmt_vec3(k[1]), fmt_vec3(k[2]),
                                                  fmt_vec3(k[3])) for k in keys)
        else:
            self.indent(6, "positionkey %d" % len(keys))
            self.out.extend("%s%s %s\n" % (pre, fmt_float(k[0]), fmt_vec3(k[1])) for k in keys)
        self.indent(6, "endlist")

    def write_faces(self, level, faces):
        if not faces:
            return
        self.indent(level, "faces %d" % len(faces))
        pre = "  " * (level + 2)
        self.out.extend("%s%d %d %d %d %d %d %d %d\n" % (pre, f.verts[0], f.verts[1], f.verts[2],
                                                          _uint64(f.smooth_group), f.uvs[0], f.uvs[1], f.uvs[2],
                                                          f.material) for f in faces)

    def vec3_lines(self, level, vecs):
        pre = "  " * level
        self.out.extend("%s%s\n" % (pre, fmt_vec3(v)) for v in vecs)

    def vec3_list(self, level, name, vecs):
        if not vecs:
            return
        self.indent(level, "%s %d" % (name, len(vecs)))
        self.vec3_lines(level + 2, vecs)
