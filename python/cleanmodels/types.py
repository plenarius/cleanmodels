"""Model types: a port of pkg/mdl/types.go.

Every float is a float32 value held in a Python float (exactly representable).
Vectors are tuples: Vec3 (x, y, z), Vec4 (x, y, z, w).
"""

from .gostr import equal_fold

VEC3_ZERO = (0.0, 0.0, 0.0)


class Face:
    __slots__ = ("verts", "smooth_group", "uvs", "material", "normal", "plane_d")

    def __init__(self, verts=(0, 0, 0), smooth_group=0, uvs=(0, 0, 0), material=0,
                 normal=VEC3_ZERO, plane_d=0.0):
        self.verts = list(verts)
        self.smooth_group = smooth_group
        self.uvs = list(uvs)
        self.material = material
        self.normal = normal
        self.plane_d = plane_d


class VertexWeight:
    __slots__ = ("bones", "weights")

    def __init__(self):
        self.bones = []
        self.weights = []


class MeshData:
    def __init__(self):
        self.verts = []
        self.faces = []
        self.normals = []
        self.corner_normals = []  # per-face-corner normals, filled in by the compiler
        self.tangents = []
        self.colors = []
        self.tverts = []
        self.tverts1 = []
        self.tverts2 = []
        self.tverts3 = []
        self.tex_indices0 = []
        self.tex_indices1 = []
        self.tex_indices2 = []
        self.tex_indices3 = []
        self.mirror_list = []
        self.diffuse = VEC3_ZERO
        self.ambient = VEC3_ZERO
        self.specular = VEC3_ZERO
        self.shininess = 0.0
        self.bitmap = ""
        self.texture1 = ""
        self.texture2 = ""
        self.material_name = ""
        self.render_hint = ""
        self.shadow = 0
        self.beaming = 0
        self.render = 1
        self.transparency_hint = 0
        self.alpha = 1.0
        self.self_illum_color = VEC3_ZERO
        self.tile_fade = 0
        self.rotate_texture = 0
        self.multimaterial = []


class SkinData:
    def __init__(self):
        self.weights = []


class DanglyData:
    def __init__(self):
        self.displacement = 0.0
        self.tightness = 0.0
        self.period = 0.0
        self.displ_type = 0
        self.constraints = []


# Emitter float properties set by controllers, keyed by controller name.
EMITTER_FLOAT_FIELDS = (
    "alphastart", "alphamid", "alphaend", "birthrate", "blurlength", "bounce_co", "combinetime",
    "drag", "fps", "framestart", "frameend", "grav", "lifeexp", "lightningdelay", "lightningradius",
    "lightningscale", "lightningsubdiv", "mass", "p2p_bezier2", "p2p_bezier3", "particlerot",
    "percentstart", "percentmid", "percentend", "randvel", "sizestart", "sizemid", "sizeend",
    "sizestart_y", "sizemid_y", "sizeend_y", "spread", "threshold", "velocity", "xsize", "ysize",
)
EMITTER_COLOR_FIELDS = ("colorstart", "colormid", "colorend")


class EmitterData:
    def __init__(self):
        self.dead_space = 0.0
        self.blast_radius = 0.0
        self.blast_length = 0.0
        self.x_grid = 0
        self.y_grid = 0
        self.spawn_type = 0
        self.update = ""
        self.render = ""
        self.blend = ""
        self.texture = ""
        self.chunk_name = ""
        self.two_sided_tex = 0
        self.loop = 0
        self.render_order = 0
        self.p2p = 0
        self.p2p_sel = 0
        self.affected_by_wind = 0
        self.is_tinted = 0
        self.bounce = 0
        self.random = 0
        self.inherit = 0
        self.inherit_vel = 0
        self.inherit_local = 0
        self.splat = 0
        self.inherit_part = 0
        # NWMax-only properties, read from ASCII but not written
        self.p2p_type = ""
        self.render_sel = 0
        self.blend_sel = 0
        self.update_sel = 0
        self.spawn_type_sel = 0
        self.opacity = 0
        self.icon_size = 0
        self.lock_axes = 0
        self.chunky = 0
        # Controller-animatable values, keyed by controller name
        self.floats = {name: 0.0 for name in EMITTER_FLOAT_FIELDS}
        self.colors = {name: VEC3_ZERO for name in EMITTER_COLOR_FIELDS}


class LightData:
    def __init__(self):
        self.flare_radius = 0.0
        self.lens_flares = 0
        self.flare_sizes = []
        self.flare_positions = []
        self.flare_color_shifts = []
        self.texture_names = []
        self.light_priority = 0
        self.ambient_only = 0
        self.n_dynamic_type = 0
        self.affect_dynamic = 0
        self.shadow = 0
        self.generate_flare = 0
        self.fading_light = 0
        self.negative_light = 0
        self.color = VEC3_ZERO
        self.radius = 0.0
        self.multiplier = 0.0
        self.shadow_radius = 0.0
        self.vertical_displacement = 0.0


class ReferenceData:
    def __init__(self):
        self.ref_model = ""
        self.reattachable = 0


class AabbEntry:
    __slots__ = ("bound_min", "bound_max", "leaf_face", "plane")

    def __init__(self, bound_min, bound_max, leaf_face, plane):
        self.bound_min = bound_min
        self.bound_max = bound_max
        self.leaf_face = leaf_face
        self.plane = plane


class AabbData:
    def __init__(self):
        self.entries = []


class AnimMeshData:
    def __init__(self):
        self.sample_period = 0.0
        self.clip_u = 0.0
        self.clip_v = 0.0
        self.clip_w = 0.0
        self.clip_h = 0.0
        self.anim_verts = []
        self.anim_tverts = []


class Node:
    def __init__(self, name="", parent=""):
        self.name = name
        self.parent = parent
        self.part_number = 0
        self.position = VEC3_ZERO
        self.orientation = (0.0, 0.0, 0.0, 0.0)
        self.scale = 1.0
        self.inherit_color = 0
        self.wire_color = VEC3_ZERO
        self.mesh = None
        self.skin = None
        self.dangly = None
        self.emitter = None
        self.light = None
        self.reference = None
        self.aabb = None
        self.anim_mesh = None
        self.camera = False
        self.declared = set()

    def node_type(self):
        """The MDL ASCII node type keyword (types.go NodeType)."""
        if self.aabb is not None:
            return "aabb"
        if self.dangly is not None:
            return "danglymesh"
        if self.skin is not None:
            return "skin"
        if self.anim_mesh is not None:
            return "animmesh"
        if self.emitter is not None:
            return "emitter"
        if self.light is not None:
            return "light"
        if self.reference is not None:
            return "reference"
        if self.camera:
            return "camera"
        if self.mesh is not None:
            return "trimesh"
        return "dummy"

    def node_type_flag(self):
        """The binary node type flag (types.go NodeTypeFlag)."""
        if self.aabb is not None:
            return 545
        if self.dangly is not None:
            return 289
        if self.anim_mesh is not None:
            return 161
        if self.skin is not None:
            return 97
        if self.mesh is not None:
            return 33
        if self.reference is not None:
            return 17
        if self.camera:
            return 9
        if self.emitter is not None:
            return 5
        if self.light is not None:
            return 3
        return 1


# Float and colour animation controllers, in the order the writer emits them
# (parser.go animControllerFloatList / animControllerColorList).
ANIM_FLOAT_CONTROLLERS = (
    "scale", "alpha", "radius", "multiplier", "shadowradius", "verticaldisplacement",
    "alphastart", "alphamid", "alphaend", "birthrate", "blurlength", "bounce_co", "combinetime",
    "drag", "fps", "framestart", "frameend", "grav", "lifeexp", "lightningdelay", "lightningradius",
    "lightningscale", "lightningsubdiv", "mass", "p2p_bezier2", "p2p_bezier3", "particlerot",
    "percentstart", "percentmid", "percentend", "randvel", "sizestart", "sizemid", "sizeend",
    "sizestart_y", "sizemid_y", "sizeend_y", "spread", "threshold", "velocity", "xsize", "ysize",
    "detonate",
)
ANIM_COLOR_CONTROLLERS = ("selfillumcolor", "color", "colorstart", "colormid", "colorend")


class AnimNode:
    def __init__(self, name="", parent=""):
        self.name = name
        self.parent = parent
        # position keys: (time, value, tan_in, tan_out)
        self.position_keys = []
        self.position_bezier = False
        self.orientation_keys = []  # (time, (x, y, z, w))
        # controller name -> [(time, value)], or (time, value, handle_in, handle_out)
        # for a controller in self.bezier; linear keys there have zero handles
        self.float_keys = {}
        self.color_keys = {}        # the same with (r, g, b) values and handles
        self.bezier = set()         # float/colour controllers read from <name>bezierkey
        self.anim_mesh = None
        self.mesh = None


class Animation:
    def __init__(self, name="", length=0.0, trans_time=0.0, root=""):
        self.name = name
        self.length = length
        self.trans_time = trans_time
        self.root = root
        self.events = []   # (time, name)
        self.nodes = []


# DecompileWarning kinds
WARN_UNKNOWN_CONTROLLER = 0
WARN_POINTER_OUT_OF_BOUNDS = 2
WARN_GENERAL = 4
WARN_TRUNCATED_DATA = 6


class DecompileWarning:
    __slots__ = ("kind", "offset", "node", "message")

    def __init__(self, kind, offset, node, message):
        self.kind = kind
        self.offset = offset
        self.node = node
        self.message = message

    def __repr__(self):
        return "DecompileWarning(%d, %d, %r, %r)" % (self.kind, self.offset, self.node, self.message)


class Model:
    def __init__(self):
        self.name = ""
        self.super_model = ""
        self.classification = ""
        self.animation_scale = 1.0
        self.ignore_fog = 0
        self.file_dependancy = ""
        self.file_type = ""
        self.use_texture0 = False
        self.nodes = []
        self.animations = []
        self.warnings = []
        self.notes = []  # Model.Notes: dropped data that is known and harmless (info)

    def find_node(self, name):
        for n in self.nodes:
            if equal_fold(n.name, name):
                return n
        return None

    def root_node(self):
        for n in self.nodes:
            if equal_fold(n.parent, "NULL"):
                return n
        return None


def classification_from_code(code):
    return {1: "EFFECT", 2: "TILE", 4: "CHARACTER", 8: "DOOR"}.get(code, "OTHER")
