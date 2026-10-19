"""Controller encoding: a port of pkg/mdl/compiler_controllers.go.

Each controller's times are followed directly by its own values in one shared
float array (DataStart == TimeStart + ValueCount), as BioWare's compiler lays
them out. Geometry nodes store one key at t=0 per controller.
"""

import struct

from .compiler_mesh import CompileError
from .compiler_tree import is_root_parent
from .controllers import BIOWARE_EMITTER, NODE
from .types import EMITTER_COLOR_FIELDS, EMITTER_FLOAT_FIELDS
from .gomath import go_quote
from .vec import axis_angle_to_quat

_KEY = struct.Struct("<I3HBB")

# controllers.go emitterNameToID: the emitter controller IDs by name.
_EMITTER_ID = {name: tid for (tid, flag), (name, _) in NODE.items() if flag == 5}
_EMITTER_ID.update({name: tid for tid, (name, _) in BIOWARE_EMITTER.items()})
_EMITTER_FLOATS_SORTED = tuple(sorted(EMITTER_FLOAT_FIELDS))
_EMITTER_COLORS_SORTED = tuple(sorted(EMITTER_COLOR_FIELDS))

# encodeAnimNodeControllers' emitter controllers, in the order Go adds them:
# (kind, type ID, controller name); kind "f" float, "c" colour, "d" detonate.
_ANIM_EMITTER = (
    ("f", 84, "alphastart"), ("f", 448, "alphamid"), ("f", 80, "alphaend"), ("f", 88, "birthrate"),
    ("f", 204, "blurlength"), ("f", 92, "bounce_co"), ("c", 108, "colorstart"), ("c", 452, "colormid"),
    ("c", 96, "colorend"), ("f", 120, "combinetime"), ("d", 228, "detonate"), ("f", 124, "drag"),
    ("f", 128, "fps"), ("f", 136, "framestart"), ("f", 132, "frameend"), ("f", 140, "grav"),
    ("f", 144, "lifeexp"), ("f", 208, "lightningdelay"), ("f", 212, "lightningradius"),
    ("f", 216, "lightningscale"), ("f", 220, "lightningsubdiv"), ("f", 148, "mass"),
    ("f", 152, "p2p_bezier2"), ("f", 156, "p2p_bezier3"), ("f", 160, "particlerot"),
    ("f", 464, "percentstart"), ("f", 465, "percentmid"), ("f", 466, "percentend"), ("f", 164, "randvel"),
    ("f", 168, "sizestart"), ("f", 468, "sizemid"), ("f", 172, "sizeend"), ("f", 176, "sizestart_y"),
    ("f", 472, "sizemid_y"), ("f", 180, "sizeend_y"), ("f", 184, "spread"), ("f", 188, "threshold"),
    ("f", 192, "velocity"), ("f", 196, "xsize"), ("f", 200, "ysize"),
)

def _lightning_subdiv_warning(kind, node):
    """compiler_controllers.go lightningSubDivWarning: the game compiles no lightningsubdiv."""
    return "%s %s: lightningsubdiv is not compiled; the game reads no such keyword" % (kind, go_quote(node))


_ZERO6 = (0.0, 0.0, 0.0, 0.0, 0.0, 0.0)


class ControllersMixin:
    """The compiler's controller encoders (compiler_controllers.go)."""

    def write_ctrl_block(self, keys, data, keys_ptr, keys_num, keys_alc, data_ptr, data_num, data_alc):
        """writeCtrlBlock: the key records, then the shared float array."""
        if not keys:
            return
        core = self.core
        keys_off = core.len()
        core.raw(b"".join(_KEY.pack(t & 0xFFFFFFFF, n & 0xFFFF, ts & 0xFFFF, ds & 0xFFFF, cols & 0xFF, 0)
                          for t, n, ts, ds, cols in keys))
        data_off = core.len()
        core.f32s(data)
        core.patch_u32(keys_ptr, keys_off)
        core.patch_u32(keys_num, len(keys))
        core.patch_u32(keys_alc, len(keys))
        core.patch_u32(data_ptr, data_off)
        core.patch_u32(data_num, len(data))
        core.patch_u32(data_alc, len(data))

    def encode_geom_node_controllers(self, n):
        keys = []
        data = []

        def add(type_id, cols, vals):
            if not vals:
                return
            if len(data) + 1 + len(vals) >= 65535:
                if self.err is None:
                    self.err = CompileError("geometry controller data exceeds uint16 index limit")
                return
            ts = len(data)
            data.append(0.0)
            ds = len(data)
            data.extend(vals)
            keys.append((type_id, 1, ts, ds, cols))

        declared = n.declared
        if not is_root_parent(n.parent):
            add(8, 3, list(n.position[:3]))
            add(20, 4, list(axis_angle_to_quat(n.orientation)))
        if n.scale != 1 or "scale" in declared:
            add(36, 1, [n.scale])

        mesh = n.mesh
        if mesh is not None:
            sic = mesh.self_illum_color
            if sic[0] != 0 or sic[1] != 0 or sic[2] != 0 or "selfillumcolor" in declared:
                add(100, 3, list(sic[:3]))
            if mesh.alpha != 1 or "alpha" in declared:
                add(128, 1, [mesh.alpha])

        l = n.light
        if l is not None:
            col = l.color
            if col[0] != 0 or col[1] != 0 or col[2] != 0 or "color" in declared:
                add(76, 3, list(col[:3]))
            if l.radius != 0 or "radius" in declared:
                add(88, 1, [l.radius])
            if l.multiplier != 0 or "multiplier" in declared:
                add(140, 1, [l.multiplier])
            if l.shadow_radius != 0 or "shadowradius" in declared:
                add(96, 1, [l.shadow_radius])
            if l.vertical_displacement != 0 or "verticaldisplacement" in declared:
                add(100, 1, [l.vertical_displacement])

        em = n.emitter
        if em is not None:
            for name in _EMITTER_FLOATS_SORTED:
                v = em.floats[name]
                if v == 0 and name not in declared:
                    continue
                if name == "lightningsubdiv":
                    if self.warn is not None:
                        self.warn(_lightning_subdiv_warning("emitter", n.name))
                    continue
                tid = _EMITTER_ID.get(name, 0)
                if tid:
                    add(tid, 1, [v])
            for name in _EMITTER_COLORS_SORTED:
                v = em.colors[name]
                if v[0] == 0 and v[1] == 0 and v[2] == 0 and name not in declared:
                    continue
                tid = _EMITTER_ID.get(name, 0)
                if tid:
                    add(tid, 3, list(v[:3]))
        return keys, data

    def encode_anim_node_controllers(self, an, node_flag):
        keys = []
        data = []

        def overflow(k):
            if len(data) + k > 65535:
                if self.err is None:
                    self.err = CompileError("animation controller data exceeds uint16 index limit (%d entries)"
                                            % (len(data) + k))
                return True
            return False

        def add_rows(type_id, times, rows, width, cols):
            n = len(times)
            if n == 0 or overflow(n + width * n):
                return
            ts = len(data)
            data.extend(times)
            ds = len(data)
            for r in rows:
                data.extend(r)
            keys.append((type_id, n, ts, ds, cols))

        # a bezier controller's keys are the value then its two handles, with
        # 0x10 on the column count (linear keys in it have zero handles)
        def add_float(type_id, name):
            kfs = an.float_keys.get(name)
            if not kfs:
                return
            if name in an.bezier:
                add_rows(type_id, [k[0] for k in kfs],
                         [(k[1], k[2], k[3]) if len(k) > 2 else (k[1], 0.0, 0.0) for k in kfs], 3, 1 | 0x10)
            else:
                add_rows(type_id, [k[0] for k in kfs], [(k[1],) for k in kfs], 1, 1)

        def add_color(type_id, name):
            kfs = an.color_keys.get(name)
            if not kfs:
                return
            if name in an.bezier:
                add_rows(type_id, [k[0] for k in kfs],
                         [tuple(k[1][:3]) + (tuple(k[2][:3]) + tuple(k[3][:3]) if len(k) > 2 else _ZERO6)
                          for k in kfs], 9, 3 | 0x10)
            else:
                add_rows(type_id, [k[0] for k in kfs], [k[1][:3] for k in kfs], 3, 3)

        fk = an.float_keys

        pk = an.position_keys
        if pk:
            if an.position_bezier:
                add_rows(8, [k[0] for k in pk], [tuple(k[1][:3]) + tuple(k[2][:3]) + tuple(k[3][:3]) for k in pk],
                         9, 3 | 0x10)
            else:
                add_rows(8, [k[0] for k in pk], [k[1][:3] for k in pk], 3, 3)
        ok = an.orientation_keys
        if ok:
            add_rows(20, [k[0] for k in ok], [axis_angle_to_quat(k[1]) for k in ok], 4, 4)
        add_float(36, "scale")

        if node_flag & 0x20 != 0:
            add_color(100, "selfillumcolor")
            add_float(128, "alpha")

        if node_flag == 3:
            add_color(76, "color")
            add_float(88, "radius")
            add_float(140, "multiplier")
            add_float(96, "shadowradius")
            add_float(100, "verticaldisplacement")

        if node_flag == 5:
            for kind, tid, name in _ANIM_EMITTER:
                if name == "lightningsubdiv":
                    if an.float_keys.get(name) and self.warn is not None:
                        self.warn(_lightning_subdiv_warning("animation node", an.name))
                elif kind == "f":
                    add_float(tid, name)
                elif kind == "c":
                    add_color(tid, name)
                else:
                    kfs = fk.get(name)
                    n = len(kfs) if kfs else 0
                    if n == 0 or overflow(n):
                        continue
                    ts = len(data)
                    data.extend(k[0] for k in kfs)
                    keys.append((228, n, ts, len(data), 0))
        return keys, data
