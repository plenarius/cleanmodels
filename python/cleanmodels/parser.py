"""ASCII MDL parser: a port of pkg/mdl/parser.go (and declared.go's declaredName).

parse(data) reads an ASCII model into the same Model the Go parser builds,
with the same warnings ("line N: ..."), in the same order. A damaged line is
reported and skipped or zeroed, never fatal; only a file that is not a model
at all is refused (NotModelError), as Go refuses it with ErrNotModel.

Text is handled as the file's bytes: a str argument is taken as latin-1
decoded bytes (what writer.write returns), bytes as they are.
"""

from . import gostr
from .controllers import EMITTER_DECLARED_NAMES
from .f32 import fmt_float
from .gostr import parse_float32, quote, scan_float, scan_int
from .nearmatch import near_match_param
from .binary import set_static_controller
from .types import (ANIM_COLOR_CONTROLLERS, ANIM_FLOAT_CONTROLLERS, EMITTER_COLOR_FIELDS, EMITTER_FLOAT_FIELDS, AabbData, AabbEntry, AnimMeshData, AnimNode,
                    Animation, DanglyData, EmitterData, Face, LightData, MeshData, Model, Node, ReferenceData,
                    SkinData, VertexWeight)

MAX_ALLOC = 512 * 1024 * 1024
MAX_LINE = 1024 * 1024 - 1  # bufio.Scanner with a 1 MiB buffer: longer lines stop the scan
MAX_LIST_WARNINGS = 5

ANIM_FLOAT_KEYS = (
    "scale", "alpha", "radius", "multiplier", "shadowradius", "verticaldisplacement",
    "alphastart", "alphamid", "alphaend", "birthrate", "blurlength", "bounce_co", "combinetime",
    "drag", "fps", "framestart", "frameend", "grav", "lifeexp", "lightningdelay", "lightningradius",
    "lightningscale", "lightningsubdiv", "mass", "p2p_bezier2", "p2p_bezier3", "particlerot",
    "percentstart", "percentmid", "percentend", "randvel", "sizestart", "sizemid", "sizeend",
    "sizestart_y", "sizemid_y", "sizeend_y", "spread", "threshold", "velocity", "xsize", "ysize",
    "detonate",
)
ANIM_COLOR_KEYS = ("selfillumcolor", "color", "colorstart", "colormid", "colorend")
_FLOAT_KEY_TABLE = {n + "key": n for n in ANIM_FLOAT_KEYS}
_COLOR_KEY_TABLE = {n + "key": n for n in ANIM_COLOR_KEYS}
_FLOAT_CTRL = frozenset(ANIM_FLOAT_KEYS)
_COLOR_CTRL = frozenset(ANIM_COLOR_KEYS)
_EMITTER_FLOATS = frozenset(EMITTER_FLOAT_FIELDS)
_EMITTER_COLORS = frozenset(EMITTER_COLOR_FIELDS)
_LIGHT_DECLARED = frozenset(("color", "radius", "multiplier", "shadowradius", "verticaldisplacement"))

_STRUCTURE = frozenset(("newmodel", "donemodel", "beginmodelgeom", "endmodelgeom", "node", "newanim",
                        "setsupermodel"))


class ParseError:
    """A non-fatal parse issue (Go's ParseError): a warning, not an exception."""
    __slots__ = ("line", "message")

    def __init__(self, line, message):
        self.line = line
        self.message = message

    def __str__(self):
        return "line %d: %s" % (self.line, self.message)

    def __repr__(self):
        return "ParseError(%d, %r)" % (self.line, self.message)


class ParseResult:
    __slots__ = ("model", "errors")

    def __init__(self, model, errors):
        self.model = model
        self.errors = errors


class NotModelError(ValueError):
    """The input is not an ASCII MDL at all (Go's ErrNotModel)."""


def parse(data):
    """Parse ASCII MDL bytes (or a latin-1 str) into a ParseResult (parser.go Parse).

    Raises NotModelError for an empty file or one with no model structure."""
    text = data if isinstance(data, str) else data.decode("latin-1")
    lines = text.split("\n")
    if lines[-1] == "":
        lines.pop()
    read_error = None
    for i, ln in enumerate(lines):
        if len(ln) > MAX_LINE:
            del lines[i:]
            read_error = "bufio.Scanner: token too long"
            break
        if ln and ln[-1] == "\r":
            lines[i] = ln[:-1]
    p = _Parser(lines, gostr.simple_spaces(text))
    p.parse()
    if read_error is not None:
        p.errorf("read error: %s" % read_error)
    result = ParseResult(p.model, p.errors)
    if not p.saw_model:
        if not p.saw_content:
            raise NotModelError("not an ASCII MDL model: the file is empty (or holds only comments)")
        raise NotModelError('not an ASCII MDL model: no "newmodel" line (it should start with "newmodel <name>")')
    if p.model.name == "":
        p.errorf('no "newmodel" line, so the model has no name')
    return result


def parse_file(path):
    """ParseFile."""
    with open(path, "rb") as f:
        return parse(f.read())


def new_parsed_mesh_data(walkmesh):
    """newParsedMeshData: the game compiler's defaults for a parsed mesh."""
    m = MeshData()
    m.diffuse = (1.0, 1.0, 1.0)
    m.ambient = (1.0, 1.0, 1.0)
    m.shininess = 1.0
    if walkmesh:
        m.render = 0
    else:
        m.shadow = 1
    return m


# parser.go caseSensitiveWords
_CASE_SENSITIVE = frozenset(("node", "endnode", "endlist", "event", "animlength", "endmodelgeom", "doneanim",
                             "donemodel"))


def _first_anim_key(an):
    """firstAnimKey: (controller, first key's value, key count), or (None, None, 0)."""
    if an.position_keys:
        k = an.position_keys
        return "position", list(k[0][1]), len(k)
    if an.orientation_keys:
        k = an.orientation_keys
        return "orientation", list(k[0][1]), len(k)
    for name in ANIM_FLOAT_CONTROLLERS:
        k = an.float_keys.get(name)
        if k:
            return name, [k[0][1]], len(k)
    for name in ANIM_COLOR_CONTROLLERS:
        k = an.color_keys.get(name)
        if k:
            return name, list(k[0][1]), len(k)
    return None, None, 0


def new_parsed_light_data():
    """newParsedLightData: the game compiler's defaults for a parsed light."""
    l = LightData()
    l.light_priority = 5
    l.n_dynamic_type = 1
    l.affect_dynamic = 1
    l.shadow = 1
    l.fading_light = 1
    return l


def _declared_name(n, keyword):
    """declared.go declaredName, or None."""
    if keyword == "scale":
        return keyword
    if (keyword == "alpha" or keyword == "selfillumcolor") and n.mesh is not None:
        return keyword
    if n.light is not None and keyword in _LIGHT_DECLARED:
        return keyword
    if n.emitter is not None and keyword in EMITTER_DECLARED_NAMES:
        return keyword
    if n.emitter is not None and keyword == "detonate":
        return keyword
    return None


def _tok(tokens, i):
    """safeIndex / tok."""
    return tokens[i] if i < len(tokens) else ""


class _ListWarn:
    """listWarn: damaged entries are kept, zeroed and reported, at most 5 per list."""
    __slots__ = ("p", "shown", "total", "stopped")

    def __init__(self, p):
        self.p = p
        self.shown = 0
        self.total = 0
        self.stopped = False

    def row(self):
        """listWarn.row: a new entry; sscanf stops an entry at a bad value."""
        self.stopped = False

    def add(self, msg):
        self.total += 1
        if self.shown < MAX_LIST_WARNINGS:
            self.shown += 1
            self.p.errorf(msg)

    def done(self):
        if self.total > self.shown:
            self.p.errorf("%d more damaged value(s) in this list were set to 0" % (self.total - self.shown))

    def float(self, tok):
        if self.stopped:
            return 0.0
        v, n = scan_float(tok)
        if v is None:
            if n == 0 or n < len(tok):
                self.add("%s is not a usable number, set to 0, as are the values after it in the entry" % quote(tok))
                self.stopped = True
            else:
                self.add("%s is not a usable number, set to 0" % quote(tok))
            return 0.0
        if n < len(tok):
            self.add("%s is read as %s, as the game reads it, and the values after it in the entry are set to 0"
                     % (quote(tok), fmt_float(v)))
            self.stopped = True
        return v

    def int(self, tok):
        if self.stopped:
            return 0
        v, n = scan_int(tok)
        if v is None:
            if n == 0 or n < len(tok):
                self.add("%s is not a usable integer, set to 0, as are the values after it in the entry" % quote(tok))
                self.stopped = True
            else:
                self.add("%s is not a usable integer, set to 0" % quote(tok))
            return 0
        if n < len(tok):
            self.add("%s is read as %d, as the game reads it, and the values after it in the entry are set to 0"
                     % (quote(tok), v))
            self.stopped = True
        return v

    def short(self, have, want):
        self.add("entry has %d value(s), expected %d; the missing ones are set to 0" % (have, want))

    def key_values(self, tokens, n):
        """keyValues: the first n values of a key line, or None (skipped)."""
        if len(tokens) < n:
            self.add("key has %d value(s), expected %d; the key is skipped" % (len(tokens), n))
            return None
        t, tn = scan_float(tokens[0])
        if t is None:
            self.add("key time %s is not a usable number; the key is skipped" % quote(tokens[0]))
            return None
        if tn < len(tokens[0]):
            self.add("key time %s is read as %s, as the game reads it, and the values after it in the key are set to 0"
                     % (quote(tokens[0]), fmt_float(t)))
            self.stopped = True
        v = [t]
        for i in range(1, n):
            v.append(self.float(tokens[i]))
        return v


class _Parser:
    def __init__(self, lines, simple):
        self.lines = lines
        self.pos = 0
        self.line = 0
        if simple:
            self.trim, self.tokenize = str.strip, str.split
        else:
            self.trim, self.tokenize = gostr.trim_space, gostr.fields
        self.saw_content = False
        self.saw_model = False
        self.model = Model()
        self.errors = []
        self.current_node = None
        self.current_anim = None
        self.current_anim_node = None
        self.node_stack = []
        self.alloc_bytes = 0

    def errorf(self, msg):
        self.errors.append(ParseError(self.line, msg))

    def track_alloc(self, n):
        self.alloc_bytes += n
        return self.alloc_bytes <= MAX_ALLOC

    def next_line(self):
        lines, trim = self.lines, self.trim
        while self.pos < len(lines):
            raw = lines[self.pos]
            self.pos += 1
            self.line += 1
            line = trim(raw)
            if line == "" or line[0] == "#":
                continue
            return line
        return None

    def next_weight_line(self):
        """nextWeightLine: a line of only spaces is an entry (a vertex with no weights)."""
        lines = self.lines
        while self.pos < len(lines):
            raw = lines[self.pos].rstrip("\r")
            self.pos += 1
            self.line += 1
            if raw == "":
                continue
            line = self.trim(raw)
            if line.startswith("#"):
                continue
            return line
        return None

    # --- one-value parameters ---

    def float_param(self, tokens, obj, attr):
        v = self.float_value(tokens)
        if v is not None:
            setattr(obj, attr, v)

    def float_value(self, tokens):
        """floatValue: the value of a one-value line, or None (reported)."""
        tok = _tok(tokens, 1)
        v, n = scan_float(tok)
        if v is None:
            self.bad_param(tokens, "number")
            return None
        if n < len(tok):
            self.errorf("%s for %s is read as %s, as the game reads it; the rest is ignored"
                        % (quote(tok), quote(tokens[0]), fmt_float(v)))
        return v

    def int_param(self, tokens, obj, attr):
        tok = _tok(tokens, 1)
        v, n = scan_int(tok)
        if v is None:
            if gostr.equal_fold(tok, "true"):
                setattr(obj, attr, 1)
            elif gostr.equal_fold(tok, "false"):
                setattr(obj, attr, 0)
            else:
                self.bad_param(tokens, "integer")
            return
        setattr(obj, attr, v)
        if n < len(tok):
            f = parse_float32(tok)
            if f is None or f != v:
                self.errorf("%s for %s is read as %d, as the game reads it; the rest is ignored"
                            % (quote(tok), quote(tokens[0]), v))

    def bool_param(self, tokens, obj, attr):
        s = gostr.go_lower(_tok(tokens, 1))
        if s == "true":
            setattr(obj, attr, 1)
        elif s == "false":
            setattr(obj, attr, 0)
        else:
            self.int_param(tokens, obj, attr)

    def float_at(self, tokens, i):
        tok = _tok(tokens, i)
        v, n = scan_float(tok)
        if v is None:
            self.errorf("%s is not a usable number for value %d of %s, set to 0" % (quote(tok), i, quote(tokens[0])))
            return 0.0
        if n < len(tok):
            self.errorf("%s for value %d of %s is read as %s, as the game reads it; the rest is ignored"
                        % (quote(tok), i, quote(tokens[0]), fmt_float(v)))
        return v

    def short_param(self, tokens, want):
        self.errorf("%s has %d value(s), expected %d; the line is ignored" % (quote(tokens[0]), len(tokens) - 1, want))

    def vec_param(self, tokens, cur):
        """vecParam: cur with the line's values read over it as sscanf "%f %f %f"
        does: in order, up to the first bad value (reported)."""
        out = list(cur)
        vals = tokens[1:]
        for i in range(len(out)):
            if i >= len(vals):
                self.errorf("%s has %d value(s), expected %d; the others keep their values"
                            % (quote(tokens[0]), len(vals), len(out)))
                break
            v, n = scan_float(vals[i])
            if v is None:
                if n > 0 and n == len(vals[i]):
                    self.errorf("%s has an unusable number (%s), which keeps its value" % (quote(tokens[0]), quote(vals[i])))
                    continue
                self.errorf("%s has an unusable number (%s); it and the values after it keep their values"
                            % (quote(tokens[0]), quote(vals[i])))
                break
            out[i] = v
            if n < len(vals[i]):
                self.errorf("%s: %s is read as %s, as the game reads it; the rest of the line is ignored"
                            % (quote(tokens[0]), quote(vals[i]), fmt_float(v)))
                break
        return tuple(out)

    def vec3_param(self, tokens, obj, attr):
        setattr(obj, attr, self.vec_param(tokens, getattr(obj, attr)))

    def count_param(self, tokens):
        """countParam: a count with more after it is refused unless that changes nothing ("3.0")."""
        tok = _tok(tokens, 1)
        v, n = scan_int(tok)
        if v is not None and n < len(tok):
            f = parse_float32(tok)
            if f is None or f != v:
                v = None
        if v is None:
            self.bad_param(tokens, "count")
        return v

    def bad_param(self, tokens, kind):
        if len(tokens) < 2:
            self.errorf("%s has no value, keeping the default" % quote(tokens[0]))
            return
        self.errorf("%s is not a usable %s for %s, keeping the default" % (quote(tokens[1]), kind, quote(tokens[0])))

    # --- top level ---

    def parse(self):
        m = self.model
        while True:
            line = self.next_line()
            if line is None:
                break
            tokens = self.tokenize(line)
            if not tokens:
                continue
            self.saw_content = True
            kw = gostr.go_lower(tokens[0])
            if kw in _STRUCTURE:
                self.saw_model = True
            if kw in _CASE_SENSITIVE and tokens[0] != kw:
                self.errorf("%s: the game reads only lower-case %s and does not read this line; cleanmodels does"
                            % (quote(tokens[0]), quote(kw)))
            if kw == "newmodel":
                if len(tokens) >= 2:
                    m.name = tokens[1]
                m.file_type = "ascii"
            elif kw == "donemodel":
                pass
            elif kw == "setsupermodel":
                if len(tokens) >= 3:
                    m.super_model = tokens[2]
                elif len(tokens) >= 2:
                    m.super_model = tokens[1]
            elif kw == "setanimationscale":
                if len(tokens) >= 2:
                    self.float_param(tokens, m, "animation_scale")
            elif kw == "classification":
                if len(tokens) >= 2:
                    m.classification = gostr.go_upper(tokens[1])
            elif kw == "ignorefog":
                if len(tokens) >= 2:
                    self.int_param(tokens, m, "ignore_fog")
            elif kw == "filedependancy" or kw == "filedependency":
                if len(tokens) >= 2:
                    m.file_dependancy = tokens[1]
            elif kw == "beginmodelgeom" or kw == "endmodelgeom":
                pass
            elif kw == "node":
                self.parse_node_start(tokens)
            elif kw == "endnode":
                self.parse_node_end()
            elif kw == "newanim":
                if len(tokens) < 3:
                    self.errorf("newanim requires name and model name")
                else:
                    self.current_anim = Animation(name=tokens[1])
            elif kw == "doneanim":
                if self.current_anim is not None:
                    m.animations.append(self.current_anim)
                    self.current_anim = None
            elif (kw == "length" or kw == "animlength") and self.current_anim is not None \
                    and self.current_anim_node is None:
                self.parse_anim_length(tokens)
            elif kw == "transtime" and self.current_anim is not None and self.current_anim_node is None:
                if len(tokens) >= 2:
                    self.float_param(tokens, self.current_anim, "trans_time")
            elif kw == "animroot":
                if self.current_anim is not None and len(tokens) >= 2:
                    self.current_anim.root = tokens[1]
            elif kw == "event" and self.current_anim is not None:
                if len(tokens) >= 3:
                    t, tn = scan_float(tokens[1])
                    if t is not None:
                        if tn < len(tokens[1]):
                            self.errorf("event time %s is read as %s, as the game reads it; the rest is ignored"
                                        % (quote(tokens[1]), fmt_float(t)))
                        self.current_anim.events.append((t, tokens[2]))
            else:
                self.parse_parameter(tokens)

    def parse_node_start(self, tokens):
        if len(tokens) < 3:
            self.errorf("node requires type and name")
            return
        node_type = gostr.go_lower(tokens[1])
        name = tokens[2]
        if not self.track_alloc(512):
            self.errorf("allocation limit exceeded creating node %s" % quote(name))
            return
        node = Node(name)
        if node_type == "trimesh":
            node.mesh = new_parsed_mesh_data(False)
        elif node_type == "skin":
            node.mesh = new_parsed_mesh_data(False)
            node.skin = SkinData()
        elif node_type == "animmesh":
            node.mesh = new_parsed_mesh_data(False)
            node.anim_mesh = AnimMeshData()
        elif node_type == "danglymesh":
            node.mesh = new_parsed_mesh_data(False)
            node.dangly = DanglyData()
        elif node_type == "aabb":
            node.mesh = new_parsed_mesh_data(True)
            node.aabb = AabbData()
        elif node_type == "emitter":
            node.emitter = EmitterData()
        elif node_type == "light":
            node.light = new_parsed_light_data()
        elif node_type == "reference":
            node.reference = ReferenceData()
        elif node_type == "camera":
            node.camera = True
        elif node_type != "dummy":
            self.errorf("unknown node type %s, treating as dummy" % quote(node_type))
        if self.current_anim is not None:
            self.current_anim_node = AnimNode(name)
            return
        self.current_node = node
        if self.node_stack:
            node.parent = self.node_stack[-1].name
        self.node_stack.append(node)
        self.model.nodes.append(node)

    def parse_node_end(self):
        if self.current_anim is not None and self.current_anim_node is not None:
            self.current_anim.nodes.append(self.current_anim_node)
            self.current_anim_node = None
            return
        if self.node_stack:
            self.node_stack.pop()
        self.current_node = self.node_stack[-1] if self.node_stack else None

    def parse_anim_length(self, tokens):
        if len(tokens) < 2:
            return
        if gostr.equal_fold_ascii(tokens[0], "animlength") and len(tokens) >= 3:
            tokens = tokens[1:]
        self.float_param(tokens, self.current_anim, "length")

    # --- geometry node parameters ---

    def parse_parameter(self, tokens):
        kw = gostr.go_lower(tokens[0])
        if self.current_anim is not None and self.current_anim_node is not None:
            self.parse_anim_node_param(kw, tokens)
            return
        node = self.current_node
        if node is None:
            return
        name = _declared_name(node, kw)
        if name is not None:
            node.declared.add(name)
        if kw == "parent":
            if self.parent_line(tokens, node.name):
                node.parent = tokens[1]
        elif kw == "#part-number":
            if len(tokens) >= 2:
                self.int_param(tokens, node, "part_number")
        elif kw == "position":
            self.vec3_param(tokens, node, "position")
        elif kw == "orientation":
            node.orientation = self.vec_param(tokens, node.orientation)
        elif kw == "scale":
            if len(tokens) >= 2:
                self.float_param(tokens, node, "scale")
        elif kw == "inheritcolor":
            if len(tokens) >= 2:
                self.int_param(tokens, node, "inherit_color")
        elif kw == "wirecolor":
            self.vec3_param(tokens, node, "wire_color")
        else:
            self.parse_node_specific_param(kw, tokens)

    def parse_node_specific_param(self, kw, tokens):
        node = self.current_node
        if node.mesh is not None and self.parse_mesh_param(node.mesh, kw, tokens):
            return
        if node.skin is not None:
            if self.parse_skin_param(node, kw, tokens):
                return
        elif node.dangly is not None:
            if self.parse_dangly_param(node.dangly, kw, tokens):
                return
        elif node.anim_mesh is not None:
            if self.parse_anim_mesh_param(node.anim_mesh, kw, tokens):
                return
        elif node.aabb is not None:
            if self.parse_aabb_param(node.aabb, kw, tokens):
                return
        elif node.emitter is not None:
            if self.parse_emitter_param(node.emitter, kw, tokens):
                return
        elif node.light is not None:
            if self.parse_light_param(node.light, kw, tokens):
                return
        elif node.reference is not None:
            if self.parse_reference_param(node.reference, kw, tokens):
                return
        if self.parse_geom_key_list(node, kw, tokens):
            return
        matched = near_match_param(kw, valid_params_for_type(node.node_type()))
        if matched is not None and matched != kw:
            self.errorf("%s is not a keyword the game reads, so the line is ignored, as in the game; "
                        "did you mean %s?" % (quote(kw), quote(matched)))

    def parse_geom_key_list(self, node, kw, tokens):
        """parseGeomKeyList: a key list on a geometry node sets the node's value to its first key."""
        if not kw.endswith("key") or kw in ("detonatekey", "detonatebezierkey"):
            return False
        scratch = AnimNode(node.name)
        saved = self.current_anim_node
        self.current_anim_node = scratch
        self.parse_anim_node_param(kw, tokens)
        self.current_anim_node = saved
        name, vals, n = _first_anim_key(scratch)
        if name is None:
            return False
        set_static_controller(node, name, vals)
        if n > 1:
            self.errorf("%s on geometry node %s: only the first of its %d keys is used, as in the game, "
                        "which evaluates geometry at time 0" % (kw, quote(node.name), n))
        return True

    def parent_line(self, tokens, node):
        """parentLine: the game reads only a lower-case parent line."""
        if tokens[0] != "parent":
            self.errorf("%s is not read by the game, which knows only lower-case \"parent\", so node %s takes no "
                        "parent from it, as in the game" % (quote(tokens[0]), quote(node)))
            return False
        return len(tokens) >= 2

    def counted(self, tokens):
        """The count of a "keyword N" list line when it is a usable N > 0, else None."""
        if len(tokens) >= 2:
            c = self.count_param(tokens)
            if c is not None and c > 0:
                return c
        return None

    def parse_mesh_param(self, mesh, kw, tokens):
        if kw == "diffuse":
            self.vec3_param(tokens, mesh, "diffuse")
        elif kw == "ambient":
            self.vec3_param(tokens, mesh, "ambient")
        elif kw == "specular":
            self.vec3_param(tokens, mesh, "specular")
        elif kw == "shininess":
            self.float_param(tokens, mesh, "shininess")
        elif kw in _MESH_STRINGS:
            if len(tokens) >= 2:
                setattr(mesh, _MESH_STRINGS[kw], tokens[1])
        elif kw in _MESH_INTS:
            self.int_param(tokens, mesh, _MESH_INTS[kw])
        elif kw == "alpha":
            self.float_param(tokens, mesh, "alpha")
        elif kw == "selfillumcolor":
            self.vec3_param(tokens, mesh, "self_illum_color")
        elif kw == "center":
            pass  # mn3 or a name; not stored
        elif kw in _MESH_VEC3_LISTS:
            c = self.counted(tokens)
            if c is not None:
                self.read_vec_list(getattr(mesh, _MESH_VEC3_LISTS[kw]), c, 3)
        elif kw == "faces":
            c = self.counted(tokens)
            if c is not None:
                self.read_face_list(mesh.faces, c)
        elif kw == "tangents":
            c = self.counted(tokens)
            if c is not None:
                self.read_vec_list(mesh.tangents, c, 4)
        elif kw in _MESH_INT3_LISTS:
            c = self.counted(tokens)
            if c is not None:
                self.read_int3_list(getattr(mesh, _MESH_INT3_LISTS[kw]), c)
        elif kw == "multimaterial":
            c = self.counted(tokens)
            if c is not None:
                self.read_string_list(mesh.multimaterial, c)
        elif kw == "mirrorlist":
            c = self.counted(tokens)
            if c is not None:
                self.read_int_list(mesh.mirror_list, c)
        else:
            return False
        return True

    def parse_skin_param(self, node, kw, tokens):
        if kw != "weights":
            return False
        c = self.counted(tokens)
        if c is not None:
            self.read_weight_list(node.skin, c)
        return True

    def parse_dangly_param(self, d, kw, tokens):
        if kw == "displacement":
            self.float_param(tokens, d, "displacement")
        elif kw == "tightness":
            self.float_param(tokens, d, "tightness")
        elif kw == "period":
            self.float_param(tokens, d, "period")
        elif kw == "constraints":
            c = self.counted(tokens)
            if c is not None:
                self.read_float_list(d.constraints, c)
        elif kw == "displtype":
            self.int_param(tokens, d, "displ_type")
        elif kw in ("danglymesh", "showdispl", "gizmo"):
            pass  # display-only
        else:
            return False
        return True

    def parse_anim_mesh_param(self, am, kw, tokens):
        if kw in _ANIM_MESH_FLOATS:
            self.float_param(tokens, am, _ANIM_MESH_FLOATS[kw])
        elif kw == "animverts" or kw == "animtverts":
            c = self.counted(tokens)
            if c is not None:
                self.read_vec_list(am.anim_verts if kw == "animverts" else am.anim_tverts, c, 3)
        else:
            return False
        return True

    def parse_aabb_param(self, aabb, kw, tokens):
        if kw != "aabb":
            return False
        if len(tokens) >= 7:
            # the game's and NWMax's form: the root entry on the keyword's line, no count
            self.read_inline_aabb(aabb.entries, tokens[1:])
        else:
            c = self.counted(tokens)
            if c is not None:
                self.read_aabb_list(aabb.entries, c)
        return True

    def read_inline_aabb(self, out, tokens):
        """readInlineAabb: a preorder tree; an entry with a negative face has two children."""
        pending = 1
        while pending > 0:
            if len(tokens) < 7:
                self.errorf("aabb entry needs 7 values, found %d" % len(tokens))
                return
            if not self.track_alloc(32):
                self.errorf("allocation limit exceeded reading aabb entries")
                return
            w = _ListWarn(self)
            f = [w.float(tokens[k]) for k in range(6)]
            face = w.int(tokens[6])
            w.done()
            out.append(AabbEntry((f[0], f[1], f[2]), (f[3], f[4], f[5]), face, 0))
            pending -= 1
            if face < 0:
                pending += 2
            if pending == 0:
                return
            line = self.next_line()
            if line is None:
                self.errorf("aabb tree ends early: %d entries still expected" % pending)
                return
            tokens = self.tokenize(line)

    def parse_emitter_param(self, em, kw, tokens):
        if kw == "detonate":
            pass  # declared in parse_parameter; its controller has no value
        elif kw in _EMITTER_FLOAT_ATTRS:
            self.float_param(tokens, em, _EMITTER_FLOAT_ATTRS[kw])
        elif kw in _EMITTER_INTS:
            self.int_param(tokens, em, _EMITTER_INTS[kw])
        elif kw in _EMITTER_STRINGS:
            if len(tokens) >= 2:
                setattr(em, _EMITTER_STRINGS[kw], tokens[1])
        elif kw == "affectedbywind":
            self.bool_param(tokens, em, "affected_by_wind")
        elif kw in _EMITTER_FLOATS:
            v = self.float_value(tokens)
            if v is not None:
                em.floats[kw] = v
        elif kw in _EMITTER_COLORS:
            em.colors[kw] = self.vec_param(tokens, em.colors[kw])
        else:
            return False
        return True

    def parse_light_param(self, light, kw, tokens):
        if kw in _LIGHT_INTS:
            self.int_param(tokens, light, _LIGHT_INTS[kw])
        elif kw in _LIGHT_FLOATS:
            self.float_param(tokens, light, _LIGHT_FLOATS[kw])
        elif kw == "color":
            self.vec3_param(tokens, light, "color")
        elif kw == "flaresizes" or kw == "flarepositions":
            c = self.counted(tokens)
            if c is not None:
                self.read_float_list(light.flare_sizes if kw == "flaresizes" else light.flare_positions, c)
        elif kw == "flarecolorshifts":
            c = self.counted(tokens)
            if c is not None:
                self.read_vec_list(light.flare_color_shifts, c, 3)
        elif kw == "texturenames":
            c = self.counted(tokens)
            if c is not None:
                self.read_string_list(light.texture_names, c)
        else:
            return False
        return True

    def parse_reference_param(self, ref, kw, tokens):
        if kw == "refmodel":
            if len(tokens) >= 2:
                ref.ref_model = tokens[1]
        elif kw == "reattachable":
            self.int_param(tokens, ref, "reattachable")
        else:
            return False
        return True

    # --- animation node parameters ---

    def parse_anim_node_param(self, kw, tokens):
        an = self.current_anim_node
        if kw == "parent":
            if self.parent_line(tokens, an.name):
                an.parent = tokens[1]
        elif kw == "#part-number":
            pass
        elif kw == "position":
            if len(tokens) >= 4:
                v = (self.float_at(tokens, 1), self.float_at(tokens, 2), self.float_at(tokens, 3))
                if not an.position_keys:
                    an.position_keys.append((0.0, v, (0.0, 0.0, 0.0), (0.0, 0.0, 0.0)))
            else:
                self.short_param(tokens, 3)
        elif kw == "orientation":
            if len(tokens) >= 5:
                v = (self.float_at(tokens, 1), self.float_at(tokens, 2), self.float_at(tokens, 3),
                     self.float_at(tokens, 4))
                if not an.orientation_keys:
                    an.orientation_keys.append((0.0, v))
            else:
                self.short_param(tokens, 4)
        elif kw == "positionkey":
            self.key_list(tokens, an.position_keys, 16, "position keys", self.position_key)
        elif kw == "positionbezierkey":
            # time x y z [tanIn.xyz tanOut.xyz]; short rows are zero-tangent keys
            an.position_bezier = True
            self.key_list(tokens, an.position_keys, 40, "position bezier keys", self.position_bezier_key)
        elif kw == "orientationkey":
            self.key_list(tokens, an.orientation_keys, 20, "orientation keys", self.orientation_key)
        elif kw == "verts" or kw == "faces" or kw == "tverts":
            if len(tokens) >= 2:
                if an.mesh is None:
                    an.mesh = MeshData()
                c = self.count_param(tokens)
                if c is not None:
                    if kw == "faces":
                        self.read_face_list(an.mesh.faces, c)
                    else:
                        self.read_vec_list(an.mesh.verts if kw == "verts" else an.mesh.tverts, c, 3)
        elif kw in _ANIM_MESH_FLOATS:
            if len(tokens) >= 2:
                if an.anim_mesh is None:
                    an.anim_mesh = AnimMeshData()
                self.float_param(tokens, an.anim_mesh, _ANIM_MESH_FLOATS[kw])
        elif kw == "animverts" or kw == "animtverts":
            if len(tokens) >= 2:
                if an.anim_mesh is None:
                    an.anim_mesh = AnimMeshData()
                c = self.count_param(tokens)
                if c is not None:
                    am = an.anim_mesh
                    self.read_vec_list(am.anim_verts if kw == "animverts" else am.anim_tverts, c, 3)
        elif kw == "endlist":
            pass
        else:
            if self.try_anim_key_table(kw, tokens, an):
                return
            # orientationbezierkey and detonatebezierkey are read as linear keys
            # (time and value only); float and colour ones by try_anim_key_table
            if kw.endswith("bezierkey"):
                self.parse_anim_node_param(kw[:-len("bezierkey")] + "key", tokens)
                return
            # a static value on an animation node is a key at t=0
            if kw in _FLOAT_CTRL:
                if len(tokens) >= 2:
                    v = self.float_value(tokens)
                    if v is not None and not an.float_keys.get(kw):
                        an.float_keys[kw] = [(0.0, v)]
                return
            if kw in _COLOR_CTRL:
                if len(tokens) >= 4:
                    v = (self.float_at(tokens, 1), self.float_at(tokens, 2), self.float_at(tokens, 3))
                    if not an.color_keys.get(kw):
                        an.color_keys[kw] = [(0.0, v)]
                else:
                    self.short_param(tokens, 3)
                return

    def try_anim_key_table(self, kw, tokens, an):
        if kw.endswith("bezierkey") and kw != "detonatebezierkey":
            base = kw[:-len("bezierkey")]
            if base in _FLOAT_CTRL:
                an.bezier.add(base)
                keys = an.float_keys.get(base, [])
                self.key_list(tokens, keys, 16, "float bezier keys", self.float_bezier_key)
                if keys:
                    an.float_keys[base] = keys
                return True
            if base in _COLOR_CTRL:
                an.bezier.add(base)
                keys = an.color_keys.get(base, [])
                self.key_list(tokens, keys, 40, "color bezier keys", self.color_bezier_key)
                if keys:
                    an.color_keys[base] = keys
                return True
        if kw == "detonatekey":
            if len(tokens) >= 2:
                c = self.count_param(tokens)
                if c is not None:
                    keys = an.float_keys.get("detonate", [])
                    self.read_keys(c, 8, "detonate keys", self.detonate_key, keys)
                    if keys:
                        an.float_keys["detonate"] = keys
            return True
        name = _FLOAT_KEY_TABLE.get(kw)
        if name is not None:
            keys = an.float_keys.get(name, [])
            self.key_list(tokens, keys, 8, "float keys", self.float_key)
            if keys:
                an.float_keys[name] = keys
            return True
        name = _COLOR_KEY_TABLE.get(kw)
        if name is not None:
            keys = an.color_keys.get(name, [])
            self.key_list(tokens, keys, 16, "color keys", self.color_key)
            if keys:
                an.color_keys[name] = keys
            return True
        return False

    def key_list(self, tokens, out, size, label, parse):
        """A counted ("positionkey 2") or uncounted ("positionkey" ... "endlist") key list."""
        if len(tokens) >= 2:
            c = self.count_param(tokens)
            if c is not None:
                self.read_keys(c, size, label, parse, out)
        else:
            self.read_keys_until_end(size, label, parse, out)

    # --- list readers ---

    def read_vec_list(self, out, count, n):
        """readVec3List / readVec4List."""
        if not self.track_alloc(count * 4 * n):
            self.errorf("allocation limit exceeded reading %d vec%d entries" % (count, n))
            return
        w = _ListWarn(self)
        next_line, tokenize, sf = self.next_line, self.tokenize, scan_float
        zero = (0.0,) * n
        for _ in range(count):
            line = next_line()
            if line is None:
                break
            tokens = tokenize(line)
            if len(tokens) == n:
                rs = [sf(t) for t in tokens]
                if all(r[0] is not None and r[1] == len(t) for r, t in zip(rs, tokens)):
                    out.append(tuple(r[0] for r in rs))
                    continue
            w.row()
            if len(tokens) < n:
                w.short(len(tokens), n)
            v = [w.float(t) for t in tokens[:n]]
            out.append(tuple(v) + zero[len(v):])
        w.done()

    def read_face_list(self, out, count):
        if not self.track_alloc(count * 48):
            self.errorf("allocation limit exceeded reading %d face entries" % count)
            return
        w = _ListWarn(self)
        for _ in range(count):
            line = self.next_line()
            if line is None:
                break
            tokens = self.tokenize(line)
            w.row()
            if len(tokens) < 8:
                w.short(len(tokens), 8)
            v = [w.int(t) for t in tokens[:8]]
            v += [0] * (8 - len(v))
            out.append(Face((v[0], v[1], v[2]), v[3], (v[4], v[5], v[6]), v[7]))
        w.done()

    def read_int_list(self, out, count):
        if not self.track_alloc(count * 4):
            self.errorf("allocation limit exceeded reading %d int entries" % count)
            return
        w = _ListWarn(self)
        for _ in range(count):
            line = self.next_line()
            if line is None:
                break
            w.row()
            out.append(w.int(_tok(self.tokenize(line), 0)))
        w.done()

    def read_float_list(self, out, count):
        if not self.track_alloc(count * 4):
            self.errorf("allocation limit exceeded reading %d float entries" % count)
            return
        w = _ListWarn(self)
        for _ in range(count):
            line = self.next_line()
            if line is None:
                break
            w.row()
            out.append(w.float(_tok(self.tokenize(line), 0)))
        w.done()

    def read_int3_list(self, out, count):
        if not self.track_alloc(count * 12):
            self.errorf("allocation limit exceeded reading %d int3 entries" % count)
            return
        w = _ListWarn(self)
        for _ in range(count):
            line = self.next_line()
            if line is None:
                break
            tokens = self.tokenize(line)
            w.row()
            if len(tokens) < 3:
                w.short(len(tokens), 3)
            v = [w.int(t) for t in tokens[:3]]
            v += [0] * (3 - len(v))
            out.append(tuple(v))
        w.done()

    def read_string_list(self, out, count):
        if not self.track_alloc(count * 32):
            self.errorf("allocation limit exceeded reading %d string entries" % count)
            return
        for _ in range(count):
            line = self.next_line()
            if line is None:
                break
            out.append(self.trim(line))

    def read_aabb_list(self, out, count):
        if not self.track_alloc(count * 32):
            self.errorf("allocation limit exceeded reading %d aabb entries" % count)
            return
        w = _ListWarn(self)
        for _ in range(count):
            line = self.next_line()
            if line is None:
                break
            tokens = self.tokenize(line)
            w.row()
            if len(tokens) < 7:
                w.short(len(tokens), 7)
            f = [w.float(t) for t in tokens[:6]]
            f += [0.0] * (6 - len(f))
            face = w.int(tokens[6]) if len(tokens) > 6 else 0
            out.append(AabbEntry((f[0], f[1], f[2]), (f[3], f[4], f[5]), face, 0))
        w.done()

    def read_weight_list(self, skin, count):
        if not self.track_alloc(count * 512):
            self.errorf("allocation limit exceeded reading %d skin weights" % count)
            return
        ww = _ListWarn(self)
        for _ in range(count):
            line = self.next_weight_line()
            if line is None:
                break
            tokens = self.tokenize(line)
            vw = VertexWeight()
            for j in range(0, len(tokens) - 1, 2):
                wt, wn = scan_float(tokens[j + 1])
                if wt is None:
                    ww.add("weight %s for bone %s is not a usable number, that influence is ignored"
                           % (quote(tokens[j + 1]), quote(tokens[j])))
                    continue
                if wn < len(tokens[j + 1]):
                    ww.add("weight %s for bone %s is read as %s, as the game reads it"
                           % (quote(tokens[j + 1]), quote(tokens[j]), fmt_float(wt)))
                if wt > 0:
                    vw.bones.append(tokens[j])
                    vw.weights.append(wt)
            skin.weights.append(vw)
        ww.done()

    def read_keys(self, count, size, label, parse, out):
        """readKeys: a counted run of key lines (a negative count reads none)."""
        if not self.track_alloc(count * size):
            self.errorf("allocation limit exceeded reading %d %s" % (count, label))
            return
        w = _ListWarn(self)
        for _ in range(count):
            line = self.next_line()
            if line is None:
                break
            w.row()
            parse(w, self.tokenize(line), out)
        w.done()

    def read_keys_until_end(self, size, label, parse, out):
        """readKeysUntilEnd: key lines up to "endlist"."""
        w = _ListWarn(self)
        while True:
            line = self.next_line()
            if line is None:
                break
            tokens = self.tokenize(line)
            if not tokens:
                continue
            if gostr.equal_fold_ascii(tokens[0], "endlist"):
                break
            if not self.track_alloc(size):
                self.errorf("allocation limit exceeded reading %s" % label)
                break
            w.row()
            parse(w, tokens, out)
        w.done()

    @staticmethod
    def position_key(w, tokens, out):
        v = w.key_values(tokens, 4)
        if v is not None:
            out.append((v[0], (v[1], v[2], v[3]), (0.0, 0.0, 0.0), (0.0, 0.0, 0.0)))

    @staticmethod
    def position_bezier_key(w, tokens, out):
        v = w.key_values(tokens, 4)
        if v is None:
            return
        for i in range(4, min(10, len(tokens))):
            v.append(w.float(tokens[i]))
        v += [0.0] * (10 - len(v))
        out.append((v[0], (v[1], v[2], v[3]), (v[4], v[5], v[6]), (v[7], v[8], v[9])))

    @staticmethod
    def orientation_key(w, tokens, out):
        v = w.key_values(tokens, 5)
        if v is not None:
            out.append((v[0], (v[1], v[2], v[3], v[4])))

    @staticmethod
    def float_key(w, tokens, out):
        v = w.key_values(tokens, 2)
        if v is not None:
            out.append((v[0], v[1]))

    @staticmethod
    def float_bezier_key(w, tokens, out):
        """time value [handle_in handle_out]; short rows have zero handles."""
        v = w.key_values(tokens, 2)
        if v is None:
            return
        for i in range(2, min(4, len(tokens))):
            v.append(w.float(tokens[i]))
        v += [0.0] * (4 - len(v))
        out.append((v[0], v[1], v[2], v[3]))

    @staticmethod
    def color_bezier_key(w, tokens, out):
        """time r g b [in.rgb out.rgb]; short rows have zero handles."""
        v = w.key_values(tokens, 4)
        if v is None:
            return
        for i in range(4, min(10, len(tokens))):
            v.append(w.float(tokens[i]))
        v += [0.0] * (10 - len(v))
        out.append((v[0], (v[1], v[2], v[3]), (v[4], v[5], v[6]), (v[7], v[8], v[9])))

    @staticmethod
    def color_key(w, tokens, out):
        v = w.key_values(tokens, 4)
        if v is not None:
            out.append((v[0], (v[1], v[2], v[3])))

    @staticmethod
    def detonate_key(w, tokens, out):
        """Time only (value 1), or time and value."""
        if len(tokens) == 1:
            v = w.key_values(tokens, 1)
            if v is not None:
                out.append((v[0], 1.0))
            return
        _Parser.float_key(w, tokens, out)


_MESH_STRINGS = {"bitmap": "bitmap", "texture0": "bitmap", "texture1": "texture1", "texture2": "texture2",
                 "materialname": "material_name", "renderhint": "render_hint"}
_MESH_INTS = {"shadow": "shadow", "beaming": "beaming", "render": "render", "transparencyhint": "transparency_hint",
              "tilefade": "tile_fade", "rotatetexture": "rotate_texture", "lightmapped": "light_mapped"}
_MESH_VEC3_LISTS = {"verts": "verts", "tverts": "tverts", "tverts1": "tverts1", "tverts2": "tverts2",
                    "tverts3": "tverts3", "colors": "colors", "normals": "normals"}
_MESH_INT3_LISTS = {"texindices0": "tex_indices0", "texindices1": "tex_indices1", "texindices2": "tex_indices2",
                    "texindices3": "tex_indices3"}
_ANIM_MESH_FLOATS = {"sampleperiod": "sample_period", "clipu": "clip_u", "clipv": "clip_v", "clipw": "clip_w",
                     "cliph": "clip_h"}
_EMITTER_FLOAT_ATTRS = {"deadspace": "dead_space", "blastradius": "blast_radius", "blastlength": "blast_length"}
_EMITTER_INTS = {
    "xgrid": "x_grid", "ygrid": "y_grid", "spawntype": "spawn_type", "twosidedtex": "two_sided_tex", "loop": "loop",
    "renderorder": "render_order", "p2p": "p2p", "p2p_sel": "p2p_sel", "m_istinted": "is_tinted", "bounce": "bounce",
    "random": "random", "inherit": "inherit", "inheritvel": "inherit_vel", "inherit_local": "inherit_local",
    "splat": "splat", "inherit_part": "inherit_part", "render_sel": "render_sel", "blend_sel": "blend_sel",
    "update_sel": "update_sel", "spawntype_sel": "spawn_type_sel", "opacity": "opacity", "iconsize": "icon_size",
    "lockaxes": "lock_axes", "chunky": "chunky",
}
_EMITTER_STRINGS = {"update": "update", "render": "render", "blend": "blend", "texture": "texture",
                    "chunkname": "chunk_name", "p2p_type": "p2p_type"}
_LIGHT_INTS = {
    "lensflares": "lens_flares", "lightpriority": "light_priority", "ambientonly": "ambient_only",
    "isdynamic": "n_dynamic_type", "ndynamictype": "n_dynamic_type",
    "affectdynamic": "affect_dynamic", "shadow": "shadow", "generateflare": "generate_flare",
    "fadinglight": "fading_light", "negativelight": "negative_light",
}
_LIGHT_FLOATS = {"flareradius": "flare_radius", "radius": "radius", "multiplier": "multiplier",
                 "shadowradius": "shadow_radius", "verticaldisplacement": "vertical_displacement"}

_BASE_PARAMS = ("parent", "position", "orientation", "scale", "inheritcolor", "wirecolor")
_MESH_PARAMS = (
    "selfillumcolor", "alpha", "diffuse", "ambient", "specular", "shininess",
    "shadow", "beaming", "render", "transparencyhint", "renderhint",
    "bitmap", "texture0", "texture1", "texture2", "materialname",
    "tilefade", "rotatetexture", "lightmapped", "center",
    "multimaterial", "verts", "faces", "tverts", "tverts1", "tverts2", "tverts3",
    "texindices0", "texindices1", "texindices2", "texindices3",
    "colors", "normals", "tangents", "mirrorlist",
)
_EMITTER_PARAMS = (
    "deadspace", "blastradius", "blastlength", "xgrid", "ygrid", "spawntype",
    "update", "render", "blend", "texture", "chunkname", "twosidedtex",
    "loop", "renderorder", "p2p", "p2p_sel", "affectedbywind", "m_istinted",
    "bounce", "random", "inherit", "inheritvel", "inherit_local", "splat", "inherit_part",
    "p2p_type", "render_sel", "blend_sel", "update_sel", "spawntype_sel",
    "opacity", "iconsize", "lockaxes", "chunky",
    "alphastart", "alphamid", "alphaend",
    "colorstart", "colormid", "colorend",
    "sizestart", "sizemid", "sizeend",
    "sizestart_y", "sizemid_y", "sizeend_y",
    "birthrate", "lifeexp", "mass", "spread", "particlerot", "velocity", "randvel",
    "bounce_co", "blurlength", "fps", "framestart", "frameend",
    "grav", "drag", "threshold", "combinetime",
    "percentstart", "percentmid", "percentend",
    "lightningdelay", "lightningradius", "lightningscale", "lightningsubdiv",
    "p2p_bezier2", "p2p_bezier3", "xsize", "ysize",
)
_LIGHT_PARAMS = (
    "flareradius", "lensflares", "lightpriority", "ambientonly",
    "isdynamic", "ndynamictype", "affectdynamic", "shadow",
    "generateflare", "fadinglight", "negativelight",
    "color", "radius", "multiplier", "shadowradius", "verticaldisplacement",
    "flaresizes", "flarepositions", "flarecolorshifts", "texturenames",
)
_VALID_PARAMS = {
    "dummy": _BASE_PARAMS,
    "trimesh": _BASE_PARAMS + _MESH_PARAMS,
    "skin": _BASE_PARAMS + _MESH_PARAMS + ("weights",),
    "animmesh": _BASE_PARAMS + _MESH_PARAMS + ("sampleperiod", "clipu", "clipv", "clipw", "cliph", "animverts",
                                               "animtverts"),
    "danglymesh": _BASE_PARAMS + _MESH_PARAMS + ("displacement", "tightness", "period", "constraints", "danglymesh",
                                                 "showdispl", "displtype", "gizmo"),
    "aabb": _BASE_PARAMS + _MESH_PARAMS + ("aabb",),
    "emitter": _BASE_PARAMS + _EMITTER_PARAMS,
    "light": _BASE_PARAMS + _LIGHT_PARAMS,
    "reference": _BASE_PARAMS + ("refmodel", "reattachable"),
}


def valid_params_for_type(node_type):
    """validParamsForType: the parameter names fuzzy matching may pick."""
    return _VALID_PARAMS.get(node_type, _BASE_PARAMS)
