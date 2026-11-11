"""Round-trip fixes of the decompiler, as pkg/mdl/roundtrip_fixes_test.go tests them.

    python python/decompile_fixes_test.py

Each test runs the port's own parser, compiler and decompiler, so the port's
logic is checked, not only its output on the parity fixtures (no fixture has
these cases).
"""

import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from cleanmodels import compile, decompile, parse, write  # noqa: E402,A004
from cleanmodels import binary as _binary  # noqa: E402
from cleanmodels.binary import _Decompiler, unmerge_render_lists  # noqa: E402
from cleanmodels.compiler_node import aabb_split_plane, build_aabb_tree  # noqa: E402
from cleanmodels.binary_order import declaration_order  # noqa: E402
from cleanmodels.compiler_order import reorder_children  # noqa: E402
from cleanmodels.binary_order import _walks_as  # noqa: E402
from cleanmodels.compiler_tree import resolve_geom_tree  # noqa: E402
from cleanmodels.f32 import f32, f32_bits  # noqa: E402
from cleanmodels.share_neighbours import stored_neighbours  # noqa: E402
from cleanmodels.types import Face, MeshData, Model, Node  # noqa: E402
from cleanmodels.vec import axis_angle_to_quat, model_bounds  # noqa: E402

ENGINE = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "tests", "fixtures", "engine")

IN_ORDER_SEAM = """newmodel ios
setsupermodel ios NULL
classification PLACEABLE
setanimationscale 1.0
beginmodelgeom ios
  node dummy ios
    parent NULL
  endnode
  node trimesh fold
    parent ios
    bitmap blank
    renderhint NormalAndSpecMapped
    verts 4
      0 0 0
      1 0 0
      1 1 0.5
      0 1 0
    tverts 6
      0 0 0
      1 0 0
      1 1 0
      1 1 0
      0 1 0
      0 0 0
    faces 2
      0 1 2  1  0 1 2  0
      0 2 3  1  3 4 5  0
  endnode
endmodelgeom
donemodel ios
"""

DROPPED_FACE = """newmodel dfs
setsupermodel dfs NULL
classification PLACEABLE
setanimationscale 1.0
beginmodelgeom dfs
  node dummy dfs
    parent NULL
  endnode
  node trimesh strip
    parent dfs
    bitmap blank
    verts 9
      0 0 0
      1 0 0
      1 1 0
      0 1 0
      2 0 0
      2 1 0
      5 5 0
      6 5 0
      5 6 0
    tverts 9
      0 0 0
      1 0 0
      1 1 0
      0.5 0 0
      0.5 0.5 0
      0 0.5 0
      0.2 0 0
      0.2 0.2 0
      0.4 0.2 0
    faces 6
      0 1 2  1  0 1 2  0
      0 0 1  1  0 0 1  0
      0 2 3  1  3 4 5  0
      1 4 5  1  6 7 8  0
      1 5 2  1  6 8 2  0
      6 7 8  1  0 1 2  0
  endnode
endmodelgeom
donemodel dfs
"""

JUNK_EMITTER = """newmodel ejt
setsupermodel ejt NULL
classification EFFECT
setanimationscale 1.0
beginmodelgeom ejt
  node dummy ejt
    parent NULL
  endnode
  node emitter chunks
    parent ejt
    update Explosion
    render Normal
    blend Normal
    texture zzjunkmarker
    chunkName fx_chunk
  endnode
endmodelgeom
donemodel ejt
"""

LIGHT = """newmodel lco
setsupermodel lco NULL
classification EFFECT
setanimationscale 1.0
beginmodelgeom lco
  node dummy lco
    parent NULL
  endnode
  node light lamp
    parent lco
    color 1 1 1
    radius 5
    multiplier 1
    shadowradius 7
    verticaldisplacement 2
  endnode
endmodelgeom
donemodel lco
"""


def compile_text(text):
    return compile(parse(text.encode("latin-1")).model)


def tsort50(super_model="zz_missing"):
    with open(os.path.join(ENGINE, "zcm_tsort50.ascii.mdl"), encoding="latin-1") as f:
        text = f.read()
    return text.replace("setsupermodel zcm_tsort50 NULL", "setsupermodel zcm_tsort50 %s" % super_model, 1)


def stored_tree(data):
    """storedTree: parent/name per node, as stored (no declaration order)."""
    keep = _binary.declaration_order
    _binary.declaration_order = lambda m, hints=None: None
    try:
        m = decompile(data)
    finally:
        _binary.declaration_order = keep
    return m


def _engine(name):
    with open(os.path.join(ENGINE, name + ".ascii.mdl"), "rb") as f:
        src = f.read()
    with open(os.path.join(ENGINE, name + ".mdl"), "rb") as f:
        golden = f.read()
    return src, golden


def _anim_count_nodes(data, name):
    """An animation's stored count_nodes, found by its 64-byte name field."""
    i = data.find(name.encode("ascii").ljust(64, b"\0"))
    return int.from_bytes(data[i + 68:i + 72], "little")


def _anim_nodes(m):
    return [(a.name, [n.name for n in a.nodes]) for a in m.animations]


class RoundTripFixes(unittest.TestCase):
    def test_animroot_prunes_as_the_engine(self):
        """zspec_animroot (MDL-NODE-045), as TestCompileAnimRootPrunesAsTheEngine."""
        src, golden = _engine("zspec_animroot")
        ours = compile(parse(src).model)
        self.assertEqual(_anim_nodes(decompile(ours)), _anim_nodes(decompile(golden)))
        for a in decompile(golden).animations:
            self.assertEqual(_anim_count_nodes(ours, a.name), _anim_count_nodes(golden, a.name), a.name)

    def test_decompile_animroot_keeps_what_it_matches(self):
        """As TestDecompileAnimRootKeepsWhatItMatches."""
        _, golden = _engine("zspec_animroot")
        m = decompile(golden)
        self.assertEqual([(a.name, a.root) for a in m.animations],
                         [("prune", "arm"), ("missing", "nosuchnode"), ("case", "ARM"), ("geomonly", "leg")])
        self.assertEqual(_anim_nodes(decompile(compile(m))), _anim_nodes(m))
        nodes = [Node(), Node()]
        nodes[0].name, nodes[1].name = "root", "arm"
        for stored, matched, wide, want in (("Arm", True, False, "arm"), ("arm", False, False, "ARM"),
                                            ("arm", True, True, "ARM"), ("nosuchnode", False, False, "nosuchnode"),
                                            ("NULL", False, False, "null"), ("", False, False, "")):
            self.assertEqual(_binary.written_anim_root(stored, matched, wide, nodes), want, stored)

    def test_stored_anim_root_match(self):
        """As TestStoredAnimRootMatch: a_ogre's animroot rootdummy beside headconjure, handconjure."""
        names = ["a_ogre", "rootdummy", "pelvis_g", "headconjure", "handconjure"]
        parents = [-1, 0, 1, 0, 0]
        match = _binary.stored_anim_root_match
        self.assertEqual(match("rootdummy", names, parents), (True, True))
        self.assertEqual(match("rootdummy", names[:3], parents[:3]), (True, False))
        self.assertEqual(match("ROOTDUMMY", names, parents), (False, False))
        self.assertEqual(match("a_ogre", names, parents), (True, False))

    def test_latin1_node_names_kept(self):
        """zspec_namelat (MDL-BIN-007), as TestDecompileKeepsLatin1NodeNames."""
        _, golden = _engine("zspec_namelat")
        m = decompile(golden)
        self.assertIsNotNone(m.find_node("pesta\xf1as"), [n.name for n in m.nodes])
        self.assertIn("node dummy pesta\xf1as\n", write(m))
        self.assertEqual(_binary.written_anim_root("pesta\xf1as", False, False, [m.find_node("pesta\xf1as")]),
                         "PESTA\xf1AS")

    def test_duplicate_animations_kept_in_order(self):
        """zspec_animdup (MDL-ASCII-062), as TestCompileKeepsDuplicateAnimationsInOrder."""
        src, golden = _engine("zspec_animdup")
        describe = lambda m: [(a.name.lower(), a.length) for a in m.animations]  # noqa: E731
        want = describe(decompile(golden))
        self.assertEqual(describe(decompile(compile(parse(src).model))), want)
        self.assertEqual([n for n, _ in want][:2], ["swing", "idle"])

    def test_in_order_generated_tangents(self):
        first = compile_text(IN_ORDER_SEAM)
        m = decompile(first)
        fold = m.find_node("fold").mesh
        self.assertEqual(len(fold.tangents), 0, "tangents written: a compile generates the same ones")
        self.assertTrue(fold.tangents_omitted, "the mesh should say its stored tangents were left out")
        self.assertFalse(decompile(first, keep_vertex_splits=True).find_node("fold").mesh.tangents_omitted)
        self.assertEqual(len(fold.verts), 4, "the seam's positions not shared")
        self.assertEqual(compile_text(write(m)), first)

    def test_after_dropped_face(self):
        first = compile_text(DROPPED_FACE)
        a = decompile(first, keep_vertex_splits=True).find_node("strip").mesh
        self.assertEqual(len(a.faces), 5)
        m = decompile(first)
        self.assertEqual(len(m.find_node("strip").mesh.verts), 9, "a seam not shared")
        b = decompile(compile_text(write(m)), keep_vertex_splits=True).find_node("strip").mesh
        self.assertEqual((a.verts, a.tverts, a.normals), (b.verts, b.tverts, b.normals))
        self.assertEqual([f.verts for f in a.faces], [f.verts for f in b.faces])

    def test_stored_neighbours_renumbers(self):
        m = MeshData()
        m.verts = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0), (0.0, 1.0, 0.0), (2.0, 1.0, 0.0)]
        m.faces = [Face((0, 1, 2)), Face((0, 2, 3)), Face((1, 4, 2))]
        m.stored_adjacency = [[-1, -1, 2], [-1, 0, -1], [0, -1, -1]]
        self.assertEqual(stored_neighbours(m), [[-1, -1, 1], [0, -1, -1], [-1, -1, -1]])
        m.stored_adjacency = [[-1, 2, 1], [0, -1, -1], [-1, -1, 0]]
        self.assertEqual(stored_neighbours(m), [[-1, 2, 1], [0, -1, -1], [-1, -1, 0]])

    def test_model_bounds_from_stored_quaternion(self):
        a = tuple(f32(v) for v in (-0.5202803, 0.5914203, -0.6160604, 2.1787696))
        b = tuple(f32(v) for v in (-0.5202803, 0.5914203, -0.6160603, 2.1787696))
        self.assertEqual([f32_bits(v) for v in axis_angle_to_quat(a)], [f32_bits(v) for v in axis_angle_to_quat(b)])

        def box(o):
            m = Model()
            root = Node("root", "NULL")
            mesh = Node("mesh", "root")
            mesh.orientation = o
            mesh.position = (5.0, 5.0, 0.0)
            mesh.mesh = MeshData()
            mesh.mesh.verts = [tuple(f32(c) for c in v) for v in
                               ((-3.1, 2.7, 0.9), (4.3, -1.9, 2.2), (0.7, 5.3, -3.3), (12.5, 7.25, 3.125))]
            m.nodes = [root, mesh]
            bmin, bmax, _ = model_bounds(m)
            return [f32_bits(v) for v in bmin + bmax]

        self.assertEqual(box(a), box(b))

    def test_order_duplicate_child_name(self):
        text = tsort50().replace("node trimesh m03\n", "node trimesh m02\n", 1)
        first = compile_text(text)
        again = compile_text(write(decompile(first)))
        names = [(n.parent, n.name) for n in stored_tree(first).nodes]
        self.assertEqual([(n.parent, n.name) for n in stored_tree(again).nodes], names)

    def test_order_duplicate_parent_name(self):
        nodes = [Node("root", "NULL"), Node("p", "root")]
        for _ in range(20):
            n = Node("m", "p")
            n.mesh = MeshData()
            n.mesh.transparency_hint = 1
            nodes.append(n)
        nodes.append(Node("p", "root"))
        m = Model()
        m.super_model = "NULL"
        m.nodes = list(nodes)
        declaration_order(m)
        _, children = resolve_geom_tree(m.nodes)
        reorder_children(m.nodes[0], children)
        self.assertTrue(_walks_as(m.nodes, children, nodes), "a compile of the order taken changes the stored tree")

    def test_order_per_parent(self):
        m = stored_tree(compile_text(tsort50()))
        top = Node("top", "NULL")
        m.nodes[0].parent = "top"

        def tri(name, hint):
            n = Node(name, "q")
            n.mesh = MeshData()
            n.mesh.transparency_hint = hint
            n.mesh.verts = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (0.0, 1.0, 0.0)]
            n.mesh.tverts = list(n.mesh.verts)
            n.mesh.faces = [Face((0, 1, 2), 1, (0, 1, 2))]
            return n

        tail = [Node("q", "top"), tri("qa", 1), Node("qb", "q")]
        tail += [tri("q%sx" % chr(ord("a") + i), 0) for i in range(20)]
        stored = [top] + m.nodes + tail
        m.nodes = list(stored)
        declaration_order(m)
        self.assertFalse(all(a is b for a, b in zip(m.nodes, stored)),
                         "stored order kept: the unsorted parent dropped the other's inverse sort")
        _, children = resolve_geom_tree(m.nodes)
        reorder_children(m.nodes[0], children)
        _, stored_kids = resolve_geom_tree(stored)
        self.assertTrue(all(a is b for a, b in zip(children[stored[1]], stored_kids[stored[1]])))
        _, from_stored = resolve_geom_tree(stored)
        reorder_children(stored[0], from_stored)
        q = tail[0]
        self.assertTrue(all(a is b for a, b in zip(children[q], from_stored[q])))

    def test_emitter_junk_texture(self):
        data = bytearray(compile_text(JUNK_EMITTER))
        at = data.find(b"zzjunkmarker")
        self.assertGreaterEqual(at, 0)
        data[at:at + 12] = bytes([0xCD, 0xCC, ord("L"), ord(">")] + [0] * 8)
        m = decompile(bytes(data))
        self.assertEqual(m.find_node("chunks").emitter.texture, "")
        self.assertNotIn("texture ", write(m))
        self.assertTrue(any('emitter "chunks": texture field holds bytes that are not text (cd cc 4c 3e)' in w.message
                            for w in m.warnings), [w.message for w in m.warnings])

    def test_light_controllers_once(self):
        text = write(decompile(compile_text(LIGHT)))
        for kw in ("shadowradius 7", "verticaldisplacement 2"):
            self.assertEqual(text.count(kw), 1, kw)

    def test_animmesh_normals(self):
        with open(os.path.join(ENGINE, "zspec_animnrm.mdl"), "rb") as f:
            golden = f.read()
        with open(os.path.join(ENGINE, "zspec_animnrm.ascii.mdl"), "rb") as f:
            ours = compile(parse(f.read()).model)
        want, got = write(decompile(golden)), write(decompile(ours))
        self.assertIn("normals 4", want.split("newanim", 1)[1])
        self.assertEqual([ln for ln in got.splitlines() if "orientation 0 0 1 0" not in ln],
                         [ln for ln in want.splitlines() if "orientation 0 0 1 0" not in ln])

    def test_order_stored_donor_hint(self):
        def quad(bitmap, render, dx, hint):
            q = MeshData()
            q.bitmap = bitmap
            q.render = render
            q.transparency_hint = hint
            q.verts = [(dx, 0.0, 0.0), (dx + 1, 0.0, 0.0), (dx + 1, 1.0, 0.0), (dx, 1.0, 0.0)]
            q.tverts = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0), (0.0, 1.0, 0.0)]
            q.faces = [Face((0, 1, 2), 0, (0, 1, 2)), Face((0, 2, 3), 0, (0, 2, 3))]
            return q

        def node(name, mesh, number, x=0.0):
            n = Node(name, "root")
            n.mesh = mesh
            n.part_number = number
            n.position = (x, 0.0, 0.0)
            return n

        root = Node("root", "NULL")
        b = node("b", None, 3)
        lower = node("lower", quad("tex", 0, 5.0, 0), 1, -3.0)
        trim = node("trim", quad("othertex", 1, 9.0, 1), 2)
        wall = node("wall", quad("tex", 1, 0.0, 2), 4)
        wall.mesh.verts += [(2.0, 0.0, 0.0), (3.0, 0.0, 0.0), (3.0, 1.0, 0.0), (2.0, 1.0, 0.0)]
        wall.mesh.tverts += [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0), (0.0, 1.0, 0.0)]
        d = _Decompiler(b"", [])
        d.model.super_model = "NULL"
        d.model.nodes = [root, b, lower, trim, wall]
        d.render_lists = {wall: [0, 1, 2, 0, 2, 3, 4, 5, 6, 4, 6, 7], trim: []}
        unmerge_render_lists(d)
        self.assertEqual((lower.mesh.render, lower.mesh.transparency_hint), (1, 2))
        self.assertEqual(list(d.stored_hints.items()), [(lower, 0)])
        declaration_order(d.model, d.stored_hints)
        self.assertEqual([n.name for n in d.model.nodes], ["root", "lower", "trim", "b", "wall"])
        self.assertEqual(lower.mesh.transparency_hint, 2)

    def test_order_donor_sorted_by_new_hint(self):
        # trlk1_o08_01's plane10: the donor's stored hint 1 was the only one above
        # 0; restored with its receiver's 0, nothing is sorted, and the stored
        # order must be kept
        def quad(bitmap, render, dx):
            q = MeshData()
            q.bitmap = bitmap
            q.render = render
            q.verts = [(dx, 0.0, 0.0), (dx + 1, 0.0, 0.0), (dx + 1, 1.0, 0.0), (dx, 1.0, 0.0)]
            q.tverts = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0), (0.0, 1.0, 0.0)]
            q.faces = [Face((0, 1, 2), 0, (0, 1, 2)), Face((0, 2, 3), 0, (0, 2, 3))]
            return q

        def node(name, mesh, x=0.0):
            n = Node(name, "root")
            n.mesh = mesh
            n.position = (x, 0.0, 0.0)
            return n

        stored = [Node("root", "NULL")] + [node("m%d" % i, quad("other", 1, 10.0 + 2 * i)) for i in range(20)]
        wall = node("wall", quad("tex", 1, 0.0))
        wall.mesh.verts += [(2.0, 0.0, 0.0), (3.0, 0.0, 0.0), (3.0, 1.0, 0.0), (2.0, 1.0, 0.0)]
        wall.mesh.tverts += [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0), (0.0, 1.0, 0.0)]
        lower = node("lower", quad("tex", 0, 5.0), -3.0)
        lower.mesh.transparency_hint = 1
        stored += [wall, lower]
        d = _Decompiler(b"", [])
        d.model.super_model = "NULL"
        d.model.nodes = list(stored)
        d.render_lists = {wall: [0, 1, 2, 0, 2, 3, 4, 5, 6, 4, 6, 7]}
        unmerge_render_lists(d)
        self.assertEqual((lower.mesh.transparency_hint, d.stored_hints.get(lower)), (0, 1))
        declaration_order(d.model, d.stored_hints)
        _, children = resolve_geom_tree(d.model.nodes)
        reorder_children(d.model.nodes[0], children)
        self.assertTrue(_walks_as(d.model.nodes, children, stored), [n.name for n in d.model.nodes])

    def test_order_part_numbers_with_stored_donor_hint(self):
        # plc_tn2_13: part-number order planned with the donor's stored hint gives
        # the numbers, and its compile moves only the donor
        def quad(bitmap, render, dx, hint=0):
            q = MeshData()
            q.bitmap = bitmap
            q.render = render
            q.transparency_hint = hint
            q.verts = [(dx, 0.0, 0.0), (dx + 1, 0.0, 0.0), (dx + 1, 1.0, 0.0), (dx, 1.0, 0.0)]
            q.tverts = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0), (0.0, 1.0, 0.0)]
            q.faces = [Face((0, 1, 2), 0, (0, 1, 2)), Face((0, 2, 3), 0, (0, 2, 3))]
            return q

        def node(name, mesh, number, x=0.0):
            n = Node(name, "root")
            n.mesh = mesh
            n.part_number = number
            n.position = (x, 0.0, 0.0)
            return n

        root = Node("root", "NULL")
        b = node("b", None, 3)
        lower = node("lower", quad("tex", 0, 5.0), 2, -3.0)
        trim = node("trim", quad("othertex", 1, 9.0, 1), 1)
        wall = node("wall", quad("tex", 1, 0.0, 1), 4)
        wall.mesh.verts += [(2.0, 0.0, 0.0), (3.0, 0.0, 0.0), (3.0, 1.0, 0.0), (2.0, 1.0, 0.0)]
        wall.mesh.tverts += [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0), (0.0, 1.0, 0.0)]
        d = _Decompiler(b"", [])
        d.model.super_model = "NULL"
        d.model.nodes = [root, b, lower, trim, wall]
        d.render_lists = {wall: [0, 1, 2, 0, 2, 3, 4, 5, 6, 4, 6, 7], trim: []}
        unmerge_render_lists(d)
        self.assertEqual((lower.mesh.transparency_hint, d.stored_hints.get(lower)), (1, 0))
        declaration_order(d.model, d.stored_hints)
        self.assertEqual([n.name for n in d.model.nodes], ["root", "trim", "lower", "b", "wall"])

    def test_order_part_numbers_duplicate_leaf(self):
        first = compile_text(DUP_LEAF)
        again = compile_text(write(decompile(first)))
        tree = [(n.parent, n.name, n.part_number) for n in stored_tree(first).nodes]
        self.assertEqual([(n.parent, n.name, n.part_number) for n in stored_tree(again).nodes], tree)

    def test_order_child_named_like_root(self):
        text = ["newmodel dpr\nsetsupermodel dpr NULL\nclassification TILE\nsetanimationscale 1.0\n"
                "beginmodelgeom dpr\n  node dummy dpr\n    parent NULL\n  endnode\n  node dummy dpr\n    parent dpr\n  endnode\n"]
        for i in range(24):
            text.append("  node trimesh m%02d\n    parent dpr\n    transparencyhint %d\n    bitmap blank\n"
                        "    verts 3\n      0 0 0\n      1 0 0\n      0 1 %d\n    faces 1\n      0 1 2  1  0 0 0  0\n"
                        "  endnode\n" % (i, 1 if i == 3 else 0, i))
        text.append("endmodelgeom dpr\ndonemodel dpr\n")
        first = compile_text("".join(text))
        again = compile_text(write(decompile(first)))
        tree = [(n.parent, n.name) for n in stored_tree(first).nodes]
        self.assertEqual([(n.parent, n.name) for n in stored_tree(again).nodes], tree)

    def test_write_duplicate_parent_name_tree(self):
        first = compile_text(DUP_PARENT_BRANCHES)
        again = compile_text(write(decompile(first)))
        tree = [(n.parent, n.name, n.part_number) for n in stored_tree(first).nodes]
        self.assertEqual([(n.parent, n.name, n.part_number) for n in stored_tree(again).nodes], tree)

    def test_duplicate_anim_controller_keeps_last(self):
        data = bytearray(compile_text(DUP_CTRL))
        hits = [i for i in range(len(data) - 11) if data[i:i + 6] == bytes([36, 0, 0, 0, 1, 0]) and data[i + 10] == 1]
        self.assertEqual(len(hits), 1)
        data[hits[0]] = 128
        an = decompile(bytes(data)).animations[0].nodes[1]
        self.assertEqual(an.float_keys.get("alpha"), [(0.0, 0.25)])
        self.assertNotIn("scale", an.float_keys)

    def test_built_aabb_tree_planes(self):
        model = parse(BUILT_AABB.encode("latin-1")).model
        tree = build_aabb_tree(model.find_node("walk").mesh)
        for e in tree:
            if e.leaf_face >= 0:
                self.assertEqual(e.plane, 0)
            else:
                self.assertEqual(e.plane, aabb_split_plane(tree[e.left], tree[e.right]))
        first = compile(model)
        again = compile_text(write(decompile(first)))

        def entries(data):
            return [(e.bound_min, e.bound_max, e.leaf_face, e.plane) for e in decompile(data).find_node("walk").aabb.entries]

        self.assertEqual(entries(again), entries(first))

    def test_anim_mesh_keys_without_geometry(self):
        first = compile_text(ANIM_NO_GEOM)
        m = decompile(first)
        self.assertEqual(compile_text(write(m)), first)
        an = m.animations[0].nodes[2]
        self.assertEqual((an.name, an.type, len(an.float_keys.get("alpha", []))), ("wg_closed", "trimesh", 2))
        text = write(m)
        self.assertIn("node trimesh wg_closed", text)
        self.assertIn("node dummy gone", text)

    def test_anim_nodes_without_geometry_follow_type_word(self):
        # MDL-NODE-044, the game's compile of zspec_animnogeo
        with open(os.path.join(ENGINE, "zspec_animnogeo.mdl"), "rb") as f:
            golden = f.read()
        with open(os.path.join(ENGINE, "zspec_animnogeo.ascii.mdl"), "rb") as f:
            ours = compile(parse(f.read()).model)
        wants = {
            "x": ("trimesh", ["alpha", "orientation", "position"]),
            "y": ("light", ["color", "position", "radius"]),
            "z": ("trimesh", ["scale", "selfillumcolor"]),
            "w": ("", ["position"]),
            "v": ("trimesh", ["position"]),
            "u": ("emitter", ["birthrate", "position"]),
            "t": ("", []),
        }

        def ctrls(an):
            out = [k for k, v in an.float_keys.items() if v] + [k for k, v in an.color_keys.items() if v]
            if an.position_keys:
                out.append("position")
            if an.orientation_keys:
                out.append("orientation")
            return sorted(out)

        for label, data in (("game", golden), ("ours", ours)):
            got = {an.name: (an.type, ctrls(an)) for an in decompile(data).animations[0].nodes if an.name in wants}
            self.assertEqual(got, wants, label)
        text = write(decompile(golden))
        for line in ("node trimesh x", "node light y", "node trimesh v", "node emitter u", "node dummy t"):
            self.assertIn(line, text)
        self.assertEqual(compile_text(write(decompile(ours))), ours)


DUP_LEAF = """newmodel pdl
setsupermodel pdl NULL
classification TILE
setanimationscale 1.0
beginmodelgeom pdl
  node dummy pdl
    parent NULL
  endnode
  node dummy a
    parent pdl
  endnode
  node dummy m
    parent pdl
  endnode
  node dummy m
    parent a
  endnode
endmodelgeom pdl
donemodel pdl
"""

DUP_PARENT_BRANCHES = """newmodel wdp
setsupermodel wdp NULL
classification PLACEABLE
setanimationscale 1.0
beginmodelgeom wdp
  node dummy wdp
    parent NULL
  endnode
  node dummy p
    parent wdp
  endnode
  node dummy q
    parent p
  endnode
  node dummy c1
    parent q
  endnode
  node dummy q
    parent wdp
  endnode
  node dummy c2
    parent q
  endnode
endmodelgeom wdp
donemodel wdp
"""

DUP_CTRL = """newmodel dac
setsupermodel dac NULL
classification PLACEABLE
setanimationscale 1.0
beginmodelgeom dac
  node dummy dac
    parent NULL
  endnode
  node trimesh quad
    parent dac
    bitmap blank
    verts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2  1  0 0 0  0
  endnode
endmodelgeom dac
newanim fade dac
  length 1
  transtime 0
  animroot dac
  node dummy dac
    parent NULL
  endnode
  node trimesh quad
    parent dac
    scalekey 1
      0 0.5
    alphakey 1
      0 0.25
  endnode
doneanim fade dac
donemodel dac
"""

BUILT_AABB = """newmodel bat
setsupermodel bat NULL
classification TILE
setanimationscale 1.0
beginmodelgeom bat
  node dummy bat
    parent NULL
  endnode
  node aabb walk
    parent bat
    bitmap blank
    verts 9
      0 0 0
      1 0 0
      2 0 0
      0 1 0
      1 1 0
      2 1 0
      0 3 0
      1 3 0
      2 3 0
    faces 8
      0 1 4  1  0 0 0  1
      0 4 3  1  0 0 0  1
      1 2 5  1  0 0 0  1
      1 5 4  1  0 0 0  1
      3 4 7  1  0 0 0  1
      3 7 6  1  0 0 0  1
      4 5 8  1  0 0 0  1
      4 8 7  1  0 0 0  1
  endnode
endmodelgeom bat
donemodel bat
"""

ANIM_NO_GEOM = """newmodel agn
setsupermodel agn NULL
classification DOOR
setanimationscale 1.0
beginmodelgeom agn
  node dummy agn
    parent NULL
  endnode
  node dummy grnd
    parent agn
  endnode
endmodelgeom agn
newanim closed agn
  length 1
  transtime 0
  animroot agn
  node dummy agn
    parent NULL
  endnode
  node dummy gone
    parent agn
  endnode
  node trimesh wg_closed
    parent gone
    alphakey 2
      0 0
      1 1
  endnode
doneanim closed agn
donemodel agn
"""


if __name__ == "__main__":
    unittest.main()
