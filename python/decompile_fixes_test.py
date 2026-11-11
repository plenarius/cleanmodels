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
from cleanmodels.binary_order import declaration_order  # noqa: E402
from cleanmodels.compiler_order import reorder_children  # noqa: E402
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
    _binary.declaration_order = lambda m: None
    try:
        m = decompile(data)
    finally:
        _binary.declaration_order = keep
    return m


class RoundTripFixes(unittest.TestCase):
    def test_in_order_generated_tangents(self):
        first = compile_text(IN_ORDER_SEAM)
        m = decompile(first)
        fold = m.find_node("fold").mesh
        self.assertEqual(len(fold.tangents), 0, "tangents written: a compile generates the same ones")
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
        self.assertTrue(all(a is b for a, b in zip(m.nodes, nodes)), "reordered although a duplicate is a parent")

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



if __name__ == "__main__":
    unittest.main()
