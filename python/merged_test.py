"""Unit tests of the merged render lists (binary_merged.go, pkg/mdl/binary_merged_test.go).

    python python/merged_test.py

Each test builds the decompiler's state by hand, as the Go tests do, so the
port's own logic is checked, not only its output on the parity fixtures.
"""

import math
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from cleanmodels.binary import _Decompiler, unmerge_render_lists  # noqa: E402
from cleanmodels.types import Animation, AnimNode, Face, MeshData, Node  # noqa: E402


def merge_quad(bitmap, render, dx):
    """mergeQuad: a 1 x 1 quad at x offset dx, two triangles, vertices 0-3."""
    m = MeshData()
    m.bitmap = bitmap
    m.render = render
    m.verts = [(dx, 0.0, 0.0), (dx + 1, 0.0, 0.0), (dx + 1, 1.0, 0.0), (dx, 1.0, 0.0)]
    m.tverts = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0), (0.0, 1.0, 0.0)]
    m.faces = [Face((0, 1, 2), 0, (0, 1, 2)), Face((0, 2, 3), 0, (0, 2, 3))]
    return m


def node(name, mesh, position=(0.0, 0.0, 0.0)):
    n = Node(name)
    n.mesh = mesh
    n.position = position
    return n


def merged_model(extra=False):
    """mergedModel: receiver "wall" drawing its quad and a copy of donor "lower"'s."""
    recv = node("wall", merge_quad("tex", 1, 0.0))
    donor = node("lower", merge_quad("tex", 0, 5.0), (-3.0, 0.0, 0.0))
    other = node("trim", merge_quad("othertex", 0, 9.0))
    recv.mesh.verts += [(2.0, 0.0, 0.0), (3.0001, 0.0, 0.0), (3.0, 1.0, 0.0), (2.0, 1.0, 0.0)]
    recv.mesh.tverts += [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0), (0.0, 1.0, 0.0)]
    rl = [0, 1, 2, 0, 2, 3, 4, 5, 6, 4, 6, 7]
    if extra:
        recv.mesh.verts += [(0.0, 0.0, 3.0), (0.0, 2.0, 3.0), (0.0, 0.0, 4.0)]
        recv.mesh.tverts += [(0.5, 0.0, 0.0), (0.5, 1.0, 0.0), (0.2, 0.0, 0.0)]
        rl += [8, 9, 10]
    d = _Decompiler(b"", [])
    d.model.nodes = [recv, donor, other]
    d.render_lists = {recv: rl, other: []}
    return d


def f32(x):
    import struct
    return struct.unpack("<f", struct.pack("<f", x))[0]


class MergedRenderLists(unittest.TestCase):
    def test_restores_donor(self):
        d = merged_model()
        unmerge_render_lists(d)
        m = d.model
        self.assertEqual(m.find_node("lower").mesh.render, 1)
        self.assertEqual(m.find_node("trim").mesh.render, 0)
        self.assertEqual(m.warnings, [])
        self.assertEqual(len(m.notes), 1)
        self.assertIn("also draws lower", m.notes[0].message)
        self.assertEqual(m.render_merges, [("wall", ["lower"], 0, [])])

    def test_takes_receiver_drawing(self):
        d = merged_model()
        recv, donor = d.model.find_node("wall"), d.model.find_node("lower")
        recv.mesh.specular = (f32(0.05),) * 3
        recv.mesh.shininess = 10.0
        recv.mesh.self_illum_color = (0.0, f32(0.38), f32(0.02))
        recv.declared.add("selfillumcolor")
        donor.mesh.transparency_hint = 1
        donor.mesh.rotate_texture = 1
        donor.mesh.alpha = f32(0.6)
        donor.declared.add("alpha")
        donor.mesh.self_illum_color = (0.0, f32(0.27), f32(0.01))
        unmerge_render_lists(d)
        m = donor.mesh
        self.assertEqual(m.render, 1)
        self.assertEqual((m.specular, m.shininess, m.transparency_hint, m.rotate_texture, m.alpha, m.self_illum_color),
                         (recv.mesh.specular, 10.0, 0, 0, 1.0, recv.mesh.self_illum_color))
        self.assertIn("selfillumcolor", donor.declared)
        self.assertEqual(len(d.model.notes), 2)
        self.assertIn("lower (specular, shininess, transparencyhint, rotatetexture, alpha, selfillumcolor)",
                      d.model.notes[1].message)

    def test_drops_copy_vertices(self):
        for extra in (False, True):
            d = merged_model(extra)
            recv = d.model.find_node("wall")
            recv.mesh.normals = [(0.0, 0.0, 1.0)] * len(recv.mesh.verts)
            unmerge_render_lists(d)
            m = recv.mesh
            self.assertEqual((len(m.verts), len(m.tverts), len(m.normals)), (4, 4, 4), extra)
            self.assertEqual(m.faces[1].verts, [0, 2, 3])
            if extra:
                self.assertEqual(d.model.render_merges[0][2], 1)

    def tangent_model(self):
        d = merged_model()
        recv, donor = d.model.find_node("wall"), d.model.find_node("lower")
        rm = recv.mesh
        rm.verts += [rm.verts[4], rm.verts[6]]
        rm.tverts += [rm.tverts[4], rm.tverts[6]]
        rm.tangents = [(1.0, 0.0, 0.0, 1.0)] * len(rm.verts)
        for i in (7, 8, 9):
            rm.tangents[i] = (1.0, 0.0, 0.0, -1.0)
        d.render_lists[recv] = [0, 1, 2, 0, 2, 3, 4, 5, 6, 8, 9, 7]
        donor.mesh.tangents = [(1.0, 0.0, 0.0, 1.0)] * 4
        return d, donor

    def test_donor_tangent_clones(self):
        d, donor = self.tangent_model()
        unmerge_render_lists(d)
        m = donor.mesh
        self.assertEqual((len(m.verts), len(m.tverts), len(m.tangents)), (6, 6, 6))
        self.assertEqual([f.verts for f in m.faces], [[0, 1, 2], [4, 5, 3]])
        self.assertEqual(m.faces[1].uvs, m.faces[1].verts)
        self.assertEqual([t[3] for t in m.tangents], [1, 1, 1, -1, -1, -1])
        self.assertEqual((m.verts[4], m.verts[5], m.tverts[4]), (m.verts[0], m.verts[2], m.tverts[0]))

    def test_donor_tangent_clones_reused(self):
        d, donor = self.tangent_model()
        m = donor.mesh
        m.verts += [m.verts[0], m.verts[2]]
        m.tverts += [m.tverts[0], m.tverts[2]]
        m.tangents += [(1.0, 0.0, 0.0, -1.0), (1.0, 0.0, 0.0, -1.0)]
        unmerge_render_lists(d)
        self.assertEqual(len(m.verts), 6)
        self.assertEqual(m.faces[1].verts, [4, 5, 3])
        self.assertEqual(m.tangents[3][3], -1)

    def test_donor_normal_conflict_splits(self):
        d = merged_model()
        recv, donor = d.model.find_node("wall"), d.model.find_node("lower")
        rm = recv.mesh
        rm.verts.append(rm.verts[4])
        rm.tverts.append(rm.tverts[4])
        rm.normals = [(0.0, 0.0, 1.0)] * len(rm.verts)
        rm.normals[8] = (f32(0.6), 0.0, f32(0.8))
        d.render_lists[recv] = [0, 1, 2, 0, 2, 3, 4, 5, 6, 8, 6, 7]
        donor.mesh.normals = [(0.0, 0.0, 1.0)] * 4
        unmerge_render_lists(d)
        m = donor.mesh
        self.assertEqual(len(m.verts), 5)
        self.assertEqual(m.faces[1].verts, [4, 2, 3])
        self.assertAlmostEqual(m.normals[4][0], 0.6, places=6)
        self.assertEqual(m.normals[0], (0.0, 0.0, 1.0))

    def test_folds_animated_donor(self):
        d = merged_model()
        recv, donor = d.model.find_node("wall"), d.model.find_node("lower")
        recv.mesh.shadow = donor.mesh.shadow = 1
        recv.mesh.stored_adjacency = [[-1, -1, 1], [0, -1, -1]]
        donor.mesh.stored_adjacency = [[-1, -1, 1], [0, -1, -1]]
        donor.mesh.faces[0].smooth_group = donor.mesh.faces[1].smooth_group = 4
        an = AnimNode("lower")
        an.position_keys = [(0.0, (-3.0, 0.0, 0.0), None, None), (1.0, (-3.0, 0.0, 1.0), None, None)]
        a = Animation("open")
        a.nodes = [an]
        d.model.animations = [a]
        unmerge_render_lists(d)
        m = recv.mesh
        self.assertEqual((donor.mesh.render, donor.mesh.shadow), (0, 0))
        self.assertEqual([f.verts for f in m.faces[2:]], [[4, 5, 6], [4, 6, 7]])
        self.assertEqual(m.faces[3].uvs, m.faces[3].verts)
        self.assertEqual(m.faces[2].smooth_group, 4)
        self.assertEqual(len(m.verts), 8)
        self.assertEqual(m.stored_adjacency[2:], [[-1, -1, 3], [2, -1, -1]])
        self.assertEqual(d.model.render_merges, [("wall", [], 0, ["lower"])])
        self.assertEqual(len(d.model.notes), 1)
        self.assertIn("lower (an animation moves lower, but the game drew it as part of wall)", d.model.notes[0].message)
        self.assertIn("lower get shadow 0", d.model.notes[0].message)

    def test_fold_rule(self):
        def scale(d):
            d.model.find_node("lower").scale = 1.5

        def receiver_moves(d):
            an = AnimNode("wall")
            an.orientation_keys = [(0.0, (0.0, 0.0, 1.0, 0.5))]
            a = Animation("a")
            a.nodes = [an]
            d.model.animations = [a]

        def alpha(d):
            an = AnimNode("wall")
            an.float_keys["alpha"] = [(0.0, 1.0), (1.0, f32(0.2))]
            a = Animation("a")
            a.nodes = [an]
            d.model.animations = [a]

        def at_rest(d):
            an = AnimNode("lower")
            an.position_keys = [(0.0, (-3.0, 0.0, 0.0), None, None)]
            an.orientation_keys = [(0.0, (1.0, 0.0, 0.0, 0.0))]
            an.float_keys["scale"] = [(0.0, 1.0)]
            a = Animation("a")
            a.nodes = [an]
            d.model.animations = [a]

        for name, setup, why in (("scale", scale, "without the scale 1.5 of lower"),
                                 ("receiver moves", receiver_moves, "an animation moves wall, with the copies"),
                                 ("alpha", alpha, "changes the alpha of wall"),
                                 ("at rest", at_rest, "")):
            d = merged_model()
            setup(d)
            unmerge_render_lists(d)
            folded = d.model.find_node("lower").mesh.render == 0
            self.assertEqual(folded, why != "", name)
            if why:
                self.assertEqual(len(d.model.notes), 1, name)
                self.assertIn(why, d.model.notes[0].message, name)

    def test_prefers_copy_in_place(self):
        recv = node("wall", merge_quad("tex", 1, 0.0))
        a = node("a", merge_quad("tex", 0, 5.0), (-3.0, 0.0, 0.0))
        b = node("b", merge_quad("tex", 0, 5.0), (7.0, 0.0, 0.0))
        recv.mesh.verts += [(12.0, 0.0, 0.0), (13.0, 0.0, 0.0), (13.0, 1.0, 0.0), (12.0, 1.0, 0.0),
                            (2.0, 0.0, 0.0), (3.0, 0.0, 0.0), (3.0, 1.0, 0.0), (2.0, 1.0, 0.0)]
        recv.mesh.tverts += [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0), (0.0, 1.0, 0.0)] * 2
        d = _Decompiler(b"", [])
        d.model.nodes = [recv, a, b]
        d.render_lists = {recv: [0, 1, 2, 0, 2, 3, 4, 5, 6, 4, 6, 7, 8, 9, 10, 8, 10, 11]}
        unmerge_render_lists(d)
        self.assertEqual(d.model.render_merges, [("wall", ["a", "b"], 0, [])])

    def test_donor_to_receiver_in_place(self):
        r1 = node("wall1", merge_quad("tex", 1, 0.0))
        r2 = node("wall2", merge_quad("tex", 1, 20.0))
        a = node("a", merge_quad("tex", 0, 5.0), (-3.0, 0.0, 0.0))  # drawn at 2..3, in wall2
        b = node("b", merge_quad("tex", 0, 5.0), (7.0, 0.0, 0.0))  # drawn at 12..13, in wall1
        r1.mesh.verts += [(12.0, 0.0, 0.0), (13.0, 0.0, 0.0), (13.0, 1.0, 0.0), (12.0, 1.0, 0.0)]
        r2.mesh.verts += [(2.0, 0.0, 0.0), (3.0, 0.0, 0.0), (3.0, 1.0, 0.0), (2.0, 1.0, 0.0)]
        for r in (r1, r2):
            r.mesh.tverts += [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0), (0.0, 1.0, 0.0)]
        rl = [0, 1, 2, 0, 2, 3, 4, 5, 6, 4, 6, 7]
        d = _Decompiler(b"", [])
        d.model.nodes = [r1, r2, a, b]
        d.render_lists = {r1: rl, r2: rl}
        unmerge_render_lists(d)
        self.assertEqual(d.model.render_merges, [("wall1", ["b"], 0, []), ("wall2", ["a"], 0, [])])

    def test_donor_normals_from_turned_copy(self):
        recv = node("wall", merge_quad("tex", 1, 0.0))
        donor = node("lower", merge_quad("tex", 0, 5.0), (2.0, -5.0, 0.0))
        donor.orientation = (0.0, 0.0, 1.0, f32(math.pi / 2))
        recv.mesh.verts += [(2.0, 0.0, 0.0), (2.0, 1.0, 0.0), (1.0, 1.0, 0.0), (1.0, 0.0, 0.0)]
        recv.mesh.tverts += [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0), (0.0, 1.0, 0.0)]
        recv.mesh.normals = [(0.0, 0.0, 0.0)] * 4 + [(0.0, 1.0, 0.0)] * 4
        donor.mesh.normals = [(0.0, 0.0, 0.0)] * 4
        d = _Decompiler(b"", [])
        d.model.nodes = [recv, donor]
        d.render_lists = {recv: [0, 1, 2, 0, 2, 3, 4, 5, 6, 4, 6, 7]}
        unmerge_render_lists(d)
        self.assertEqual(donor.mesh.render, 1)
        for n in donor.mesh.normals:
            self.assertAlmostEqual(n[0], 1, places=6)
            self.assertAlmostEqual(n[1], 0, places=6)
            self.assertAlmostEqual(n[2], 0, places=6)


if __name__ == "__main__":
    unittest.main()
