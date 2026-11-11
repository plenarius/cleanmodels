"""Parser and writer fixes, as pkg/mdl/haks104_test.go tests them.

    python python/parse_fixes_test.py

Found while fixing Layonara's models (layonara/nwn-haks#104).
"""

import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from cleanmodels import compile, decompile, parse, write  # noqa: E402,A004

SMOOTH_GROUP = """newmodel sg
setsupermodel sg NULL
classification PLACEABLE
beginmodelgeom sg
  node dummy sg
    parent NULL
  endnode
  node trimesh Skirt_B
    parent sg
    bitmap skirt
    verts 3
      0 0 0
      1 0 0
      0 1 0
    tverts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2 -2147483647 0 1 2 0
  endnode
endmodelgeom sg
donemodel sg
"""

TAIL = """e
node dummy sgimpc
  parent sg
endnode
doneanim closed sg

newanim opened2 sg
  length 1
  transtime 0.25
  animroot sg
  node dummy sg
    parent NULL
  endnode
doneanim opened2 sg
donemodel sg
"""


class ParseFixes(unittest.TestCase):
    def test_negative_smooth_group_written_signed(self):
        text = write(parse(SMOOTH_GROUP.encode("latin-1")).model)
        self.assertIn("0 1 2 -2147483647 0 1 2 0", text)
        again = parse(text.encode("latin-1"))
        self.assertEqual(again.errors, [])
        self.assertEqual(again.model.find_node("Skirt_B").mesh.faces[0].smooth_group, -2147483647)

    def test_nodes_after_donemodel_not_read(self):
        r = parse((SMOOTH_GROUP + TAIL).encode("latin-1"))
        m = r.model
        self.assertIsNone(m.find_node("sgimpc"))
        self.assertEqual(len(m.nodes), 2)
        self.assertEqual([a.name for a in m.animations], ["opened2"])
        a = m.after_done_model
        self.assertEqual((a.line, a.lines, a.ignored, a.nodes, a.animations), (24, 14, 5, ["sgimpc"], ["opened2"]))
        self.assertEqual(r.errors, [])
        self.assertEqual(len(decompile(compile(m)).nodes), 2)
        self.assertIsNone(parse((SMOOTH_GROUP + "\n# comment\n\n").encode("latin-1")).model.after_done_model)


if __name__ == "__main__":
    unittest.main()
