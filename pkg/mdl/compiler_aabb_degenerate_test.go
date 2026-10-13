package mdl

import (
	"bytes"
	"testing"
)

// A walkmesh (AABB) node whose second face collapses to a line (it names vertex 0 twice).
const aabbDegenerateSrc = `newmodel aabbdeg
setsupermodel aabbdeg NULL
classification TILE
setanimationscale 1.00
beginmodelgeom aabbdeg
  node dummy aabbdeg
    parent NULL
  endnode
  node aabb wok
    parent aabbdeg
    verts 4
      0 0 0
      1 0 0
      0 1 0
      1 1 0
    faces 3
      0 1 2 1 0 1 2 4
      0 0 1 1 0 0 1 4
      1 3 2 1 1 3 2 4
    aabb 5
      0 0 0 1 1 0 -1
      0 0 0 1 1 0 0
      0 0 0 1 1 0 -1
      0 0 0 1 1 0 1
      0 0 0 1 1 0 2
  endnode
endmodelgeom
donemodel aabbdeg
`

// The engine deletes degenerate faces on walkmesh nodes too, but its delete loop
// (MdlNodeTriMesh/AABB InternalGenVertices) never touches the authored tree, so
// the leaf face indices would point one face too far afterwards (see
// NWMAIN_FINDINGS.md, finding 6, and 121 real walkmesh models that have such a
// face). We keep every face on AABB nodes so the tree stays valid.
func TestAabbNodeKeepsItsDegenerateFaces(t *testing.T) {
	m := mustParseASCII(t, aabbDegenerateSrc)
	node := m.FindNode("wok")
	if node == nil || node.Mesh == nil || len(node.Mesh.Faces) != 3 {
		t.Fatalf("source not parsed as expected: %+v", node)
	}

	// Under the generic rule that other meshes get, the second face is degenerate.
	stripped, err := buildExpandedMeshOpts(node.Mesh, expandOpts{stripDegenerate: true})
	if err != nil {
		t.Fatal(err)
	}
	if len(stripped.faceVerts) != 2 {
		t.Fatalf("test setup: expected the degenerate face to be strippable, got %d faces", len(stripped.faceVerts))
	}

	var bin bytes.Buffer
	if err := Compile(m, &bin); err != nil {
		t.Fatal(err)
	}
	back, err := Decompile(bytes.NewReader(bin.Bytes()), int64(bin.Len()))
	if err != nil {
		t.Fatal(err)
	}
	got := back.FindNode("wok")
	if got == nil || got.Mesh == nil {
		t.Fatal("walkmesh node lost in the round trip")
	}
	if len(got.Mesh.Faces) != 3 {
		t.Errorf("compiled walkmesh has %d faces, want all 3 kept so the authored tree stays valid", len(got.Mesh.Faces))
	}
	if got.Aabb == nil || len(got.Aabb.Entries) != 5 {
		t.Errorf("tree changed: %+v", got.Aabb)
	}
}
