package mdl

import (
	"bytes"
	"strings"
	"testing"
)

// Two bugs found while fixing Layonara's models (layonara/nwn-haks#104).

const smoothGroupModel = `newmodel sg
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
`

// A negative smoothing group (group 32 is the sign bit) was written as an
// unsigned 64-bit number, 18446744071562067969, which no reader gives back
// (glibc's sscanf("%d") reads -1, cleanmodels 0). pfb0_robe117 and
// pfb2_robe117 lost the smoothing of Skirt_B that way. It is written signed,
// as the game reads it (MDL-ASCII-041, MDL-ASCII-052).
func TestNegativeSmoothGroupIsWrittenSigned(t *testing.T) {
	pr, err := Parse(strings.NewReader(smoothGroupModel))
	if err != nil {
		t.Fatal(err)
	}
	if g := pr.Model.FindNode("Skirt_B").Mesh.Faces[0].SmoothGroup; g != -2147483647 {
		t.Fatalf("parsed smoothing group %d", g)
	}
	var out bytes.Buffer
	if err := Write(pr.Model, &out); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(out.String(), "0 1 2 -2147483647 0 1 2 0") {
		t.Errorf("face written as:\n%s", out.String())
	}
	again, err := Parse(bytes.NewReader(out.Bytes()))
	if err != nil {
		t.Fatal(err)
	}
	if len(again.Errors) != 0 {
		t.Errorf("rewritten model has parse warnings: %v", again.Errors)
	}
	if g := again.Model.FindNode("Skirt_B").Mesh.Faces[0].SmoothGroup; g != -2147483647 {
		t.Errorf("smoothing group after a rewrite: %d", g)
	}
}

// tps01_gdoor_01 had a pasted tail after donemodel: two node blocks outside
// any block, then more animations. The game does not stop at donemodel, but
// outside a block it runs only the top-level keywords, so it reads the
// animations and not the nodes. cleanmodels added the nodes to the geometry.
func TestNodesAfterDoneModelAreNotRead(t *testing.T) {
	src := strings.Replace(smoothGroupModel, "donemodel sg\n", `donemodel sg
e
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
`, 1)
	pr, err := Parse(strings.NewReader(src))
	if err != nil {
		t.Fatal(err)
	}
	m := pr.Model
	if n := m.FindNode("sgimpc"); n != nil {
		t.Errorf("the node after donemodel was read: %+v", n)
	}
	if len(m.Nodes) != 2 {
		t.Errorf("%d nodes, want 2", len(m.Nodes))
	}
	if len(m.Animations) != 1 || m.Animations[0].Name != "opened2" {
		t.Errorf("animations %+v, want opened2, which the game reads", m.Animations)
	}
	a := m.AfterDoneModel
	if a == nil || a.Line != 24 || a.Ignored != 5 || len(a.Nodes) != 1 || a.Nodes[0] != "sgimpc" ||
		len(a.Animations) != 1 || a.Animations[0] != "opened2" {
		t.Errorf("AfterDoneModel = %+v", a)
	}
	for _, e := range pr.Errors {
		t.Errorf("parse warning for a line the game does not read: %v", e)
	}
	var bin bytes.Buffer
	if err := Compile(m, &bin); err != nil {
		t.Fatal(err)
	}
	dm, err := Decompile(bytes.NewReader(bin.Bytes()), int64(bin.Len()))
	if err != nil {
		t.Fatal(err)
	}
	if dm.FindNode("sgimpc") != nil || len(dm.Nodes) != 2 {
		t.Errorf("compiled model has %d nodes", len(dm.Nodes))
	}

	// A model that ends at donemodel has nothing there.
	pr, _ = Parse(strings.NewReader(smoothGroupModel + "\n# comment\n\n"))
	if pr.Model.AfterDoneModel != nil {
		t.Errorf("AfterDoneModel for a model that ends at donemodel: %+v", pr.Model.AfterDoneModel)
	}
}
