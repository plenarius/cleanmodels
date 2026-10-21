package mdl

import (
	"strings"
	"testing"
)

// mergeQuad is a 1 x 1 quad at x offset dx: two triangles, vertices 0-3, UVs
// from the corners.
func mergeQuad(bitmap string, render int32, dx float32) *MeshData {
	m := NewMeshData()
	m.Bitmap = bitmap
	m.Render = render
	m.Verts = []Vec3{{dx, 0, 0}, {dx + 1, 0, 0}, {dx + 1, 1, 0}, {dx, 1, 0}}
	m.TVerts = []Vec3{{0, 0, 0}, {1, 0, 0}, {1, 1, 0}, {0, 1, 0}}
	m.Faces = []Face{{Verts: [3]int32{0, 1, 2}, UVs: [3]int32{0, 1, 2}}, {Verts: [3]int32{0, 2, 3}, UVs: [3]int32{0, 2, 3}}}
	return m
}

// mergedModel: receiver "wall" (render 1) whose render list draws its own
// quad and a copy of donor "lower"'s (render 0, no list), placed elsewhere;
// extra adds a triangle to the list that no face has.
func mergedModel(extra bool) *decompiler {
	recv := &Node{Name: "wall", Mesh: mergeQuad("tex", 1, 0)}
	donor := &Node{Name: "lower", Mesh: mergeQuad("tex", 0, 5)}
	other := &Node{Name: "trim", Mesh: mergeQuad("othertex", 0, 9)}
	// The donor's copy in the receiver's frame: vertices 4-7, shifted,
	// with the 0.1 mm of float error moving it there leaves.
	recv.Mesh.Verts = append(recv.Mesh.Verts, Vec3{2, 0, 0}, Vec3{3.0001, 0, 0}, Vec3{3, 1, 0}, Vec3{2, 1, 0})
	recv.Mesh.TVerts = append(recv.Mesh.TVerts, Vec3{0, 0, 0}, Vec3{1, 0, 0}, Vec3{1, 1, 0}, Vec3{0, 1, 0})
	rl := []uint16{0, 1, 2, 0, 2, 3, 4, 5, 6, 4, 6, 7}
	if extra {
		recv.Mesh.Verts = append(recv.Mesh.Verts, Vec3{0, 0, 3}, Vec3{0, 2, 3}, Vec3{0, 0, 4})
		recv.Mesh.TVerts = append(recv.Mesh.TVerts, Vec3{0.5, 0, 0}, Vec3{0.5, 1, 0}, Vec3{0.2, 0, 0})
		rl = append(rl, 8, 9, 10)
	}
	d := &decompiler{model: &Model{Nodes: []*Node{recv, donor, other}}}
	d.renderLists = map[*Node][]uint16{recv: rl, other: nil}
	return d
}

func TestUnmergeRenderLists(t *testing.T) {
	d := mergedModel(false)
	d.unmergeRenderLists()
	m := d.model
	if m.FindNode("lower").Mesh.Render != 1 {
		t.Error("donor lower not put back to render 1")
	}
	if m.FindNode("trim").Mesh.Render != 0 {
		t.Error("trim (another texture, not in the list) changed")
	}
	if len(m.Warnings) != 0 {
		t.Errorf("warnings: %+v", m.Warnings)
	}
	if len(m.Notes) != 1 || m.Notes[0].Node != "wall" || !strings.Contains(m.Notes[0].Message, "also draws lower") {
		t.Errorf("notes: %+v", m.Notes)
	}
	if len(m.RenderMerges) != 1 || m.RenderMerges[0].Lost != 0 || len(m.RenderMerges[0].Donors) != 1 {
		t.Errorf("merges: %+v", m.RenderMerges)
	}
}

func TestUnmergeRenderListsWarnsLostTriangles(t *testing.T) {
	d := mergedModel(true)
	d.unmergeRenderLists()
	m := d.model
	if m.FindNode("lower").Mesh.Render != 1 {
		t.Error("donor lower not put back")
	}
	if len(m.RenderMerges) != 1 || m.RenderMerges[0].Lost != 1 {
		t.Fatalf("merges: %+v", m.RenderMerges)
	}
	if len(m.Warnings) != 1 || !strings.HasPrefix(m.Warnings[0].Message, "1 of the triangles") {
		t.Errorf("warnings: %+v", m.Warnings)
	}
}

// A mesh whose render list just repeats its faces (the usual case) is left
// alone, and so is an untextured donor matched by shape alone.
func TestUnmergeRenderListsUntexturedAndPlain(t *testing.T) {
	d := mergedModel(false)
	for _, n := range d.model.Nodes {
		n.Mesh.TVerts = nil
	}
	plain := &Node{Name: "floor", Mesh: mergeQuad("tex", 1, 20)}
	d.model.Nodes = append(d.model.Nodes, plain)
	d.renderLists[plain] = []uint16{0, 1, 2, 0, 2, 3}
	d.unmergeRenderLists()
	if d.model.FindNode("lower").Mesh.Render != 1 || len(d.model.RenderMerges) != 1 {
		t.Errorf("untextured donor not put back: %+v", d.model.RenderMerges)
	}
}

// A donor an animation moves was drawn still in game (its triangles are in
// the receiver's list): the note says it moves once put back.
func TestUnmergeRenderListsNotesAnimatedDonor(t *testing.T) {
	d := mergedModel(false)
	d.model.Animations = []Animation{{Name: "open", Nodes: []AnimNode{{Name: "lower",
		PositionKeys: []PositionKey{{Time: 0}, {Time: 1, Value: Vec3{0, 0, 1}}}}}}}
	d.unmergeRenderLists()
	if len(d.model.Notes) != 2 || !strings.Contains(d.model.Notes[1].Message, "animations move lower") {
		t.Errorf("notes: %+v", d.model.Notes)
	}
}
