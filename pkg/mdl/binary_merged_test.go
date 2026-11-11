package mdl

import (
	"math"
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
// quad and a copy of donor "lower"'s (render 0, no list); extra adds a
// triangle to the list that no face has. The donor's node is placed so its
// quad is where the copy is in world space.
func mergedModel(extra bool) *decompiler {
	recv := &Node{Name: "wall", Mesh: mergeQuad("tex", 1, 0)}
	donor := &Node{Name: "lower", Mesh: mergeQuad("tex", 0, 5), Position: Vec3{X: -3}}
	other := &Node{Name: "trim", Mesh: mergeQuad("othertex", 0, 9)}
	// The donor's copy in the receiver's frame: vertices 4-7, where the
	// donor is, with the 0.1 mm of float error moving it there leaves.
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

// A donor put back takes the normals its copy was drawn with, turned from the
// receiver's frame into its own: here the copy is turned 90 degrees about z,
// the donor's stored normals are (0, 0, 0) (back-to-back faces sharing
// vertices) and the copy's are (0, 1, 0) in the receiver's frame, which is
// (1, 0, 0) in the donor's.
func TestUnmergeRenderListsDonorNormalsFromCopy(t *testing.T) {
	recv := &Node{Name: "wall", Mesh: mergeQuad("tex", 1, 0)}
	// The donor's node turns it by 90 degrees about z and moves it, so its
	// (5,0) (6,0) (6,1) (5,1) are at the copy's (2,0) (2,1) (1,1) (1,0).
	donor := &Node{Name: "lower", Mesh: mergeQuad("tex", 0, 5), Position: Vec3{X: 2, Y: -5},
		Orientation: Vec4{Z: 1, W: math.Pi / 2}}
	recv.Mesh.Verts = append(recv.Mesh.Verts, Vec3{2, 0, 0}, Vec3{2, 1, 0}, Vec3{1, 1, 0}, Vec3{1, 0, 0})
	recv.Mesh.TVerts = append(recv.Mesh.TVerts, Vec3{0, 0, 0}, Vec3{1, 0, 0}, Vec3{1, 1, 0}, Vec3{0, 1, 0})
	recv.Mesh.Normals = make([]Vec3, 8)
	for i := 4; i < 8; i++ {
		recv.Mesh.Normals[i] = Vec3{0, 1, 0}
	}
	donor.Mesh.Normals = make([]Vec3, 4) // all (0, 0, 0)
	d := &decompiler{model: &Model{Nodes: []*Node{recv, donor}}}
	d.renderLists = map[*Node][]uint16{recv: {0, 1, 2, 0, 2, 3, 4, 5, 6, 4, 6, 7}}
	d.unmergeRenderLists()
	if donor.Mesh.Render != 1 {
		t.Fatal("donor not put back")
	}
	for i, n := range donor.Mesh.Normals {
		if math.Abs(float64(n.X)-1) > 1e-6 || math.Abs(float64(n.Y)) > 1e-6 || math.Abs(float64(n.Z)) > 1e-6 {
			t.Errorf("normal %d = %v, want (1, 0, 0)", i, n)
		}
	}
}

// A restored donor is drawn with the receiver's drawing settings, as the game
// drew its copies, not with its own, which the game never drew.
func TestUnmergeRenderListsDonorTakesReceiverDrawing(t *testing.T) {
	d := mergedModel(false)
	recv, donor := d.model.FindNode("wall"), d.model.FindNode("lower")
	recv.Mesh.Specular = Vec3{0.05, 0.05, 0.05}
	recv.Mesh.Shininess = 10
	recv.Mesh.SelfIllumColor = Vec3{0, 0.38, 0.02}
	recv.declare("selfillumcolor")
	donor.Mesh.TransparencyHint = 1
	donor.Mesh.RotateTexture = 1
	donor.Mesh.Alpha = 0.6
	donor.declare("alpha")
	donor.Mesh.SelfIllumColor = Vec3{0, 0.27, 0.01}
	d.unmergeRenderLists()
	m := donor.Mesh
	if m.Render != 1 {
		t.Fatal("donor not put back")
	}
	if m.Specular != recv.Mesh.Specular || m.Shininess != 10 || m.TransparencyHint != 0 || m.RotateTexture != 0 ||
		m.Alpha != 1 || m.SelfIllumColor != recv.Mesh.SelfIllumColor || !donor.isDeclared("selfillumcolor") {
		t.Errorf("donor drawing settings not the receiver's: %+v", m)
	}
	if len(d.model.Notes) != 2 || !strings.Contains(d.model.Notes[1].Message,
		"lower (specular, shininess, transparencyhint, rotatetexture, alpha, selfillumcolor)") {
		t.Errorf("notes: %+v", d.model.Notes)
	}
}

// The receiver's vertices that only the copies (and lost triangles) used are
// dropped: written unused, they would still count in the compile's bounds.
func TestUnmergeRenderListsDropsCopyVertices(t *testing.T) {
	for _, extra := range []bool{false, true} {
		d := mergedModel(extra)
		recv := d.model.FindNode("wall")
		recv.Mesh.Normals = make([]Vec3, len(recv.Mesh.Verts))
		for i := range recv.Mesh.Normals {
			recv.Mesh.Normals[i] = Vec3{Z: 1}
		}
		d.unmergeRenderLists()
		m := recv.Mesh
		if len(m.Verts) != 4 || len(m.TVerts) != 4 || len(m.Normals) != 4 {
			t.Errorf("extra=%v: receiver keeps %d verts, %d tverts, %d normals, want 4", extra, len(m.Verts), len(m.TVerts), len(m.Normals))
		}
		if m.Faces[1].Verts != [3]int32{0, 2, 3} {
			t.Errorf("extra=%v: faces renumbered: %+v", extra, m.Faces)
		}
	}
}

// tangentMergeModel: the donor's quad, whose two faces share vertices 0 and
// 2, was drawn through copies whose second triangle has the other
// handedness: the receiver's list uses clones of those two corners (8, 9).
func tangentMergeModel() (*decompiler, *Node) {
	d := mergedModel(false)
	recv, donor := d.model.FindNode("wall"), d.model.FindNode("lower")
	rm := recv.Mesh
	rm.Verts = append(rm.Verts, rm.Verts[4], rm.Verts[6])
	rm.TVerts = append(rm.TVerts, rm.TVerts[4], rm.TVerts[6])
	rm.Tangents = make([]Vec4, len(rm.Verts))
	for i := range rm.Tangents {
		rm.Tangents[i] = Vec4{X: 1, W: 1}
	}
	rm.Tangents[7] = Vec4{X: 1, W: -1}
	rm.Tangents[8] = Vec4{X: 1, W: -1}
	rm.Tangents[9] = Vec4{X: 1, W: -1}
	d.renderLists[recv] = []uint16{0, 1, 2, 0, 2, 3, 4, 5, 6, 8, 9, 7}
	donor.Mesh.Tangents = []Vec4{{X: 1, W: 1}, {X: 1, W: 1}, {X: 1, W: 1}, {X: 1, W: 1}}
	return d, donor
}

// A donor has no index list, so the tangent clones its copies use reach none
// of its faces: the corners that were drawn with the other handedness move to
// a copy of their vertex with the copy's tangent.
func TestUnmergeRenderListsDonorTangentClones(t *testing.T) {
	d, donor := tangentMergeModel()
	d.unmergeRenderLists()
	m := donor.Mesh
	if len(m.Verts) != 6 || len(m.TVerts) != 6 || len(m.Tangents) != 6 {
		t.Fatalf("donor has %d verts, %d tverts, %d tangents, want 6", len(m.Verts), len(m.TVerts), len(m.Tangents))
	}
	if m.Faces[0].Verts != [3]int32{0, 1, 2} || m.Faces[1].Verts != [3]int32{4, 5, 3} || m.Faces[1].UVs != m.Faces[1].Verts {
		t.Errorf("faces: %+v", m.Faces)
	}
	for i, want := range []float32{1, 1, 1, -1, -1, -1} {
		if m.Tangents[i].W != want {
			t.Errorf("tangent %d = %v, want handedness %v", i, m.Tangents[i], want)
		}
	}
	if m.Verts[4] != m.Verts[0] || m.Verts[5] != m.Verts[2] || m.TVerts[4] != m.TVerts[0] {
		t.Errorf("clones are not copies of their vertices: %v %v", m.Verts, m.TVerts)
	}
}

// Where the donor still has its own unused tangent clones, the corners move
// to those instead of new copies.
func TestUnmergeRenderListsDonorTangentClonesReused(t *testing.T) {
	d, donor := tangentMergeModel()
	m := donor.Mesh
	m.Verts = append(m.Verts, m.Verts[0], m.Verts[2])
	m.TVerts = append(m.TVerts, m.TVerts[0], m.TVerts[2])
	m.Tangents = append(m.Tangents, Vec4{X: 1, W: -1}, Vec4{X: 1, W: -1})
	d.unmergeRenderLists()
	if len(m.Verts) != 6 || m.Faces[1].Verts != [3]int32{4, 5, 3} || m.Tangents[3].W != -1 {
		t.Errorf("verts %d, faces %+v, tangents %v", len(m.Verts), m.Faces, m.Tangents)
	}
}

// Copies that disagree on a donor vertex's normal: each corner gets the
// normal it was drawn with, the second through a copy of the vertex.
func TestUnmergeRenderListsDonorNormalConflictSplits(t *testing.T) {
	d := mergedModel(false)
	recv, donor := d.model.FindNode("wall"), d.model.FindNode("lower")
	rm := recv.Mesh
	rm.Verts = append(rm.Verts, rm.Verts[4])
	rm.TVerts = append(rm.TVerts, rm.TVerts[4])
	rm.Normals = make([]Vec3, len(rm.Verts))
	for i := range rm.Normals {
		rm.Normals[i] = Vec3{Z: 1}
	}
	rm.Normals[8] = Vec3{X: 0.6, Z: 0.8}
	d.renderLists[recv] = []uint16{0, 1, 2, 0, 2, 3, 4, 5, 6, 8, 6, 7}
	donor.Mesh.Normals = []Vec3{{Z: 1}, {Z: 1}, {Z: 1}, {Z: 1}}
	d.unmergeRenderLists()
	m := donor.Mesh
	if len(m.Verts) != 5 || m.Faces[1].Verts != [3]int32{4, 2, 3} || math.Abs(float64(m.Normals[4].X)-0.6) > 1e-6 ||
		m.Normals[0] != (Vec3{Z: 1}) {
		t.Errorf("verts %d, faces %+v, normals %v", len(m.Verts), m.Faces, m.Normals)
	}
}

// A donor an animation moves was drawn still in game, as part of its
// receiver: its copies become the receiver's faces, it keeps render 0, and
// its own shadow goes, since the copies now cast it.
func TestUnmergeRenderListsFoldsAnimatedDonor(t *testing.T) {
	d := mergedModel(false)
	recv, donor := d.model.FindNode("wall"), d.model.FindNode("lower")
	recv.Mesh.Shadow, donor.Mesh.Shadow = 1, 1
	recv.Mesh.StoredAdjacency = [][3]int16{{-1, -1, 1}, {0, -1, -1}}
	donor.Mesh.StoredAdjacency = [][3]int16{{-1, -1, 1}, {0, -1, -1}}
	donor.Mesh.Faces[0].SmoothGroup, donor.Mesh.Faces[1].SmoothGroup = 4, 4
	d.model.Animations = []Animation{{Name: "open", Nodes: []AnimNode{{Name: "lower",
		PositionKeys: []PositionKey{{Time: 0, Value: Vec3{X: -3}}, {Time: 1, Value: Vec3{X: -3, Z: 1}}}}}}}
	d.unmergeRenderLists()
	m := recv.Mesh
	if donor.Mesh.Render != 0 || donor.Mesh.Shadow != 0 {
		t.Errorf("donor render %d shadow %d, want 0 0", donor.Mesh.Render, donor.Mesh.Shadow)
	}
	if len(m.Faces) != 4 || m.Faces[2].Verts != [3]int32{4, 5, 6} || m.Faces[3].Verts != [3]int32{4, 6, 7} ||
		m.Faces[3].UVs != m.Faces[3].Verts || m.Faces[2].SmoothGroup != 4 || len(m.Verts) != 8 {
		t.Errorf("receiver faces %+v, %d verts", m.Faces, len(m.Verts))
	}
	if len(m.StoredAdjacency) != 4 || m.StoredAdjacency[2] != [3]int16{-1, -1, 3} || m.StoredAdjacency[3] != [3]int16{2, -1, -1} {
		t.Errorf("adjacency %v", m.StoredAdjacency)
	}
	rm := d.model.RenderMerges
	if len(rm) != 1 || len(rm[0].Donors) != 0 || len(rm[0].Folded) != 1 {
		t.Errorf("merges: %+v", rm)
	}
	if len(d.model.Notes) != 1 || !strings.Contains(d.model.Notes[0].Message, "lower (an animation moves lower, but the game drew it as part of wall)") ||
		!strings.Contains(d.model.Notes[0].Message, "lower get shadow 0") {
		t.Errorf("notes: %+v", d.model.Notes)
	}
}

// The other cases restoring would draw differently fold too: copies baked
// without the donor's scale, a receiver an animation moves (the copies move
// with it), an animated alpha. An animation that keeps the donor at rest does
// not.
func TestUnmergeRenderListsFoldRule(t *testing.T) {
	for _, tc := range []struct {
		name string
		set  func(d *decompiler)
		why  string
	}{
		{"scale", func(d *decompiler) { d.model.FindNode("lower").Scale = 1.5 }, "without the scale 1.5 of lower"},
		{"receiver moves", func(d *decompiler) {
			d.model.Animations = []Animation{{Name: "a", Nodes: []AnimNode{{Name: "wall",
				OrientationKeys: []OrientationKey{{Value: Vec4{Z: 1, W: 0.5}}}}}}}
		}, "an animation moves wall, with the copies"},
		{"alpha", func(d *decompiler) {
			d.model.Animations = []Animation{{Name: "a", Nodes: []AnimNode{{Name: "wall",
				AlphaKeys: []FloatKey{{Value: 1}, {Time: 1, Value: 0.2}}}}}}
		}, "changes the alpha of wall"},
		{"at rest", func(d *decompiler) {
			d.model.Animations = []Animation{{Name: "a", Nodes: []AnimNode{{Name: "lower",
				PositionKeys: []PositionKey{{Value: Vec3{X: -3}}}, OrientationKeys: []OrientationKey{{Value: Vec4{X: 1}}},
				ScaleKeys: []FloatKey{{Value: 1}}}}}}
		}, ""},
	} {
		d := mergedModel(false)
		tc.set(d)
		d.unmergeRenderLists()
		folded := d.model.FindNode("lower").Mesh.Render == 0
		if folded != (tc.why != "") {
			t.Errorf("%s: folded %v", tc.name, folded)
			continue
		}
		if tc.why != "" && (len(d.model.Notes) != 1 || !strings.Contains(d.model.Notes[0].Message, tc.why)) {
			t.Errorf("%s: notes %+v", tc.name, d.model.Notes)
		}
	}
}

// Two donors of the same shape and UVs: each takes the copy that lies where
// it is, so neither is folded for being matched with the other's.
func TestUnmergeRenderListsPrefersCopyInPlace(t *testing.T) {
	recv := &Node{Name: "wall", Mesh: mergeQuad("tex", 1, 0)}
	a := &Node{Name: "a", Mesh: mergeQuad("tex", 0, 5), Position: Vec3{X: -3}}
	b := &Node{Name: "b", Mesh: mergeQuad("tex", 0, 5), Position: Vec3{X: 7}}
	// b's copy (12..13) first, then a's (2..3).
	recv.Mesh.Verts = append(recv.Mesh.Verts, Vec3{12, 0, 0}, Vec3{13, 0, 0}, Vec3{13, 1, 0}, Vec3{12, 1, 0},
		Vec3{2, 0, 0}, Vec3{3, 0, 0}, Vec3{3, 1, 0}, Vec3{2, 1, 0})
	for i := 0; i < 2; i++ {
		recv.Mesh.TVerts = append(recv.Mesh.TVerts, Vec3{0, 0, 0}, Vec3{1, 0, 0}, Vec3{1, 1, 0}, Vec3{0, 1, 0})
	}
	d := &decompiler{model: &Model{Nodes: []*Node{recv, a, b}}}
	d.renderLists = map[*Node][]uint16{recv: {0, 1, 2, 0, 2, 3, 4, 5, 6, 4, 6, 7, 8, 9, 10, 8, 10, 11}}
	d.unmergeRenderLists()
	if rm := d.model.RenderMerges; len(rm) != 1 || len(rm[0].Donors) != 2 || len(rm[0].Folded) != 0 {
		t.Errorf("merges: %+v", rm)
	}
}

// Two receivers of one texture, each holding a copy of a donor of the same
// shape: each donor goes to the receiver that holds its copy where it lies,
// not to the first receiver its shape fits, and neither is folded.
func TestUnmergeRenderListsDonorToReceiverInPlace(t *testing.T) {
	r1 := &Node{Name: "wall1", Mesh: mergeQuad("tex", 1, 0)}
	r2 := &Node{Name: "wall2", Mesh: mergeQuad("tex", 1, 20)}
	a := &Node{Name: "a", Mesh: mergeQuad("tex", 0, 5), Position: Vec3{X: -3}} // drawn at 2..3, in wall2
	b := &Node{Name: "b", Mesh: mergeQuad("tex", 0, 5), Position: Vec3{X: 7}}  // drawn at 12..13, in wall1
	r1.Mesh.Verts = append(r1.Mesh.Verts, Vec3{12, 0, 0}, Vec3{13, 0, 0}, Vec3{13, 1, 0}, Vec3{12, 1, 0})
	r2.Mesh.Verts = append(r2.Mesh.Verts, Vec3{2, 0, 0}, Vec3{3, 0, 0}, Vec3{3, 1, 0}, Vec3{2, 1, 0})
	for _, r := range []*Node{r1, r2} {
		r.Mesh.TVerts = append(r.Mesh.TVerts, Vec3{0, 0, 0}, Vec3{1, 0, 0}, Vec3{1, 1, 0}, Vec3{0, 1, 0})
	}
	rl := []uint16{0, 1, 2, 0, 2, 3, 4, 5, 6, 4, 6, 7}
	d := &decompiler{model: &Model{Nodes: []*Node{r1, r2, a, b}}}
	d.renderLists = map[*Node][]uint16{r1: rl, r2: rl}
	d.unmergeRenderLists()
	rm := d.model.RenderMerges
	if len(rm) != 2 || len(rm[0].Donors) != 1 || rm[0].Donors[0] != "b" || len(rm[1].Donors) != 1 || rm[1].Donors[0] != "a" ||
		len(rm[0].Folded)+len(rm[1].Folded) != 0 {
		t.Errorf("merges: %+v", rm)
	}
}
