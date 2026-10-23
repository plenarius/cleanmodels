package mdl

import (
	"math"
	"strings"
	"testing"
)

// robeTorsoModel is a robe with a shoulder pad under torso_g turned 180
// degrees about x, a panel hanging from it, a turned bicep copy (a body bone),
// a turned dummy with nothing under it, and a turned emitter.
func robeTorsoModel(name string) *Model {
	m := &Model{Name: name, SuperModel: "a_ba", Classification: "CHARACTER", AnimationScale: 1}
	add := func(n *Node) *Node { n.Scale = 1; m.Nodes = append(m.Nodes, n); return n }
	add(&Node{Name: name, Parent: "NULL"})
	add(&Node{Name: "rootdummy", Parent: name, Position: Vec3{Z: 1.2}})
	add(&Node{Name: "torso_g", Parent: "rootdummy"})
	pad := add(&Node{Name: "pad", Parent: "torso_g", Position: Vec3{0.2, 0, 0.4}, Orientation: Vec4{-1, 0, 0, -math.Pi}, Mesh: NewMeshData()})
	pad.Mesh.Verts = []Vec3{{0, 0.1, 0.05}}
	pad.Mesh.Normals = []Vec3{{0, 0, 1}}
	pad.Mesh.Tangents = []Vec4{{0, 1, 0, -1}}
	panel := add(&Node{Name: "panel", Parent: "pad", Position: Vec3{0.1, 0, 0.1}, Orientation: Vec4{0, 0, 1, 0.5}, Mesh: NewMeshData()})
	panel.Mesh.Verts = []Vec3{{0, 0, 0.8}, {0.1, 0, 0}}
	add(&Node{Name: "lbicep_g", Parent: "torso_g", Position: Vec3{-0.2, 0, 0.35}, Orientation: Vec4{0, 1, 0, 0.3}, Mesh: NewMeshData()})
	add(&Node{Name: "impact", Parent: "torso_g", Orientation: Vec4{1, 0, 0, 2}})
	add(&Node{Name: "sparks", Parent: "torso_g", Orientation: Vec4{1, 0, 0, 1}, Emitter: &EmitterData{}})
	return m
}

func robeWorldVerts(m *Model) map[string][]Vec3 {
	w := nodeTransforms(m)
	out := map[string][]Vec3{}
	for _, n := range m.Nodes {
		if n.Mesh == nil {
			continue
		}
		for _, v := range n.Mesh.Verts {
			out[n.Name] = append(out[n.Name], vec3f(w[n].apply(vec3d(v))))
		}
	}
	return out
}

// TestBakeTorsoTurns: the pad's turn goes into its vertices, normals and
// tangents and the panel's placement, drawn the same; the bone, the empty
// dummy and the emitter are left alone, the emitter with a message.
func TestBakeTorsoTurns(t *testing.T) {
	m := robeTorsoModel("pmh0_robe999")
	turns := TorsoTurns(m)
	if len(turns) != 2 || turns[0].Node.Name != "pad" || math.Abs(turns[0].Angle-180) > 0.01 || turns[0].Why != "" ||
		turns[1].Node.Name != "sparks" || turns[1].Why == "" {
		t.Fatalf("turns %+v: want the pad (180, bakeable) and the emitter (not bakeable)", turns)
	}
	before := robeWorldVerts(m)
	msgs := BakeTorsoTurns(m)
	if len(msgs) != 2 || !strings.Contains(msgs[0], "1 children") || !strings.Contains(msgs[1], "left as it is") {
		t.Errorf("messages %q", msgs)
	}
	after := robeWorldVerts(m)
	for name, vs := range before {
		for i := range vs {
			if a, b := vs[i], after[name][i]; math.Abs(float64(a.X-b.X)) > 1e-5 || math.Abs(float64(a.Y-b.Y)) > 1e-5 || math.Abs(float64(a.Z-b.Z)) > 1e-5 {
				t.Errorf("%s vertex %d moved from %v to %v", name, i, a, b)
			}
		}
	}
	pad := m.FindNode("pad")
	if pad.Orientation != (Vec4{}) {
		t.Errorf("pad orientation %v, want the identity", pad.Orientation)
	}
	if !near(pad.Mesh.Normals[0], Vec3{0, 0, -1}) || !near(Vec3{pad.Mesh.Tangents[0].X, pad.Mesh.Tangents[0].Y, pad.Mesh.Tangents[0].Z}, Vec3{0, -1, 0}) || pad.Mesh.Tangents[0].W != -1 {
		t.Errorf("pad normal %v, tangent %v: want them turned, the handedness kept", pad.Mesh.Normals[0], pad.Mesh.Tangents[0])
	}
	if got := m.FindNode("lbicep_g").Orientation; got != (Vec4{0, 1, 0, 0.3}) {
		t.Errorf("bicep orientation changed to %v", got)
	}
	if len(TorsoTurns(m)) != 1 {
		t.Errorf("after the bake: %+v, want only the emitter", TorsoTurns(m))
	}
	if got := TorsoTurns(robeTorsoModel("pmh0_chest999")); got != nil {
		t.Errorf("a body part: %+v, want nothing (robes only)", got)
	}
}

// TestBakeTorsoTurnsAnimated: a node the robe's own animations turn is
// reported and left alone.
func TestBakeTorsoTurnsAnimated(t *testing.T) {
	m := robeTorsoModel("pmh0_robe999")
	m.Animations = []Animation{{Name: "flap", Nodes: []AnimNode{{Name: "pad", OrientationKeys: []OrientationKey{{}, {}}}}}}
	before := m.FindNode("pad").Orientation
	msgs := BakeTorsoTurns(m)
	if m.FindNode("pad").Orientation != before || len(msgs) == 0 || !strings.Contains(msgs[0], "animations") {
		t.Errorf("messages %q, orientation %v: want the pad left, with a message", msgs, m.FindNode("pad").Orientation)
	}
}
