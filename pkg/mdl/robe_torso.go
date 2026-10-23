package mdl

import (
	"fmt"
	"math"
	"strings"
)

// Worn robes (nwn-mdl-spec MDL-NODE-050). When a creature wears a robe, the
// game draws a node of it whose parent is torso_g as if its orientation were
// the identity; its position is kept, and the nodes below it are turned from
// there. The game's compiler keeps the orientation, so the loss is at run
// time. Found in game on a server's robe 202, whose cape panel, hung from a
// shoulder pad turned 180 degrees, stood up above the shoulder on every race;
// a child of pelvis_g kept its turn. Putting the turn into the node's own
// vertices and its children's transforms, with the node at the identity, is
// drawn as modelled.

// IsRobeModel reports whether name is a robe's (pmh0_robe202).
func IsRobeModel(name string) bool {
	name = strings.ToLower(strings.TrimSpace(name))
	i := strings.IndexByte(name, '_')
	return i > 0 && bodyPrefix.MatchString(name[:i]) && partKind(name) == "robe"
}

// TorsoTurn is a robe node under torso_g with an orientation the game drops,
// and whether BakeTorsoTurns can put it into the geometry (Why says why not).
type TorsoTurn struct {
	Node  *Node
	Angle float64 // degrees
	Why   string  // empty when it can be baked
}

// TorsoTurns lists the nodes of robe m whose parent is torso_g and whose
// orientation is not the identity (MDL-NODE-050). Nothing for other models.
func TorsoTurns(m *Model) []TorsoTurn {
	if m == nil || !IsRobeModel(m.Name) {
		return nil
	}
	animated := map[string]bool{}
	for _, a := range m.Animations {
		for _, an := range a.Nodes {
			if len(an.OrientationKeys) > 0 {
				animated[strings.ToLower(an.Name)] = true
			}
		}
	}
	parents := map[string]bool{}
	for _, n := range m.Nodes {
		if n != nil {
			parents[strings.ToLower(n.Parent)] = true
		}
	}
	var out []TorsoTurn
	for _, n := range m.Nodes {
		if n == nil || !strings.EqualFold(n.Parent, "torso_g") {
			continue
		}
		// A body bone (the robe's lbicep_g) is turned by the animations and
		// skins bind to its rest pose: it is not one of the nodes the game
		// was seen to draw unturned, and turning its geometry would move them.
		if isBodyBone(strings.ToLower(n.Name)) {
			continue
		}
		// A dummy with nothing under it (Impact, a hit point) shows nothing
		// to turn.
		if n.Mesh == nil && n.Emitter == nil && n.Light == nil && n.Reference == nil && !n.Camera && !parents[strings.ToLower(n.Name)] {
			continue
		}
		q := axisAngleToQuat(n.Orientation)
		half := math.Acos(math.Min(1, math.Abs(float64(q.W))))
		if half < 1e-4 {
			continue
		}
		t := TorsoTurn{Node: n, Angle: 2 * half * 180 / math.Pi}
		switch {
		case animated[strings.ToLower(n.Name)]:
			t.Why = "the robe's own animations turn it"
		case n.Emitter != nil || n.Light != nil || n.Reference != nil || n.Camera:
			t.Why = "it has no vertices to turn, and its direction is its own"
		case n.Aabb != nil || n.AnimMesh != nil:
			t.Why = "its mesh kind is not supported"
		}
		out = append(out, t)
	}
	return out
}

// BakeTorsoTurns puts the orientation of every node TorsoTurns can bake into
// the node's vertices, normals and tangents and into its children's positions
// and orientations, and sets the node to the identity: the robe is then drawn
// in game as modelled. It returns a message per node, the ones it could not
// bake included.
func BakeTorsoTurns(m *Model) []string {
	var out []string
	for _, t := range TorsoTurns(m) {
		n := t.Node
		if t.Why != "" {
			out = append(out, fmt.Sprintf("node %q under torso_g is turned %.0f degrees, which the game does not draw on a worn robe, and was left as it is: %s",
				n.Name, t.Angle, t.Why))
			continue
		}
		a := axisAngleToQuat(n.Orientation)
		r := quatNormalize([4]float64{float64(a.X), float64(a.Y), float64(a.Z), float64(a.W)})
		if n.Mesh != nil {
			for i, v := range n.Mesh.Verts {
				n.Mesh.Verts[i] = vec3f(quatRotate(r, vec3d(v)))
			}
			for i, v := range n.Mesh.Normals {
				n.Mesh.Normals[i] = vec3f(quatRotate(r, vec3d(v)))
			}
			for i, v := range n.Mesh.Tangents {
				d := quatRotate(r, [3]float64{float64(v.X), float64(v.Y), float64(v.Z)})
				n.Mesh.Tangents[i] = Vec4{X: float32(d[0]), Y: float32(d[1]), Z: float32(d[2]), W: v.W}
			}
			n.Mesh.CornerNormals = nil
		}
		children := 0
		for _, c := range m.Nodes {
			if c == nil || c == n || !strings.EqualFold(c.Parent, n.Name) {
				continue
			}
			c.Position = vec3f(quatRotate(r, vec3d(c.Position)))
			cq := axisAngleToQuat(c.Orientation)
			c.Orientation = quatToAxisAngle(quatMul(r, [4]float64{float64(cq.X), float64(cq.Y), float64(cq.Z), float64(cq.W)}))
			children++
		}
		n.Orientation = Vec4{}
		msg := fmt.Sprintf("node %q under torso_g: put its %.0f-degree turn, which the game does not draw on a worn robe, into its geometry", n.Name, t.Angle)
		if children > 0 {
			msg += fmt.Sprintf(" and its %d children's placement", children)
		}
		out = append(out, msg)
	}
	return out
}

// quatToAxisAngle converts a quaternion (x, y, z, w) to the axis and angle
// an ASCII orientation holds, the angle in [0, pi]; the identity is 0 0 0 0.
func quatToAxisAngle(q [4]float64) Vec4 {
	q = quatNormalize(q)
	if q[3] < 0 {
		q = [4]float64{-q[0], -q[1], -q[2], -q[3]}
	}
	s := math.Sqrt(q[0]*q[0] + q[1]*q[1] + q[2]*q[2])
	if s < 1e-9 {
		return Vec4{}
	}
	return Vec4{X: float32(q[0] / s), Y: float32(q[1] / s), Z: float32(q[2] / s), W: float32(2 * math.Atan2(s, q[3]))}
}
