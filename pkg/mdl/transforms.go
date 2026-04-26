package mdl

import (
	"math"
	"strings"
)

// RotateVectorAxisAngle applies an axis-angle rotation to v using Rodrigues'
// formula. Mirrors fix_pivots.pl f_rotate_vector/3 (line 415).
//
// The axis (X, Y, Z) does not need to be unit-length; it is normalised
// internally. When the angle is zero or the axis degenerates to (0, 0, 0),
// the input vector is returned unchanged.
func RotateVectorAxisAngle(v Vec3, axis Vec4) Vec3 {
	angle := float64(axis.W)
	if angle == 0 {
		return v
	}
	ax, ay, az := float64(axis.X), float64(axis.Y), float64(axis.Z)
	r := math.Sqrt(ax*ax + ay*ay + az*az)
	if r == 0 {
		return v
	}
	x, y, z := ax/r, ay/r, az/r
	c := math.Cos(angle)
	s := math.Sin(angle)
	t := 1.0 - c
	ux, uy, uz := float64(v.X), float64(v.Y), float64(v.Z)
	return Vec3{
		X: float32((t*x*x+c)*ux + (t*x*y-z*s)*uy + (t*x*z+y*s)*uz),
		Y: float32((t*x*y+z*s)*ux + (t*y*y+c)*uy + (t*y*z-x*s)*uz),
		Z: float32((t*x*z-y*s)*ux + (t*y*z+x*s)*uy + (t*z*z+c)*uz),
	}
}

func nodeIndex(model *Model) map[string]*Node {
	if model == nil {
		return nil
	}
	return nodeIndexFromSlice(model.Nodes)
}

// nodeIndexFromSlice builds the same lowercase-name → *Node map as
// nodeIndex but accepts a raw slice so test helpers and other callers that
// already hold the slice don't need to wrap it in a *Model.
func nodeIndexFromSlice(nodes []*Node) map[string]*Node {
	idx := make(map[string]*Node, len(nodes))
	for _, n := range nodes {
		if n != nil && n.Name != "" {
			idx[lowerName(n.Name)] = n
		}
	}
	return idx
}

func lowerName(s string) string {
	b := make([]byte, len(s))
	for i := 0; i < len(s); i++ {
		c := s[i]
		if c >= 'A' && c <= 'Z' {
			c += 'a' - 'A'
		}
		b[i] = c
	}
	return string(b)
}

// parentChain walks from n up through Parent links until it reaches the model
// root (Parent="" or "NULL") or a cycle is detected. Returns the chain in
// innermost-first order (n is chain[0]).
//
// Cycle detection uses a visited set keyed on *Node pointer identity, so a
// chain like A→B→A breaks cleanly instead of looping forever.
func parentChain(idx map[string]*Node, n *Node) []*Node {
	chain := make([]*Node, 0, 4)
	visited := make(map[*Node]bool)
	cur := n
	for cur != nil {
		if visited[cur] {
			break
		}
		visited[cur] = true
		chain = append(chain, cur)
		if cur.Parent == "" || strings.EqualFold(cur.Parent, "NULL") {
			break
		}
		next := idx[lowerName(cur.Parent)]
		if next == nil {
			break
		}
		cur = next
	}
	return chain
}

// applyNodeTransform translates v from this node's local frame into its
// parent's frame, applying Scale, Orientation, and Position in that order.
//
// Mirrors localToParent in repair_tilefade.go and t_abs_verts/4 in tilefade.pl
// (line 46).
func applyNodeTransform(n *Node, v Vec3) Vec3 {
	s := n.Scale
	if s == 0 {
		s = 1
	}
	sv := Vec3{X: v.X * s, Y: v.Y * s, Z: v.Z * s}
	rv := RotateVectorAxisAngle(sv, n.Orientation)
	return Vec3{X: rv.X + n.Position.X, Y: rv.Y + n.Position.Y, Z: rv.Z + n.Position.Z}
}

// inverseNodeTransform maps a point from this node's parent space back into
// this node's local frame: subtracts Position, rotates by -Orientation, then
// divides by Scale.
//
// Mirrors parentToLocal in repair_tilefade.go.
func inverseNodeTransform(n *Node, p Vec3) Vec3 {
	t := Vec3{X: p.X - n.Position.X, Y: p.Y - n.Position.Y, Z: p.Z - n.Position.Z}
	rv := RotateVectorAxisAngle(t, Vec4{X: n.Orientation.X, Y: n.Orientation.Y, Z: n.Orientation.Z, W: -n.Orientation.W})
	s := n.Scale
	if s == 0 {
		s = 1
	}
	return Vec3{X: rv.X / s, Y: rv.Y / s, Z: rv.Z / s}
}

// LocalToWorld composes the chain of orientation + position + scale transforms
// from n up to the model root, returning the world-space coordinate of a point
// expressed in n's local frame.
//
// Mirrors the absolute-vertex unwinding done by tilefade.pl t_abs_verts/4
// (line 46) and make_checks.pl vertex_to_local/6 (line 5724) inverted.
func LocalToWorld(idx map[string]*Node, n *Node, local Vec3) Vec3 {
	chain := parentChain(idx, n)
	cur := local
	for _, nd := range chain {
		cur = applyNodeTransform(nd, cur)
	}
	return cur
}

// WorldToLocal returns the world-space point expressed in n's local frame,
// i.e. the inverse of LocalToWorld. Mirrors vertex_to_local/6 in
// make_checks.pl line 5724.
func WorldToLocal(idx map[string]*Node, n *Node, world Vec3) Vec3 {
	chain := parentChain(idx, n)
	v := world
	for i := len(chain) - 1; i >= 0; i-- {
		v = inverseNodeTransform(chain[i], v)
	}
	return v
}

// LocalNormalToWorld rotates a normal from n's local frame into world space
// using only the orientation chain (no translation, no scale). Caller is
// responsible for re-normalising if downstream consumers require it; the
// helper does normalise on its way out so common cases are correct.
func LocalNormalToWorld(idx map[string]*Node, n *Node, normal Vec3) Vec3 {
	chain := parentChain(idx, n)
	cur := normal
	for _, nd := range chain {
		cur = RotateVectorAxisAngle(cur, nd.Orientation)
	}
	return vecNormalize(cur)
}

// WorldNormalToLocal is the inverse of LocalNormalToWorld: rotates a
// world-space normal back into n's local frame using the orientation chain
// only. Result is normalised.
func WorldNormalToLocal(idx map[string]*Node, n *Node, normal Vec3) Vec3 {
	chain := parentChain(idx, n)
	cur := normal
	for i := len(chain) - 1; i >= 0; i-- {
		nd := chain[i]
		cur = RotateVectorAxisAngle(cur, Vec4{X: nd.Orientation.X, Y: nd.Orientation.Y, Z: nd.Orientation.Z, W: -nd.Orientation.W})
	}
	return vecNormalize(cur)
}

// WorldVertices returns the world-space coordinate of each vertex in n's mesh.
// Returns nil if n has no mesh. Builds nodeIndex internally; for hot loops
// over many nodes prefer WorldVerticesCached with a precomputed idx.
func WorldVertices(model *Model, n *Node) []Vec3 {
	if n == nil || n.Mesh == nil {
		return nil
	}
	return WorldVerticesCached(nodeIndex(model), n)
}

// WorldVerticesCached is WorldVertices with a caller-supplied node index, so
// loops over many mesh nodes don't rebuild the same lookup table per call.
func WorldVerticesCached(idx map[string]*Node, n *Node) []Vec3 {
	if n == nil || n.Mesh == nil {
		return nil
	}
	chain := parentChain(idx, n)
	out := make([]Vec3, len(n.Mesh.Verts))
	for i, v := range n.Mesh.Verts {
		cur := v
		for _, nd := range chain {
			cur = applyNodeTransform(nd, cur)
		}
		out[i] = cur
	}
	return out
}
