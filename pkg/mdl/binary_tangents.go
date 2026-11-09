package mdl

import (
	"fmt"
	"strings"
)

// A tangent stream that is read as tangents (MDL-BIN-048) can still hold bad
// vectors: NaN or infinite, zero, or (with a normal-mapped render hint) not
// unit length. Layonara has 251 meshes whose stream is 96.6% unit vectors
// with some NaN and zero ones, made by a tool other than the game. Dropping
// the whole stream would replace every stored tangent with a generated one,
// so only the bad vectors are replaced: each gets the tangent the game would
// generate for that vertex (MDL-MESH-043, run on the binary's own vertex list
// and index list), and the good ones stay exactly as stored.

// tangentFill is a mesh whose tangent stream holds bad vectors.
type tangentFill struct {
	node *Node
	bad  []int // vertex indices, ascending
	// what the bad vectors were
	nonFinite, zero, other int
}

// fillTangents replaces the bad tangents of every mesh in d.tangentFills and
// warns once per mesh. It runs once the whole model is read, so the index
// list and the node type are known.
func (d *decompiler) fillTangents() {
	for _, f := range d.tangentFills {
		n := f.node
		m := n.Mesh
		gen, hand := binaryMeshTangents(m, d.drawnTriangles(n), n.AnimMesh != nil)
		fallback := 0
		for _, i := range f.bad {
			if i < len(hand) && hand[i] != 0 && isUnitTangent(gen[i]) {
				m.Tangents[i] = Vec4{X: gen[i].X, Y: gen[i].Y, Z: gen[i].Z, W: hand[i]}
				continue
			}
			var nv Vec3
			if i < len(m.Normals) {
				nv = m.Normals[i]
			}
			m.Tangents[i] = fallbackTangent(nv)
			fallback++
		}
		var what []string
		if f.nonFinite > 0 {
			what = append(what, fmt.Sprintf("%d NaN or infinite", f.nonFinite))
		}
		if f.zero > 0 {
			what = append(what, fmt.Sprintf("%d zero", f.zero))
		}
		if f.other > 0 {
			what = append(what, fmt.Sprintf("%d not unit length", f.other))
		}
		msg := fmt.Sprintf("mesh %q: %d of %d stored tangents are unusable (%s); filled with the tangents the game generates for those vertices, the others kept as stored",
			n.Name, len(f.bad), len(m.Tangents), strings.Join(what, ", "))
		if fallback > 0 {
			msg += fmt.Sprintf(". %d of them get none (no triangle with UVs and normals reaches them), so they get a tangent perpendicular to their normal", fallback)
		}
		d.warn(WarnGeneral, 0, n.Name, "%s", msg)
	}
}

// drawnTriangles returns the triangles the game draws for n: its render index
// list (MDL-BIN-049) when it has one, otherwise its face records.
func (d *decompiler) drawnTriangles(n *Node) [][3]uint16 {
	var tris [][3]uint16
	if rl := d.renderLists[n]; len(rl) > 0 {
		for k := 0; k+2 < len(rl); k += 3 {
			tris = append(tris, [3]uint16{rl[k], rl[k+1], rl[k+2]})
		}
		return tris
	}
	for _, f := range n.Mesh.Faces {
		var t [3]uint16
		ok := true
		for k := 0; k < 3; k++ {
			if f.Verts[k] < 0 || f.Verts[k] > 0xFFFF {
				ok = false
				break
			}
			t[k] = uint16(f.Verts[k])
		}
		if ok {
			tris = append(tris, t)
		}
	}
	return tris
}

// binaryMeshTangents runs the game's tangent generation (generateTangents,
// MDL-MESH-043) on a decompiled mesh's own vertex list and the triangles tris,
// and returns the tangent and handedness of each of its vertices (handedness
// 0 for a vertex no triangle reaches). The clones the generation makes are
// not returned: a vertex keeps the tangent of the first handedness to reach
// it, as the game's vertex does. nil when the mesh lacks UVs or normals.
func binaryMeshTangents(m *MeshData, tris [][3]uint16, animMesh bool) ([]Vec3, []float32) {
	n := len(m.Verts)
	if n == 0 || len(m.TVerts) < n || len(m.Normals) < n {
		return nil, nil
	}
	exp := &expandedMesh{
		positions: m.Verts[:n:n],
		uvs:       m.TVerts[:n:n],
		normals:   m.Normals[:n:n],
		faceVerts: append([][3]uint16(nil), tris...),
		origVert:  make([]int32, n),
	}
	for i := range exp.origVert {
		exp.origVert[i] = int32(i)
	}
	tan, hand := generateTangents(exp, animMesh)
	if len(tan) < n {
		return nil, nil
	}
	return tan[:n], hand[:n]
}

// fallbackTangent is the tangent of a vertex the generation gives none: the
// one the game gives a triangle without UV area (the normal with its
// components rotated to y, z, x), made perpendicular to the normal, with
// handedness +1. Where that is parallel to the normal (a normal along
// (1, 1, 1)), the X axis, then the Y axis, is used instead.
func fallbackTangent(nv Vec3) Vec4 {
	if l := float32(nv.X*nv.X) + float32(nv.Y*nv.Y) + float32(nv.Z*nv.Z); l != 0 {
		nv = gameNormalize(nv)
	}
	for _, t := range [3]Vec3{{X: nv.Y, Y: nv.Z, Z: nv.X}, {X: 1}, {Y: 1}} {
		d := float32(nv.X*t.X) + float32(nv.Y*t.Y) + float32(nv.Z*t.Z)
		p := Vec3{X: t.X - float32(nv.X*d), Y: t.Y - float32(nv.Y*d), Z: t.Z - float32(nv.Z*d)}
		if l := float32(p.X*p.X) + float32(p.Y*p.Y) + float32(p.Z*p.Z); l > 0x1p-20 {
			p = gameNormalize(p)
			return Vec4{X: p.X, Y: p.Y, Z: p.Z, W: 1}
		}
	}
	return Vec4{X: 1, W: 1}
}
