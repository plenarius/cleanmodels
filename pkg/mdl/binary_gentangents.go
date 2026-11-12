package mdl

import (
	"math"
	"strings"
)

// When the game (or cleanmodels) generates a normal-mapped mesh's tangents, a
// vertex reached by triangles of both handedness gets a clone, appended to the
// end of the vertex list (nwn-mdl-spec MDL-MESH-043). Tangents written in the
// ASCII are part of the merge key instead (MDL-MESH-010, MDL-MESH-044): a
// compile numbers each vertex where a face first uses it, so a clone moves from
// the end of the list to its first use, and every later vertex moves with it.
// No ASCII with tangents can give the binary's order back.
//
// omitGeneratedTangents therefore writes such a mesh without its tangents, and
// with each clone folded back into the vertex it was cloned from, when the
// compile of that mesh generates exactly the binary's vertices again: the same
// count and order, the same positions, UVs, normals, colours, skin weights and
// constraints, the same tangents and handedness bit for bit, and the same index
// list. Anything else keeps its tangents as stored. A mesh only gets tangents
// generated when it is normal-mapped (MDL-MESH-042): by its own render hint, or
// by its material, looked up in DecompileOptions.MaterialDirs as the compile
// looks it up.
func (d *decompiler) omitGeneratedTangents() {
	filled := map[*Node]bool{}
	for _, f := range d.tangentFills {
		filled[f.node] = true
	}
	for _, n := range d.model.Nodes {
		if n.Mesh == nil || len(n.Mesh.Tangents) == 0 || filled[n] {
			continue
		}
		if clones, ok := regenerableTangents(n, d.renderLists[n], d.opts.MaterialDirs); ok {
			n.Mesh.TangentsOmitted = true
			if clones == 0 {
				d.note(WarnGeneral, 0, n.Name,
					"mesh %q: tangents left out: a compile generates the same ones (the mesh is normal-mapped)", n.Name)
				continue
			}
			d.note(WarnGeneral, 0, n.Name,
				"mesh %q: tangents left out: a compile generates the same ones (the mesh is normal-mapped), and only a generating compile keeps their %d cloned vertices at the end of the vertex list",
				n.Name, clones)
		}
	}
}

// regenerableTangents rewrites n's mesh without tangents, as described at
// omitGeneratedTangents, and returns how many clone vertices it folded; false
// when it leaves the mesh as it is. rl is the mesh's render index list.
func regenerableTangents(n *Node, rl []uint16, materialDirs []string) (int, bool) {
	m := n.Mesh
	if n.Aabb != nil || n.AnimMesh != nil {
		return 0, false
	}
	nv, nf := len(m.Verts), len(m.Faces)
	if nv == 0 || nf == 0 || len(rl) != 3*nf || len(m.Tangents) != nv || len(m.Normals) != nv || len(m.TVerts) != nv {
		return 0, false
	}
	var weights []VertexWeight
	if n.Skin != nil {
		weights = n.Skin.Weights
	}
	var constraints []float32
	if n.Dangly != nil {
		constraints = n.Dangly.Constraints
	}
	full := func(l int) bool { return l == 0 || l == nv }
	if !full(len(m.Colors)) || !full(len(m.TVerts1)) || !full(len(m.TVerts2)) || !full(len(m.TVerts3)) ||
		!full(len(weights)) || !full(len(constraints)) ||
		len(m.TexIndices0)+len(m.TexIndices1)+len(m.TexIndices2)+len(m.TexIndices3)+len(m.MirrorList)+len(m.CornerNormals) > 0 {
		return 0, false
	}
	// Even a list that uses each vertex first in vertex order, which compiles
	// back to the same vertex order with its tangents written, is taken: with
	// tangents written, the face records would name the clones (a generating
	// compile keeps the vertex each was cloned from), and positions across a UV
	// seam whose generated tangents differ could not be shared (sharePositions).
	for _, v := range rl {
		if int(v) >= nv {
			return 0, false
		}
	}
	// A NaN anywhere: the comparisons below could not tell what the compile
	// merges, so the stored tangents stay.
	for _, l := range [][]Vec3{m.Verts, m.TVerts, m.TVerts1, m.TVerts2, m.TVerts3, m.Normals, m.Colors} {
		for _, v := range l {
			if v.X != v.X || v.Y != v.Y || v.Z != v.Z {
				return 0, false
			}
		}
	}
	for _, t := range m.Tangents {
		if t.X != t.X || t.Y != t.Y || t.Z != t.Z || t.W != t.W {
			return 0, false
		}
	}
	for _, c := range constraints {
		if c != c {
			return 0, false
		}
	}
	for _, w := range weights {
		for _, f := range w.Weights {
			if f != f {
				return 0, false
			}
		}
	}
	if !isNormalMapped(m, materialDirs, nil) {
		return 0, false
	}

	// Each vertex's attributes other than its tangent; a clone has the same
	// as the vertex it was cloned from.
	var skinKeys []string
	if len(weights) > 0 {
		skinKeys = skinMergeKeys(n.Skin)
	}
	var b strings.Builder
	put := func(fs ...float32) {
		for _, f := range fs {
			bits := math.Float32bits(f)
			b.WriteByte(byte(bits))
			b.WriteByte(byte(bits >> 8))
			b.WriteByte(byte(bits >> 16))
			b.WriteByte(byte(bits >> 24))
		}
	}
	first := make(map[string]int32, nv)
	base := make([]int32, nv)
	for i := 0; i < nv; i++ {
		b.Reset()
		put(m.Verts[i].X, m.Verts[i].Y, m.Verts[i].Z, m.TVerts[i].X, m.TVerts[i].Y, m.TVerts[i].Z,
			m.Normals[i].X, m.Normals[i].Y, m.Normals[i].Z)
		for _, l := range [][]Vec3{m.TVerts1, m.TVerts2, m.TVerts3, m.Colors} {
			if len(l) > 0 {
				put(l[i].X, l[i].Y, l[i].Z)
			}
		}
		if len(constraints) > 0 {
			put(constraints[i])
		}
		if len(skinKeys) > 0 {
			b.WriteString(skinKeys[i])
		}
		k := b.String()
		if j, ok := first[k]; ok {
			base[i] = j
		} else {
			first[k] = int32(i)
			base[i] = int32(i)
		}
	}

	// The faces on the folded vertices, which must be the faces' own
	// vertices or clones of them; then the vertices still used, in order.
	used := make([]bool, nv)
	faces := make([]Face, nf)
	for i, f := range m.Faces {
		for k := 0; k < 3; k++ {
			a := f.Verts[k]
			if a < 0 || int(a) >= nv || base[a] != base[rl[3*i+k]] {
				return 0, false
			}
			used[base[a]] = true
		}
		faces[i] = f
	}
	newIdx := make([]int32, nv)
	var keep []int
	for i := 0; i < nv; i++ {
		if used[i] {
			newIdx[i] = int32(len(keep))
			keep = append(keep, i)
		}
	}
	for i := range faces {
		for k := 0; k < 3; k++ {
			v := newIdx[base[m.Faces[i].Verts[k]]]
			faces[i].Verts[k], faces[i].UVs[k] = v, v
		}
	}
	pick3 := func(l []Vec3) []Vec3 {
		if len(l) == 0 {
			return nil
		}
		out := make([]Vec3, len(keep))
		for ni, oi := range keep {
			out[ni] = l[oi]
		}
		return out
	}
	c := *m
	c.Verts, c.TVerts, c.Normals, c.Colors = pick3(m.Verts), pick3(m.TVerts), pick3(m.Normals), pick3(m.Colors)
	c.TVerts1, c.TVerts2, c.TVerts3 = pick3(m.TVerts1), pick3(m.TVerts2), pick3(m.TVerts3)
	c.Tangents = nil
	c.Faces = faces
	var skin *SkinData
	var newWeights []VertexWeight
	if len(weights) > 0 {
		newWeights = make([]VertexWeight, len(keep))
		for ni, oi := range keep {
			newWeights[ni] = weights[oi]
		}
		skin = &SkinData{Weights: newWeights}
	}
	var dangly *DanglyData
	var newConstraints []float32
	if n.Dangly != nil {
		dc := *n.Dangly
		if len(constraints) > 0 {
			newConstraints = make([]float32, len(keep))
			for ni, oi := range keep {
				newConstraints[ni] = constraints[oi]
			}
			dc.Constraints = newConstraints
		}
		dangly = &dc
	}

	// What the compile makes of it (writeMeshHeaderInner) must be the binary.
	exp, err := buildExpandedMeshOpts(&c, expandOpts{stripDegenerate: true, skinKeys: skinMergeKeys(skin), danglyKeys: dangly})
	if err != nil {
		return 0, false
	}
	tan, hand := generateTangents(&exp, false)
	if len(exp.positions) != nv || len(tan) != nv || len(exp.faceVerts) != nf {
		return 0, false
	}
	for k, fv := range exp.faceVerts {
		if exp.faceSrc[k] != k || fv != [3]uint16{rl[3*k], rl[3*k+1], rl[3*k+2]} {
			return 0, false
		}
	}
	same := func(a, b Vec3) bool {
		return math.Float32bits(a.X) == math.Float32bits(b.X) && math.Float32bits(a.Y) == math.Float32bits(b.Y) &&
			math.Float32bits(a.Z) == math.Float32bits(b.Z)
	}
	sameUV := func(a, b Vec3) bool {
		return math.Float32bits(a.X) == math.Float32bits(b.X) && math.Float32bits(a.Y) == math.Float32bits(b.Y)
	}
	newSkinKeys := skinMergeKeys(skin)
	for i := 0; i < nv; i++ {
		o := int(exp.origVert[i])
		t := m.Tangents[i]
		w := float32(1)
		if t.W < 0 {
			w = -1
		}
		if !same(exp.positions[i], m.Verts[i]) || !sameUV(exp.uvs[i], m.TVerts[i]) || !same(exp.normals[i], m.Normals[i]) ||
			!same(tan[i], Vec3{X: t.X, Y: t.Y, Z: t.Z}) || hand[i] != w {
			return 0, false
		}
		if len(m.Colors) > 0 && !same(exp.colors[i], m.Colors[i]) {
			return 0, false
		}
		for s, l := range [][]Vec3{m.TVerts1, m.TVerts2, m.TVerts3} {
			if len(l) > 0 && !sameUV([][]Vec3{exp.uvs1, exp.uvs2, exp.uvs3}[s][i], l[i]) {
				return 0, false
			}
		}
		if len(newSkinKeys) > 0 && newSkinKeys[o] != skinKeys[i] {
			return 0, false
		}
		if len(newConstraints) > 0 && math.Float32bits(newConstraints[o]) != math.Float32bits(constraints[i]) {
			return 0, false
		}
	}

	*m = c
	if n.Skin != nil && newWeights != nil {
		n.Skin.Weights = newWeights
	}
	if n.Dangly != nil && newConstraints != nil {
		n.Dangly.Constraints = newConstraints
	}
	return nv - len(keep), true
}
