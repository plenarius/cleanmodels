package mdl

import (
	"math"
	"strings"
)

// sharePositions gives binary vertices one position again where the source the
// binary was compiled from shared it, so a compile of the ASCII links the same
// faces (nwn-mdl-spec MDL-MESH-025).
//
// A binary stores one vertex per distinct corner (MDL-MESH-010), so a UV seam
// splits a position in two. Written one position per vertex, a compile of the
// ASCII computes the face neighbours on those positions (MDL-MESH-022) and
// links no faces across the seam; the shadow volume then treats every such edge
// as a silhouette edge (E68). Faces index positions (verts) and UVs (tverts)
// separately, so the UV split can stay while the position is shared.
//
// Which vertices to join comes from the binary's stored neighbours: when face i
// names face j across an edge, that edge's ends were one source vertex in each
// (the neighbours were computed on source indices), so their vertices are joined.
// Nothing else is: duplicate positions the source kept apart stay apart, so a
// compile links no face the original did not. Normals,
// tangents, colours, skin weights and dangly constraints are listed once per
// position (MDL-ASCII-053), so only vertices equal in all of them are joined,
// floats compared as the engine compares them (==, so -0 equals +0, NaN never
// equal); a stored link between vertices that differ in any of them is lost (a
// hard edge whose normals the ASCII can only write per position). A compile
// gives back the same merged vertices in the same order, since the merge
// compares values (MDL-MESH-011).
//
// Left as they are: walkmeshes (merged differently, MDL-MESH-012, and their
// neighbours are recomputed at load), animmeshes (animation data per source
// vertex), meshes with UV sets 1-3 or explicit texindices (how the game indexes
// tverts1-3 without texindices is not known), and any mesh whose per-vertex
// lists don't all match its vertex count, or that has no stored neighbours.
func sharePositions(n *Node) {
	if n == nil || n.Mesh == nil || n.Aabb != nil || n.AnimMesh != nil {
		return
	}
	m := n.Mesh
	nv := len(m.Verts)
	if nv == 0 || len(m.StoredAdjacency) != len(m.Faces) || len(m.TVerts1)+len(m.TVerts2)+len(m.TVerts3) > 0 ||
		len(m.TexIndices0)+len(m.TexIndices1)+len(m.TexIndices2)+len(m.TexIndices3) > 0 {
		return
	}
	full := func(l int) bool { return l == 0 || l == nv }
	var weights []VertexWeight
	if n.Skin != nil {
		weights = n.Skin.Weights
	}
	var constraints []float32
	if n.Dangly != nil {
		constraints = n.Dangly.Constraints
	}
	if !full(len(m.Normals)) || !full(len(m.Tangents)) || !full(len(m.Colors)) ||
		!full(len(weights)) || !full(len(constraints)) || len(m.TVerts) != nv {
		return
	}
	for _, f := range m.Faces {
		for c := 0; c < 3; c++ {
			if f.Verts[c] != f.UVs[c] || f.Verts[c] < 0 || int(f.Verts[c]) >= nv {
				return // not the decompiler's one-index-per-corner layout
			}
		}
	}

	var b strings.Builder
	nan := false
	put := func(fs ...float32) {
		for _, f := range fs {
			if f != f {
				nan = true
			}
			if f == 0 {
				f = 0 // -0 merges with +0
			}
			bits := math.Float32bits(f)
			b.WriteByte(byte(bits))
			b.WriteByte(byte(bits >> 8))
			b.WriteByte(byte(bits >> 16))
			b.WriteByte(byte(bits >> 24))
		}
	}
	keys := make([]string, nv)
	nans := make([]bool, nv)
	for i := 0; i < nv; i++ {
		b.Reset()
		nan = false
		v := m.Verts[i]
		put(v.X, v.Y, v.Z)
		if len(m.Normals) > 0 {
			put(m.Normals[i].X, m.Normals[i].Y, m.Normals[i].Z)
		}
		if len(m.Tangents) > 0 {
			t := m.Tangents[i]
			put(t.X, t.Y, t.Z, t.W)
		}
		if len(m.Colors) > 0 {
			put(m.Colors[i].X, m.Colors[i].Y, m.Colors[i].Z)
		}
		if len(constraints) > 0 {
			put(constraints[i])
		}
		if len(weights) > 0 {
			for k, bone := range weights[i].Bones {
				b.WriteString(strings.ToLower(bone))
				b.WriteByte(0)
				if k < len(weights[i].Weights) {
					put(weights[i].Weights[k])
				}
			}
		}
		keys[i], nans[i] = b.String(), nan
	}

	parent := make([]int32, nv)
	for i := range parent {
		parent[i] = int32(i)
	}
	var find func(x int32) int32
	find = func(x int32) int32 {
		for parent[x] != x {
			parent[x] = parent[parent[x]]
			x = parent[x]
		}
		return x
	}
	join := func(a, b int32) {
		if keys[a] != keys[b] || nans[a] || nans[b] {
			return
		}
		ra, rb := find(a), find(b)
		if ra < rb {
			parent[rb] = ra
		} else if rb < ra {
			parent[ra] = rb
		}
	}
	for fi, f := range m.Faces {
		for k := 0; k < 3; k++ {
			j := int(m.StoredAdjacency[fi][k])
			if j < 0 || j >= len(m.Faces) || j == fi {
				continue
			}
			a, c := f.Verts[k], f.Verts[(k+1)%3]
			g := m.Faces[j].Verts
			// The neighbour's edge with the same two positions (either direction).
			for t := 0; t < 3; t++ {
				x, y := g[t], g[(t+1)%3]
				pa, pc, px, py := m.Verts[a], m.Verts[c], m.Verts[x], m.Verts[y]
				if pa == px && pc == py {
					join(a, x)
					join(c, y)
					break
				}
				if pa == py && pc == px {
					join(a, y)
					join(c, x)
					break
				}
			}
		}
	}
	remap := make([]int32, nv)
	newIndex := make(map[int32]int32, nv)
	var keep []int
	for i := 0; i < nv; i++ {
		r := find(int32(i))
		if j, ok := newIndex[r]; ok {
			remap[i] = j
			continue
		}
		j := int32(len(keep))
		newIndex[r] = j
		remap[i] = j
		keep = append(keep, i)
	}
	if len(keep) == nv {
		return
	}

	pick := func(l int, take func(newI, oldI int)) {
		if l == 0 {
			return
		}
		for ni, oi := range keep {
			take(ni, oi)
		}
	}
	verts := make([]Vec3, len(keep))
	pick(nv, func(ni, oi int) { verts[ni] = m.Verts[oi] })
	if len(m.Normals) > 0 {
		normals := make([]Vec3, len(keep))
		pick(nv, func(ni, oi int) { normals[ni] = m.Normals[oi] })
		m.Normals = normals
	}
	if len(m.Tangents) > 0 {
		tangents := make([]Vec4, len(keep))
		pick(nv, func(ni, oi int) { tangents[ni] = m.Tangents[oi] })
		m.Tangents = tangents
	}
	if len(m.Colors) > 0 {
		colors := make([]Vec3, len(keep))
		pick(nv, func(ni, oi int) { colors[ni] = m.Colors[oi] })
		m.Colors = colors
	}
	if len(constraints) > 0 {
		cs := make([]float32, len(keep))
		pick(nv, func(ni, oi int) { cs[ni] = constraints[oi] })
		n.Dangly.Constraints = cs
	}
	if len(weights) > 0 {
		ws := make([]VertexWeight, len(keep))
		pick(nv, func(ni, oi int) { ws[ni] = weights[oi] })
		n.Skin.Weights = ws
	}
	m.Verts = verts
	for fi := range m.Faces {
		for c := 0; c < 3; c++ {
			m.Faces[fi].Verts[c] = remap[m.Faces[fi].Verts[c]] // UVs keep indexing TVerts
		}
	}
}
