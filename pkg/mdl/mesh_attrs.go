package mdl

// vertexAttrPresence captures which per-vertex companion arrays (Normals,
// Colors, Tangents, Skin.Weights, Dangly.Constraints) are populated and in
// lockstep with mesh.Verts. Repair passes that mutate vertex count must keep
// the present arrays in sync, so all such passes go through this bag of
// flags rather than re-deriving them ad-hoc.
type vertexAttrPresence struct {
	Normals  bool
	Colors   bool
	Tangents bool
	Skin     bool
	Dangly   bool
}

// vertexAttrsOf inspects n's mesh and returns the presence flags. A
// companion array counts as "present" only if its length matches Verts;
// mismatched legacy data is treated as absent so the caller doesn't trip an
// out-of-range index when fanning out.
func vertexAttrsOf(n *Node) vertexAttrPresence {
	if n == nil || n.Mesh == nil {
		return vertexAttrPresence{}
	}
	m := n.Mesh
	a := vertexAttrPresence{
		Normals:  len(m.Normals) == len(m.Verts),
		Colors:   len(m.Colors) == len(m.Verts),
		Tangents: len(m.Tangents) == len(m.Verts),
	}
	if n.Skin != nil {
		a.Skin = len(n.Skin.Weights) == len(m.Verts)
	}
	if n.Dangly != nil {
		a.Dangly = len(n.Dangly.Constraints) == len(m.Verts)
	}
	return a
}

// compactVertexAttrs filters every present per-vertex array on n to keep
// only entries whose mask[i] is true. mesh.Verts must have already been
// scanned to produce mask. The caller is responsible for remapping face
// vertex indices afterwards (see remapForKept).
func compactVertexAttrs(n *Node, attrs vertexAttrPresence, mask []bool) {
	m := n.Mesh
	m.Verts = filterByMask(m.Verts, mask)
	if attrs.Normals {
		m.Normals = filterByMask(m.Normals, mask)
	}
	if attrs.Colors {
		m.Colors = filterByMask(m.Colors, mask)
	}
	if attrs.Tangents {
		m.Tangents = filterByMask(m.Tangents, mask)
	}
	if attrs.Skin {
		n.Skin.Weights = filterVertexWeights(n.Skin.Weights, mask)
	}
	if attrs.Dangly {
		n.Dangly.Constraints = filterByMask(n.Dangly.Constraints, mask)
	}
}

// appendDefaultVertexAttrs appends a default value to each present
// per-vertex array, used by repair passes that synthesise new vertices
// (e.g. chamfer corner vertices) without an interpolation source. The
// defaults match what the original Prolog tool emits.
func appendDefaultVertexAttrs(n *Node, attrs vertexAttrPresence) {
	m := n.Mesh
	if attrs.Normals {
		m.Normals = append(m.Normals, Vec3{Z: 1})
	}
	if attrs.Colors {
		m.Colors = append(m.Colors, Vec3{X: 1, Y: 1, Z: 1})
	}
	if attrs.Tangents {
		m.Tangents = append(m.Tangents, Vec4{X: 1, W: 1})
	}
	if attrs.Skin {
		n.Skin.Weights = append(n.Skin.Weights, VertexWeight{})
	}
	if attrs.Dangly {
		n.Dangly.Constraints = append(n.Dangly.Constraints, 0)
	}
}

// filterByMask returns src filtered to indices flagged true in mask.
// Generic so it works for Vec3, Vec4, float32, etc.
func filterByMask[T any](src []T, mask []bool) []T {
	out := make([]T, 0, len(src))
	for i, v := range src {
		if i < len(mask) && mask[i] {
			out = append(out, v)
		}
	}
	return out
}

// filterVertexWeights is filterByMask with deep-clone for VertexWeight,
// which carries slice fields that would otherwise alias the source.
func filterVertexWeights(src []VertexWeight, mask []bool) []VertexWeight {
	out := make([]VertexWeight, 0, len(src))
	for i, w := range src {
		if i < len(mask) && mask[i] {
			out = append(out, CloneVertexWeight(w))
		}
	}
	return out
}

// remapForKept returns an old-index -> new-index map matching the result of
// filterByMask. Indices not kept map to -1 so callers can detect them.
// Returns nil when every index is kept (i.e. nothing was filtered).
func remapForKept(mask []bool) []int32 {
	all := true
	for _, u := range mask {
		if !u {
			all = false
			break
		}
	}
	if all {
		return nil
	}
	remap := make([]int32, len(mask))
	next := int32(0)
	for i, u := range mask {
		if u {
			remap[i] = next
			next++
		} else {
			remap[i] = -1
		}
	}
	return remap
}

// remapFaceVerts rewrites face V0/V1/V2 indices through remap (built by
// remapForKept). Out-of-range or dropped (-1) entries are left untouched
// so callers can spot dangling references.
func remapFaceVerts(faces []Face, remap []int32) {
	if remap == nil {
		return
	}
	for fi := range faces {
		f := &faces[fi]
		for k := 0; k < 3; k++ {
			old := f.Verts[k]
			if int(old) >= 0 && int(old) < len(remap) && remap[old] >= 0 {
				f.Verts[k] = remap[old]
			}
		}
	}
}

// remapFaceUVs is remapFaceVerts for the UV channel (face.UVs). Used after
// compacting a TVerts channel to align face references.
func remapFaceUVs(faces []Face, remap []int32) {
	if remap == nil {
		return
	}
	for fi := range faces {
		f := &faces[fi]
		for k := 0; k < 3; k++ {
			old := f.UVs[k]
			if int(old) >= 0 && int(old) < len(remap) && remap[old] >= 0 {
				f.UVs[k] = remap[old]
			}
		}
	}
}

// remapTexIndices is remapFaceVerts for an extra texture-channel (per-face
// [3]int32 index). Used after compacting TVerts1/2/3.
func remapTexIndices(idxs [][3]int32, remap []int32) {
	if remap == nil {
		return
	}
	for fi := range idxs {
		for k := 0; k < 3; k++ {
			old := idxs[fi][k]
			if int(old) >= 0 && int(old) < len(remap) && remap[old] >= 0 {
				idxs[fi][k] = remap[old]
			}
		}
	}
}
