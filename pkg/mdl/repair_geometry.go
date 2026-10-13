package mdl

// StripDegenerateFaces removes faces where two or more vertex indices are
// identical (zero-area triangles). Returns the number of faces removed.
func StripDegenerateFaces(model *Model) int {
	total := 0
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil || len(n.Mesh.Faces) == 0 {
			continue
		}
		mesh := n.Mesh
		origCount := len(mesh.Faces)
		kept := mesh.Faces[:0]
		var keptIdx []int
		for fi, f := range mesh.Faces {
			if f.Verts[0] == f.Verts[1] || f.Verts[1] == f.Verts[2] || f.Verts[0] == f.Verts[2] {
				total++
				continue
			}
			kept = append(kept, f)
			keptIdx = append(keptIdx, fi)
		}
		mesh.Faces = kept
		if len(keptIdx) < origCount {
			mesh.TexIndices0 = filterByIdx(mesh.TexIndices0, keptIdx)
			mesh.TexIndices1 = filterByIdx(mesh.TexIndices1, keptIdx)
			mesh.TexIndices2 = filterByIdx(mesh.TexIndices2, keptIdx)
			mesh.TexIndices3 = filterByIdx(mesh.TexIndices3, keptIdx)
		}
	}
	return total
}

func filterByIdx(src [][3]int32, indices []int) [][3]int32 {
	if len(src) == 0 {
		return src
	}
	out := make([][3]int32, len(indices))
	for i, idx := range indices {
		if idx >= 0 && idx < len(src) {
			out[i] = src[idx]
		}
	}
	return out
}

// dupUV duplicates a UV vertex for a face's vertex index in the given texcoord
// channel, updating the TexIndices entry to point to the new copy.
func dupUV(has bool, fi, vi int, tverts *[]Vec3, texIndices [][3]int32) {
	if !has || fi >= len(texIndices) {
		return
	}
	uvIdx := texIndices[fi][vi]
	if uvIdx < 0 || int(uvIdx) >= len(*tverts) {
		return
	}
	newUV := int32(len(*tverts))
	*tverts = append(*tverts, (*tverts)[uvIdx])
	texIndices[fi][vi] = newUV
}

// SplitMultipleEdges splits faces at edges shared by 3+ faces by duplicating
// the shared vertices so each edge is shared by at most 2 faces. This fixes
// shadow tearing caused by non-manifold edges in NWN's stencil shadow renderer.
// Returns the number of edges fixed.
func SplitMultipleEdges(n *Node) int {
	mesh := n.Mesh

	edgeFaces := BuildEdgeFaceMap(mesh.Faces)

	var multiEdges []EdgeKey
	for e, faces := range edgeFaces {
		if len(faces) > 2 {
			multiEdges = append(multiEdges, e)
		}
	}
	if len(multiEdges) == 0 {
		return 0
	}

	hasNormals := len(mesh.Normals) == len(mesh.Verts)
	hasColors := len(mesh.Colors) == len(mesh.Verts)
	hasTangents := len(mesh.Tangents) == len(mesh.Verts)
	hasTVerts := len(mesh.TVerts) > 0
	hasTVerts1 := len(mesh.TVerts1) > 0
	hasTVerts2 := len(mesh.TVerts2) > 0
	hasTVerts3 := len(mesh.TVerts3) > 0
	hasSkin := n.Skin != nil && len(n.Skin.Weights) == len(mesh.Verts)
	hasDangly := n.Dangly != nil && len(n.Dangly.Constraints) == len(mesh.Verts)

	for _, e := range multiEdges {
		faces := edgeFaces[e]
		for _, fi := range faces[2:] {
			f := &mesh.Faces[fi]
			for vi := 0; vi < 3; vi++ {
				if f.Verts[vi] == e.V0 || f.Verts[vi] == e.V1 {
					oldIdx := f.Verts[vi]
					if oldIdx < 0 || int(oldIdx) >= len(mesh.Verts) {
						continue
					}
					newIdx := int32(len(mesh.Verts))
					mesh.Verts = append(mesh.Verts, mesh.Verts[oldIdx])
					if hasNormals {
						mesh.Normals = append(mesh.Normals, mesh.Normals[oldIdx])
					}
					if hasColors {
						mesh.Colors = append(mesh.Colors, mesh.Colors[oldIdx])
					}
					if hasTangents {
						mesh.Tangents = append(mesh.Tangents, mesh.Tangents[oldIdx])
					}
					if hasSkin {
						n.Skin.Weights = append(n.Skin.Weights, CloneVertexWeight(n.Skin.Weights[oldIdx]))
					}
					if hasDangly {
						n.Dangly.Constraints = append(n.Dangly.Constraints, n.Dangly.Constraints[oldIdx])
					}
				f.Verts[vi] = newIdx
				if hasTVerts {
					uvIdx := f.UVs[vi]
					if uvIdx >= 0 && int(uvIdx) < len(mesh.TVerts) {
						f.UVs[vi] = int32(len(mesh.TVerts))
						mesh.TVerts = append(mesh.TVerts, mesh.TVerts[uvIdx])
					}
				} else {
					f.UVs[vi] = newIdx
				}
				dupUV(hasTVerts, fi, vi, &mesh.TVerts, mesh.TexIndices0)
					dupUV(hasTVerts1, fi, vi, &mesh.TVerts1, mesh.TexIndices1)
					dupUV(hasTVerts2, fi, vi, &mesh.TVerts2, mesh.TexIndices2)
					dupUV(hasTVerts3, fi, vi, &mesh.TVerts3, mesh.TexIndices3)
				}
			}
		}
	}

	return len(multiEdges)
}
