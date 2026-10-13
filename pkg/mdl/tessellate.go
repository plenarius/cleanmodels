package mdl

// TessellateMesh subdivides every triangle in the mesh whose longest edge
// exceeds maxEdge until no edge is longer than maxEdge. Each subdivision
// inserts a midpoint vertex on the longest edge of one triangle and splits
// the two triangles sharing that edge into four.
//
// Mirrors make_checks.pl tesselate_mesh/5 (line 6152). Returns the number of
// edge-bisections performed.
//
// The mesh's TVerts (channel 0) are co-bisected. Per-vertex normals, colors,
// tangents, dangly constraints, and skin weights are interpolated linearly
// for any midpoint vertex created. Other UV channels (TVerts1/2/3) are not
// updated; callers using them on water meshes will need to extend this.
func TessellateMesh(n *Node, maxEdge float32) int {
	if n == nil || n.Mesh == nil || maxEdge <= 0 {
		return 0
	}
	mesh := n.Mesh

	attrs := vertexAttrsOf(n)
	hasTVerts := len(mesh.TVerts) > 0

	maxSq := float64(maxEdge) * float64(maxEdge)
	count := 0
	const safety = 1 << 16
	for iter := 0; iter < safety; iter++ {
		fi, edge := findLongestEdge(mesh, maxSq)
		if fi < 0 {
			break
		}
		bisectEdge(n, fi, edge, attrs, hasTVerts)
		count++
	}
	return count
}

// tvUVInRange is a bounds check used by bisectEdge / midUVFor to avoid
// panicking when a face references a UV index that isn't backed by a TVerts
// entry (e.g. legacy meshes that lost a TVerts channel during repair).
func tvUVInRange(mesh *MeshData, idx int32) bool {
	return idx >= 0 && int(idx) < len(mesh.TVerts)
}

func edgeLenSq(mesh *MeshData, a, b int32) float64 {
	if a < 0 || b < 0 || int(a) >= len(mesh.Verts) || int(b) >= len(mesh.Verts) {
		return 0
	}
	dx := float64(mesh.Verts[a].X - mesh.Verts[b].X)
	dy := float64(mesh.Verts[a].Y - mesh.Verts[b].Y)
	dz := float64(mesh.Verts[a].Z - mesh.Verts[b].Z)
	return dx*dx + dy*dy + dz*dz
}

// findLongestEdge returns the (face index, corner) of the longest edge among
// all triangles whose length-squared exceeds maxSq. Returns (-1, -1) when no
// such edge exists.
func findLongestEdge(mesh *MeshData, maxSq float64) (int, int) {
	bestFace, bestEdge := -1, -1
	bestLen := maxSq
	for fi, f := range mesh.Faces {
		for ei := 0; ei < 3; ei++ {
			a, b := f.Verts[ei], f.Verts[(ei+1)%3]
			l := edgeLenSq(mesh, a, b)
			if l > bestLen {
				bestLen = l
				bestFace = fi
				bestEdge = ei
			}
		}
	}
	return bestFace, bestEdge
}

// bisectEdge inserts a midpoint vertex on the (face, edge) of fi/ei and
// splits every triangle containing that undirected edge into two triangles
// that share the midpoint.
func bisectEdge(n *Node, fi, ei int, attrs vertexAttrPresence, hasTVerts bool) {
	mesh := n.Mesh
	f := mesh.Faces[fi]
	a, b := f.Verts[ei], f.Verts[(ei+1)%3]

	mid := int32(len(mesh.Verts))
	mesh.Verts = append(mesh.Verts, vecLerp3(mesh.Verts[a], mesh.Verts[b], 0.5))
	if attrs.Normals {
		mesh.Normals = append(mesh.Normals, vecNormalize(vecLerp3(mesh.Normals[a], mesh.Normals[b], 0.5)))
	}
	if attrs.Colors {
		mesh.Colors = append(mesh.Colors, vecLerp3(mesh.Colors[a], mesh.Colors[b], 0.5))
	}
	if attrs.Tangents {
		mesh.Tangents = append(mesh.Tangents, vecLerp4(mesh.Tangents[a], mesh.Tangents[b], 0.5))
	}
	if attrs.Skin {
		n.Skin.Weights = append(n.Skin.Weights, lerpVertexWeight(n.Skin.Weights[a], n.Skin.Weights[b], 0.5))
	}
	if attrs.Dangly {
		n.Dangly.Constraints = append(n.Dangly.Constraints, (n.Dangly.Constraints[a]+n.Dangly.Constraints[b])/2)
	}

	tvA, tvB := f.UVs[ei], f.UVs[(ei+1)%3]
	var tvMid int32 = -1
	if hasTVerts && tvUVInRange(mesh, tvA) && tvUVInRange(mesh, tvB) {
		tvMid = int32(len(mesh.TVerts))
		mesh.TVerts = append(mesh.TVerts, vecLerp3(mesh.TVerts[tvA], mesh.TVerts[tvB], 0.5))
	}

	var newFaces []Face
	keep := mesh.Faces[:0]
	for _, face := range mesh.Faces {
		split := -1
		for k := 0; k < 3; k++ {
			va, vb := face.Verts[k], face.Verts[(k+1)%3]
			if (va == a && vb == b) || (va == b && vb == a) {
				split = k
				break
			}
		}
		if split == -1 {
			keep = append(keep, face)
			continue
		}
		k := split
		v0, v1, v2 := face.Verts[k], face.Verts[(k+1)%3], face.Verts[(k+2)%3]
		var u0, u1, u2 int32 = face.UVs[k], face.UVs[(k+1)%3], face.UVs[(k+2)%3]

		var midUV0, midUV1 int32 = -1, -1
		if hasTVerts && tvMid >= 0 {
			if v0 == a {
				midUV0 = midUVFor(mesh, u0, u1, tvA, tvB, tvMid)
			} else {
				midUV0 = midUVFor(mesh, u1, u0, tvA, tvB, tvMid)
			}
			midUV1 = midUV0
		}

		f1 := Face{
			Verts:       [3]int32{v0, mid, v2},
			UVs:         [3]int32{u0, midUV0, u2},
			SmoothGroup: face.SmoothGroup,
			Material:    face.Material,
			Normal:      face.Normal,
			PlaneD:      face.PlaneD,
		}
		f2 := Face{
			Verts:       [3]int32{mid, v1, v2},
			UVs:         [3]int32{midUV1, u1, u2},
			SmoothGroup: face.SmoothGroup,
			Material:    face.Material,
			Normal:      face.Normal,
			PlaneD:      face.PlaneD,
		}
		newFaces = append(newFaces, f1, f2)
	}
	mesh.Faces = append(keep, newFaces...)
}

// midUVFor returns the UV index for the midpoint of the original edge.
// (uOrigA, uOrigB) is the UV pair on the bisected face for vertex pair
// (origA, origB) such that origA == a (the source-of-truth orientation that
// generated tvMid). For the opposite face the midpoint UV is interpolated
// fresh because the two faces may use different UVs at the shared edge.
func midUVFor(mesh *MeshData, uA, uB, tvA, tvB, tvMid int32) int32 {
	if uA == tvA && uB == tvB {
		return tvMid
	}
	if !tvUVInRange(mesh, uA) || !tvUVInRange(mesh, uB) {
		return tvMid
	}
	out := int32(len(mesh.TVerts))
	mesh.TVerts = append(mesh.TVerts, vecLerp3(mesh.TVerts[uA], mesh.TVerts[uB], 0.5))
	return out
}

