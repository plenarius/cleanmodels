// compiler_normals.go — normal and color generation for the binary MDL compiler.
//
// When ASCII models lack explicit normals, the compiler generates them from
// face geometry and smoothing groups, matching nwnmdlcomp's algorithm.
//
// Ref: _NmcLib/NmcMesh.cpp NmcInitializeFaceData (lines 225-345)
package mdl

import "math"

// generateNormals computes per-face-corner normals from face geometry and
// smoothing groups. The result is stored in mesh.Normals indexed the same
// way as mesh.Verts — but because different smoothing groups at the same
// vertex produce different normals, the normals are computed per face corner
// and stored back into a per-vertex array by splitting vertices that need
// different normals.
//
// The algorithm matches nwnmdlcomp:
//   - For each face corner, sum the (unnormalized) face normals of all faces
//     that share the same vertex index AND have overlapping smoothing group
//     bits (or are the same face).
//   - Normalize the sum.
//   - Cross product uses (p2-p1) × (p3-p2) for area-weighted normals.
//
// If the mesh already has normals (len >= len(Verts)), this is a no-op.
func generateNormals(mesh *MeshData) {
	if mesh == nil || len(mesh.Verts) == 0 || len(mesh.Faces) == 0 {
		return
	}
	if len(mesh.Normals) >= len(mesh.Verts) {
		return
	}

	nVerts := len(mesh.Verts)
	nFaces := len(mesh.Faces)

	// Compute unnormalized face normals (area-weighted).
	faceNormals := make([]Vec3, nFaces)
	for fi, f := range mesh.Faces {
		v0i, v1i, v2i := int(f.Verts[0]), int(f.Verts[1]), int(f.Verts[2])
		if v0i < 0 || v0i >= nVerts || v1i < 0 || v1i >= nVerts || v2i < 0 || v2i >= nVerts {
			continue
		}
		p1, p2, p3 := mesh.Verts[v0i], mesh.Verts[v1i], mesh.Verts[v2i]
		// nwnmdlcomp uses (p2-p1) × (p3-p2)
		e1 := Vec3{X: p2.X - p1.X, Y: p2.Y - p1.Y, Z: p2.Z - p1.Z}
		e2 := Vec3{X: p3.X - p2.X, Y: p3.Y - p2.Y, Z: p3.Z - p2.Z}
		faceNormals[fi] = Vec3{
			X: e1.Y*e2.Z - e1.Z*e2.Y,
			Y: e1.Z*e2.X - e1.X*e2.Z,
			Z: e1.X*e2.Y - e1.Y*e2.X,
		}
	}

	// Build per-vertex face list for efficient lookup.
	vertFaces := make([][]int, nVerts)
	for fi, f := range mesh.Faces {
		for vi := 0; vi < 3; vi++ {
			idx := int(f.Verts[vi])
			if idx >= 0 && idx < nVerts {
				vertFaces[idx] = append(vertFaces[idx], fi)
			}
		}
	}

	// Compute per-face-corner normals following nwnmdlcomp's algorithm.
	// Each face corner gets the sum of face normals from all faces sharing
	// the same vertex AND having overlapping smoothing group bits.
	cornerNormals := make([][3]Vec3, nFaces)
	for fi, f := range mesh.Faces {
		sg := f.SmoothGroup
		for vi := 0; vi < 3; vi++ {
			vertIdx := int(f.Verts[vi])
			if vertIdx < 0 || vertIdx >= nVerts {
				continue
			}
			var sx, sy, sz float64
			for _, fj := range vertFaces[vertIdx] {
				// nwnmdlcomp smooth group rule: skip when (mask & mask2) == 0,
				// which means mask=0 NEVER contributes across faces (0 & x == 0).
				// Only the current face (fi==fj) always contributes.
				sgj := mesh.Faces[fj].SmoothGroup
				if fi != fj && (sg&sgj) == 0 {
					continue
				}
				fn := faceNormals[fj]
				sx += float64(fn.X)
				sy += float64(fn.Y)
				sz += float64(fn.Z)
			}
			length := math.Sqrt(sx*sx + sy*sy + sz*sz)
			if length > 1e-10 {
				cornerNormals[fi][vi] = Vec3{
					X: float32(sx / length),
					Y: float32(sy / length),
					Z: float32(sz / length),
				}
			} else {
				cornerNormals[fi][vi] = Vec3{Z: 1}
			}
		}
	}

	// Store normals per-vertex. If all face corners sharing a vertex have
	// the same normal (common when smoothing groups don't split), this
	// produces len(Verts) normals. When smoothing groups split a vertex,
	// buildExpandedMesh handles the split via the dedup key (which includes
	// normals), so we only need to store one representative normal per
	// original vertex here. buildExpandedMesh will create separate GPU
	// vertices for corners with different normals.
	//
	// We store the normal from the first face corner encountered for each
	// vertex, then let buildExpandedMesh's per-corner lookup handle splits.
	mesh.Normals = make([]Vec3, nVerts)
	set := make([]bool, nVerts)
	for fi, f := range mesh.Faces {
		for vi := 0; vi < 3; vi++ {
			idx := int(f.Verts[vi])
			if idx >= 0 && idx < nVerts && !set[idx] {
				mesh.Normals[idx] = cornerNormals[fi][vi]
				set[idx] = true
			}
		}
	}

	// Store the per-face-corner normals so buildExpandedMesh can use the
	// correct normal for each face corner (not just the first-seen one).
	mesh.CornerNormals = cornerNormals
}
