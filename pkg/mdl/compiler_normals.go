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
// The algorithm is a port of the game's MdlNodeTriMesh::ProcessVerts (see
// NWMAIN_FINDINGS.md): earlier smoothing-group neighbours donate their
// already-resolved normal unless nearly opposite, later neighbours are summed
// in, and a zero sum falls back to the face's own normal. The cross product
// is (p2-p1) × (p3-p2), area-weighted.
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

	// Build per-vertex face list for efficient lookup. Face indices are
	// appended in ascending order, which the earlier/later split below
	// relies on.
	vertFaces := make([][]int, nVerts)
	for fi, f := range mesh.Faces {
		for vi := 0; vi < 3; vi++ {
			idx := int(f.Verts[vi])
			if idx >= 0 && idx < nVerts {
				vertFaces[idx] = append(vertFaces[idx], fi)
			}
		}
	}

	// Per-face-corner normals, ported from the game's
	// MdlNodeTriMesh::ProcessVerts. For each corner of face i at vertex v:
	//   - Faces j < i that share v and a smoothing-group bit have already
	//     been resolved; if face i's own unit normal is not nearly opposite
	//     (dot >= -0.99) to the normal that face j got at v, reuse it.
	//   - Otherwise the corner starts from face i's own area-weighted normal
	//     and adds only the faces j > i that share v and a smoothing bit.
	//   - A zero sum falls back to face i's own unit normal.
	// Smoothing group 0 never joins another face. The sums are float32 and
	// the comparison is exact, as in the game.
	cornerNormals := make([][3]Vec3, nFaces)
	for fi, f := range mesh.Faces {
		sg := f.SmoothGroup
		own := gameNormalize(faceNormals[fi])
		for vi := 0; vi < 3; vi++ {
			vertIdx := int(f.Verts[vi])
			if vertIdx < 0 || vertIdx >= nVerts {
				continue
			}
			acc := faceNormals[fi]
			reused := false
			if sg != 0 {
				for _, fj := range vertFaces[vertIdx] {
					if fj == fi || mesh.Faces[fj].SmoothGroup&sg == 0 {
						continue
					}
					if fj < fi {
						cn := cornerNormals[fj][cornerAt(mesh.Faces[fj], vertIdx)]
						dot := own.X*cn.X + own.Y*cn.Y + own.Z*cn.Z
						if float64(dot) >= -0.99 {
							cornerNormals[fi][vi] = cn
							reused = true
							break
						}
						continue
					}
					fn := faceNormals[fj]
					acc = Vec3{X: acc.X + fn.X, Y: acc.Y + fn.Y, Z: acc.Z + fn.Z}
				}
			}
			if reused {
				continue
			}
			switch {
			case acc.X*acc.X+acc.Y*acc.Y+acc.Z*acc.Z != 0:
				cornerNormals[fi][vi] = gameNormalize(acc)
			default:
				cornerNormals[fi][vi] = own
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

// cornerAt returns which corner of f uses vertex v, checking corner 2, then
// 1, then 0 like the game does when a face repeats a vertex.
func cornerAt(f Face, v int) int {
	switch {
	case int(f.Verts[2]) == v:
		return 2
	case int(f.Verts[1]) == v:
		return 1
	}
	return 0
}

// gameNormalize ports the game's Vector::Normalize bit for bit, in float32:
// a vector whose squared length is already within 1e-5 of 1 is left alone, a
// zero vector becomes (1, 0, 0), and anything else is scaled by
// sqrt(1/lengthSquared). The shortcut matters: it keeps nearly-unit normals
// from picking up a rounding step, which would split vertices the game keeps
// whole (or merge ones it keeps apart).
func gameNormalize(v Vec3) Vec3 {
	l := float32(v.X*v.X) + float32(v.Y*v.Y)
	l += float32(v.Z * v.Z)
	if l == 1 {
		return v
	}
	if l >= 0.9999899864196777 && l <= 1.0000100135803223 {
		return v
	}
	if l == 0 {
		return Vec3{X: 1}
	}
	s := float32(math.Sqrt(float64(float32(1) / l)))
	return Vec3{X: v.X * s, Y: v.Y * s, Z: v.Z * s}
}
