// compiler_tangents.go — tangent / bitangent generation for the binary MDL compiler.
//
// Computes per-GPU-vertex tangent and bitangent vectors from positions,
// normals, and UV0 coordinates using the Mikktspace algorithm. Called after
// buildExpandedMesh has produced the deduplicated GPU vertex arrays.
//
// Without baked tangents the NWN:EE engine has to derive them at load time;
// pre-baking matches the behaviour of the game's own model compiler and
// removes that startup cost. The companion read path lives in
// binary.go readMDXTangents, which reconstructs the per-vertex Vec4
// (tangent.xyz + handedness W) from the two MDX Vec3 streams we write here.
package mdl

import "math"

// generateTangents computes tangent and bitangent arrays (one per GPU vertex)
// from the geometry in exp using the Mikktspace algorithm.
//
// Returns (nil, nil) if normals or UV0 data are missing — the caller should
// leave the MDX tangent / bitangent pointers at -1 in that case.
//
// Both returned slices have length len(exp.positions) and contain unit-length
// vectors. The bitangent is reconstructible at runtime as
// cross(normal, tangent) * w, but we still write it explicitly so the
// decompiler can recover the W handedness sign without ambiguity.
func generateTangents(exp *expandedMesh) ([]Vec3, []Vec3) {
	if exp == nil {
		return nil, nil
	}
	n := len(exp.positions)
	if n == 0 || len(exp.normals) < n || len(exp.uvs) < n {
		return nil, nil
	}

	// Accumulate tangent / bitangent contributions in float64 for numerical
	// stability — many real models have very small UV deltas where float32
	// rounding would visibly skew the tangent direction.
	tAccum := make([][3]float64, n)
	bAccum := make([][3]float64, n)

	for _, fv := range exp.faceVerts {
		i0, i1, i2 := int(fv[0]), int(fv[1]), int(fv[2])
		if i0 >= n || i1 >= n || i2 >= n || i0 < 0 || i1 < 0 || i2 < 0 {
			continue
		}

		p0, p1, p2 := exp.positions[i0], exp.positions[i1], exp.positions[i2]
		uv0, uv1, uv2 := exp.uvs[i0], exp.uvs[i1], exp.uvs[i2]

		e1x := float64(p1.X - p0.X)
		e1y := float64(p1.Y - p0.Y)
		e1z := float64(p1.Z - p0.Z)
		e2x := float64(p2.X - p0.X)
		e2y := float64(p2.Y - p0.Y)
		e2z := float64(p2.Z - p0.Z)

		du1 := float64(uv1.X - uv0.X)
		dv1 := float64(uv1.Y - uv0.Y)
		du2 := float64(uv2.X - uv0.X)
		dv2 := float64(uv2.Y - uv0.Y)

		// Degenerate UV mapping (collinear or zero-area in UV space) — its
		// contribution would be infinite. Skip the triangle entirely; other
		// triangles sharing these vertices will still drive their tangents.
		denom := du1*dv2 - du2*dv1
		if denom == 0 || math.Abs(denom) < 1e-20 {
			continue
		}
		r := 1.0 / denom

		tx := (e1x*dv2 - e2x*dv1) * r
		ty := (e1y*dv2 - e2y*dv1) * r
		tz := (e1z*dv2 - e2z*dv1) * r
		bx := (e2x*du1 - e1x*du2) * r
		by := (e2y*du1 - e1y*du2) * r
		bz := (e2z*du1 - e1z*du2) * r

		if math.IsNaN(tx) || math.IsInf(tx, 0) ||
			math.IsNaN(ty) || math.IsInf(ty, 0) ||
			math.IsNaN(tz) || math.IsInf(tz, 0) ||
			math.IsNaN(bx) || math.IsInf(bx, 0) ||
			math.IsNaN(by) || math.IsInf(by, 0) ||
			math.IsNaN(bz) || math.IsInf(bz, 0) {
			continue
		}

		for _, idx := range [3]int{i0, i1, i2} {
			tAccum[idx][0] += tx
			tAccum[idx][1] += ty
			tAccum[idx][2] += tz
			bAccum[idx][0] += bx
			bAccum[idx][1] += by
			bAccum[idx][2] += bz
		}
	}

	tangents := make([]Vec3, n)
	bitangents := make([]Vec3, n)

	for i := 0; i < n; i++ {
		nrm := exp.normals[i]
		nx, ny, nz := float64(nrm.X), float64(nrm.Y), float64(nrm.Z)
		tx, ty, tz := tAccum[i][0], tAccum[i][1], tAccum[i][2]

		// Gram-Schmidt orthogonalise the accumulated tangent against the
		// vertex normal so it lies in the tangent plane.
		dotNT := nx*tx + ny*ty + nz*tz
		tx -= nx * dotNT
		ty -= ny * dotNT
		tz -= nz * dotNT

		l := math.Sqrt(tx*tx + ty*ty + tz*tz)
		if l < 1e-12 {
			// Tangent collapsed — either no triangle contributed (isolated
			// vertex) or every contribution was perpendicular-cancelled by
			// the normal. Pick an arbitrary axis perpendicular to the
			// normal so we still emit a finite, unit-length tangent.
			tx, ty, tz = orthogonalAxis(nx, ny, nz)
			l = math.Sqrt(tx*tx + ty*ty + tz*tz)
			if l < 1e-12 {
				// Normal is also degenerate; fall back to +X.
				tx, ty, tz, l = 1, 0, 0, 1
			}
		}
		tx, ty, tz = tx/l, ty/l, tz/l

		// Handedness: sign of dot(cross(n, t), accumulated_bitangent).
		// Recover the bitangent as cross(n, t) * w so the decompiler can
		// re-derive W via dot(cross(n, t), bitangent).
		cnx := ny*tz - nz*ty
		cny := nz*tx - nx*tz
		cnz := nx*ty - ny*tx

		bx, by, bz := bAccum[i][0], bAccum[i][1], bAccum[i][2]
		w := 1.0
		if (cnx*bx + cny*by + cnz*bz) < 0 {
			w = -1.0
		}

		tangents[i] = Vec3{X: float32(tx), Y: float32(ty), Z: float32(tz)}
		bitangents[i] = Vec3{X: float32(cnx * w), Y: float32(cny * w), Z: float32(cnz * w)}
	}

	return tangents, bitangents
}

// resolveTangents returns the per-GPU-vertex tangent and bitangent arrays to
// write into MDX. When the source mesh already carries hand-authored Vec4
// tangents (parsed from ASCII or recovered from a previous decompile), they
// are expanded to GPU vertices and split into tangent + bitangent rather than
// regenerated — this preserves authored handedness and avoids round-trip
// drift. Otherwise tangents are generated from geometry only when the mesh
// declares RenderHint NormalAndSpecMapped, mirroring the game compiler's
// behavior — non-normal-mapped meshes don't need tangents at runtime, so
// emitting them would just waste MDX bytes.
//
// Returns (nil, nil) when no tangents can be produced (no UVs, no normals,
// the existing tangent table doesn't cover the referenced vertices, or the
// mesh isn't normal-mapped and has no authored tangents).
func resolveTangents(mesh *MeshData, exp *expandedMesh) ([]Vec3, []Vec3) {
	if exp == nil || len(exp.positions) == 0 {
		return nil, nil
	}
	if useExistingTangents(mesh, exp) {
		return expandExistingTangents(mesh, exp)
	}
	if mesh == nil || mesh.RenderHint != "NormalAndSpecMapped" {
		return nil, nil
	}
	return generateTangents(exp)
}

// useExistingTangents reports whether mesh.Tangents covers every GPU vertex
// (indirectly via origVert) and is therefore safe to expand without
// generating from geometry.
func useExistingTangents(mesh *MeshData, exp *expandedMesh) bool {
	if mesh == nil || len(mesh.Tangents) == 0 {
		return false
	}
	if len(exp.normals) < len(exp.positions) {
		return false
	}
	for _, ov := range exp.origVert {
		i := int(ov)
		if i < 0 || i >= len(mesh.Tangents) {
			return false
		}
	}
	return true
}

// expandExistingTangents projects the per-original-vertex Vec4 tangents into
// per-GPU-vertex tangent + bitangent Vec3 arrays. Bitangents are
// reconstructed via cross(normal, tangent.xyz) * tangent.W, mirroring how
// the runtime would derive them.
func expandExistingTangents(mesh *MeshData, exp *expandedMesh) ([]Vec3, []Vec3) {
	n := len(exp.positions)
	tangents := make([]Vec3, n)
	bitangents := make([]Vec3, n)
	for i := 0; i < n; i++ {
		ov := int(exp.origVert[i])
		t := mesh.Tangents[ov]
		txyz := Vec3{X: t.X, Y: t.Y, Z: t.Z}
		tangents[i] = txyz
		w := t.W
		if w == 0 {
			w = 1
		}
		bitangents[i] = vecScale(vecCross(exp.normals[i], txyz), w)
	}
	return tangents, bitangents
}

// orthogonalAxis returns an arbitrary vector perpendicular to (nx, ny, nz).
// Used as a fallback when the accumulated tangent collapses to zero (e.g.
// every contributing triangle has its tangent direction perfectly cancelled
// by the normal projection).
func orthogonalAxis(nx, ny, nz float64) (float64, float64, float64) {
	ax, ay, az := math.Abs(nx), math.Abs(ny), math.Abs(nz)
	switch {
	case ax <= ay && ax <= az:
		return 0, nz, -ny
	case ay <= az:
		return -nz, 0, nx
	default:
		return ny, -nx, 0
	}
}
