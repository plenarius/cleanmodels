// compiler_tangents.go — tangent generation for the binary MDL compiler.
//
// Ported from the game's MdlNodeTriMesh::CalculateTangentSpaceBasis (see
// NWMAIN_FINDINGS.md). It runs on the merged GPU vertices, so it lives behind
// buildExpandedMeshOpts. The companion read path is binary.go readMDXTangents,
// which turns the tangent stream plus the per-vertex handedness stream back
// into Vec4{tangent.xyz, w}.
package mdl

import (
	"strings"
)

// generateTangents computes one tangent and one handedness (+1, -1, or 0 for
// a vertex no triangle touches) per GPU vertex, exactly as the game does:
//
//   - Per triangle, the tangent is dP/du from the UV determinant,
//     (dv2*e1 - dv1*e2) / det, left unnormalised so bigger triangles weigh
//     more. The triangle's handedness is +1 when det >= 0, else -1. A
//     triangle with det == 0 is NOT skipped: it contributes the sum of its
//     three vertex normals / 3 with the components rotated (y, z, x), and
//     handedness +1.
//   - The first triangle to reach a vertex sets its tangent and handedness;
//     later triangles with the same handedness add to it. A triangle whose
//     handedness differs gets a clone of that vertex (one clone per
//     original vertex and handedness), so mirrored UV charts keep their own
//     basis. Clones are appended to exp (positions, UVs, normals, colors,
//     origVert) and the face indices are rewritten to point at them.
//   - animMesh nodes skip the accumulation: every triangle overwrites.
//   - Finally each tangent is made perpendicular to its normal
//     (Gram-Schmidt) and normalised.
//
// Returns (nil, nil) when normals or UV0 are missing.
func generateTangents(exp *expandedMesh, animMesh bool) ([]Vec3, []float32) {
	if exp == nil {
		return nil, nil
	}
	n := len(exp.positions)
	if n == 0 || len(exp.normals) < n || len(exp.uvs) < n {
		return nil, nil
	}

	type triBasis struct {
		t Vec3
		w float32
	}
	tris := make([]triBasis, len(exp.faceVerts))
	for f, fv := range exp.faceVerts {
		i0, i1, i2 := int(fv[0]), int(fv[1]), int(fv[2])
		if i0 >= n || i1 >= n || i2 >= n {
			continue
		}
		p0, p1, p2 := exp.positions[i0], exp.positions[i1], exp.positions[i2]
		u0, u1, u2 := exp.uvs[i0], exp.uvs[i1], exp.uvs[i2]
		dv2 := u2.Y - u0.Y
		dv1 := u1.Y - u0.Y
		det := float32((u1.X-u0.X)*dv2) - float32((u2.X-u0.X)*dv1)
		w := float32(1)
		if !(det >= 0) {
			w = -1
		}
		var t Vec3
		if det != 0 {
			a, b := dv1/det, dv2/det
			t = Vec3{
				X: float32(b*(p1.X-p0.X)) - float32(a*(p2.X-p0.X)),
				Y: float32((p1.Y-p0.Y)*b) - float32((p2.Y-p0.Y)*a),
				Z: float32((p1.Z-p0.Z)*b) - float32((p2.Z-p0.Z)*a),
			}
		} else {
			const third = float32(0.3333333432674408)
			n0, n1, n2 := exp.normals[i0], exp.normals[i1], exp.normals[i2]
			t = Vec3{
				X: (n0.Y + n1.Y + n2.Y) * third,
				Y: (n0.Z + n1.Z + n2.Z) * third,
				Z: (n0.X + n1.X + n2.X) * third,
			}
		}
		tris[f] = triBasis{t: t, w: w}
	}

	tan := make([]Vec3, n)
	hand := make([]float32, n)
	type clone struct {
		src uint16
		w   float32
		t   Vec3
	}
	var clones []clone
	type rewrite struct {
		face, corner int
		to           uint16
	}
	var rewrites []rewrite

	for f, fv := range exp.faceVerts {
		tb := tris[f]
		for c := 0; c < 3; c++ {
			v := fv[c]
			if int(v) >= n {
				continue
			}
			switch {
			case hand[v] == 0 || animMesh:
				hand[v] = tb.w
				tan[v] = tb.t
			case hand[v] == tb.w:
				tan[v] = Vec3{X: tan[v].X + tb.t.X, Y: tan[v].Y + tb.t.Y, Z: tan[v].Z + tb.t.Z}
			default:
				k := -1
				for ci, cl := range clones {
					if cl.src == v && cl.w == tb.w {
						k = ci
						break
					}
				}
				if k < 0 {
					clones = append(clones, clone{src: v, w: tb.w})
					k = len(clones) - 1
				}
				cl := &clones[k]
				cl.t = Vec3{X: cl.t.X + tb.t.X, Y: cl.t.Y + tb.t.Y, Z: cl.t.Z + tb.t.Z}
				rewrites = append(rewrites, rewrite{face: f, corner: c, to: uint16(n + k)})
			}
		}
	}

	if len(clones) > 0 {
		for _, cl := range clones {
			s := int(cl.src)
			exp.positions = append(exp.positions, exp.positions[s])
			exp.origVert = append(exp.origVert, exp.origVert[s])
			if len(exp.uvs) > s {
				exp.uvs = append(exp.uvs, exp.uvs[s])
			}
			if len(exp.uvs1) > s {
				exp.uvs1 = append(exp.uvs1, exp.uvs1[s])
			}
			if len(exp.uvs2) > s {
				exp.uvs2 = append(exp.uvs2, exp.uvs2[s])
			}
			if len(exp.uvs3) > s {
				exp.uvs3 = append(exp.uvs3, exp.uvs3[s])
			}
			if len(exp.normals) > s {
				exp.normals = append(exp.normals, exp.normals[s])
			}
			if len(exp.colors) > s {
				exp.colors = append(exp.colors, exp.colors[s])
			}
			tan = append(tan, cl.t)
			hand = append(hand, cl.w)
		}
		for _, r := range rewrites {
			exp.faceVerts[r.face][r.corner] = r.to
		}
	}

	for i := range tan {
		nv := exp.normals[i]
		t := tan[i]
		d := float32(nv.X*t.X) + float32(nv.Y*t.Y) + float32(nv.Z*t.Z)
		t = Vec3{X: t.X - float32(nv.X*d), Y: t.Y - float32(nv.Y*d), Z: t.Z - float32(nv.Z*d)}
		tan[i] = gameNormalize(t)
	}
	return tan, hand
}

// resolveTangents returns the per-GPU-vertex tangents and handedness values to
// write into MDX. When the source mesh already carries Vec4 tangents (parsed
// from ASCII or recovered by the decompiler) and they cover every GPU vertex,
// they are used as authored. Otherwise tangents are generated, but only for
// normal-mapped meshes: a NormalAndSpecMapped or NormalTangents render hint on
// the mesh or in its material (see isNormalMapped), which is when the game
// generates them. Other meshes leave the pointers unset and the engine
// regenerates anything it needs at load.
//
// Generation can append cloned vertices to exp, so callers must read the
// vertex count from exp afterwards. Returns (nil, nil) when no tangents can
// be produced (no UVs, no normals, or the mesh isn't normal-mapped and has
// no authored tangents).
func resolveTangents(mesh *MeshData, exp *expandedMesh, normalMapped, animMesh bool) ([]Vec3, []float32) {
	if exp == nil || len(exp.positions) == 0 {
		return nil, nil
	}
	if useExistingTangents(mesh, exp) {
		return expandExistingTangents(mesh, exp)
	}
	if mesh == nil || !normalMapped {
		return nil, nil
	}
	return generateTangents(exp, animMesh)
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
// per-GPU-vertex tangent + handedness arrays. A zero W counts as +1.
func expandExistingTangents(mesh *MeshData, exp *expandedMesh) ([]Vec3, []float32) {
	n := len(exp.positions)
	tangents := make([]Vec3, n)
	handed := make([]float32, n)
	for i := 0; i < n; i++ {
		t := mesh.Tangents[int(exp.origVert[i])]
		tangents[i] = Vec3{X: t.X, Y: t.Y, Z: t.Z}
		handed[i] = 1
		if t.W < 0 {
			handed[i] = -1
		}
	}
	return tangents, handed
}

// renderHintValue is SharedMaterial::ExtractRenderHint: the number the engine
// keeps for a renderhint, matched case-insensitively. Anything else is 0.
func renderHintValue(hint string) uint32 {
	switch {
	case strings.EqualFold(hint, "NormalAndSpecMapped"):
		return 2
	case strings.EqualFold(hint, "NormalTangents"):
		return 3
	case strings.EqualFold(hint, "None"):
		return 1
	}
	return 0
}

// hintWantsTangents reports whether a RenderHint makes the game generate
// tangents: ExtractRenderHint maps "normalandspecmapped" to 2 and
// "normaltangents" to 3, case-insensitively, and only those two qualify.
func hintWantsTangents(hint string) bool {
	return strings.EqualFold(hint, "NormalAndSpecMapped") || strings.EqualFold(hint, "NormalTangents")
}
