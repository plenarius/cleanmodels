package mdl

import (
	"fmt"
	"math"
)

// chamferSmoothGroup is the magic smoothing-group tag that marks Rosenkrantz
// chamfer triangles. The ASCII faces line is
// `V0 V1 V2 SmoothGroup UV0 UV1 UV2 Material`; the legacy Prolog matches the
// 5th token (smoothing group) against this constant, see make_checks.pl
// line 5604 and 5720.
const chamferSmoothGroup int32 = 1048576

// chamferMaterial is the material index used by chamfer triangles. Matches
// the literal 21 in add_chamfer_face/7 (line 5720).
const chamferMaterial int32 = 21

// chamferOffset is the inward + downward offset applied when adding a
// chamfer vertex. Matches the snap(0.03,...) call in
// make_checks.pl add_chamfer_vertex/6 (line 5694).
const chamferOffset float32 = 0.03

// DeleteChamfers removes every chamfer face (smoothing group ==
// chamferSmoothGroup) whose absolute geometry matches one of the patterns in
// is_chamfer/3, then drops vertices and tverts that no remaining face
// references.
//
// Mirrors make_checks.pl delete_chamfer/4 (line 5625) and is_chamfer/3
// (lines 5641-5665). Returns one message per affected node.
func DeleteChamfers(model *Model) []string {
	var msgs []string
	idx := nodeIndex(model)
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil || len(n.Mesh.Faces) == 0 {
			continue
		}
		removed := deleteChamferOnNode(idx, n)
		if removed > 0 {
			msgs = append(msgs, fmt.Sprintf("removed %d chamfer face(s) from %q", removed, n.Name))
		}
	}
	return msgs
}

func deleteChamferOnNode(idx map[string]*Node, n *Node) int {
	mesh := n.Mesh
	world := WorldVerticesCached(idx, n)
	if len(world) != len(mesh.Verts) {
		return 0
	}

	keep := mesh.Faces[:0]
	keepIdx := make([]int, 0, len(mesh.Faces))
	removed := 0
	for fi, f := range mesh.Faces {
		if f.SmoothGroup != chamferSmoothGroup {
			keep = append(keep, f)
			keepIdx = append(keepIdx, fi)
			continue
		}
		if !int32InRange(f.Verts[0], world) || !int32InRange(f.Verts[1], world) || !int32InRange(f.Verts[2], world) {
			keep = append(keep, f)
			keepIdx = append(keepIdx, fi)
			continue
		}
		w0 := world[f.Verts[0]]
		w1 := world[f.Verts[1]]
		w2 := world[f.Verts[2]]
		if !isChamferTriangle(w0, w1, w2) {
			keep = append(keep, f)
			keepIdx = append(keepIdx, fi)
			continue
		}
		removed++
	}
	if removed == 0 {
		return 0
	}
	mesh.Faces = keep
	mesh.TexIndices0 = filterByIdx(mesh.TexIndices0, keepIdx)
	mesh.TexIndices1 = filterByIdx(mesh.TexIndices1, keepIdx)
	mesh.TexIndices2 = filterByIdx(mesh.TexIndices2, keepIdx)
	mesh.TexIndices3 = filterByIdx(mesh.TexIndices3, keepIdx)

	dropUnusedVerticesAndTVerts(n)
	return removed
}

func int32InRange(idx int32, slice []Vec3) bool {
	return int(idx) >= 0 && int(idx) < len(slice)
}

// isChamferTriangle reports whether the world-space triangle (a, b, c)
// matches one of the chamfer patterns in make_checks.pl is_chamfer/3
// (lines 5641-5665).
//
// All cases are: two vertices sit on a tile boundary (X = ±5 or Y = ±5);
// the third vertex sits just outside the boundary (offset 0.01 .. 0.05).
// Patterns 5641-5652 ("axis-aligned") have both boundary vertices on the
// same tile line; patterns 5654-5665 ("corner-aligned") allow the boundary
// vertex pair to span the corner.
func isChamferTriangle(a, b, c Vec3) bool {
	for _, perm := range [3][3]int{{0, 1, 2}, {1, 2, 0}, {2, 0, 1}} {
		v := [3]Vec3{a, b, c}
		x1, x2, x3 := v[perm[0]].X, v[perm[1]].X, v[perm[2]].X
		y1, y2, y3 := v[perm[0]].Y, v[perm[1]].Y, v[perm[2]].Y

		if isChamferAxisAligned(x1, x2, x3, -5) {
			return true
		}
		if isChamferAxisAligned(x1, x2, x3, 5) {
			return true
		}
		if isChamferAxisAligned(y1, y2, y3, -5) {
			return true
		}
		if isChamferAxisAligned(y1, y2, y3, 5) {
			return true
		}
		if isChamferCornerAligned(x1, x2, x3, -5) {
			return true
		}
		if isChamferCornerAligned(x1, x2, x3, 5) {
			return true
		}
		if isChamferCornerAligned(y1, y2, y3, -5) {
			return true
		}
		if isChamferCornerAligned(y1, y2, y3, 5) {
			return true
		}
	}
	return false
}

// isChamferAxisAligned matches the patterns 5641-5652: two vertices on the
// boundary and the third one offset.
func isChamferAxisAligned(c1, c2, c3, boundary float32) bool {
	if !floatEqual(c1, boundary) || !floatEqual(c2, boundary) {
		return false
	}
	return offsetMatches(c3, boundary)
}

// isChamferCornerAligned matches 5654-5665: one vertex on the boundary, the
// other two sharing a coordinate, the shared one offset.
func isChamferCornerAligned(c1, c2, c3, boundary float32) bool {
	if !floatEqual(c1, boundary) {
		return false
	}
	if !floatEqual(c2, c3) {
		return false
	}
	return offsetMatches(c2, boundary)
}

// offsetMatches reports whether c is in (boundary-0.05, boundary-0.01) for
// negative boundary, or (boundary+0.01, boundary+0.05) for positive
// boundary. Matches the inequality constraints in is_chamfer/3.
func offsetMatches(c, boundary float32) bool {
	if boundary < 0 {
		return c < boundary-0.01 && c > boundary-0.05
	}
	return c > boundary+0.01 && c < boundary+0.05
}

func floatEqual(a, b float32) bool {
	return math.Abs(float64(a-b)) < 1e-5
}

// dropUnusedVerticesAndTVerts is the post-pass after deleting chamfer faces.
// Mirrors make_checks.pl delete_unused_vertices/4 + delete_unused_tverts/4.
//
// Compacts every per-vertex companion array via compactVertexAttrs, then
// every populated TVerts channel (0/1/2/3) along with its per-face TexIndices
// references.
func dropUnusedVerticesAndTVerts(n *Node) {
	mesh := n.Mesh

	usedV := make([]bool, len(mesh.Verts))
	usedT0 := make([]bool, len(mesh.TVerts))
	usedT1 := make([]bool, len(mesh.TVerts1))
	usedT2 := make([]bool, len(mesh.TVerts2))
	usedT3 := make([]bool, len(mesh.TVerts3))
	for fi, f := range mesh.Faces {
		for k := 0; k < 3; k++ {
			if int(f.Verts[k]) >= 0 && int(f.Verts[k]) < len(usedV) {
				usedV[f.Verts[k]] = true
			}
			if int(f.UVs[k]) >= 0 && int(f.UVs[k]) < len(usedT0) {
				usedT0[f.UVs[k]] = true
			}
			// TexIndices0 takes precedence over Face.UVs at compile time
			// (compiler_mesh.go:404-405); without marking + remapping these
			// here, a chamfer-delete pass would silently break stage-0 UVs on
			// any mesh that uses the per-face TexIndices0 channel.
			markTexUsed(mesh.TexIndices0, fi, k, usedT0)
			markTexUsed(mesh.TexIndices1, fi, k, usedT1)
			markTexUsed(mesh.TexIndices2, fi, k, usedT2)
			markTexUsed(mesh.TexIndices3, fi, k, usedT3)
		}
	}

	attrs := vertexAttrsOf(n)
	if remap := remapForKept(usedV); remap != nil {
		compactVertexAttrs(n, attrs, usedV)
		remapFaceVerts(mesh.Faces, remap)
	}

	if remap := remapForKept(usedT0); remap != nil {
		mesh.TVerts = filterByMask(mesh.TVerts, usedT0)
		remapFaceUVs(mesh.Faces, remap)
		remapTexIndices(mesh.TexIndices0, remap)
	}
	if remap := remapForKept(usedT1); remap != nil {
		mesh.TVerts1 = filterByMask(mesh.TVerts1, usedT1)
		remapTexIndices(mesh.TexIndices1, remap)
	}
	if remap := remapForKept(usedT2); remap != nil {
		mesh.TVerts2 = filterByMask(mesh.TVerts2, usedT2)
		remapTexIndices(mesh.TexIndices2, remap)
	}
	if remap := remapForKept(usedT3); remap != nil {
		mesh.TVerts3 = filterByMask(mesh.TVerts3, usedT3)
		remapTexIndices(mesh.TexIndices3, remap)
	}
}

// markTexUsed flags the UV index used by face fi corner k in the given
// per-face tex-index slice (TexIndices1/2/3) as referenced.
func markTexUsed(idxs [][3]int32, fi, k int, used []bool) {
	if fi < 0 || fi >= len(idxs) {
		return
	}
	v := idxs[fi][k]
	if int(v) >= 0 && int(v) < len(used) {
		used[v] = true
	}
}
