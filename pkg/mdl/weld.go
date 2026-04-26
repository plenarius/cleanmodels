package mdl

import "math"

// WeldOptions controls vertex-welding behaviour.
type WeldOptions struct {
	// Eps is the absolute tolerance used to consider two coordinates equal.
	// 0 means bit-exact comparison (matches the legacy term_hash hash bucket
	// in make_checks.pl hash_verts/4).
	Eps float32

	// DropUnused removes vertices that participate in no faces.
	DropUnused bool

	// PreserveOpposite skips merging two vertices when their averaged face
	// normals point in nearly opposite directions (dot < -0.9). Mirrors the
	// safety check on weld_vertices/7. When false, all coincident vertices
	// are merged regardless of normal divergence.
	PreserveOpposite bool
}

// WeldVertices coalesces coincident vertices in a mesh, updating face vertex
// indices, per-vertex normals, colors, tangents, dangly constraints, and skin
// weights. Returns the number of vertices removed.
//
// For each pair of vertices (V1, V2) with V2 < V1 whose positions are within
// Eps on all axes, V1 is collapsed into V2. If DropUnused is set, vertices
// that no face references after the merge pass are also removed.
//
// Face UV indices and the TVerts arrays are not modified by this function;
// callers that need to weld UVs should run a separate UV-welding pass.
func WeldVertices(n *Node, opts WeldOptions) int {
	if n == nil || n.Mesh == nil || len(n.Mesh.Verts) == 0 {
		return 0
	}
	mesh := n.Mesh

	attrs := vertexAttrsOf(n)

	canon := make([]int32, len(mesh.Verts))
	for i := range canon {
		canon[i] = int32(i)
	}

	var faceNormals []Vec3
	if opts.PreserveOpposite {
		faceNormals = make([]Vec3, len(mesh.Verts))
		count := make([]int, len(mesh.Verts))
		for _, f := range mesh.Faces {
			a, b, c := f.Verts[0], f.Verts[1], f.Verts[2]
			if !(int(a) < len(mesh.Verts) && int(b) < len(mesh.Verts) && int(c) < len(mesh.Verts)) {
				continue
			}
			va, vb, vc := mesh.Verts[a], mesh.Verts[b], mesh.Verts[c]
			nrm := vecNormalize(vecCross(vecSub(vb, va), vecSub(vc, va)))
			for _, vi := range []int32{a, b, c} {
				faceNormals[vi].X += nrm.X
				faceNormals[vi].Y += nrm.Y
				faceNormals[vi].Z += nrm.Z
				count[vi]++
			}
		}
		for i := range faceNormals {
			if count[i] > 0 {
				faceNormals[i] = vecNormalize(faceNormals[i])
			}
		}
	}

	for i := 1; i < len(mesh.Verts); i++ {
		for j := 0; j < i; j++ {
			if canon[j] != int32(j) {
				continue
			}
			if !approxEq(mesh.Verts[i], mesh.Verts[j], opts.Eps) {
				continue
			}
			if opts.PreserveOpposite {
				if vecDot(faceNormals[i], faceNormals[j]) < -0.9 {
					continue
				}
			}
			canon[i] = int32(j)
			break
		}
	}

	for fi := range mesh.Faces {
		f := &mesh.Faces[fi]
		for vi := 0; vi < 3; vi++ {
			old := f.Verts[vi]
			if int(old) >= 0 && int(old) < len(canon) {
				f.Verts[vi] = canon[old]
			}
		}
	}

	used := make([]bool, len(mesh.Verts))
	for _, f := range mesh.Faces {
		if f.Verts[0] == f.Verts[1] || f.Verts[1] == f.Verts[2] || f.Verts[0] == f.Verts[2] {
			continue
		}
		for _, v := range f.Verts {
			if int(v) >= 0 && int(v) < len(used) {
				used[v] = true
			}
		}
	}

	keep := make([]bool, len(mesh.Verts))
	for i := range mesh.Verts {
		if canon[i] != int32(i) {
			continue
		}
		if !opts.DropUnused {
			keep[i] = true
			continue
		}
		keep[i] = used[i]
	}

	remap := remapForKept(keep)
	if remap == nil {
		return 0
	}

	originalCount := len(mesh.Verts)
	compactVertexAttrs(n, attrs, keep)
	remapFaceVerts(mesh.Faces, remap)

	return originalCount - len(mesh.Verts)
}

func approxEq(a, b Vec3, eps float32) bool {
	if eps == 0 {
		return a.X == b.X && a.Y == b.Y && a.Z == b.Z
	}
	return float32(math.Abs(float64(a.X-b.X))) <= eps &&
		float32(math.Abs(float64(a.Y-b.Y))) <= eps &&
		float32(math.Abs(float64(a.Z-b.Z))) <= eps
}
