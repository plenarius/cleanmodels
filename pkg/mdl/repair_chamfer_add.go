package mdl

import (
	"fmt"
	"math"
)

// AddChamfers walks every mesh-bearing node, finds open boundary edges that
// sit on a tile boundary (X = ±5 or Y = ±5), and synthesises two chamfer
// triangles per edge that bevel the tile seam outward and downward.
//
// Mirrors make_checks.pl add_chamfer/4 (line 5609), chamfer_edge/6 (5667),
// add_chamfer/6 (5685), add_chamfer_vertex/6 (5693) and add_chamfer_face/7
// (5718). Returns one message per affected node.
func AddChamfers(model *Model) []string {
	var msgs []string
	idx := nodeIndex(model)
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil || len(n.Mesh.Faces) == 0 {
			continue
		}
		added := addChamferOnNode(idx, n)
		if added > 0 {
			msgs = append(msgs, fmt.Sprintf("applied Rosenkrantz chamfer to %d edge(s) in %q", added, n.Name))
		}
	}
	return msgs
}

func addChamferOnNode(idx map[string]*Node, n *Node) int {
	mesh := n.Mesh
	worldVerts := WorldVerticesCached(idx, n)
	if len(worldVerts) != len(mesh.Verts) {
		return 0
	}

	dirEdges := BuildDirectedEdgeMap(mesh.Faces)
	candidates := findChamferEdges(dirEdges, worldVerts)
	if len(candidates) == 0 {
		return 0
	}

	ensureChamferUVSeed(mesh)

	added := 0
	for _, e := range candidates {
		w1 := worldVerts[e.V0]
		w2 := worldVerts[e.V1]

		v3 := materialiseChamferVertex(idx, n, w1, &worldVerts)
		v4 := materialiseChamferVertex(idx, n, w2, &worldVerts)
		if v3 < 0 || v4 < 0 {
			continue
		}

		appendChamferFace(mesh, e.V0, v3, e.V1)
		appendChamferFace(mesh, v3, v4, e.V1)
		added += 2
	}
	return added / 2
}

type chamferCandidate struct{ V0, V1 int32 }

// findChamferEdges enumerates open boundary half-edges (V0 -> V1) where
// both endpoints sit on the same tile boundary (X = ±5 or Y = ±5) and the
// sole face containing the half-edge has a roughly upward face normal whose
// horizontal component perpendicular to the boundary is < 0.5 in
// magnitude. Mirrors chamfer_edge/6 (lines 5667-5683).
func findChamferEdges(dirEdges map[DirectedEdge][]DirectedEdgeFace, world []Vec3) []chamferCandidate {
	var out []chamferCandidate
	for de, faces := range dirEdges {
		if _, twin := dirEdges[DirectedEdge{de.V1, de.V0}]; twin {
			continue
		}
		if len(faces) == 0 {
			continue
		}
		v1 := de.V0
		v2 := de.V1
		if v1 < 0 || v2 < 0 || int(v1) >= len(world) || int(v2) >= len(world) {
			continue
		}
		w1 := world[v1]
		w2 := world[v2]

		boundary := boundaryOf(w1, w2)
		if boundary == boundaryNone {
			continue
		}

		v3 := faces[0].V3
		if int(v3) >= len(world) {
			continue
		}
		w3 := world[v3]

		nrm := faceNormal(w1, w2, w3)
		switch boundary {
		case boundaryX:
			if math.Abs(float64(nrm.X)) >= 0.5 || nrm.Z <= 0.866 {
				continue
			}
		case boundaryY:
			if math.Abs(float64(nrm.Y)) >= 0.5 || nrm.Z <= 0.866 {
				continue
			}
		}
		out = append(out, chamferCandidate{V0: v1, V1: v2})
	}
	return out
}

const (
	boundaryNone = iota
	boundaryX
	boundaryY
)

// boundaryOf returns boundaryX if both vertices share the same |X| = 5
// boundary, boundaryY for the same on Y, otherwise boundaryNone.
func boundaryOf(a, b Vec3) int {
	if floatEqual(a.X, b.X) && (floatEqual(a.X, 5) || floatEqual(a.X, -5)) {
		return boundaryX
	}
	if floatEqual(a.Y, b.Y) && (floatEqual(a.Y, 5) || floatEqual(a.Y, -5)) {
		return boundaryY
	}
	return boundaryNone
}

func faceNormal(a, b, c Vec3) Vec3 {
	return vecNormalize(vecCross(vecSub(b, a), vecSub(c, a)))
}

// ensureChamferUVSeed makes sure the mesh has at least three TVerts so the
// chamfer faces' fixed UV indices [0, 1, 2] resolve. The first three TVerts
// are not mutated if they already exist.
func ensureChamferUVSeed(mesh *MeshData) {
	for len(mesh.TVerts) < 3 {
		mesh.TVerts = append(mesh.TVerts, Vec3{})
	}
}

// materialiseChamferVertex creates a chamfer vertex offset from the
// world-space anchor (X±0.03 if on X-boundary, Y±0.03 if on Y-boundary,
// always Z-0.03), translates it back to local space, and either reuses an
// existing matching vertex or appends a new one. Returns the local vertex
// index, or -1 if anything went wrong.
func materialiseChamferVertex(idx map[string]*Node, n *Node, anchorWorld Vec3, worldOut *[]Vec3) int32 {
	w := anchorWorld
	if floatEqual(w.X, -5) {
		w.X -= chamferOffset
	} else if floatEqual(w.X, 5) {
		w.X += chamferOffset
	}
	if floatEqual(w.Y, -5) {
		w.Y -= chamferOffset
	} else if floatEqual(w.Y, 5) {
		w.Y += chamferOffset
	}
	w.Z -= chamferOffset

	local := WorldToLocal(idx, n, w)

	for i, v := range n.Mesh.Verts {
		if approxEq(v, local, 1e-5) {
			return int32(i)
		}
	}

	mesh := n.Mesh
	attrs := vertexAttrsOf(n)
	idxNew := int32(len(mesh.Verts))
	mesh.Verts = append(mesh.Verts, local)
	appendDefaultVertexAttrs(n, attrs)
	*worldOut = append(*worldOut, w)
	return idxNew
}

// appendChamferFace appends a chamfer triangle with the canonical
// SmoothGroup = 1048576, UVs = (0, 1, 2), Material = 21. Mirrors
// add_chamfer_face/7 line 5720.
func appendChamferFace(mesh *MeshData, v1, v2, v3 int32) {
	mesh.Faces = append(mesh.Faces, Face{
		Verts:       [3]int32{v1, v2, v3},
		SmoothGroup: chamferSmoothGroup,
		UVs:         [3]int32{0, 1, 2},
		Material:    chamferMaterial,
	})
}
