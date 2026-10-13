package mdl

// DirectedEdge identifies a directed edge in a triangle mesh: an oriented
// vertex pair as it appears in the winding order of some face.
type DirectedEdge struct{ V0, V1 int32 }

// DirectedEdgeFace records the third vertex of the triangle that contains a
// given directed edge, plus the face index.
type DirectedEdgeFace struct {
	V3   int32
	Face int
}

// BuildDirectedEdgeMap returns a map from each directed edge V0->V1 to the
// list of (V3, faceIndex) pairs whose triangle contains that directed edge in
// its winding order. Each face contributes three directed edges.
//
// Mirrors make_checks.pl clockwise_edge/6 (line 5145).
func BuildDirectedEdgeMap(faces []Face) map[DirectedEdge][]DirectedEdgeFace {
	m := make(map[DirectedEdge][]DirectedEdgeFace, len(faces)*3)
	for fi, f := range faces {
		v := f.Verts
		m[DirectedEdge{v[0], v[1]}] = append(m[DirectedEdge{v[0], v[1]}], DirectedEdgeFace{V3: v[2], Face: fi})
		m[DirectedEdge{v[1], v[2]}] = append(m[DirectedEdge{v[1], v[2]}], DirectedEdgeFace{V3: v[0], Face: fi})
		m[DirectedEdge{v[2], v[0]}] = append(m[DirectedEdge{v[2], v[0]}], DirectedEdgeFace{V3: v[1], Face: fi})
	}
	return m
}

// IsExteriorDirectedEdge reports whether V0->V1 has no opposite half-edge
// V1->V0 in the directed edge map. A half-edge with no twin is the boundary
// of an "open" mesh region. Mirrors exterior_edge/6 in make_checks.pl line
// 5132.
func IsExteriorDirectedEdge(m map[DirectedEdge][]DirectedEdgeFace, v0, v1 int32) bool {
	if _, fwd := m[DirectedEdge{v0, v1}]; !fwd {
		return false
	}
	_, back := m[DirectedEdge{v1, v0}]
	return !back
}
