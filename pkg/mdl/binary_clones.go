package mdl

// The game draws a mesh's index list, not its face records (nwn-mdl-spec
// MDL-BIN-049). They differ after tangent generation: a vertex reached by
// triangles of both handedness is cloned, the clone appended to the vertex
// list, and only the index list is rewritten to use it (MDL-MESH-043). Built
// from the face records, those triangles would use the original vertex, with
// the other handedness and tangent, and the clones would be left unused.

// applyTangentClones points each face at the vertices its triangle of the
// index list uses, when they are the face's own vertices or clones of them
// (the same position and UV). A face whose triangle in the list is another
// one (a list not in face order) is left as read.
func (d *decompiler) applyTangentClones() {
	for _, n := range d.model.Nodes {
		m := n.Mesh
		rl := d.renderLists[n]
		if m == nil || len(rl) == 0 {
			continue
		}
		for i := range m.Faces {
			if 3*i+2 >= len(rl) {
				break
			}
			f := &m.Faces[i]
			want := [3]int32{int32(rl[3*i]), int32(rl[3*i+1]), int32(rl[3*i+2])}
			if want == f.Verts {
				continue
			}
			ok := true
			for k := 0; k < 3 && ok; k++ {
				ok = sameCorner(m, int(f.Verts[k]), int(want[k]))
			}
			if !ok {
				continue
			}
			f.Verts = want
			f.UVs = want
		}
	}
}

// sameCorner reports whether vertices a and b of m are one vertex or a
// tangent clone of it: the same position and, when the mesh has UVs, the
// same UV.
func sameCorner(m *MeshData, a, b int) bool {
	if a == b {
		return true
	}
	if a < 0 || b < 0 || a >= len(m.Verts) || b >= len(m.Verts) || m.Verts[a] != m.Verts[b] {
		return false
	}
	if len(m.TVerts) > 0 {
		if a >= len(m.TVerts) || b >= len(m.TVerts) || m.TVerts[a] != m.TVerts[b] {
			return false
		}
	}
	return true
}
