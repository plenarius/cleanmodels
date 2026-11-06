package mdl

import (
	"encoding/binary"
	"fmt"
)

// Shadow volumes and stored adjacency (nwn-mdl-spec E68, MDL-MESH-022).
//
// The game builds a trimesh's shadow silhouette from its face records alone:
// a face lit by the light contributes each edge whose stored neighbour faces
// away, and a missing neighbour (-1) counts as facing away. Each face stores
// one neighbour per edge, the lowest-numbered other face that shared the edge
// when the model was compiled, so at an edge shared by three or more faces
// some lit/unlit pairs never name each other and the edge drops out of the
// silhouette: the shadow tears. Setting those faces' neighbours across that
// edge to -1 makes the game emit the edge for every lit face, which is what
// SplitMultipleEdges achieves for an ASCII model.

// ShadowEdgeEntry is one face's stored neighbour slot: face Face, edge Edge
// (corner Edge to corner Edge+1).
type ShadowEdgeEntry struct {
	Face, Edge int
}

// StoredMultiEdges finds the edges of a binary mesh where three or more faces
// are linked by their stored neighbours, the edges the game's shadow volume can
// miss. It returns the neighbour slots across those edges that are not already
// -1, and the number of such edges. ok is false when the mesh has no stored
// adjacency (an ASCII model, or a binary whose faces a repair changed).
//
// Edges are matched by vertex position, as the compiler that wrote the file
// shared them. Zero-length edges and zero-area faces are left out: a face with
// two corners at one position has the same edge twice and would join unrelated
// pairs.
func StoredMultiEdges(m *MeshData) (entries []ShadowEdgeEntry, edges int, ok bool) {
	if m == nil || len(m.StoredAdjacency) == 0 || len(m.StoredAdjacency) != len(m.Faces) {
		return nil, 0, false
	}
	nv := int32(len(m.Verts))
	pos := func(i int32) Vec3 {
		if i < 0 || i >= nv {
			return Vec3{}
		}
		return m.Verts[i]
	}
	type posEdge struct{ a, b Vec3 }
	less := func(a, b Vec3) bool {
		if a.X != b.X {
			return a.X < b.X
		}
		if a.Y != b.Y {
			return a.Y < b.Y
		}
		return a.Z < b.Z
	}
	edgeOf := func(f Face, k int) (posEdge, bool) {
		a, b := pos(f.Verts[k]), pos(f.Verts[(k+1)%3])
		if a == b {
			return posEdge{}, false
		}
		if less(b, a) {
			a, b = b, a
		}
		return posEdge{a, b}, true
	}
	flat := func(f Face) bool {
		a, b, c := pos(f.Verts[0]), pos(f.Verts[1]), pos(f.Verts[2])
		return a == b || b == c || a == c
	}

	// Union faces linked by a stored neighbour along the same edge.
	type node struct {
		e    posEdge
		face int
	}
	parent := map[node]node{}
	var find func(x node) node
	find = func(x node) node {
		p, seen := parent[x]
		if !seen {
			parent[x] = x
			return x
		}
		if p == x {
			return x
		}
		r := find(p)
		parent[x] = r
		return r
	}
	for fi, f := range m.Faces {
		if flat(f) {
			continue
		}
		for k := 0; k < 3; k++ {
			j := int(m.StoredAdjacency[fi][k])
			if j < 0 || j >= len(m.Faces) || flat(m.Faces[j]) {
				continue
			}
			e, real := edgeOf(f, k)
			if !real {
				continue
			}
			ra, rb := find(node{e, fi}), find(node{e, j})
			if ra != rb {
				parent[ra] = rb
			}
		}
	}
	size := map[node]int{}
	for n := range parent {
		size[find(n)]++
	}
	for _, s := range size {
		if s > 2 {
			edges++
		}
	}
	if edges == 0 {
		return nil, 0, true
	}
	for fi, f := range m.Faces {
		if flat(f) {
			continue
		}
		for k := 0; k < 3; k++ {
			e, real := edgeOf(f, k)
			if !real {
				continue
			}
			n := node{e, fi}
			if _, in := parent[n]; !in || size[find(n)] <= 2 {
				continue
			}
			if m.StoredAdjacency[fi][k] != -1 {
				entries = append(entries, ShadowEdgeEntry{fi, k})
			}
		}
	}
	return entries, edges, true
}

// OpenMultiEdgesBinary returns a copy of a binary model's bytes with the
// stored neighbours across every shadow-casting mesh's multiple edges
// (StoredMultiEdges) set to -1. model must be the decompile of data, unchanged.
// Nothing else in the file changes. The messages say what was opened per node.
func OpenMultiEdgesBinary(data []byte, model *Model) ([]byte, []string, error) {
	if model == nil || model.FileType != "binary" {
		return nil, nil, fmt.Errorf("opening multiple edges needs a binary model (for ASCII use --split-multiedge)")
	}
	out := append([]byte(nil), data...)
	var msgs []string
	for _, n := range model.Nodes {
		if n == nil || !n.IsShadowCaster() {
			continue
		}
		entries, edges, ok := StoredMultiEdges(n.Mesh)
		if !ok || len(entries) == 0 {
			continue
		}
		for _, e := range entries {
			off := n.Mesh.FacesOffset + int64(e.Face)*32 + 20 + int64(e.Edge)*2
			if off < 0 || off+2 > int64(len(out)) {
				return nil, nil, fmt.Errorf("node %q: face %d is past the end of the file", n.Name, e.Face)
			}
			binary.LittleEndian.PutUint16(out[off:], 0xFFFF)
			n.Mesh.StoredAdjacency[e.Face][e.Edge] = -1
		}
		msgs = append(msgs, fmt.Sprintf("opened %d multiple edge(s) on shadow node %q (%d stored neighbour(s) set to none) to fix shadow tearing", edges, n.Name, len(entries)))
	}
	return out, msgs, nil
}

// ClearStoredAdjacency forgets every mesh's stored adjacency, for when the
// faces no longer match the file they were read from.
func (m *Model) ClearStoredAdjacency() {
	for _, n := range m.Nodes {
		if n != nil && n.Mesh != nil {
			n.Mesh.StoredAdjacency = nil
		}
	}
}
