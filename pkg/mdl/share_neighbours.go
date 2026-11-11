package mdl

// Stored neighbours of a mesh whose compile dropped degenerate faces.
//
// A compile drops a degenerate face (nwn-mdl-spec MDL-MESH-020) but writes
// face record k with the neighbours of source face k, numbered as source faces
// (MDL-MESH-021, MDL-MESH-022): the game's compiler does, and cleanmodels does
// as the game does. From the first dropped face on, a record's neighbours
// belong to a later face than the record's own, and name faces by their
// source number, which is one more than their binary number for each face
// dropped before them. Read as binary face numbers they name faces that do
// not share the edge, so sharePositions would join nothing.
//
// The dropped faces are not in the binary, but the neighbours tell where they
// were. Source face s is binary face s - c(s), where c(s) counts the faces
// dropped before s, and its record is record s; a record whose source face was
// dropped holds that face's neighbours, which have no binary face. So for the
// right c, record r's owner r - c(r) shares each linked edge, by position,
// with face j - c(j) for each stored neighbour j. storedNeighbours finds the c
// that makes the most links share their edge (a dynamic programme over the
// records, c rising by one at each dropped record) and renumbers the
// neighbours with it. The compile searched only the first n source faces for
// neighbours (n binary faces), and the file holds n records, so the last
// source faces' links are lost: their faces have none (-1).
//
// The renumbering is only taken when every renumbered link shares its edge
// and more links do than as stored; otherwise the stored neighbours are
// returned as they are.

// maxDroppedFaces is how many dropped faces storedNeighbours looks for.
const maxDroppedFaces = 64

// posEdge is an edge as its two end positions, the lesser first; ok is false
// for an edge of zero length or with a NaN coordinate, which joins nothing.
type posEdge struct {
	a, b Vec3
	ok   bool
}

func vec3Less(a, b Vec3) bool {
	if a.X != b.X {
		return a.X < b.X
	}
	if a.Y != b.Y {
		return a.Y < b.Y
	}
	return a.Z < b.Z
}

func edgeAt(m *MeshData, f Face, k int) posEdge {
	nv := int32(len(m.Verts))
	i, j := f.Verts[k], f.Verts[(k+1)%3]
	if i < 0 || i >= nv || j < 0 || j >= nv {
		return posEdge{}
	}
	a, b := m.Verts[i], m.Verts[j]
	for _, v := range []float32{a.X, a.Y, a.Z, b.X, b.Y, b.Z} {
		if v != v {
			return posEdge{}
		}
	}
	if a == b {
		return posEdge{}
	}
	if vec3Less(b, a) {
		a, b = b, a
	}
	if a.X == 0 {
		a.X = 0 // -0 is +0, as == compares them
	}
	if a.Y == 0 {
		a.Y = 0
	}
	if a.Z == 0 {
		a.Z = 0
	}
	if b.X == 0 {
		b.X = 0
	}
	if b.Y == 0 {
		b.Y = 0
	}
	if b.Z == 0 {
		b.Z = 0
	}
	return posEdge{a, b, true}
}

// storedNeighbours returns each face's stored neighbours in binary face
// numbering (-1 for none), as described above.
func storedNeighbours(m *MeshData) [][3]int32 {
	nf := len(m.Faces)
	adj := make([][3]int32, nf)
	for i, a := range m.StoredAdjacency {
		for k := 0; k < 3; k++ {
			adj[i][k] = int32(a[k])
		}
	}
	edges := make([][3]posEdge, nf)
	byEdge := make(map[posEdge][]int32)
	for i, f := range m.Faces {
		for k := 0; k < 3; k++ {
			e := edgeAt(m, f, k)
			edges[i][k] = e
			if e.ok {
				byEdge[e] = append(byEdge[e], int32(i))
			}
		}
	}
	// shares reports whether face n is another face than o with o's edge k.
	shares := func(o int32, k int, n int32) bool {
		if n < 0 || int(n) >= nf || n == o {
			return false
		}
		e := edges[o][k]
		return e == edges[n][0] || e == edges[n][1] || e == edges[n][2]
	}
	// count tells how many links of l share their edge and how many don't.
	count := func(l [][3]int32) (valid, invalid int) {
		for o := range l {
			for k := 0; k < 3; k++ {
				j := l[o][k]
				switch {
				case j < 0 || j == int32(o) || !edges[o][k].ok:
				case shares(int32(o), k, j):
					valid++
				default:
					invalid++
				}
			}
		}
		return
	}
	valid, invalid := count(adj)
	if invalid == 0 {
		return adj
	}

	maxC := min(maxDroppedFaces, nf)
	// linked reports whether record r's link k, read with c faces dropped
	// before r, can share its edge with some face j - c(j) for a c(j) the
	// order allows (c(j) >= c after r, <= c before it).
	linked := func(r, c, k int) int32 {
		j := adj[r][k]
		o := int32(r - c)
		if j < 0 || int(j) == r || int(j) >= nf || !edges[o][k].ok {
			return 0
		}
		for _, n := range byEdge[edges[o][k]] {
			s := int(j - n)
			if n == o || s < 0 || s > maxC || (int(j) > r && s < c) || (int(j) < r && s > c) {
				continue
			}
			return 2
		}
		return -2
	}
	const none = -1 << 30
	score := make([]int32, maxC+1)
	next := make([]int32, maxC+1)
	for c := range score {
		score[c] = none
	}
	score[0] = 0
	// dropped[r*(maxC+1)+c]: the best way to c faces dropped before r+1
	// drops record r.
	dropped := make([]bool, nf*(maxC+1))
	for r := 0; r < nf; r++ {
		for c := 0; c <= maxC; c++ {
			next[c] = none
			if c <= r && score[c] != none {
				s := score[c]
				for k := 0; k < 3; k++ {
					s += linked(r, c, k)
				}
				next[c] = s
			}
			if c > 0 && score[c-1] != none && score[c-1]-1 > next[c] {
				next[c] = score[c-1] - 1
				dropped[r*(maxC+1)+c] = true
			}
		}
		score, next = next, score
	}
	end := 0
	for c := 1; c <= maxC; c++ {
		if score[c] > score[end] {
			end = c
		}
	}
	if end == 0 {
		return adj
	}
	shift := make([]int32, nf) // c(r)
	gone := make([]bool, nf)
	c := end
	for r := nf - 1; r >= 0; r-- {
		if dropped[r*(maxC+1)+c] {
			gone[r] = true
			c--
		}
		shift[r] = int32(c)
	}

	out := make([][3]int32, nf)
	for i := range out {
		out[i] = [3]int32{-1, -1, -1}
	}
	for r := 0; r < nf; r++ {
		if gone[r] {
			continue
		}
		o := r - int(shift[r])
		for k := 0; k < 3; k++ {
			if j := adj[r][k]; j >= 0 && int(j) < nf && !gone[j] {
				out[o][k] = j - shift[j]
			}
		}
	}
	if v, inv := count(out); inv > 0 || v <= valid {
		return adj
	}
	return out
}
