package mdl

import (
	"math"
	"sort"
	"strings"
)

// Merged meshes. Some tools compile a model with the meshes of one texture
// drawn as one: the receiving mesh's render index list (MDL-BIN-049) holds its
// own faces' triangles and copies of the other meshes' (the donors), whose
// vertices it appends to its own; each donor keeps its faces but gets render 0
// and an empty index list. The game draws the index lists, so the model looks
// whole. ASCII has only faces, so a decompile that reads them alone loses the
// copies and writes the donors invisible (Layonara's tin01 tiles lost their
// lower walls that way after a repair, nwn-module#476).
//
// The decompiler puts the merge back: a render-0 donor with no index list of
// its own, the same texture as a receiver, and every triangle among the
// receiver's triangles beyond its own faces, is written with render 1. That
// draws the same thing as separate meshes. Triangles are matched by their
// corners' texture coordinates, copied exactly, and their side lengths
// (within 1 mm), so neither node's placement matters.

// RenderMerge records a mesh whose render index list draws more than its
// faces: the donors put back, and the triangles none of them accounts for.
type RenderMerge struct {
	Receiver string
	Donors   []string
	Lost     int
}

// mergeTri is a triangle as unmergeRenderLists compares them: its corners'
// texture coordinates, sorted (the key, copied exactly into the receiver),
// and its side lengths, sorted (compared within mergeTolerance: moving the
// donor into the receiver's frame changes them by about 0.1 mm).
type mergeTri struct {
	uv   [3][2]float32
	side [3]float64
	mesh *MeshData // where the triangle's corners are, for donorNormals
	vi   [3]int    // its vertex indices in mesh, in the order given
}

const mergeTolerance = 1e-3 // m

// meshTri returns the triangle at vertex indices a, b and c of mesh, or
// false when one is out of range. A vertex with no texture coordinates (an
// untextured mesh) has (0, 0), so such triangles are matched by shape alone.
func meshTri(mesh *MeshData, a, b, c int) (mergeTri, bool) {
	idx := [3]int{a, b, c}
	for _, i := range idx {
		if i < 0 || i >= len(mesh.Verts) {
			return mergeTri{}, false
		}
	}
	t := mergeTri{mesh: mesh, vi: idx}
	for j, i := range idx {
		if i < len(mesh.TVerts) {
			t.uv[j] = [2]float32{mesh.TVerts[i].X, mesh.TVerts[i].Y}
		}
	}
	sort.Slice(t.uv[:], func(x, y int) bool {
		if t.uv[x][0] != t.uv[y][0] {
			return t.uv[x][0] < t.uv[y][0]
		}
		return t.uv[x][1] < t.uv[y][1]
	})
	for j := 0; j < 3; j++ {
		p, q := mesh.Verts[idx[j]], mesh.Verts[idx[(j+1)%3]]
		dx, dy, dz := float64(p.X)-float64(q.X), float64(p.Y)-float64(q.Y), float64(p.Z)-float64(q.Z)
		t.side[j] = math.Sqrt(dx*dx + dy*dy + dz*dz)
	}
	sort.Float64s(t.side[:])
	return t, true
}

// triPool holds triangles by their texture coordinates, so each can be taken
// once by a triangle of the same shape.
type triPool struct {
	byUV map[[3][2]float32][]mergeTri
	n    int
}

func (p *triPool) add(t mergeTri) {
	if p.byUV == nil {
		p.byUV = map[[3][2]float32][]mergeTri{}
	}
	p.byUV[t.uv] = append(p.byUV[t.uv], t)
	p.n++
}

// find returns the index in its bucket of a triangle the shape of t, or -1.
func (p *triPool) find(t mergeTri) int {
	for i, c := range p.byUV[t.uv] {
		if math.Abs(c.side[0]-t.side[0]) <= mergeTolerance && math.Abs(c.side[1]-t.side[1]) <= mergeTolerance &&
			math.Abs(c.side[2]-t.side[2]) <= mergeTolerance {
			return i
		}
	}
	return -1
}

func (p *triPool) get(t mergeTri, i int) mergeTri { return p.byUV[t.uv][i] }

func (p *triPool) take(t mergeTri, i int) {
	b := p.byUV[t.uv]
	p.byUV[t.uv] = append(b[:i:i], b[i+1:]...)
	p.n--
}

// unmergeRenderLists finds merged meshes in the model just read and puts the
// donors back (see above), with a note for each receiver, and a warning for
// triangles no donor accounts for.
func (d *decompiler) unmergeRenderLists() {
	if len(d.renderLists) == 0 {
		return
	}
	// Nodes an animation moves (more than one position or orientation key):
	// a donor among them is drawn still in game, as part of its receiver.
	moves := map[string]bool{}
	for _, a := range d.model.Animations {
		for _, an := range a.Nodes {
			if len(an.PositionKeys) > 1 || len(an.OrientationKeys) > 1 {
				moves[strings.ToLower(an.Name)] = true
			}
		}
	}
	used := map[*Node]bool{}
	for _, r := range d.model.Nodes {
		rl := d.renderLists[r]
		if r.Mesh == nil || len(rl) <= 3*len(r.Mesh.Faces) {
			continue
		}
		// A triangle of the index list that cannot be read (a vertex index
		// past the vertex list) counts as lost.
		var extra triPool
		unreadable := 0
		for k := 0; k+2 < len(rl); k += 3 {
			if t, ok := meshTri(r.Mesh, int(rl[k]), int(rl[k+1]), int(rl[k+2])); ok {
				extra.add(t)
			} else {
				unreadable++
			}
		}
		for _, f := range r.Mesh.Faces {
			if t, ok := meshTri(r.Mesh, int(f.Verts[0]), int(f.Verts[1]), int(f.Verts[2])); ok {
				if i := extra.find(t); i >= 0 {
					extra.take(t, i)
				}
			}
		}
		if extra.n == 0 && unreadable == 0 {
			continue
		}
		merge := RenderMerge{Receiver: r.Name}
		for _, n := range d.model.Nodes {
			m := n.Mesh
			if n == r || m == nil || used[n] || m.Render != 0 || len(d.renderLists[n]) > 0 || len(m.Faces) == 0 ||
				!strings.EqualFold(m.Bitmap, r.Mesh.Bitmap) {
				continue
			}
			// Take the donor's triangles from a copy: a donor counts only
			// when all of them are there.
			trial := triPool{byUV: map[[3][2]float32][]mergeTri{}, n: extra.n}
			for k, v := range extra.byUV {
				trial.byUV[k] = append([]mergeTri(nil), v...)
			}
			ok := true
			var pairs [][2]mergeTri // each donor face and the copy it matched
			for _, f := range m.Faces {
				t, good := meshTri(m, int(f.Verts[0]), int(f.Verts[1]), int(f.Verts[2]))
				i := -1
				if good {
					i = trial.find(t)
				}
				if i < 0 {
					ok = false
					break
				}
				pairs = append(pairs, [2]mergeTri{t, trial.get(t, i)})
				trial.take(t, i)
			}
			if !ok {
				continue
			}
			extra = trial
			used[n] = true
			m.Render = 1
			donorNormals(m, pairs)
			merge.Donors = append(merge.Donors, n.Name)
		}
		merge.Lost = extra.n + unreadable
		d.model.RenderMerges = append(d.model.RenderMerges, merge)
		if len(merge.Donors) > 0 {
			d.note(WarnGeneral, 0, r.Name,
				"its render list also draws %s, merged into it by the tool that compiled the model (render 0 and no render list of their own): written with render 1, which draws the same as separate meshes",
				strings.Join(merge.Donors, ", "))
		}
		var moving []string
		for _, n := range merge.Donors {
			if moves[strings.ToLower(n)] {
				moving = append(moving, n)
			}
		}
		if len(moving) > 0 {
			d.note(WarnGeneral, 0, r.Name,
				"this model's animations move %s, which the game drew still, as part of this mesh's render list: with render 1, the animations move it",
				strings.Join(moving, ", "))
		}
		if merge.Lost > 0 {
			d.warn(WarnGeneral, 0, r.Name,
				"%d of the triangles its render list draws are in no mesh's faces: ASCII has no place for them, so a decompile, repair or recompile loses them",
				merge.Lost)
		}
	}
}

// donorNormals gives a donor put back the normals its copies were drawn with.
// The copy is what the game drew, and its normals can differ from the donor's
// own: where a donor's back-to-back faces share a vertex its stored normal is
// (0, 0, 0), while the copy holds the normalised one (MDL-MESH-032 turns the
// zero vector into (1, 0, 0)). The copy sits in the receiver's frame: one
// rotation, taken from the donor's largest triangle, turns it into the
// donor's. Each matched triangle that rotation maps onto its copy (sides within
// mergeTolerance) gives its corners' copy normals; a vertex takes them when
// every such triangle using it agrees (within mergeNormalTolerance) and its own
// normal differs. Otherwise it keeps its own.
func donorNormals(m *MeshData, pairs [][2]mergeTri) {
	if len(m.Normals) < len(m.Verts) {
		return
	}
	type match struct {
		d, c   mergeTri
		r      int
		dp, cp [3][3]float64
	}
	var ms []match
	best, bestArea := -1, 0.0
	for _, p := range pairs {
		d, c := p[0], p[1]
		if len(c.mesh.Normals) < len(c.mesh.Verts) {
			continue
		}
		r, ok := copyCorners(d, c)
		if !ok {
			continue
		}
		mt := match{d: d, c: c, r: r}
		for j := 0; j < 3; j++ {
			mt.dp[j] = vec3d(d.mesh.Verts[d.vi[j]])
			mt.cp[j] = vec3d(c.mesh.Verts[c.vi[(j+r)%3]])
		}
		e1 := [3]float64{mt.dp[1][0] - mt.dp[0][0], mt.dp[1][1] - mt.dp[0][1], mt.dp[1][2] - mt.dp[0][2]}
		e2 := [3]float64{mt.dp[2][0] - mt.dp[0][0], mt.dp[2][1] - mt.dp[0][1], mt.dp[2][2] - mt.dp[0][2]}
		z := cross3(e1, e2)
		if a := math.Sqrt(dot3(z, z)); a > bestArea {
			best, bestArea = len(ms), a
		}
		ms = append(ms, mt)
	}
	if best < 0 {
		return
	}
	fd, okd := triFrame(ms[best].dp)
	fc, okc := triFrame(ms[best].cp)
	if !okd || !okc {
		return
	}
	// toDonor turns a vector in the receiver's frame into the donor's: its
	// components along the copy triangle's axes, put on the donor's.
	toDonor := func(v [3]float64) [3]float64 {
		var n [3]float64
		for a := 0; a < 3; a++ {
			k := fc[a][0]*v[0] + fc[a][1]*v[1] + fc[a][2]*v[2]
			n[0] += k * fd[a][0]
			n[1] += k * fd[a][1]
			n[2] += k * fd[a][2]
		}
		return n
	}
	type cand struct {
		n             [3]float64
		set, conflict bool
	}
	cands := make([]cand, len(m.Verts))
	for _, mt := range ms {
		fits := true
		for j := 0; j < 3 && fits; j++ {
			k := (j + 1) % 3
			ed := [3]float64{mt.dp[k][0] - mt.dp[j][0], mt.dp[k][1] - mt.dp[j][1], mt.dp[k][2] - mt.dp[j][2]}
			ec := toDonor([3]float64{mt.cp[k][0] - mt.cp[j][0], mt.cp[k][1] - mt.cp[j][1], mt.cp[k][2] - mt.cp[j][2]})
			fits = dist3(ed, ec) <= mergeTolerance
		}
		if !fits {
			continue
		}
		for j := 0; j < 3; j++ {
			n := toDonor(vec3d(mt.c.mesh.Normals[mt.c.vi[(j+mt.r)%3]]))
			v := mt.d.vi[j]
			switch {
			case !cands[v].set:
				cands[v] = cand{n: n, set: true}
			case dist3(cands[v].n, n) > mergeNormalTolerance:
				cands[v].conflict = true
			}
		}
	}
	for v, c := range cands {
		if c.set && !c.conflict && dist3(vec3d(m.Normals[v]), c.n) > mergeNormalTolerance {
			m.Normals[v] = vec3f(c.n)
		}
	}
}

const mergeNormalTolerance = 0.01

// copyCorners returns how far the copy's corners are turned from the donor
// triangle's: corner j of d is corner (j+r)%3 of c. The texture coordinates
// must be equal and the sides within mergeTolerance; false when no turn fits,
// or more than one does with different results (a symmetric triangle).
func copyCorners(d, c mergeTri) (int, bool) {
	found := -1
	for r := 0; r < 3; r++ {
		fits := true
		for j := 0; j < 3 && fits; j++ {
			a, b := d.vi[j], c.vi[(j+r)%3]
			if uvAt(d.mesh, a) != uvAt(c.mesh, b) {
				fits = false
				break
			}
			a2, b2 := d.vi[(j+1)%3], c.vi[(j+r+1)%3]
			sd := dist3(vec3d(d.mesh.Verts[a]), vec3d(d.mesh.Verts[a2]))
			sc := dist3(vec3d(c.mesh.Verts[b]), vec3d(c.mesh.Verts[b2]))
			if math.Abs(sd-sc) > mergeTolerance {
				fits = false
			}
		}
		if !fits {
			continue
		}
		if found >= 0 {
			return 0, false
		}
		found = r
	}
	return found, found >= 0
}

func uvAt(m *MeshData, i int) [2]float32 {
	if i < len(m.TVerts) {
		return [2]float32{m.TVerts[i].X, m.TVerts[i].Y}
	}
	return [2]float32{}
}

// triFrame returns an orthonormal frame of the triangle p: the first edge, the
// in-plane perpendicular, and the face normal; false for a degenerate one.
func triFrame(p [3][3]float64) ([3][3]float64, bool) {
	e1 := [3]float64{p[1][0] - p[0][0], p[1][1] - p[0][1], p[1][2] - p[0][2]}
	e2 := [3]float64{p[2][0] - p[0][0], p[2][1] - p[0][1], p[2][2] - p[0][2]}
	z := cross3(e1, e2)
	l1, lz := math.Sqrt(dot3(e1, e1)), math.Sqrt(dot3(z, z))
	if l1 < 1e-9 || lz < 1e-12 {
		return [3][3]float64{}, false
	}
	x := scale3(e1, 1/l1)
	zz := scale3(z, 1/lz)
	return [3][3]float64{x, cross3(zz, x), zz}, true
}

func dist3(a, b [3]float64) float64 {
	dx, dy, dz := a[0]-b[0], a[1]-b[1], a[2]-b[2]
	return math.Sqrt(dx*dx + dy*dy + dz*dz)
}

func cross3(a, b [3]float64) [3]float64 {
	return [3]float64{a[1]*b[2] - a[2]*b[1], a[2]*b[0] - a[0]*b[2], a[0]*b[1] - a[1]*b[0]}
}
