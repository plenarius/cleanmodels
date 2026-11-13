package mdl

import (
	"fmt"
	"math"
	"sort"
	"strings"
)

// Merged meshes. Some tools compile a model with the meshes of one texture
// drawn as one: the receiving mesh's render index list (MDL-BIN-049) holds its
// own faces' triangles and copies of the other meshes' (the donors), whose
// vertices it appends to its own; each donor keeps its faces but gets render 0
// and an empty index list. The game draws the index lists, never a render-0
// mesh, so the model looks whole. ASCII has only faces, so a decompile that
// reads them alone loses the copies and writes the donors invisible
// (Layonara's tin01 tiles lost their lower walls that way after a repair,
// nwn-module#476).
//
// The decompiler puts the merge back. A donor is a render-0 mesh with no index
// list of its own, the same texture as a receiver, and every triangle among
// the receiver's triangles beyond its own faces. Triangles are matched by
// their corners' texture coordinates, copied exactly, and their side lengths
// (within 1 mm), preferring the copy that lies where the donor's triangle
// does in world space. The output must draw what the game drew, so each donor
// is either:
//
//   - Restored (the usual case): written with render 1, it draws as a separate
//     mesh. It is given what its copies were drawn with, which its own values
//     were not, since the game never drew it: the receiver's drawing settings
//     (copyDrawingState) and the copies' corner normals, tangents and
//     handedness (donorCorners).
//   - Folded: when the donor restored would draw something other than its
//     copies, the copies are written as faces of the receiver, as the game
//     drew them, and the donor keeps render 0. The rule (foldReason): the
//     restored donor must draw where its copies were drawn, in the rest pose
//     and in every animation, and look as they did. It does not when its
//     vertices, placed in world space, miss its copies (a copy baked without
//     the donor's scale), when an animation moves a node on the donor's or the
//     receiver's parent chain below the nodes they share (the copies stay
//     with the receiver, the donor would move on its own), or when an
//     animation changes the alpha or self-illumination of either one away
//     from the receiver's. A folded donor that casts a shadow (from its faces,
//     whatever its render, E68) stops casting one when the receiver casts
//     shadows, as the copies now cast it.
//
// Either way, the receiver's vertices that only the copies used are dropped:
// the compile computes bounds over every vertex (MDL-MESH-050), so unused ones
// would change the model's bounding box.

// RenderMerge records a mesh whose render index list draws more than its
// faces: the donors put back, and the triangles none of them accounts for.
type RenderMerge struct {
	Receiver string
	Donors   []string // put back with render 1
	Folded   []string // left at render 0, their copies written as the receiver's faces
	Lost     int
}

// mergeTri is a triangle as unmergeRenderLists compares them: its corners'
// texture coordinates, sorted (the key, copied exactly into the receiver),
// and its side lengths, sorted (compared within mergeTolerance: moving the
// donor into the receiver's frame changes them by about 0.1 mm).
type mergeTri struct {
	uv   [3][2]float32
	side [3]float64
	mesh *MeshData // where the triangle's corners are, for donorCorners
	vi   [3]int    // its vertex indices in mesh, in the order given
	// k is the triangle's index in the receiver's render list, or the face
	// index of a donor's triangle.
	k int
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
func (p *triPool) find(t mergeTri) int { return p.findPreferring(t, nil) }

// findPreferring is find, taking the first triangle for which pref holds
// when there is one.
func (p *triPool) findPreferring(t mergeTri, pref func(c mergeTri) bool) int {
	first := -1
	for i, c := range p.byUV[t.uv] {
		if math.Abs(c.side[0]-t.side[0]) <= mergeTolerance && math.Abs(c.side[1]-t.side[1]) <= mergeTolerance &&
			math.Abs(c.side[2]-t.side[2]) <= mergeTolerance {
			if pref == nil || pref(c) {
				return i
			}
			if first < 0 {
				first = i
			}
		}
	}
	return first
}

func (p *triPool) get(t mergeTri, i int) mergeTri { return p.byUV[t.uv][i] }

func (p *triPool) take(t mergeTri, i int) {
	b := p.byUV[t.uv]
	p.byUV[t.uv] = append(b[:i:i], b[i+1:]...)
	p.n--
}

// mergeReceiver is a mesh whose render list draws more than its faces, while
// unmergeRenderLists matches donors to it.
type mergeReceiver struct {
	r          *Node
	rl         []uint16
	extra      triPool // the list's triangles no face or donor has taken yet
	unreadable int
	wr         []Vec3 // r's vertices in world space
	donors     []mergeDonor
}

// mergeDonor is a donor matched to a receiver: its faces and their copies.
type mergeDonor struct {
	n     *Node
	at    int // its index in the model's nodes
	pairs [][2]mergeTri
}

// unmergeRenderLists finds merged meshes in the model just read and restores
// or folds the donors (see above), with a note for each receiver, and a
// warning for triangles no donor accounts for.
//
// Donors are matched in two passes over the receivers: first only a donor
// whose every triangle has a copy where it lies in world space, then any by
// shape alone. Two receivers of one texture can each hold copies of a donor
// of the same shape, and a receiver must not take the other's.
func (d *decompiler) unmergeRenderLists() {
	if len(d.renderLists) == 0 {
		return
	}
	var idx map[*Node]*Node
	world := map[*Node][]Vec3{}
	worldOf := func(n *Node) []Vec3 {
		w, ok := world[n]
		if !ok {
			w = WorldVerticesCached(idx, n)
			world[n] = w
		}
		return w
	}
	var recs []*mergeReceiver
	for _, r := range d.model.Nodes {
		rl := d.renderLists[r]
		if r.Mesh == nil || len(rl) <= 3*len(r.Mesh.Faces) {
			continue
		}
		// A triangle of the index list that cannot be read (a vertex index
		// past the vertex list) counts as lost.
		rec := &mergeReceiver{r: r, rl: rl}
		for k := 0; k+2 < len(rl); k += 3 {
			if t, ok := meshTri(r.Mesh, int(rl[k]), int(rl[k+1]), int(rl[k+2])); ok {
				t.k = k / 3
				rec.extra.add(t)
			} else {
				rec.unreadable++
			}
		}
		for _, f := range r.Mesh.Faces {
			if t, ok := meshTri(r.Mesh, int(f.Verts[0]), int(f.Verts[1]), int(f.Verts[2])); ok {
				if i := rec.extra.find(t); i >= 0 {
					rec.extra.take(t, i)
				}
			}
		}
		if rec.extra.n == 0 && rec.unreadable == 0 {
			continue
		}
		if idx == nil {
			// The stored tree: by name, a node under the second of two
			// nodes of one name would take the first one's transform.
			idx = d.storedParent
			if idx == nil {
				idx = nodeIndex(d.model)
			}
		}
		rec.wr = worldOf(r)
		recs = append(recs, rec)
	}
	if len(recs) == 0 {
		return
	}
	used := map[*Node]bool{}
	for _, inPlace := range [2]bool{true, false} {
		for _, rec := range recs {
			r := rec.r
			for at, n := range d.model.Nodes {
				m := n.Mesh
				if n == r || m == nil || used[n] || m.Render != 0 || len(d.renderLists[n]) > 0 || len(m.Faces) == 0 ||
					!strings.EqualFold(m.Bitmap, r.Mesh.Bitmap) {
					continue
				}
				wd := worldOf(n)
				// Take the donor's triangles from a copy: a donor counts only
				// when all of them are there.
				trial := triPool{byUV: map[[3][2]float32][]mergeTri{}, n: rec.extra.n}
				for k, v := range rec.extra.byUV {
					trial.byUV[k] = append([]mergeTri(nil), v...)
				}
				ok := true
				var pairs [][2]mergeTri // each donor face and the copy it matched
				for fi, f := range m.Faces {
					t, good := meshTri(m, int(f.Verts[0]), int(f.Verts[1]), int(f.Verts[2]))
					i := -1
					if good {
						t.k = fi
						i = trial.findPreferring(t, func(c mergeTri) bool { return worldFits(wd, t.vi, rec.wr, c.vi) })
					}
					if i < 0 || inPlace && !worldFits(wd, t.vi, rec.wr, trial.get(t, i).vi) {
						ok = false
						break
					}
					pairs = append(pairs, [2]mergeTri{t, trial.get(t, i)})
					trial.take(t, i)
				}
				if !ok {
					continue
				}
				rec.extra = trial
				used[n] = true
				rec.donors = append(rec.donors, mergeDonor{n: n, at: at, pairs: pairs})
			}
		}
	}
	motion := newMergeMotion(d.model)
	for _, rec := range recs {
		r := rec.r
		sort.SliceStable(rec.donors, func(i, j int) bool { return rec.donors[i].at < rec.donors[j].at })
		merge := RenderMerge{Receiver: r.Name}
		var folded [][2]mergeTri // donor faces folded into r, and their copies
		var foldWhy, shadowOff, restyled []string
		for _, dn := range rec.donors {
			n, m := dn.n, dn.n.Mesh
			if why := foldReason(idx, motion, r, n, rec.wr, worldOf(n), dn.pairs); why != "" {
				folded = append(folded, dn.pairs...)
				merge.Folded = append(merge.Folded, n.Name)
				foldWhy = append(foldWhy, fmt.Sprintf("%s (%s)", n.Name, why))
				if m.Shadow != 0 && r.Mesh.Shadow != 0 {
					m.Shadow = 0
					shadowOff = append(shadowOff, n.Name)
				}
				continue
			}
			m.Render = 1
			if hint := m.TransparencyHint; hint != r.Mesh.TransparencyHint {
				// The stored tree was sorted by the stored hint
				// (declarationOrder plans with it).
				if d.storedHints == nil {
					d.storedHints = map[*Node]int32{}
				}
				d.storedHints[n] = hint
			}
			if diff := copyDrawingState(r, n); len(diff) > 0 {
				restyled = append(restyled, fmt.Sprintf("%s (%s)", n.Name, strings.Join(diff, ", ")))
			}
			donorCorners(n, dn.pairs)
			merge.Donors = append(merge.Donors, n.Name)
		}
		merge.Lost = rec.extra.n + rec.unreadable
		d.model.RenderMerges = append(d.model.RenderMerges, merge)
		foldCopies(r, folded)
		dropCopyVertices(r, rec.rl)
		if len(merge.Donors) > 0 {
			d.note(WarnGeneral, 0, r.Name,
				"its render list also draws %s, merged into it by the tool that compiled the model (render 0 and no render list of their own): written with render 1, which draws the same as separate meshes",
				strings.Join(merge.Donors, ", "))
		}
		if len(restyled) > 0 {
			d.note(WarnGeneral, 0, r.Name,
				"the game drew the triangles of %s with this mesh's drawing settings, which differ from their own: given this mesh's, so they draw as before",
				strings.Join(restyled, "; "))
		}
		if len(foldWhy) > 0 {
			msg := fmt.Sprintf("its render list also draws %s, merged into it by the tool that compiled the model, which as separate meshes would draw differently from the original: their copies are written as this mesh's faces, as the game drew them, and they keep render 0",
				strings.Join(foldWhy, "; "))
			if len(shadowOff) > 0 {
				msg += fmt.Sprintf(". %s get shadow 0, as the copies in this mesh now cast their shadow", strings.Join(shadowOff, ", "))
			}
			d.note(WarnGeneral, 0, r.Name, "%s", msg)
		}
		if merge.Lost > 0 {
			d.warn(WarnGeneral, 0, r.Name,
				"%d of the triangles its render list draws are in no mesh's faces: ASCII has no place for them, so a decompile, repair or recompile loses them",
				merge.Lost)
		}
	}
}

const (
	// mergeWorldTolerance is how far apart (m) a donor's corner and its
	// copy's may be in world space and still draw the same: 1 mm, the
	// distance at which nwn-mdl-spec's round-trip checker pairs triangles
	// (plc_candle3's donor box07 lies 1.3 mm from its copy, which the game
	// drew: restored, it would draw elsewhere).
	mergeWorldTolerance = 1e-3
	// mergeKeyTolerance compares animation keys with the rest pose, and
	// alpha and colour values.
	mergeKeyTolerance = 1e-4
)

// worldFits reports whether the triangle at vertex indices a of the world
// positions wa lies on the one at b of wb, corners in the same turn.
func worldFits(wa []Vec3, a [3]int, wb []Vec3, b [3]int) bool {
	for r := 0; r < 3; r++ {
		fits := true
		for j := 0; j < 3 && fits; j++ {
			fits = dist3(vec3d(wa[a[j]]), vec3d(wb[b[(j+r)%3]])) <= mergeWorldTolerance
		}
		if fits {
			return true
		}
	}
	return false
}

// mergeMotion is what a model's animations do to its nodes, by lower-case
// name: whether one moves it from its rest pose, and every alpha and
// self-illumination key value.
type mergeMotion struct {
	moves     map[string]bool
	alpha     map[string][]float32
	selfIllum map[string][]Vec3
}

func newMergeMotion(m *Model) mergeMotion {
	geom := map[string]*Node{}
	for _, n := range m.Nodes {
		if k := strings.ToLower(n.Name); geom[k] == nil {
			geom[k] = n
		}
	}
	mm := mergeMotion{moves: map[string]bool{}, alpha: map[string][]float32{}, selfIllum: map[string][]Vec3{}}
	for _, a := range m.Animations {
		for i := range a.Nodes {
			an := &a.Nodes[i]
			k := strings.ToLower(an.Name)
			if g := geom[k]; g != nil && animMoves(g, an) {
				mm.moves[k] = true
			}
			for _, key := range an.AlphaKeys {
				mm.alpha[k] = append(mm.alpha[k], key.Value)
			}
			for _, key := range an.SelfIllumColorKeys {
				mm.selfIllum[k] = append(mm.selfIllum[k], key.Value)
			}
		}
	}
	return mm
}

// animMoves reports whether an's keys put node g anywhere but its rest pose.
func animMoves(g *Node, an *AnimNode) bool {
	for _, k := range an.PositionKeys {
		if !nearVec3(k.Value, g.Position) {
			return true
		}
	}
	for _, k := range an.OrientationKeys {
		if !sameRotation(k.Value, g.Orientation) {
			return true
		}
	}
	s := g.Scale
	if s == 0 {
		s = 1
	}
	for _, k := range an.ScaleKeys {
		if !nearF(k.Value, s) {
			return true
		}
	}
	return false
}

func nearF(a, b float32) bool { return math.Abs(float64(a)-float64(b)) <= mergeKeyTolerance }

func nearVec3(a, b Vec3) bool { return nearF(a.X, b.X) && nearF(a.Y, b.Y) && nearF(a.Z, b.Z) }

// sameRotation reports whether two axis-angle orientations turn the X and Y
// axes to the same place.
func sameRotation(a, b Vec4) bool {
	for _, v := range [2]Vec3{{X: 1}, {Y: 1}} {
		if !nearVec3(RotateVectorAxisAngle(v, a), RotateVectorAxisAngle(v, b)) {
			return false
		}
	}
	return true
}

// foldReason says why donor n, restored, would draw something other than its
// copies in receiver r (see the rule above), or "" when it would not. wr and
// wd are their vertices in world space; pairs, n's faces and their copies.
func foldReason(idx map[*Node]*Node, motion mergeMotion, r, n *Node, wr, wd []Vec3, pairs [][2]mergeTri) string {
	rc, dc := parentChain(idx, r), parentChain(idx, n)
	inR, inD := map[*Node]bool{}, map[*Node]bool{}
	for _, x := range rc {
		inR[x] = true
	}
	for _, x := range dc {
		inD[x] = true
	}
	for _, x := range dc {
		if !inR[x] && motion.moves[strings.ToLower(x.Name)] {
			return fmt.Sprintf("an animation moves %s, but the game drew it as part of %s", x.Name, r.Name)
		}
	}
	for _, x := range rc {
		if !inD[x] && motion.moves[strings.ToLower(x.Name)] {
			return fmt.Sprintf("an animation moves %s, with the copies", x.Name)
		}
	}
	for _, x := range [2]*Node{r, n} {
		for _, v := range motion.alpha[strings.ToLower(x.Name)] {
			if !nearF(v, r.Mesh.Alpha) {
				return fmt.Sprintf("an animation changes the alpha of %s", x.Name)
			}
		}
		for _, v := range motion.selfIllum[strings.ToLower(x.Name)] {
			if !nearVec3(v, r.Mesh.SelfIllumColor) {
				return fmt.Sprintf("an animation changes the self-illumination of %s", x.Name)
			}
		}
	}
	for _, p := range pairs {
		if worldFits(wd, p[0].vi, wr, p[1].vi) {
			continue
		}
		for _, x := range dc {
			if !inR[x] && x.Scale != 0 && x.Scale != 1 {
				return fmt.Sprintf("the game drew it without the scale %s of %s", fmtFloat(x.Scale), x.Name)
			}
		}
		return "the game drew it elsewhere than the mesh itself is"
	}
	return ""
}

// copyDrawingState gives donor n receiver r's drawing settings, which the
// game drew n's copies with, and returns the ones that differed.
func copyDrawingState(r, n *Node) []string {
	rm, m := r.Mesh, n.Mesh
	var diff []string
	f1 := func(name string, dst *float32, v float32) bool {
		if *dst == v || (*dst != *dst && v != v) {
			return false
		}
		*dst = v
		diff = append(diff, name)
		return true
	}
	f3 := func(name string, dst *Vec3, v Vec3) bool {
		x, y, z := dst.X, dst.Y, dst.Z
		if (x == v.X || (x != x && v.X != v.X)) && (y == v.Y || (y != y && v.Y != v.Y)) && (z == v.Z || (z != z && v.Z != v.Z)) {
			return false
		}
		*dst = v
		diff = append(diff, name)
		return true
	}
	i1 := func(name string, dst *int32, v int32) {
		if *dst != v {
			*dst = v
			diff = append(diff, name)
		}
	}
	s1 := func(name string, dst *string, v string) {
		if !strings.EqualFold(*dst, v) && !(isNullName(*dst) && isNullName(v)) {
			*dst = v
			diff = append(diff, name)
		}
	}
	f3("diffuse", &m.Diffuse, rm.Diffuse)
	f3("ambient", &m.Ambient, rm.Ambient)
	f3("specular", &m.Specular, rm.Specular)
	f1("shininess", &m.Shininess, rm.Shininess)
	s1("texture1", &m.Texture1, rm.Texture1)
	s1("texture2", &m.Texture2, rm.Texture2)
	s1("materialname", &m.MaterialName, rm.MaterialName)
	s1("renderhint", &m.RenderHint, rm.RenderHint)
	i1("transparencyhint", &m.TransparencyHint, rm.TransparencyHint)
	i1("tilefade", &m.TileFade, rm.TileFade)
	i1("rotatetexture", &m.RotateTexture, rm.RotateTexture)
	if f1("alpha", &m.Alpha, rm.Alpha) && r.isDeclared("alpha") {
		n.declare("alpha")
	}
	if f3("selfillumcolor", &m.SelfIllumColor, rm.SelfIllumColor) && r.isDeclared("selfillumcolor") {
		n.declare("selfillumcolor")
	}
	return diff
}

func isNullName(s string) bool { return s == "" || strings.EqualFold(s, "NULL") }

// foldCopies writes the copies of folded donor faces as faces of receiver r,
// in the order of r's render list, as the game drew them: each takes its
// donor face's smoothing group and material, and its stored neighbours are
// the donor face's, where those were folded too.
func foldCopies(r *Node, folded [][2]mergeTri) {
	if len(folded) == 0 {
		return
	}
	sort.SliceStable(folded, func(i, j int) bool { return folded[i][1].k < folded[j][1].k })
	m := r.Mesh
	base := len(m.Faces)
	at := map[*MeshData]map[int]int{} // donor mesh, donor face -> r's new face
	for i, p := range folded {
		if at[p[0].mesh] == nil {
			at[p[0].mesh] = map[int]int{}
		}
		at[p[0].mesh][p[0].k] = base + i
	}
	adj := len(m.StoredAdjacency) == len(m.Faces)
	for _, p := range folded {
		dt, c := p[0], p[1]
		df := dt.mesh.Faces[dt.k]
		v := [3]int32{int32(c.vi[0]), int32(c.vi[1]), int32(c.vi[2])}
		m.Faces = append(m.Faces, Face{Verts: v, UVs: v, SmoothGroup: df.SmoothGroup, Material: df.Material})
		if !adj {
			continue
		}
		a := [3]int16{-1, -1, -1}
		if rot, ok := copyCorners(dt, c); ok && len(dt.mesh.StoredAdjacency) == len(dt.mesh.Faces) {
			for e := 0; e < 3; e++ {
				j := int(dt.mesh.StoredAdjacency[dt.k][e])
				if nf, ok := at[dt.mesh][j]; ok && j >= 0 && nf <= math.MaxInt16 {
					a[(e+rot)%3] = int16(nf)
				}
			}
		}
		m.StoredAdjacency = append(m.StoredAdjacency, a)
	}
}

// dropCopyVertices removes from receiver r the vertices its render list rl
// used and none of its faces uses: those only the copies of restored donors
// (or lost triangles) used. Returns how many it removed.
func dropCopyVertices(r *Node, rl []uint16) int {
	m := r.Mesh
	nv := len(m.Verts)
	used := make([]bool, nv)
	for _, f := range m.Faces {
		for k := 0; k < 3; k++ {
			if f.Verts[k] != f.UVs[k] {
				return 0 // not the decompiler's one-index-per-corner layout
			}
			if v := f.Verts[k]; v >= 0 && int(v) < nv {
				used[v] = true
			}
		}
	}
	drop := make([]bool, nv)
	any := false
	for _, v := range rl {
		if int(v) < nv && !used[v] {
			drop[v] = true
			any = true
		}
	}
	if !any {
		return 0
	}
	keep := make([]int, 0, nv)
	for i := 0; i < nv; i++ {
		if !drop[i] {
			keep = append(keep, i)
		}
	}
	if !keepVertices(r, keep) {
		return 0
	}
	return nv - len(keep)
}

// vertexListsOK reports whether every per-vertex list of n's mesh has one
// entry per vertex (or none) and its faces index vertices and UVs alike, so
// its vertices can be removed or copied.
func vertexListsOK(n *Node) bool {
	m := n.Mesh
	if n.Aabb != nil || n.AnimMesh != nil || len(m.CornerNormals) > 0 || len(m.MirrorList) > 0 ||
		len(m.TexIndices0)+len(m.TexIndices1)+len(m.TexIndices2)+len(m.TexIndices3) > 0 {
		return false
	}
	nv := len(m.Verts)
	full := func(l int) bool { return l == 0 || l == nv }
	if !full(len(m.Normals)) || !full(len(m.Tangents)) || !full(len(m.Colors)) || !full(len(m.TVerts)) ||
		!full(len(m.TVerts1)) || !full(len(m.TVerts2)) || !full(len(m.TVerts3)) {
		return false
	}
	if n.Skin != nil && !full(len(n.Skin.Weights)) || n.Dangly != nil && !full(len(n.Dangly.Constraints)) {
		return false
	}
	for _, f := range m.Faces {
		if f.Verts != f.UVs {
			return false
		}
	}
	return true
}

// keepVertices keeps only the vertices keep (ascending) of n's mesh and
// renumbers its faces; false, changing nothing, when vertexListsOK fails or a
// face uses a vertex not kept.
func keepVertices(n *Node, keep []int) bool {
	if !vertexListsOK(n) {
		return false
	}
	m := n.Mesh
	nv := len(m.Verts)
	newIdx := make([]int32, nv)
	for i := range newIdx {
		newIdx[i] = -1
	}
	for ni, oi := range keep {
		newIdx[oi] = int32(ni)
	}
	for _, f := range m.Faces {
		for k := 0; k < 3; k++ {
			if v := f.Verts[k]; v >= 0 && int(v) < nv && newIdx[v] < 0 {
				return false
			}
		}
	}
	pick3 := func(l []Vec3) []Vec3 {
		if len(l) == 0 {
			return l
		}
		out := make([]Vec3, len(keep))
		for ni, oi := range keep {
			out[ni] = l[oi]
		}
		return out
	}
	m.Verts, m.Normals, m.Colors = pick3(m.Verts), pick3(m.Normals), pick3(m.Colors)
	m.TVerts, m.TVerts1, m.TVerts2, m.TVerts3 = pick3(m.TVerts), pick3(m.TVerts1), pick3(m.TVerts2), pick3(m.TVerts3)
	if len(m.Tangents) > 0 {
		t := make([]Vec4, len(keep))
		for ni, oi := range keep {
			t[ni] = m.Tangents[oi]
		}
		m.Tangents = t
	}
	if n.Skin != nil && len(n.Skin.Weights) > 0 {
		w := make([]VertexWeight, len(keep))
		for ni, oi := range keep {
			w[ni] = n.Skin.Weights[oi]
		}
		n.Skin.Weights = w
	}
	if n.Dangly != nil && len(n.Dangly.Constraints) > 0 {
		c := make([]float32, len(keep))
		for ni, oi := range keep {
			c[ni] = n.Dangly.Constraints[oi]
		}
		n.Dangly.Constraints = c
	}
	for fi := range m.Faces {
		for k := 0; k < 3; k++ {
			if v := m.Faces[fi].Verts[k]; v >= 0 && int(v) < nv {
				m.Faces[fi].Verts[k] = newIdx[v]
				m.Faces[fi].UVs[k] = newIdx[v]
			}
		}
	}
	return true
}

// copyVertex appends a copy of vertex v of n's mesh, with everything listed
// per vertex, and returns its index. The caller checks vertexListsOK.
func copyVertex(n *Node, v int) int {
	m := n.Mesh
	nv := len(m.Verts)
	add3 := func(l []Vec3) []Vec3 {
		if len(l) == nv {
			return append(l, l[v])
		}
		return l
	}
	m.Verts = append(m.Verts, m.Verts[v])
	m.Normals, m.Colors = add3(m.Normals), add3(m.Colors)
	m.TVerts, m.TVerts1, m.TVerts2, m.TVerts3 = add3(m.TVerts), add3(m.TVerts1), add3(m.TVerts2), add3(m.TVerts3)
	if len(m.Tangents) == nv {
		m.Tangents = append(m.Tangents, m.Tangents[v])
	}
	if n.Skin != nil && len(n.Skin.Weights) == nv {
		n.Skin.Weights = append(n.Skin.Weights, n.Skin.Weights[v])
	}
	if n.Dangly != nil && len(n.Dangly.Constraints) == nv {
		n.Dangly.Constraints = append(n.Dangly.Constraints, n.Dangly.Constraints[v])
	}
	return nv
}

// donorCorners gives a donor put back the normals, tangents and handedness its
// copies were drawn with. The copy is what the game drew, and its corners can
// differ from the donor's own: where a donor's back-to-back faces share a
// vertex its stored normal is (0, 0, 0), while the copy holds the normalised
// one (MDL-MESH-032 turns the zero vector into (1, 0, 0)); and a donor has no
// index list, so the tangent clones of its vertices (MDL-MESH-043) are in its
// vertex list but no face reaches them, while its copies use them.
//
// The copy sits in the receiver's frame: one rotation, taken from the donor's
// largest triangle, turns it into the donor's. Each matched triangle that
// rotation maps onto its copy (sides within mergeTolerance) gives its corners'
// copy values. A vertex whose corners all get the same values (normals and
// tangents within mergeNormalTolerance, the same handedness) takes them where
// its own differ. Where they disagree, the vertex keeps the group that matches
// its own values (or else the first), and the corners of each other group
// move to a vertex of the same position and UV that has that group's values:
// an unused one already there (the donor's own clone) or a copy of the vertex.
func donorCorners(n *Node, pairs [][2]mergeTri) {
	m := n.Mesh
	nv := len(m.Verts)
	if nv == 0 || len(pairs) == 0 {
		return
	}
	cm := pairs[0][1].mesh
	useN := len(m.Normals) >= nv && len(cm.Normals) >= len(cm.Verts)
	useT := len(m.Tangents) >= nv && len(cm.Tangents) >= len(cm.Verts)
	if !useN && !useT {
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
	type values struct {
		n, t [3]float64
		w    float32
	}
	same := func(a, b values) bool {
		if useN && dist3(a.n, b.n) > mergeNormalTolerance {
			return false
		}
		return !useT || (dist3(a.t, b.t) <= mergeNormalTolerance && (a.w < 0) == (b.w < 0))
	}
	own := func(v int) values {
		var o values
		if useN {
			o.n = vec3d(m.Normals[v])
		}
		if useT {
			t := m.Tangents[v]
			o.t, o.w = [3]float64{float64(t.X), float64(t.Y), float64(t.Z)}, t.W
		}
		return o
	}
	type group struct {
		v       values
		corners [][2]int // face, corner
	}
	groups := make([][]group, nv)
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
			cv := mt.c.vi[(j+mt.r)%3]
			var want values
			if useN {
				want.n = toDonor(vec3d(cm.Normals[cv]))
			}
			if useT {
				t := cm.Tangents[cv]
				want.t, want.w = toDonor([3]float64{float64(t.X), float64(t.Y), float64(t.Z)}), t.W
			}
			v := mt.d.vi[j]
			placed := false
			for gi := range groups[v] {
				if same(groups[v][gi].v, want) {
					groups[v][gi].corners = append(groups[v][gi].corners, [2]int{mt.d.k, j})
					placed = true
					break
				}
			}
			if !placed {
				groups[v] = append(groups[v], group{v: want, corners: [][2]int{{mt.d.k, j}}})
			}
		}
	}
	set := func(v int, want values) {
		if useN && dist3(vec3d(m.Normals[v]), want.n) > mergeNormalTolerance {
			m.Normals[v] = vec3f(want.n)
		}
		if useT {
			t := m.Tangents[v]
			if dist3([3]float64{float64(t.X), float64(t.Y), float64(t.Z)}, want.t) > mergeNormalTolerance || (t.W < 0) != (want.w < 0) {
				m.Tangents[v] = Vec4{X: float32(want.t[0]), Y: float32(want.t[1]), Z: float32(want.t[2]), W: want.w}
			}
		}
	}
	usedByFace := make([]bool, nv)
	for _, f := range m.Faces {
		for k := 0; k < 3; k++ {
			if v := f.Verts[k]; v >= 0 && int(v) < nv {
				usedByFace[v] = true
			}
		}
	}
	split := vertexListsOK(n)
	for v := 0; v < nv; v++ {
		gs := groups[v]
		if len(gs) == 0 {
			continue
		}
		home := 0
		if len(gs) > 1 {
			o := own(v)
			for gi := range gs {
				if same(gs[gi].v, o) {
					home = gi
					break
				}
			}
		}
		set(v, gs[home].v)
		if !split {
			continue
		}
		for gi, g := range gs {
			if gi == home {
				continue
			}
			u := -1
			for x := 0; x < nv; x++ {
				if x != v && !usedByFace[x] && sameCorner(m, v, x) && same(own(x), g.v) {
					u = x
					break
				}
			}
			if u < 0 {
				u = copyVertex(n, v)
				set(u, g.v)
			}
			for _, c := range g.corners {
				m.Faces[c[0]].Verts[c[1]] = int32(u)
				m.Faces[c[0]].UVs[c[1]] = int32(u)
			}
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
