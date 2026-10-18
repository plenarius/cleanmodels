package mdl

import (
	"fmt"
	"math"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"
)

// Slot choices. Bioware made its race versions of the stock armour parts by
// hand, and servers do the same for custom races; every such pair in the game
// install and the haks (pmh0_chest050 and pmd0_chest050: the same mesh,
// refitted) shows how that body's version of that slot should look. For an
// armour part, slotChoiceFor tries three fits on the pairs of the source's
// and target's slot found (up to learnPairs of them, the user's own folders
// and haks first) and keeps the closest:
//   - the measured fit (partVariant) to the base parts' bounding boxes;
//   - the measured fit to their centres of mass and spreads;
//   - a learned fit: a scale and an offset per axis in the part's own space,
//     fitted to the pairs (pooled, each pair counting the same), judged by
//     predicting each pair from the others (leave one out). It must beat
//     the better measured fit by learnMargin.
// A body with no pairs gets the measured fit by bounding boxes.
//
// Against Bioware's own race versions of the stock parts, each predicted
// without its own pair: 0.52 cm mean per vertex in phenotype 0 and 1.90 cm in
// phenotype 2, against 0.84 and 2.30 cm for the measured fit alone (and 0.74
// cm for NWNArmory's 2003 table in phenotype 0). Against a server's hand-made
// custom race parts, the bounding boxes fit closer than the centres of mass
// (a half-ogre's base chest has its mass in the hunched back, which pulled
// armour backwards): wemic 0.70 cm against 2.55, half-ogre 4.1 against 6.6.

const (
	learnPairs  = 12
	learnMargin = 0.95 // the learned fit's error must be below this share of the measured one
)

// slotFit is a learned fit: q = s*p + t per axis, in the part's space.
type slotFit struct {
	s, t  [3]float64
	pairs int
}

func (f slotFit) point(p [3]float64) [3]float64 {
	return [3]float64{f.s[0]*p[0] + f.t[0], f.s[1]*p[1] + f.t[1], f.s[2]*p[2] + f.t[2]}
}

func (f slotFit) normal(n [3]float64) [3]float64 {
	return norm3([3]float64{n[0] / f.s[0], n[1] / f.s[1], n[2] / f.s[2]})
}

// slotChoice is how a slot is fitted for a target body: the learned fit, or
// (fit nil) the measured fit by the base parts' bounding boxes, or by their
// centres of mass and spreads when byMass.
type slotChoice struct {
	fit    *slotFit
	byMass bool
}

// slotChoices caches slotChoiceFor by source body, target, slot and
// locations. Read-only once stored.
var slotChoices sync.Map

// slotChoiceFor returns how slot is fitted from the body srcPrefix to target.
func slotChoiceFor(srcPrefix, target, slot string, locs []string) slotChoice {
	key := strings.Join([]string{srcPrefix, target, slot, strings.Join(locs, "\x00")}, "\x01")
	if v, ok := slotChoices.Load(key); ok {
		return v.(slotChoice)
	}
	c := chooseSlot(srcPrefix, target, slot, locs, "")
	slotChoices.Store(key, c)
	return c
}

// learnSlot returns the learned fit chooseSlot picks, or nil.
func learnSlot(srcPrefix, target, slot string, locs []string, skip string) *slotFit {
	return chooseSlot(srcPrefix, target, slot, locs, skip).fit
}

// isGameInstall reports whether loc is a game install (data/nwn_base.key).
func isGameInstall(loc string) bool {
	_, err := os.Stat(filepath.Join(loc, "data", "nwn_base.key"))
	return err == nil
}

// fromGameInstall reports whether ref was found in a game install among locs
// (as opposed to a folder or hak of the user's).
func fromGameInstall(ref *resourceRef, locs []string) bool {
	for _, loc := range locs {
		if isGameInstall(loc) && strings.HasPrefix(ref.id, loc+"#") {
			return true
		}
	}
	return false
}

// pairSums are one pair's weighted sums for the per-axis least squares.
type pairSums struct {
	sx, sy, sxx, sxy [3]float64
	n                float64
	src, dst         *Model
}

// chooseSlot works out slotChoiceFor, without the pair of part number skip
// (for testing it against that pair). With no pairs, it is the measured fit by
// bounding boxes: against a server's hand-made custom race parts it is closer
// than by centres of mass (a half-ogre's chest has its mass in the hunched
// back, which pulled the armour backwards).
func chooseSlot(srcPrefix, target, slot string, locs []string, skip string) slotChoice {
	names := listModels(locs)
	var nums []string
	for name := range names {
		num, ok := strings.CutPrefix(name, srcPrefix+"_"+slot)
		if ok && num != "" && num != skip && strings.Trim(num, "0123456789") == "" && names[target+"_"+slot+num] {
			nums = append(nums, num)
		}
	}
	sort.Strings(nums)
	// Pairs from the user's own folders and haks first (a version made by
	// hand to show how this body should look), then the game install's,
	// spread over their numbers.
	var own, game []string
	for _, num := range nums {
		if ref := findModelResource(target+"_"+slot+num, locs); ref != nil && fromGameInstall(ref, locs) {
			game = append(game, num)
		} else {
			own = append(own, num)
		}
	}
	if len(own) >= learnPairs {
		own = own[:learnPairs]
	}
	if room := learnPairs - len(own); len(game) > room {
		pick := make([]string, room)
		for i := range pick {
			pick[i] = game[i*len(game)/room]
		}
		game = pick
	}
	nums = append(own, game...)
	var ps []pairSums
	for _, num := range nums {
		a, errA := readModelRef(findModelResource(srcPrefix+"_"+slot+num, locs))
		b, errB := readModelRef(findModelResource(target+"_"+slot+num, locs))
		if errA != nil || errB != nil {
			continue
		}
		pa, pb, share := correspondByUV(a, b)
		if share < 0.9 || len(pa) < 4 {
			continue // another mesh: no vertex correspondence
		}
		p := pairSums{src: a, dst: b}
		w := 1 / float64(len(pa))
		for i := range pa {
			for k := 0; k < 3; k++ {
				p.sx[k] += w * pa[i][k]
				p.sy[k] += w * pb[i][k]
				p.sxx[k] += w * pa[i][k] * pa[i][k]
				p.sxy[k] += w * pa[i][k] * pb[i][k]
			}
			p.n += w
		}
		ps = append(ps, p)
	}
	if len(ps) == 0 {
		return slotChoice{}
	}
	// Each fit on the pairs: the measured ones by box and by mass, and the
	// learned one predicting each pair from the others (leave one out).
	var box, mass, learned float64
	for i, p := range ps {
		for _, byMass := range []bool{false, true} {
			out, err := partVariant(p.src, srcPrefix, target, VariantOptions{Locs: locs, noLearn: true, byMass: byMass}, func(string) {})
			if err != nil {
				return slotChoice{}
			}
			if d := pairDistance(out, p.dst, nil); byMass {
				mass += d
			} else {
				box += d
			}
		}
		if len(ps) >= 2 {
			f := fitPairs(ps, i)
			learned += pairDistance(p.src, p.dst, &f)
		}
	}
	c := slotChoice{byMass: mass < box}
	best := math.Min(box, mass)
	if len(ps) >= 2 && learned < learnMargin*best {
		f := fitPairs(ps, -1)
		f.pairs = len(ps)
		c.fit = &f
	}
	return c
}

// fitPairs solves the pooled per-axis least squares, without pair skip.
func fitPairs(ps []pairSums, skip int) slotFit {
	var t pairSums
	for i, p := range ps {
		if i == skip {
			continue
		}
		for k := 0; k < 3; k++ {
			t.sx[k] += p.sx[k]
			t.sy[k] += p.sy[k]
			t.sxx[k] += p.sxx[k]
			t.sxy[k] += p.sxy[k]
		}
		t.n += p.n
	}
	f := slotFit{s: [3]float64{1, 1, 1}}
	if t.n == 0 {
		return f
	}
	for k := 0; k < 3; k++ {
		mx, my := t.sx[k]/t.n, t.sy[k]/t.n
		if vx := t.sxx[k]/t.n - mx*mx; vx > 1e-8 {
			f.s[k] = (t.sxy[k]/t.n - mx*my) / vx
		}
		f.t[k] = my - f.s[k]*mx
	}
	return f
}

// partVertices returns a model's rendered vertices in its own space, each
// with a key for finding it in another version of the model: its node's name
// without the body prefix, and the texture coordinates of its face corners.
func partVertices(m *Model) (pts [][3]float64, keys []string) {
	w := newWorldTransforms(m)
	for _, n := range m.Nodes {
		if n.Mesh == nil || n.Aabb != nil || len(n.Mesh.Verts) == 0 || n.Mesh.Render == 0 {
			continue
		}
		tr := w.of(n.Name)
		sets := make([]map[string]bool, len(n.Mesh.Verts))
		for i := range sets {
			sets[i] = map[string]bool{}
		}
		for _, f := range n.Mesh.Faces {
			for k := 0; k < 3; k++ {
				if v, u := f.Verts[k], f.UVs[k]; int(v) < len(sets) && int(u) < len(n.Mesh.TVerts) {
					tv := n.Mesh.TVerts[u]
					sets[v][fmt.Sprintf("%.3f,%.3f", tv.X, tv.Y)] = true
				}
			}
		}
		node := strings.ToLower(n.Name)
		node = node[strings.IndexByte(node+"_", '_'):]
		for i, v := range n.Mesh.Verts {
			pts = append(pts, tr.apply(vec3d(v)))
			uv := make([]string, 0, len(sets[i]))
			for k := range sets[i] {
				uv = append(uv, k)
			}
			sort.Strings(uv)
			keys = append(keys, node+"|"+strings.Join(uv, ";"))
		}
	}
	return pts, keys
}

// correspondByUV pairs a's vertices with b's that have the same key, using
// only keys that occur once in each (mirrored halves share texture
// coordinates). share is the part of a's vertices with a key b has.
func correspondByUV(a, b *Model) (pa, pb [][3]float64, share float64) {
	ap, ak := partVertices(a)
	bp, bk := partVertices(b)
	inA, inB := map[string]int{}, map[string]int{}
	at := map[string]int{}
	for _, k := range ak {
		inA[k]++
	}
	for j, k := range bk {
		inB[k]++
		at[k] = j
	}
	found := 0
	for i, k := range ak {
		if inB[k] > 0 {
			found++
		}
		if inA[k] == 1 && inB[k] == 1 {
			pa, pb = append(pa, ap[i]), append(pb, bp[at[k]])
		}
	}
	if len(ap) > 0 {
		share = float64(found) / float64(len(ap))
	}
	return pa, pb, share
}

// pairDistance is the mean distance from each of a's vertices (moved by f if
// set) to the nearest vertex of b with the same key.
func pairDistance(a, b *Model, f *slotFit) float64 {
	ap, ak := partVertices(a)
	bp, bk := partVertices(b)
	by := map[string][]int{}
	for j, k := range bk {
		by[k] = append(by[k], j)
	}
	var sum float64
	n := 0
	for i, k := range ak {
		p := ap[i]
		if f != nil {
			p = f.point(p)
		}
		best := -1.0
		for _, j := range by[k] {
			if d := len3([3]float64{p[0] - bp[j][0], p[1] - bp[j][1], p[2] - bp[j][2]}); best < 0 || d < best {
				best = d
			}
		}
		if best >= 0 {
			sum += best
			n++
		}
	}
	if n == 0 {
		return 0
	}
	return sum / float64(n)
}

// learnedVariant applies a learned fit to src's meshes in the part's space,
// then Width and Length about the result's centre (Width across, in x and y,
// or in all three for slots that scale in all directions; Length along z).
func learnedVariant(src *Model, srcPrefix, target string, f *slotFit, width, length float64) *Model {
	out := cloneForBody(src, srcPrefix, target)
	w := newWorldTransforms(src)
	slot := partKind(src.Name)
	k := [3]float64{width, width, length}
	if partScalesAllAxes[slot] {
		k[2] = width
	}
	// The centre the adjustments scale about: the fitted vertices' mean.
	var c [3]float64
	cnt := 0
	for _, n := range src.Nodes {
		if n.Mesh == nil {
			continue
		}
		x := w.of(n.Name)
		for _, v := range n.Mesh.Verts {
			c = add3(c, f.point(x.apply(vec3d(v))))
			cnt++
		}
	}
	if cnt > 0 {
		c = scale3(c, 1/float64(cnt))
	}
	for i, n := range src.Nodes {
		if n.Mesh == nil || len(n.Mesh.Verts) == 0 {
			continue
		}
		x := w.of(n.Name)
		dst := out.Nodes[i]
		verts := make([]Vec3, len(n.Mesh.Verts))
		for vi, v := range n.Mesh.Verts {
			p := f.point(x.apply(vec3d(v)))
			for a := 0; a < 3; a++ {
				p[a] = c[a] + (p[a]-c[a])*k[a]
			}
			verts[vi] = vec3f(x.unapply(p))
		}
		dst.Mesh.Verts = verts
		if len(n.Mesh.Normals) == len(n.Mesh.Verts) {
			normals := make([]Vec3, len(n.Mesh.Normals))
			for vi, nv := range n.Mesh.Normals {
				m := f.normal(quatRotate(x.rot, vec3d(nv)))
				m = norm3([3]float64{m[0] / k[0], m[1] / k[1], m[2] / k[2]})
				normals[vi] = vec3f(norm3(quatRotate(quatConj(x.rot), m)))
			}
			dst.Mesh.Normals = normals
		}
	}
	return out
}
