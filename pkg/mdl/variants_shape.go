package mdl

import (
	"fmt"
	"math"
	"sort"
	"strings"
)

// Shape adjustments. VariantOptions.Shape narrows or widens a variant by body
// region (shoulders, chest, waist, hips, hem) after the fit, for a version
// that needs a different silhouette (a less square brownie robe). Each
// region's height and centre come from the body's joints: the shoulders from
// the upper arms (lbicep_g, rbicep_g), the waist from torso_g, the chest
// halfway between, the hips from the thighs, the hem at the model's lowest
// vertex. A vertex is scaled across the body (x and y) about the centre line
// by its height's factor, blended linearly between the regions around it.
// Vertices on the arms (weighted to the upper arm, forearm, hand or shoulder
// bones; arm body parts) are left where they are, so sleeves stay on the arms.

// ShapeRegions are the regions VariantOptions.Shape takes, top to bottom.
var ShapeRegions = []string{"shoulders", "chest", "waist", "hips", "hem"}

// armBones are the skeleton bones whose vertices a shape leaves alone.
var armBones = map[string]bool{
	"lbicep_g": true, "rbicep_g": true, "lforearm_g": true, "rforearm_g": true,
	"lhand_g": true, "rhand_g": true, "lshoulder_g": true, "rshoulder_g": true,
}

// shapeControl is one region: its name, height, centre (x, y) and factor.
type shapeControl struct {
	region       string
	z, cx, cy, f float64
}

// shapeFrame places a model's regions: the transform from each mesh node to
// the body's space and the regions' controls, top to bottom.
type shapeFrame struct {
	attach   string
	world    func(n *Node) rigid
	controls []shapeControl
}

// newShapeFrame places the regions for m, a variant for the body target, or
// returns why it cannot.
func newShapeFrame(m *Model, target string, locs []string) (*shapeFrame, string) {
	// The model's space: a robe or cloak is drawn in its own (its bone
	// copies give the joints); a body part hangs from its skeleton node.
	w := newWorldTransforms(m)
	place := rigid{rot: [4]float64{0, 0, 0, 1}}
	attach := partAttachNode(m.Name)
	var sk *skeleton
	if attach != "" {
		sk, _ = loadBodySkeleton(target, locs)
		if sk == nil {
			return nil, "skeleton " + target + " not found"
		}
		i, ok := sk.attachIndex(attach)
		if !ok {
			return nil, "skeleton " + target + " has no " + attach
		}
		place = rigid{rot: sk.rot[i], pos: sk.pos[i]}
	}
	// A robe's or cloak's own bone copies first; a joint it has no copy of
	// (a cloak has no arms) from the body's skeleton, whose space a cloak's
	// shares.
	var fallback *skeleton
	joint := func(name string) ([3]float64, bool) {
		if sk == nil {
			if r, ok := w.lookup(name); ok {
				return r.pos, true
			}
			if fallback == nil {
				fallback, _ = loadBodySkeleton(target, locs)
			}
			if fallback == nil {
				return [3]float64{}, false
			}
			if i, ok := fallback.index[name]; ok {
				return fallback.pos[i], true
			}
			return [3]float64{}, false
		}
		if i, ok := sk.index[name]; ok {
			return sk.pos[i], true
		}
		return [3]float64{}, false
	}
	pair := func(a, b string) ([3]float64, bool) {
		pa, okA := joint(a)
		pb, okB := joint(b)
		switch {
		case okA && okB:
			return scale3(add3(pa, pb), 0.5), true
		case okA:
			return pa, true
		case okB:
			return pb, true
		}
		return [3]float64{}, false
	}
	shoulders, okS := pair("lbicep_g", "rbicep_g")
	waist, okW := joint("torso_g")
	if !okS || !okW || shoulders[2] <= waist[2] {
		return nil, "the model has no upper arm or torso_g bones above each other to place the regions by"
	}
	fr := &shapeFrame{attach: attach, world: func(n *Node) rigid { return place.compose(w.of(n.Name)) }}
	fr.controls = []shapeControl{
		{"shoulders", shoulders[2], shoulders[0], shoulders[1], 1},
		{"chest", (shoulders[2] + waist[2]) / 2, (shoulders[0] + waist[0]) / 2, (shoulders[1] + waist[1]) / 2, 1},
		{"waist", waist[2], waist[0], waist[1], 1},
	}
	low := waist
	if hips, ok := pair("lthigh_g", "rthigh_g"); ok && hips[2] < waist[2] {
		fr.controls = append(fr.controls, shapeControl{"hips", hips[2], hips[0], hips[1], 1})
		low = hips
	}
	// The hem: the lowest vertex.
	hem := math.Inf(1)
	for _, n := range m.Nodes {
		if n.Mesh == nil || n.Aabb != nil {
			continue
		}
		x := fr.world(n)
		for _, v := range n.Mesh.Verts {
			hem = math.Min(hem, x.apply(vec3d(v))[2])
		}
	}
	if hem < low[2]-1e-3 {
		fr.controls = append(fr.controls, shapeControl{"hem", hem, low[0], low[1], 1})
	}
	sort.Slice(fr.controls, func(i, j int) bool { return fr.controls[i].z > fr.controls[j].z })
	return fr, ""
}

// at returns the blended control at height z, and the share each region has
// in it (by index into controls).
func (fr *shapeFrame) at(z float64) (shapeControl, [2]int, float64) {
	cs := fr.controls
	if z >= cs[0].z {
		return cs[0], [2]int{0, 0}, 1
	}
	for i := 1; i < len(cs); i++ {
		if z >= cs[i].z {
			a, b := cs[i-1], cs[i]
			t := (z - b.z) / (a.z - b.z)
			return shapeControl{"", z, b.cx + t*(a.cx-b.cx), b.cy + t*(a.cy-b.cy), b.f + t*(a.f-b.f)}, [2]int{i - 1, i}, t
		}
	}
	last := len(cs) - 1
	return cs[last], [2]int{last, last}, 1
}

// armShare is the part of a vertex's weight on the arm bones.
func armShare(n *Node, vi int) float64 {
	if n.Skin == nil || vi >= len(n.Skin.Weights) {
		return 0
	}
	ws := n.Skin.Weights[vi]
	var on, tot float64
	for i, b := range ws.Bones {
		if i >= len(ws.Weights) || ws.Weights[i] <= 0 {
			continue
		}
		tot += float64(ws.Weights[i])
		if armBones[strings.ToLower(b)] {
			on += float64(ws.Weights[i])
		}
	}
	if tot == 0 {
		return 0
	}
	return on / tot
}

// applyShape scales out, a variant for the body target, by region. It
// returns without change when every factor is 1.
func applyShape(out *Model, target string, shape map[string]float64, locs []string, warn func(string)) {
	changed := false
	for _, f := range shape {
		if f != 0 && f != 1 {
			changed = true
		}
	}
	if !changed {
		return
	}
	fr, why := newShapeFrame(out, target, locs)
	if fr == nil {
		warn("shape: " + why + ": the shape is not applied")
		return
	}
	for i := range fr.controls {
		if f := shape[fr.controls[i].region]; f > 0 {
			fr.controls[i].f = f
		}
	}
	attach, world := fr.attach, fr.world
	at := func(z float64) shapeControl { c, _, _ := fr.at(z); return c }

	if armBones[attach] {
		return // arm parts (upper arms, forearms, hands, shoulder pads) stay on the arm
	}

	for _, n := range out.Nodes {
		if n.Mesh == nil || n.Aabb != nil || len(n.Mesh.Verts) == 0 {
			continue
		}
		x := world(n)
		verts := make([]Vec3, len(n.Mesh.Verts))
		for vi, v := range n.Mesh.Verts {
			p := x.apply(vec3d(v))
			c := at(p[2])
			k := 1 - armShare(n, vi)
			s := 1 + k*(c.f-1)
			p[0] = c.cx + (p[0]-c.cx)*s
			p[1] = c.cy + (p[1]-c.cy)*s
			verts[vi] = vec3f(x.unapply(p))
		}
		n.Mesh.Verts = verts
		if len(n.Mesh.Normals) == len(verts) {
			normals := make([]Vec3, len(verts))
			for vi, nv := range n.Mesh.Normals {
				m := quatRotate(x.rot, vec3d(nv))
				p := x.apply(vec3d(verts[vi]))
				c := at(p[2])
				s := 1 + (1-armShare(n, vi))*(c.f-1)
				m = norm3([3]float64{m[0] / s, m[1] / s, m[2]})
				normals[vi] = vec3f(quatRotate(quatConj(x.rot), m))
			}
			n.Mesh.Normals = normals
		}
	}
}

// ParseShape reads "shoulders=0.9,waist=0.85" into factors by region; each
// from 0.5 to 2.
func ParseShape(s string) (map[string]float64, error) {
	shape := map[string]float64{}
	if strings.TrimSpace(s) == "" {
		return shape, nil
	}
	for _, kv := range strings.Split(s, ",") {
		k, v, ok := strings.Cut(strings.TrimSpace(kv), "=")
		k = strings.ToLower(strings.TrimSpace(k))
		var f float64
		if ok {
			_, err := fmt.Sscanf(strings.TrimSpace(v), "%g", &f)
			ok = err == nil
		}
		known := false
		for _, r := range ShapeRegions {
			known = known || r == k
		}
		if !ok || !known {
			return nil, fmt.Errorf("%q: give region=factor, regions %s", kv, strings.Join(ShapeRegions, ", "))
		}
		if f < 0.5 || f > 2 {
			return nil, fmt.Errorf("%s=%g: give a factor from 0.5 to 2", k, f)
		}
		shape[k] = f
	}
	return shape, nil
}

// minShapeShare is the least share of some vertex a region must have to count
// as changing a model (VariantAdjustments).
const minShapeShare = 0.25

// VariantAdjustments lists the adjustments that change the variant m (made
// for the body target): "width" always; "length" unless the part scales the
// same in every direction (feet, hands, shoulder pads, belts); and each
// ShapeRegions region that holds some of its vertices (not counting the
// arms, which a shape leaves alone). For a GUI to offer only those.
func VariantAdjustments(m *Model, target string, locs []string) []string {
	adj := []string{"width"}
	if !partScalesAllAxes[partKind(m.Name)] {
		adj = append(adj, "length")
	}
	fr, _ := newShapeFrame(m, target, locs)
	if fr == nil || armBones[fr.attach] {
		return adj
	}
	used := make([]bool, len(fr.controls))
	for _, n := range m.Nodes {
		if n.Mesh == nil || n.Aabb != nil || n.Mesh.Render == 0 {
			continue
		}
		x := fr.world(n)
		for vi, v := range n.Mesh.Verts {
			k := 1 - armShare(n, vi)
			if k < 0.1 {
				continue
			}
			_, idx, t := fr.at(x.apply(vec3d(v))[2])
			// The region above takes t of the blend, the one below 1-t; a
			// region counts where it has at least minShapeShare of a vertex
			// (a foot's top is 13% "hips": not enough to offer the slider).
			if idx[0] == idx[1] || k*t >= minShapeShare {
				used[idx[0]] = true
			}
			if idx[0] != idx[1] && k*(1-t) >= minShapeShare {
				used[idx[1]] = true
			}
		}
	}
	for _, r := range ShapeRegions {
		for i, c := range fr.controls {
			if c.region == r && used[i] {
				adj = append(adj, r)
			}
		}
	}
	return adj
}
