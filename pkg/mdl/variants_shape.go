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

// shapeControl is one region: its height, centre (x, y) and factor.
type shapeControl struct {
	z, cx, cy, f float64
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
	factor := func(region string) float64 {
		if f := shape[region]; f > 0 {
			return f
		}
		return 1
	}

	// The model's space: a robe or cloak is drawn in its own (its bone
	// copies give the joints); a body part hangs from its skeleton node.
	w := newWorldTransforms(out)
	place := rigid{rot: [4]float64{0, 0, 0, 1}}
	attach := partAttachNode(out.Name)
	var sk *skeleton
	if attach != "" {
		sk, _ = loadBodySkeleton(target, locs)
		if sk == nil {
			warn("shape: skeleton " + target + " not found: the shape is not applied")
			return
		}
		i, ok := sk.attachIndex(attach)
		if !ok {
			warn("shape: skeleton " + target + " has no " + attach + ": the shape is not applied")
			return
		}
		place = rigid{rot: sk.rot[i], pos: sk.pos[i]}
	}
	joint := func(name string) ([3]float64, bool) {
		if sk != nil {
			if i, ok := sk.index[name]; ok {
				return sk.pos[i], true
			}
			return [3]float64{}, false
		}
		r, ok := w.lookup(name)
		return r.pos, ok
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
		warn("shape: the model has no upper arm or torso_g bones above each other to place the regions by: the shape is not applied")
		return
	}
	controls := []shapeControl{
		{shoulders[2], shoulders[0], shoulders[1], factor("shoulders")},
		{(shoulders[2] + waist[2]) / 2, (shoulders[0] + waist[0]) / 2, (shoulders[1] + waist[1]) / 2, factor("chest")},
		{waist[2], waist[0], waist[1], factor("waist")},
	}
	low := waist
	if hips, ok := pair("lthigh_g", "rthigh_g"); ok && hips[2] < waist[2] {
		controls = append(controls, shapeControl{hips[2], hips[0], hips[1], factor("hips")})
		low = hips
	}

	// World transform of each mesh node, and the hem: the lowest vertex.
	world := func(n *Node) rigid { return place.compose(w.of(n.Name)) }
	hem := math.Inf(1)
	for _, n := range out.Nodes {
		if n.Mesh == nil || n.Aabb != nil {
			continue
		}
		x := world(n)
		for _, v := range n.Mesh.Verts {
			hem = math.Min(hem, x.apply(vec3d(v))[2])
		}
	}
	if hem < low[2]-1e-3 {
		controls = append(controls, shapeControl{hem, low[0], low[1], factor("hem")})
	}
	sort.Slice(controls, func(i, j int) bool { return controls[i].z > controls[j].z })
	at := func(z float64) shapeControl {
		if z >= controls[0].z {
			return controls[0]
		}
		for i := 1; i < len(controls); i++ {
			if z >= controls[i].z {
				a, b := controls[i-1], controls[i]
				t := (z - b.z) / (a.z - b.z)
				return shapeControl{z, b.cx + t*(a.cx-b.cx), b.cy + t*(a.cy-b.cy), b.f + t*(a.f-b.f)}
			}
		}
		return controls[len(controls)-1]
	}

	if armBones[attach] {
		return // arm parts (upper arms, forearms, hands, shoulder pads) stay on the arm
	}
	armShare := func(n *Node, vi int) float64 {
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
