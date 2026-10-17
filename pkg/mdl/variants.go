package mdl

import (
	"fmt"
	"math"
	"strings"
)

// Robe variants. A robe (pmh0_robe003) is one or more skins weighted to the
// robe's own copies of the skeleton bones (pelvis_g, lthigh_g, ...); the game
// loads it as a separate object, so it binds to those copies (see
// partAttachNodes). The same robe for another race, gender or phenotype
// (pmd0_robe003) is the same mesh refitted to that body: the bone copies sit
// where that body's are, and the cloth follows them.
//
// Variant does the refit. For each vertex it blends, over the bones the
// vertex is weighted to, the vertex moved from the source bone to the target
// bone, after two scalings in the bone's space:
//   - along the bone, by how much longer or shorter the target's bone is
//     (the offset to its child bones; for shins and forearms, which have no
//     child in a robe, the offset to the foot or hand in the race skeletons),
//     and only on the far side of the joint;
//   - across the bone (x and y) by half of how much wider or narrower the
//     target race's stock body part on that bone is (chest, pelvis, thighs,
//     arms). Shins take the thighs' value: a skirt hangs from the thighs, and
//     dwarf boots would otherwise flare the hem.
//
// Normals go through the inverse transpose of the same maps. Checked against
// Bioware's own variants of the stock robes 003, 004, 006 and 030 (same mesh,
// vertices matched by their UVs): 3.9 cm mean distance over every race and
// gender in phenotypes 0 and 2, against 4.7 cm for moving the bones alone and
// up to 33 cm for the unchanged human robe. In game (everything embedded in
// a test module, which outranks Beamdog's HD patch haks), generated robes,
// cloaks, classic and HD armour parts for human, dwarf, half-orc, elf and
// gnome look like Bioware's and Beamdog's while idle, casting, sitting and
// walking.

// VariantOptions configures Variant.
type VariantOptions struct {
	// Locs are searched, in order, for the target's bone template (a stock
	// robe of the target body), the source and target skeletons and their
	// stock body parts: directories, .hak/.erf/.mod archives or a game
	// install.
	Locs []string

	// Warn, if set, receives a message for each thing that could not be done
	// as described above (a missing skeleton, body part or bone), and what
	// was done instead.
	Warn func(string)

	// Width and Length adjust the fit for a variant that needs a touch more
	// or less room: Width multiplies the scale across every bone (1.05 makes
	// a robe 5% looser than the fit, a part 5% wider), Length the stretch
	// along every bone. Zero means 1. Feet, hands, shoulder pads and belts,
	// which scale in all three directions, take Width in all three.
	Width, Length float64
}

// factors returns opts' Width and Length, with zero as 1.
func (o VariantOptions) factors() (width, length float64) {
	width, length = o.Width, o.Length
	if width == 0 {
		width = 1
	}
	if length == 0 {
		length = 1
	}
	return width, length
}

// boneTemplates are the models tried, after the source's own number, as the
// target body's bone template: stock robes and cloaks that exist for every
// body. Cloaks are built like robes (skins weighted to the model's own copies
// of the skeleton's cloak bones), and Bioware's versions of a cloak share one
// mesh across all twelve bodies.
var boneTemplates = map[string][]string{
	"robe":  {"_robe003", "_robe004", "_robe005", "_robe006"},
	"cloak": {"_cloak_001", "_cloak_002", "_cloak_003", "_cloak_004", "_cloak_005", "_cloak_006", "_cloak_007"},
}

// girthStrength is the share of the body parts' width ratio applied
// across each bone. Robes hang looser than skin; half the ratio fitted
// Bioware's variants best (3.9 cm mean against 4.0 cm at full strength).
const girthStrength = 0.5

// VariantPrefix returns the body prefix of a robe or body part model's name
// (pmh0 for pmh0_robe003), or "" when the name does not start with one.
func VariantPrefix(name string) string { return partSkeletonName(name) }

// Variant returns a copy of src, a robe or an armour body part made for one
// body, refitted to the body with prefix target (pmd0 for a male dwarf in
// phenotype 0). The copy is named like src with the prefix replaced
// (pmh0_robe003 -> pmd0_robe003), node names and the supermodel included.
// Bitmaps are kept, as in Bioware's own variants. Body parts are refitted by
// partVariant (variants_parts.go).
func Variant(src *Model, target string, opts VariantOptions) (*Model, error) {
	warn := opts.Warn
	if warn == nil {
		warn = func(string) {}
	}
	target = strings.ToLower(strings.TrimSpace(target))
	srcPrefix := VariantPrefix(src.Name)
	if srcPrefix == "" {
		return nil, fmt.Errorf("%q does not start with a body prefix such as pmh0_", src.Name)
	}
	if len(target) != 4 || target[0] != 'p' {
		return nil, fmt.Errorf("target %q is not a body prefix such as pmd0", target)
	}
	if src.RootNode() == nil {
		return nil, fmt.Errorf("%s has no node tree", src.Name)
	}
	if partAttachNode(src.Name) != "" {
		return partVariant(src, srcPrefix, target, opts, warn)
	}
	kind := strings.TrimRight(partKind(src.Name), "_")
	templates, ok := boneTemplates[kind]
	if !ok {
		return nil, fmt.Errorf("%s is not a robe, cloak or armour body part (pXXX_robeNNN, pXXX_cloak_NNN, pXXX_chestNNN, ...); heads and other models are not supported", src.Name)
	}
	suffix := strings.ToLower(src.Name[len(srcPrefix):])

	var skins []*Node
	for _, n := range src.Nodes {
		if n.Skin != nil && n.Mesh != nil {
			skins = append(skins, n)
		}
	}
	if len(skins) == 0 {
		return nil, fmt.Errorf("%s has no skin to refit", src.Name)
	}

	// The target's bone copies, from a stock robe or cloak of that body.
	var tmpl *Model
	var tmplName string
	for _, s := range append([]string{suffix}, templates...) {
		ref := findModelResource(target+s, opts.Locs)
		if ref == nil {
			continue
		}
		m, err := readModelRef(ref)
		if err != nil {
			warn(fmt.Sprintf("%s: %v", ref.id, err))
			continue
		}
		tmpl, tmplName = m, target+s
		break
	}
	if tmpl == nil {
		return nil, fmt.Errorf("no %s for %s to take the bones from (tried %s%s and %s%s to %s); pass the game install or a hak with them", kind, target, target, suffix, target, templates[0], templates[len(templates)-1])
	}

	srcW := newWorldTransforms(src)
	tmplW := newWorldTransforms(tmpl)
	tmplNodes := map[string]*Node{}
	for _, n := range tmpl.Nodes {
		if _, dup := tmplNodes[strings.ToLower(n.Name)]; !dup {
			tmplNodes[strings.ToLower(n.Name)] = n
		}
	}

	// Bones: every node the skins are weighted to.
	bones := map[string]bool{}
	for _, sk := range skins {
		for _, vw := range sk.Skin.Weights {
			for _, b := range vw.Bones {
				bones[strings.ToLower(b)] = true
			}
		}
	}
	for b := range bones {
		if _, ok := tmplNodes[b]; !ok {
			warn(fmt.Sprintf("bone %s is not in %s: the vertices weighted to it stay where they are", b, tmplName))
		}
	}

	srcSkel, _ := loadSkeleton(srcPrefix, opts.Locs)
	dstSkel, _ := loadSkeleton(target, opts.Locs)
	if srcSkel == nil || dstSkel == nil {
		warn(fmt.Sprintf("skeleton %s or %s not found: shins and forearms keep their source length", srcPrefix, target))
	}
	axes := boneAxes(src, tmplNodes, bones, srcSkel, dstSkel)
	width, length := opts.factors()
	for b, ax := range axes {
		ax.ratio *= length
		axes[b] = ax
	}
	girth := bodyGirth(srcPrefix, target, opts.Locs, warn)
	widen := cloakWidening(kind, src, srcPrefix, target, tmplW, opts.Locs, warn)

	out := cloneForBody(src, srcPrefix, target)
	// Bone copies (and any other node the template has) take the template's
	// placement.
	for _, n := range out.Nodes {
		if n.Skin != nil || n == out.RootNode() {
			continue
		}
		if t, ok := tmplNodes[strings.ToLower(n.Name)]; ok {
			n.Position, n.Orientation = t.Position, t.Orientation
		}
	}
	outW := newWorldTransforms(out)

	for _, sk := range skins {
		skW := srcW.of(sk.Name)
		dst := out.Nodes[indexOfNode(src, sk)]
		dstW := outW.of(dst.Name)
		verts := make([]Vec3, len(sk.Mesh.Verts))
		for vi, v := range sk.Mesh.Verts {
			p := skW.apply(vec3d(v))
			var ws VertexWeight
			if vi < len(sk.Skin.Weights) {
				ws = sk.Skin.Weights[vi]
			}
			q := blendBones(p, ws, func(b string, l [3]float64) ([3]float64, bool) {
				bs, ok1 := srcW.lookup(b)
				bd, ok2 := tmplW.lookup(b)
				if !ok1 || !ok2 {
					return l, false
				}
				l = bs.unapply(l)
				ax := axes[b]
				if d := dot3(l, ax.dir); d > 0 {
					l = add3(l, scale3(ax.dir, (ax.ratio-1)*d))
				}
				g := girth.of(b)
				g[0], g[1] = g[0]*width, g[1]*width
				l = [3]float64{l[0] * g[0], l[1] * g[1], l[2]}
				return bd.apply(l), true
			})
			q = widen.point(q, ws)
			verts[vi] = vec3f(dstW.unapply(q))
		}
		dst.Mesh.Verts = verts

		if len(sk.Mesh.Normals) == len(sk.Mesh.Verts) {
			normals := make([]Vec3, len(sk.Mesh.Normals))
			for vi, nv := range sk.Mesh.Normals {
				nm := quatRotate(skW.rot, vec3d(nv))
				var ws VertexWeight
				if vi < len(sk.Skin.Weights) {
					ws = sk.Skin.Weights[vi]
				}
				q := blendBones(nm, ws, func(b string, l [3]float64) ([3]float64, bool) {
					bs, ok1 := srcW.lookup(b)
					bd, ok2 := tmplW.lookup(b)
					if !ok1 || !ok2 {
						return l, false
					}
					l = quatRotate(quatConj(bs.rot), l)
					ax := axes[b]
					d := dot3(l, ax.dir)
					l = add3(l, scale3(ax.dir, (1/ax.ratio-1)*d))
					g := girth.of(b)
					g[0], g[1] = g[0]*width, g[1]*width
					l = norm3([3]float64{l[0] / g[0], l[1] / g[1], l[2]})
					return quatRotate(bd.rot, l), true
				})
				q = widen.normal(norm3(q), ws)
				normals[vi] = vec3f(norm3(quatRotate(quatConj(dstW.rot), norm3(q))))
			}
			dst.Mesh.Normals = normals
		}
	}
	return out, nil
}

// blendBones returns the weighted blend of move(bone, p) over a vertex's
// weights. A bone move cannot do (not in both trees) keeps p for its share.
func blendBones(p [3]float64, ws VertexWeight, move func(bone string, p [3]float64) ([3]float64, bool)) [3]float64 {
	var acc [3]float64
	var tot float64
	for i, b := range ws.Bones {
		if i >= len(ws.Weights) || ws.Weights[i] <= 0 {
			continue
		}
		w := float64(ws.Weights[i])
		q, _ := move(strings.ToLower(b), p)
		acc = add3(acc, scale3(q, w))
		tot += w
	}
	if tot == 0 {
		return p
	}
	return scale3(acc, 1/tot)
}

// boneAxis is a bone's stretch: unit direction in the bone's space and the
// target/source length ratio along it.
type boneAxis struct {
	dir   [3]float64
	ratio float64
}

// boneAxes works out each bone's stretch from the offsets to its children
// (source robe against template robe); a bone with no child in the robe takes
// it from the race skeletons (shin -> foot, forearm -> hand), and failing
// that from its parent.
func boneAxes(src *Model, tmpl map[string]*Node, bones map[string]bool, srcSkel, dstSkel *skeleton) map[string]boneAxis {
	children := map[string][]*Node{}
	parentOf := map[string]string{}
	for _, n := range src.Nodes {
		p := strings.ToLower(n.Parent)
		children[p] = append(children[p], n)
		parentOf[strings.ToLower(n.Name)] = p
	}
	fromOffsets := func(a, b [][3]float64) (boneAxis, bool) {
		if len(a) == 0 {
			return boneAxis{}, false
		}
		var sa, sb [3]float64
		for i := range a {
			sa, sb = add3(sa, a[i]), add3(sb, b[i])
		}
		la, lb := len3(sa)/float64(len(a)), len3(sb)/float64(len(b))
		if la < 1e-4 {
			return boneAxis{}, false
		}
		return boneAxis{dir: norm3(sa), ratio: lb / la}, true
	}
	skelOffsets := func(name string) (a, b [][3]float64) {
		if srcSkel == nil || dstSkel == nil {
			return nil, nil
		}
		for _, n := range srcSkel.model.Nodes {
			if !strings.EqualFold(n.Parent, name) {
				continue
			}
			if i, ok := dstSkel.index[strings.ToLower(n.Name)]; ok {
				a = append(a, vec3d(n.Position))
				b = append(b, vec3d(dstSkel.order[i].Position))
			}
		}
		return a, b
	}

	axes := map[string]boneAxis{}
	var get func(name string, depth int) boneAxis
	get = func(name string, depth int) boneAxis {
		if ax, ok := axes[name]; ok {
			return ax
		}
		var a, b [][3]float64
		for _, c := range children[name] {
			if c.Skin != nil {
				continue
			}
			if t, ok := tmpl[strings.ToLower(c.Name)]; ok {
				a = append(a, vec3d(c.Position))
				b = append(b, vec3d(t.Position))
			}
		}
		ax, ok := fromOffsets(a, b)
		if !ok {
			ax, ok = fromOffsets(skelOffsets(name))
		}
		if !ok {
			ax = boneAxis{dir: [3]float64{0, 0, 1}, ratio: 1}
			if p := parentOf[name]; p != "" && depth < 64 {
				if _, isBone := tmpl[p]; isBone {
					ax = get(p, depth+1)
				}
			}
		}
		axes[name] = ax
		return ax
	}
	for b := range bones {
		get(b, 0)
	}
	return axes
}

// girthScales holds the across-bone scale per bone (x, y).
type girthScales map[string][2]float64

func (g girthScales) of(bone string) [2]float64 {
	if s, ok := g[bone]; ok {
		return s
	}
	return [2]float64{1, 1}
}

// robeGirth compares the source and target bodies' stock body parts (part
// 001) on each bone: the spread of their vertices across the bone.
func bodyGirth(srcPrefix, target string, locs []string, warn func(string)) girthScales {
	g := girthScales{}
	var missing []string
	for part, bone := range partAttachNodes {
		switch part {
		case "chest", "pelvis", "legl", "legr", "bicepl", "bicepr", "forel", "forer":
		default:
			continue
		}
		a := partSpread(srcPrefix+"_"+part+"001", locs)
		b := partSpread(target+"_"+part+"001", locs)
		if a == nil || b == nil || a[0] <= 0 || a[1] <= 0 {
			missing = append(missing, part)
			continue
		}
		g[bone] = [2]float64{1 + girthStrength*(b[0]/a[0]-1), 1 + girthStrength*(b[1]/a[1]-1)}
	}
	for _, side := range []string{"l", "r"} {
		if s, ok := g[side+"thigh_g"]; ok {
			g[side+"shin_g"] = s
		}
	}
	if len(missing) > 0 {
		warn(fmt.Sprintf("stock body parts for %s or %s not found (%s): those bones keep their width", srcPrefix, target, strings.Join(missing, ", ")))
	}
	return g
}

// partSpread returns the RMS spread along x, y and z of a body part model's
// vertices about their centre, or nil when the part is not found.
func partSpread(name string, locs []string) []float64 {
	_, spread, ok := partStats(name, locs)
	if !ok {
		return nil
	}
	return spread[:]
}

// partStats returns the centre of a body part model's vertices and their RMS
// spread about it along x, y and z; ok is false when the part is not found.
func partStats(name string, locs []string) (centre, spread [3]float64, ok bool) {
	ref := findModelResource(name, locs)
	if ref == nil {
		return centre, spread, false
	}
	m, err := readModelRef(ref)
	if err != nil {
		return centre, spread, false
	}
	w := newWorldTransforms(m)
	var pts [][3]float64
	for _, n := range m.Nodes {
		if n.Mesh == nil || n.Aabb != nil {
			continue
		}
		t := w.of(n.Name)
		for _, v := range n.Mesh.Verts {
			pts = append(pts, t.apply(vec3d(v)))
		}
	}
	if len(pts) == 0 {
		return centre, spread, false
	}
	for _, p := range pts {
		centre = add3(centre, p)
	}
	centre = scale3(centre, 1/float64(len(pts)))
	for _, p := range pts {
		for k := range spread {
			spread[k] += (p[k] - centre[k]) * (p[k] - centre[k])
		}
	}
	for k := range spread {
		spread[k] = math.Sqrt(spread[k] / float64(len(pts)))
	}
	return centre, spread, true
}

// cloneForBody copies src with the body prefix renamed in the model, node and
// parent names and the supermodel. Meshes are copied so their vertices and
// normals can be replaced; everything else is shared with src.
func cloneForBody(src *Model, from, to string) *Model {
	rename := func(s string) string {
		if len(s) > len(from) && strings.EqualFold(s[:len(from)+1], from+"_") {
			return to + s[len(from):]
		}
		return s
	}
	out := *src
	out.Name = rename(src.Name)
	if strings.EqualFold(src.SuperModel, from) {
		out.SuperModel = to
	}
	out.Warnings = nil
	out.Nodes = make([]*Node, len(src.Nodes))
	for i, n := range src.Nodes {
		c := *n
		c.Name, c.Parent = rename(n.Name), rename(n.Parent)
		if n.Mesh != nil {
			mesh := *n.Mesh
			c.Mesh = &mesh
		}
		out.Nodes[i] = &c
	}
	return &out
}

func indexOfNode(m *Model, n *Node) int {
	for i, x := range m.Nodes {
		if x == n {
			return i
		}
	}
	return -1
}

// readModelRef loads a model resource, binary or ASCII.
func readModelRef(ref *resourceRef) (*Model, error) {
	data, err := ref.read()
	if err != nil {
		return nil, err
	}
	m, err := readModelBytes(data)
	if err != nil {
		return nil, fmt.Errorf("%s: %v", ref.id, err)
	}
	if m == nil || m.RootNode() == nil {
		return nil, fmt.Errorf("%s: no node tree", ref.id)
	}
	return m, nil
}

// worldTransforms holds each node's rest transform in model space, by
// lower-case name (first node of a name wins).
type worldTransforms map[string]rigid

// rigid is a rotation (x, y, z, w) then a translation.
type rigid struct {
	rot [4]float64
	pos [3]float64
}

func (r rigid) apply(v [3]float64) [3]float64 { return add3(quatRotate(r.rot, v), r.pos) }
func (r rigid) unapply(v [3]float64) [3]float64 {
	return quatRotate(quatConj(r.rot), [3]float64{v[0] - r.pos[0], v[1] - r.pos[1], v[2] - r.pos[2]})
}

func newWorldTransforms(m *Model) worldTransforms {
	c := newCompiler(m)
	if root := m.RootNode(); root != nil {
		c.assignNodeIDs(root)
	}
	c.computeWorldTransforms()
	w := worldTransforms{}
	for _, n := range m.Nodes {
		key := strings.ToLower(n.Name)
		if _, dup := w[key]; dup {
			continue
		}
		if rot, ok := c.worldRot[n]; ok {
			w[key] = rigid{rot: rot, pos: c.worldPos[n]}
		}
	}
	return w
}

func (w worldTransforms) lookup(name string) (rigid, bool) {
	r, ok := w[strings.ToLower(name)]
	return r, ok
}

func (w worldTransforms) of(name string) rigid {
	if r, ok := w.lookup(name); ok {
		return r
	}
	return rigid{rot: [4]float64{0, 0, 0, 1}}
}

func vec3d(v Vec3) [3]float64 { return [3]float64{float64(v.X), float64(v.Y), float64(v.Z)} }
func vec3f(v [3]float64) Vec3 { return Vec3{float32(v[0]), float32(v[1]), float32(v[2])} }
func add3(a, b [3]float64) [3]float64 {
	return [3]float64{a[0] + b[0], a[1] + b[1], a[2] + b[2]}
}
func scale3(a [3]float64, s float64) [3]float64 { return [3]float64{a[0] * s, a[1] * s, a[2] * s} }
func dot3(a, b [3]float64) float64              { return a[0]*b[0] + a[1]*b[1] + a[2]*b[2] }
func len3(a [3]float64) float64                 { return math.Sqrt(dot3(a, a)) }
func norm3(a [3]float64) [3]float64 {
	if l := len3(a); l > 0 {
		return scale3(a, 1/l)
	}
	return a
}

// cloakStrength is the share of the chest's width ratio a cloak is widened
// by across the body. Bioware's versions of the stock cloaks fit best at 0.4
// (2.9 cm mean per vertex in phenotype 0 and 4.7 cm in phenotype 2, against
// 3.8 and 5.5 cm without widening and 18 cm for the unchanged human cloak);
// making it deeper as well only made it worse.
const cloakStrength = 0.4

// cloakWiden widens a cloak's cloth across the body (x in the torso's space)
// about the target chest's centre line, for the share of each vertex weighted
// to the cloak's bones (Cloak_g and the nodes under it). The zero value
// changes nothing, which is what robes get.
type cloakWiden struct {
	on     bool
	bones  map[string]bool
	torso  rigid
	centre float64
	scale  float64
}

func cloakWidening(kind string, src *Model, srcPrefix, target string, tmplW worldTransforms, locs []string, warn func(string)) cloakWiden {
	if kind != "cloak" {
		return cloakWiden{}
	}
	_, sa, okA := partStats(srcPrefix+"_chest001", locs)
	cb, sb, okB := partStats(target+"_chest001", locs)
	torso, okT := tmplW.lookup("torso_g")
	if !okA || !okB || !okT || sa[0] <= 1e-4 {
		warn(fmt.Sprintf("base chest %s_chest001 or %s_chest001, or the template's torso_g, not found: the cloak keeps its width", srcPrefix, target))
		return cloakWiden{}
	}
	bones := map[string]bool{}
	children := map[string][]string{}
	for _, n := range src.Nodes {
		children[strings.ToLower(n.Parent)] = append(children[strings.ToLower(n.Parent)], strings.ToLower(n.Name))
	}
	var mark func(string)
	mark = func(name string) {
		if bones[name] {
			return
		}
		bones[name] = true
		for _, c := range children[name] {
			mark(c)
		}
	}
	mark("cloak_g")
	return cloakWiden{on: true, bones: bones, torso: torso, centre: cb[0], scale: 1 + cloakStrength*(sb[0]/sa[0]-1)}
}

// share is the part of a vertex's weight on the cloak's bones.
func (c cloakWiden) share(ws VertexWeight) float64 {
	var on, tot float64
	for i, b := range ws.Bones {
		if i >= len(ws.Weights) || ws.Weights[i] <= 0 {
			continue
		}
		tot += float64(ws.Weights[i])
		if c.bones[strings.ToLower(b)] {
			on += float64(ws.Weights[i])
		}
	}
	if tot == 0 {
		return 0
	}
	return on / tot
}

func (c cloakWiden) point(p [3]float64, ws VertexWeight) [3]float64 {
	if !c.on {
		return p
	}
	f := c.share(ws)
	if f == 0 {
		return p
	}
	l := c.torso.unapply(p)
	l[0] = l[0] + f*((l[0]-c.centre)*c.scale+c.centre-l[0])
	return c.torso.apply(l)
}

func (c cloakWiden) normal(n [3]float64, ws VertexWeight) [3]float64 {
	if !c.on {
		return n
	}
	f := c.share(ws)
	if f == 0 {
		return n
	}
	l := quatRotate(quatConj(c.torso.rot), n)
	l[0] = l[0] * (1 + f*(1/c.scale-1))
	return quatRotate(c.torso.rot, norm3(l))
}
