package mdl

import (
	"fmt"
	"math"
	"path/filepath"
	"sort"
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
//   - across the bone (x and y) by part of how much wider or narrower the
//     target race's stock body part on that bone is (chest, pelvis, thighs,
//     arms; see robeGirth). Shins and feet take the thighs' value: a skirt
//     hangs from the thighs, and dwarf boots would otherwise flare the hem.
//
// The robe's other meshes, the bone copies' own (render 0, but most cast the
// robe's shadow) and any hanging from them, are refitted like a vertex
// weighted wholly to their bone; a bone with no stock part takes its nearest
// ancestor's width (a head bone copy narrows with the torso). Bones the
// target's template robe lacks come from the target's skeleton.
//
// Normals go through the inverse transpose of the same maps. Checked against
// Bioware's own variants of the stock robes 003, 004, 006 and 030 (same mesh,
// vertices matched by their UVs): 3.9 cm mean distance over every race and
// gender in phenotypes 0 and 2 (and 1.8 cm against a server's hand-made
// brownie versions), against 4.7 cm for moving the bones alone and
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

	// OutputDir, if set, is the folder the variants are written to. The
	// file a variant is about to replace there (pmd0_robe200 in it, for
	// pmh0_robe200 to pmd0) is not used as its bone template: it may be an
	// earlier run's output, which would make each run depend on the last.
	OutputDir string

	// Shape, if set, narrows or widens the variant by body region after the
	// fit: factors by ShapeRegions name (shoulders, chest, waist, hips, hem),
	// 1 meaning unchanged. See variants_shape.go.
	Shape map[string]float64

	// Width and Length adjust the fit for a variant that needs a touch more
	// or less room: Width multiplies the scale across every bone (1.05 makes
	// a robe 5% looser than the fit, a part 5% wider), Length the stretch
	// along every bone. Zero means 1. Feet, hands, shoulder pads and belts,
	// which scale in all three directions, take Width in all three.
	Width, Length float64

	// Learned, if set, is called when an armour part's fit was learned from
	// versions of its slot that already exist for both bodies (see
	// variants_learned.go), with how many pairs it was learned from. Not
	// called for the measured fit.
	Learned func(pairs int)

	// noTemplate builds a cloak's bones from the target skeleton even when
	// the target has a cloak to take them from: for judging that against
	// the template.
	noTemplate bool

	// noLearn keeps armour parts to the measured fit (no slot choice), by
	// the base parts' bounding boxes, or their centres of mass and spreads
	// when byMass: for judging the fits against each other.
	noLearn, byMass bool
}

// withoutDir returns locs without the folder dir ("" leaves them as they are).
func withoutDir(locs []string, dir string) []string {
	if dir == "" {
		return locs
	}
	abs := func(p string) string {
		if a, err := filepath.Abs(p); err == nil {
			return filepath.Clean(a)
		}
		return filepath.Clean(p)
	}
	d := abs(dir)
	out := make([]string, 0, len(locs))
	for _, l := range locs {
		if abs(l) != d {
			out = append(out, l)
		}
	}
	return out
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

// robeGirth returns the across-bone scale of a robe for r, how much wider
// the target body's base part is than the source's (target over source).
// Robes hang looser than skin, so a body of about the same size passes on
// three quarters of the difference; one half or twice as wide (a custom
// race: a brownie, a half-ogre) all of it, blended in between (for r from
// 1.3 to 2, and 1/1.3 to 1/2). Against Bioware's own race versions of the
// stock robes 003, 004, 006 and 030: 3.89 cm mean per vertex (3.87 for half
// the difference, the earlier rule). Against a server's hand-made custom
// race versions of the same robes: brownies (110 robes, width 0.3 to 0.5 of
// a human's) 1.77 cm, against 4.21 for half; half-ogres (108, 1.6 to 2.4
// times) 7.05 cm, against 9.49.
func robeGirth(r float64) float64 {
	if r <= 0 {
		return 1
	}
	t := (math.Abs(math.Log(r)) - math.Log(1.3)) / (math.Log(2) - math.Log(1.3))
	t = math.Max(0, math.Min(1, t))
	return 1 + (0.75+0.25*t)*(r-1)
}

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
	if !bodyPrefix.MatchString(target) {
		return nil, fmt.Errorf("target %q is not a body prefix such as pmd0", target)
	}
	if src.RootNode() == nil {
		return nil, fmt.Errorf("%s has no node tree", src.Name)
	}
	if partAttachNode(src.Name) != "" {
		out, err := partVariant(src, srcPrefix, target, opts, warn)
		if err == nil {
			applyShape(out, target, opts.Shape, opts.Locs, warn)
			bodySuperModel(out, target, opts.Locs)
		}
		return out, err
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
	// After the source's number and the stock ones, any of that body's (a
	// custom race may have other numbers).
	candidates := append([]string{suffix}, templates...)
	var others []string
	for name := range listModels(opts.Locs) {
		if strings.HasPrefix(name, target+"_"+kind) && VariantKind(name) == kind {
			others = append(others, name[len(target):])
		}
	}
	sort.Strings(others)
	candidates = append(candidates, others...)
	for _, s := range candidates {
		if opts.noTemplate {
			break
		}
		locs := opts.Locs
		if s == suffix {
			locs = withoutDir(locs, opts.OutputDir)
		}
		ref := findModelResource(target+s, locs)
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
	srcSkel, _ := loadBodySkeleton(srcPrefix, opts.Locs)
	dstSkel, _ := loadBodySkeleton(target, opts.Locs)
	if tmpl == nil && kind == "cloak" && srcSkel != nil && dstSkel != nil {
		if m, ok := cloakBonesFromSkeleton(src, srcSkel, dstSkel, chestRatio(srcPrefix, target, opts.Locs)); ok {
			tmpl, tmplName = m, "the "+target+" skeleton"
			if !opts.noTemplate {
				warn(fmt.Sprintf("no cloak of %s to take the bones from (no %s_cloak* found): the cloak bones were placed by the %s skeleton's torso, neck and shoulders", target, target, target))
			}
		}
	}
	if tmpl == nil {
		return nil, fmt.Errorf("no %s of %s to take the bones from (no %s_%s* found); pass the game install or a hak with them", kind, target, target, kind)
	}
	if srcSkel == nil || dstSkel == nil {
		warn(fmt.Sprintf("skeleton %s or %s not found: shins and forearms keep their source length", srcPrefix, target))
	}

	tmpl = withSkeletonBones(tmpl, src, dstSkel)
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
			warn(fmt.Sprintf("bone %s is not in %s or the %s skeleton: the vertices weighted to it stay where they are", b, tmplName, target))
		}
	}

	// A mesh that is not a skin (a bone copy's own mesh, or one hanging from
	// a bone copy, as a veil from the head) is refitted by its own bone, or
	// failing that the nearest bone above it the template has.
	rigidBone := map[*Node]string{}
	{
		byName := map[string]*Node{}
		for _, n := range src.Nodes {
			byName[strings.ToLower(n.Name)] = n
		}
		for _, n := range src.Nodes {
			if n.Skin != nil || n.Mesh == nil || len(n.Mesh.Verts) == 0 || n == src.RootNode() {
				continue
			}
			for a, depth := n, 0; a != nil && a != src.RootNode() && depth < 64; a, depth = byName[strings.ToLower(a.Parent)], depth+1 {
				if _, ok := tmplNodes[strings.ToLower(a.Name)]; ok {
					rigidBone[n] = strings.ToLower(a.Name)
					break
				}
			}
		}
	}
	axisBones := map[string]bool{}
	for b := range bones {
		axisBones[b] = true
	}
	for _, b := range rigidBone {
		axisBones[b] = true
	}

	axes := boneAxes(src, tmplNodes, axisBones, srcSkel, dstSkel)
	width, length := opts.factors()
	for b, ax := range axes {
		ax.ratio *= length
		axes[b] = ax
	}
	girth := bodyGirth(srcPrefix, target, opts.Locs, warn)
	// A cloak's cloth on its own bones (Cloak_g and the nodes under it) takes
	// the torso's width: those bones have no stock body part, and the cloth
	// would otherwise keep the source body's size around them (on a brownie,
	// a collar ring standing off the neck). Against Bioware's cloaks 1 to 7
	// of every race: 1.8 cm per vertex in phenotype 0 and 4.0 cm in phenotype
	// 2, against 2.9 and 4.7 cm for widening the cloth by 0.4 of the chest's
	// width ratio, the rule this replaced.
	if kind == "cloak" {
		if t, ok := girth["torso_g"]; ok {
			for b := range cloakBones(src) {
				if _, has := girth[b]; !has {
					girth[b] = t
				}
			}
		}
	}
	rigidGirth := girthScales{}
	for b, v := range girth {
		rigidGirth[b] = v
	}
	rigidGirth.inherit(src, axisBones)

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

	// movePoint and moveNormal refit a world point or normal by bone b: into
	// the source bone's space, stretched along it and scaled across it, and
	// out of the template's bone.
	movePointBy := func(girth girthScales, b string, l [3]float64) ([3]float64, bool) {
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
	}
	moveNormalBy := func(girth girthScales, b string, l [3]float64) ([3]float64, bool) {
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
	}
	movePoint := func(b string, l [3]float64) ([3]float64, bool) { return movePointBy(girth, b, l) }
	moveNormal := func(b string, l [3]float64) ([3]float64, bool) { return moveNormalBy(girth, b, l) }

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
			q := blendBones(p, ws, movePoint)
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
				q := blendBones(nm, ws, moveNormal)
				normals[vi] = vec3f(norm3(quatRotate(quatConj(dstW.rot), norm3(q))))
			}
			dst.Mesh.Normals = normals
		}
	}

	// The other meshes (the bone copies' own, render 0 but most casting the
	// robe's shadow) are refitted like a skin vertex weighted wholly to
	// their bone: Bioware's race versions reshape them too (stock gnome robe 3).
	for i, n := range src.Nodes {
		dst := out.Nodes[i]
		b, ok := rigidBone[n]
		if !ok {
			continue
		}
		nW, dstW := srcW.of(n.Name), outW.of(dst.Name)
		verts := make([]Vec3, len(n.Mesh.Verts))
		for vi, v := range n.Mesh.Verts {
			q, _ := movePointBy(rigidGirth, b, nW.apply(vec3d(v)))
			verts[vi] = vec3f(dstW.unapply(q))
		}
		dst.Mesh.Verts = verts
		if len(n.Mesh.Normals) == len(n.Mesh.Verts) {
			normals := make([]Vec3, len(n.Mesh.Normals))
			for vi, nv := range n.Mesh.Normals {
				q, _ := moveNormalBy(rigidGirth, b, quatRotate(nW.rot, vec3d(nv)))
				normals[vi] = vec3f(norm3(quatRotate(quatConj(dstW.rot), norm3(q))))
			}
			dst.Mesh.Normals = normals
		}
	}
	applyShape(out, target, opts.Shape, opts.Locs, warn)
	bodySuperModel(out, target, opts.Locs)
	return out, nil
}

// bodySuperModel points a variant whose supermodel is its body (pmb2_cloak_001
// on pmb2) at the skeleton that body uses. A phenotype with no skeleton of
// its own uses phenotype 0's (Layonara's Large brownies, pmb2, use pmb0), so
// there is no model by the body's own name for the supermodel to find.
func bodySuperModel(out *Model, target string, locs []string) {
	if strings.EqualFold(out.SuperModel, target) {
		out.SuperModel = bodySkeleton(target, locs)
	}
}

// cloakBonesFromSkeleton builds a cloak template for a body that has no
// cloak, from the source cloak's own bones: the tree moved so that torso_g
// is where the target skeleton has it, and the offsets below torso_g scaled,
// target over source: across and front to back by the base chest part's
// spread (chest, from chestRatio; failing that both by the biceps' distance
// from the torso); up and down by the torso-to-neck height for the bones on
// torso_g (Cloak_g), and by the neck-to-feet height for the rest (the
// cloak's hanging chains). In Bioware's cloaks of every race, Cloak_g sits
// just under the neck, the shoulder bones scale with the body's width (0.87
// to 1.48 of the human's for the gnome to the half-orc; biceps 0.90 to 1.50),
// and the chains' length with the neck-to-feet height (0.69 to 1.10, against
// 0.68 to 1.10; the female half-orc's is the exception, 1.28 against 1.11).
// Over Bioware's stock cloaks 1 to 7, every race and phenotype, the cloth
// made this way is 4.2 cm from Bioware's per vertex, against 2.7 cm with
// Bioware's cloak of that body as the template. ok is false when the
// skeletons lack torso_g, neck_g, the biceps or the feet.
// chestRatio is the target's base chest part's spread across (x) and front
// to back (y) over the source's, or zero when either is not found.
func chestRatio(srcPrefix, target string, locs []string) [2]float64 {
	a, b := partSpread(basePart(srcPrefix, "chest", locs), locs), partSpread(basePart(target, "chest", locs), locs)
	if a == nil || b == nil || a[0] <= 0 || a[1] <= 0 {
		return [2]float64{}
	}
	return [2]float64{b[0] / a[0], b[1] / a[1]}
}

func cloakBonesFromSkeleton(src *Model, srcSkel, dstSkel *skeleton, chest [2]float64) (*Model, bool) {
	at := func(sk *skeleton, name string) ([3]float64, bool) {
		i, ok := sk.index[name]
		if !ok {
			return [3]float64{}, false
		}
		return sk.pos[i], true
	}
	type frame struct{ torso, neck, lb, rb, lf, rf [3]float64 }
	get := func(sk *skeleton) (f frame, ok bool) {
		var ok1, ok2, ok3, ok4, ok5, ok6 bool
		f.torso, ok1 = at(sk, "torso_g")
		f.neck, ok2 = at(sk, "neck_g")
		f.lb, ok3 = at(sk, "lbicep_g")
		f.rb, ok4 = at(sk, "rbicep_g")
		f.lf, ok5 = at(sk, "lfoot_g")
		f.rf, ok6 = at(sk, "rfoot_g")
		return f, ok1 && ok2 && ok3 && ok4 && ok5 && ok6
	}
	a, ok1 := get(srcSkel)
	b, ok2 := get(dstSkel)
	if !ok1 || !ok2 {
		return nil, false
	}
	width := func(f frame) float64 { return (math.Abs(f.lb[0]-f.torso[0]) + math.Abs(f.rb[0]-f.torso[0])) / 2 }
	drop := func(f frame) float64 { return f.neck[2] - (f.lf[2]+f.rf[2])/2 }
	if width(a) < 1e-4 || math.Abs(a.neck[2]-a.torso[2]) < 1e-4 || drop(a) < 1e-4 {
		return nil, false
	}
	sx, sy := width(b)/width(a), width(b)/width(a)
	if chest[0] > 0 && chest[1] > 0 {
		sx, sy = chest[0], chest[1]
	}
	szNeck := (b.neck[2] - b.torso[2]) / (a.neck[2] - a.torso[2])
	szDrop := drop(b) / drop(a)

	// From the source cloak's own torso_g, not its skeleton's: Bioware's
	// female human cloaks put it 5 cm back and 1.5 cm down from pfh0's.
	srcTorso, ok := newWorldTransforms(src).lookup("torso_g")
	if !ok {
		return nil, false
	}
	at0 := srcTorso.apply([3]float64{})
	shift := [3]float64{b.torso[0] - at0[0], b.torso[1] - at0[1], b.torso[2] - at0[2]}
	out := *src
	out.Nodes = make([]*Node, 0, len(src.Nodes))
	byName := map[string]*Node{}
	for _, n := range src.Nodes {
		byName[strings.ToLower(n.Name)] = n
	}
	below := func(n *Node) bool {
		for p, depth := byName[strings.ToLower(n.Parent)], 0; p != nil && depth < 64; p, depth = byName[strings.ToLower(p.Parent)], depth+1 {
			if strings.EqualFold(p.Name, "torso_g") {
				return true
			}
		}
		return false
	}
	for _, n := range src.Nodes {
		if n.Skin != nil {
			continue
		}
		c := &Node{Name: n.Name, Parent: n.Parent, Position: n.Position, Orientation: n.Orientation, Scale: 1}
		switch {
		case n == src.RootNode():
		case strings.EqualFold(n.Parent, src.RootNode().Name):
			// The top of the tree (rootdummy) moves with the torso.
			c.Position = Vec3{X: n.Position.X + float32(shift[0]), Y: n.Position.Y + float32(shift[1]), Z: n.Position.Z + float32(shift[2])}
		case below(n):
			sz := szDrop
			if strings.EqualFold(n.Parent, "torso_g") {
				sz = szNeck
			}
			c.Position = Vec3{X: n.Position.X * float32(sx), Y: n.Position.Y * float32(sy), Z: n.Position.Z * float32(sz)}
		}
		out.Nodes = append(out.Nodes, c)
	}
	return &out, true
}

// withSkeletonBones returns tmpl, or a copy of it with the bones src has and
// tmpl lacks taken from the target body's skeleton, under their parent in
// tmpl. A robe template can lack bones the source robe uses: Layonara's
// brownie robe 3 has no feet, where human robe 26 weights its hem to them;
// without them those vertices and the foot bone copies keep the human body's
// size and place.
func withSkeletonBones(tmpl, src *Model, skel *skeleton) *Model {
	if skel == nil {
		return tmpl
	}
	have := map[string]string{}
	for _, n := range tmpl.Nodes {
		have[strings.ToLower(n.Name)] = n.Name
	}
	var added []*Node
	for _, n := range src.Nodes {
		name := strings.ToLower(n.Name)
		if _, ok := have[name]; ok || n == src.RootNode() {
			continue
		}
		i, ok := skel.index[name]
		if !ok {
			continue
		}
		sn := skel.order[i]
		parent, ok := have[strings.ToLower(sn.Parent)]
		if !ok {
			continue
		}
		added = append(added, &Node{Name: sn.Name, Parent: parent, Position: sn.Position, Orientation: sn.Orientation, Scale: 1})
		have[name] = sn.Name
	}
	if len(added) == 0 {
		return tmpl
	}
	out := *tmpl
	out.Nodes = append(append([]*Node(nil), tmpl.Nodes...), added...)
	return &out
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

// inherit gives each of bones that has no girth of its own (no stock body
// part on it: feet, neck, head) its nearest ancestor's in m, so a foot bone
// copy or a hem weighted to the feet narrows with the shin, and a head bone
// copy with the torso.
func (g girthScales) inherit(m *Model, bones map[string]bool) {
	parent := map[string]string{}
	for _, n := range m.Nodes {
		parent[strings.ToLower(n.Name)] = strings.ToLower(n.Parent)
	}
	for b := range bones {
		if _, ok := g[b]; ok {
			continue
		}
		for a, depth := parent[b], 0; a != "" && depth < 64; a, depth = parent[a], depth+1 {
			if s, ok := g[a]; ok {
				g[b] = s
				break
			}
		}
	}
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
		na, nb := basePart(srcPrefix, part, locs), basePart(target, part, locs)
		a, b := partSpread(na, locs), partSpread(nb, locs)
		if a == nil || a[0] <= 0 || a[1] <= 0 {
			missing = append(missing, na)
		}
		if b == nil {
			missing = append(missing, nb)
		}
		if a == nil || b == nil || a[0] <= 0 || a[1] <= 0 {
			continue
		}
		g[bone] = [2]float64{robeGirth(b[0] / a[0]), robeGirth(b[1] / a[1])}
	}
	for _, side := range []string{"l", "r"} {
		if s, ok := g[side+"thigh_g"]; ok {
			g[side+"shin_g"] = s
			g[side+"foot_g"] = s
		}
	}
	if len(missing) > 0 {
		sort.Strings(missing)
		warn(fmt.Sprintf("base body parts %s not found: those bones keep their width", strings.Join(missing, ", ")))
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
// partBox returns the centre of a body part model's bounding box and its
// size along x, y and z; ok is false when the part is not found.
func partBox(name string, locs []string) (centre, size [3]float64, ok bool) {
	ref := findModelResource(name, locs)
	if ref == nil {
		return centre, size, false
	}
	m, err := readModelRef(ref)
	if err != nil {
		return centre, size, false
	}
	w := newWorldTransforms(m)
	lo := [3]float64{math.Inf(1), math.Inf(1), math.Inf(1)}
	hi := [3]float64{math.Inf(-1), math.Inf(-1), math.Inf(-1)}
	for _, n := range m.Nodes {
		if n.Mesh == nil || n.Aabb != nil || n.Mesh.Render == 0 {
			continue
		}
		t := w.of(n.Name)
		for _, v := range n.Mesh.Verts {
			p := t.apply(vec3d(v))
			for k := 0; k < 3; k++ {
				lo[k], hi[k] = math.Min(lo[k], p[k]), math.Max(hi[k], p[k])
			}
		}
	}
	if math.IsInf(lo[0], 1) {
		return centre, size, false
	}
	for k := 0; k < 3; k++ {
		centre[k], size[k] = (lo[k]+hi[k])/2, hi[k]-lo[k]
	}
	return centre, size, true
}

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

// compose returns r applied after s.
func (r rigid) compose(s rigid) rigid {
	return rigid{rot: quatMul(r.rot, s.rot), pos: r.apply(s.pos)}
}

// inverse returns the transform that undoes r.
func (r rigid) inverse() rigid {
	c := quatConj(r.rot)
	return rigid{rot: c, pos: quatRotate(c, scale3(r.pos, -1))}
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

// cloakBones returns the names of a cloak's own bones: Cloak_g and every
// node under it.
func cloakBones(src *Model) map[string]bool {
	children := map[string][]string{}
	for _, n := range src.Nodes {
		children[strings.ToLower(n.Parent)] = append(children[strings.ToLower(n.Parent)], strings.ToLower(n.Name))
	}
	bones := map[string]bool{}
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
	return bones
}
