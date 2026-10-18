package mdl

import (
	"fmt"
	"strings"
)

// Body part variants. A body part (pmh0_chest050) hangs from one node of the
// creature skeleton (torso_g, see partAttachNodes), and its versions for the
// other races (pmd0_chest050, ...) are the same mesh refitted to that race's
// body, as Bioware shipped them. partVariant does the refit in the bone's
// space:
//   - along the bone, by how much longer or shorter the target skeleton's
//     bone is (the offset to its child bones), on the far side of the joint
//     only;
//   - across the bone (x and y) by how much wider or narrower the target
//     race's base body part for that slot is (pmd0_chest001 against
//     pmh0_chest001), about the source base part's centre, which moves to
//     the target's: a heavier body is not just wider, its mass sits
//     elsewhere. Armour sits close to the body, so the full ratio is used,
//     where robes take half.
//
// Skinned parts (Beamdog's HD parts) are bound to the creature skeleton:
// each vertex is refitted that way in the space of every skeleton bone it is
// weighted to (each bone with its own slot's base part), carried from the
// source skeleton to the target's, and blended by its weights. Against
// Beamdog's own race versions of their HD parts: 2.6 cm mean per vertex in
// phenotype 0 and 3.4 cm in phenotype 2, against 5.4 and 5.2 cm unchanged.
//
// Feet, hands, shoulder pads and belts do not run along a bone; they scale
// in all three directions by their base part. A bone's length is the offset
// to its child joints only (the nodes body parts hang from, and the head):
// effect nodes such as impact, cloak_g and wings under torso_g are not part of
// the body, and the female skeletons have two impact nodes there.
//
// Checked against Bioware's own race versions of stock parts (2 to 5 part
// numbers per slot, both genders, all six races), mean distance per
// vertex 1.0 cm in phenotype 0 (feet 0.0, hands 0.5, forearms 0.8, thighs
// 0.8, chests 0.9, shins 1.1, belts 1.1, biceps 1.1, necks 1.2, pelvis 1.2,
// shoulder pads 1.8) and 2.2 cm in phenotype 2, where the unchanged human
// parts are 2 to 7 cm off.

// skinGirthStrength is the share of the base parts' width ratio applied to
// each bone of a skinned part. Rigid parts take all of it; a skinned part's
// vertices blend several bones, and against Beamdog's own race versions of
// their HD parts half fitted best (2.6 cm mean per vertex in phenotype 0,
// 3.4 cm in phenotype 2, against 5.4 and 5.2 cm for the unchanged human part;
// all of it gave 2.6 and 3.9 cm).
const skinGirthStrength = 0.5

// partScalesAllAxes are the slots that do not run along their bone.
var partScalesAllAxes = map[string]bool{
	"footl": true, "footr": true, "handl": true, "handr": true,
	"shol": true, "shor": true, "belt": true,
}

// isBodyJoint reports whether a skeleton node is a joint of the body: a node
// body parts hang from, or the head.
func isBodyJoint(name string) bool {
	name = strings.ToLower(name)
	if name == "head_g" {
		return true
	}
	for _, n := range partAttachNodes {
		if n == name {
			return true
		}
	}
	return false
}

// partVariant returns src, a body part for the body srcPrefix, refitted to
// the body target. See the comment above. Rigid meshes are refitted in the
// space of the bone the part hangs from. Skins (Beamdog's HD parts) are
// bound in game to the creature skeleton, so each vertex is refitted the same
// way in the space of every skeleton bone it is weighted to, moved to the
// target skeleton's bone, and blended by its weights.
func partVariant(src *Model, srcPrefix, target string, opts VariantOptions, warn func(string)) (*Model, error) {
	locs := opts.Locs
	width, length := opts.factors()
	attach := partAttachNode(src.Name)
	srcSkel, _ := loadBodySkeleton(srcPrefix, locs)
	dstSkel, _ := loadBodySkeleton(target, locs)
	newFits := func(strength float64) *boneFits {
		return &boneFits{srcPrefix: srcPrefix, target: target, locs: locs, srcSkel: srcSkel, dstSkel: dstSkel, warn: warn, cache: map[string]*boneFit{}, strength: strength, width: width, length: length}
	}
	rigidFit := newFits(1).of(attach, partKind(src.Name))
	fits := newFits(skinGirthStrength)

	hasSkin := false
	for _, n := range src.Nodes {
		if n.Skin != nil && n.Mesh != nil {
			hasSkin = true
		}
	}
	var srcAttach, dstAttach rigid
	if hasSkin {
		i, okS := skeletonIndex(srcSkel, attach)
		j, okD := skeletonIndex(dstSkel, attach)
		if !okS || !okD {
			return nil, fmt.Errorf("%s is skinned: it needs the skeletons %s and %s with %s (pass the game install or the hak that has them)", src.Name, srcPrefix, target, attach)
		}
		srcAttach = rigid{rot: srcSkel.rot[i], pos: srcSkel.pos[i]}
		dstAttach = rigid{rot: dstSkel.rot[j], pos: dstSkel.pos[j]}
	}

	out := cloneForBody(src, srcPrefix, target)
	w := newWorldTransforms(src)
	for i, n := range src.Nodes {
		if n.Mesh == nil || len(n.Mesh.Verts) == 0 {
			continue
		}
		x := w.of(n.Name)
		dst := out.Nodes[i]
		skinned := n.Skin != nil
		verts := make([]Vec3, len(n.Mesh.Verts))
		for vi, v := range n.Mesh.Verts {
			p := x.apply(vec3d(v))
			if skinned {
				var ws VertexWeight
				if vi < len(n.Skin.Weights) {
					ws = n.Skin.Weights[vi]
				}
				q := blendBones(srcAttach.apply(p), ws, func(b string, sp [3]float64) ([3]float64, bool) {
					f := fits.of(b, "")
					if !f.bound {
						// Keep it with the part: the part's own fit, relative
						// to the bone it hangs from.
						fits.note("bone "+b+" is not in both skeletons", ": the vertices weighted to it move with the part")
						return dstAttach.apply(rigidFit.point(srcAttach.unapply(sp))), true
					}
					return f.dst.apply(f.point(f.src.unapply(sp))), true
				})
				p = dstAttach.unapply(q)
			} else {
				p = rigidFit.point(p)
			}
			verts[vi] = vec3f(x.unapply(p))
		}
		dst.Mesh.Verts = verts
		if len(n.Mesh.Normals) == len(n.Mesh.Verts) {
			normals := make([]Vec3, len(n.Mesh.Normals))
			for vi, nv := range n.Mesh.Normals {
				l := quatRotate(x.rot, vec3d(nv))
				if skinned {
					var ws VertexWeight
					if vi < len(n.Skin.Weights) {
						ws = n.Skin.Weights[vi]
					}
					q := blendBones(quatRotate(srcAttach.rot, l), ws, func(b string, sn [3]float64) ([3]float64, bool) {
						f := fits.of(b, "")
						if !f.bound {
							return quatRotate(dstAttach.rot, rigidFit.normal(quatRotate(quatConj(srcAttach.rot), sn))), true
						}
						return quatRotate(f.dst.rot, f.normal(quatRotate(quatConj(f.src.rot), sn))), true
					})
					l = quatRotate(quatConj(dstAttach.rot), norm3(q))
				} else {
					l = rigidFit.normal(l)
				}
				normals[vi] = vec3f(norm3(quatRotate(quatConj(x.rot), l)))
			}
			dst.Mesh.Normals = normals
		}
	}
	return out, nil
}

// boneFit is one bone's refit in its own space: a stretch along the bone
// (far side of the joint only), then a scale about the source base part's
// centre ca to the target's cb, and, for skinned parts, the bone's source and
// target skeleton transforms.
type boneFit struct {
	ax       boneAxis
	g        [3]float64
	ca, cb   [3]float64
	src, dst rigid
	bound    bool // src and dst are set: the bone is in both skeletons
}

func (f *boneFit) point(p [3]float64) [3]float64 {
	if d := dot3(p, f.ax.dir); d > 0 {
		p = add3(p, scale3(f.ax.dir, (f.ax.ratio-1)*d))
	}
	for k := range p {
		p[k] = (p[k]-f.ca[k])*f.g[k] + f.cb[k]
	}
	return p
}

func (f *boneFit) normal(n [3]float64) [3]float64 {
	n = add3(n, scale3(f.ax.dir, (1/f.ax.ratio-1)*dot3(n, f.ax.dir)))
	return norm3([3]float64{n[0] / f.g[0], n[1] / f.g[1], n[2] / f.g[2]})
}

// boneFits works out and caches the fit of each bone.
type boneFits struct {
	srcPrefix, target string
	locs              []string
	srcSkel, dstSkel  *skeleton
	warn              func(string)
	cache             map[string]*boneFit
	warned            map[string]bool
	strength          float64 // share of the base parts' width ratio applied
	width, length     float64 // VariantOptions.Width and Length
}

// of returns the fit of bone, whose body part slot is slot ("" to look it up
// from the bone: torso_g is the chest's, lthigh_g the left thigh's). For a
// bone in both skeletons the fit also holds its skeleton transforms (bound).
func (fs *boneFits) of(bone, slot string) *boneFit {
	key := strings.ToLower(bone)
	if f, ok := fs.cache[key]; ok {
		return f
	}
	if slot == "" {
		slot = slotOfBone(key)
	}
	f := &boneFit{ax: boneAxis{dir: [3]float64{0, 0, 1}, ratio: 1}, g: [3]float64{1, 1, 1}}
	i, okS := skeletonIndex(fs.srcSkel, key)
	j, okD := skeletonIndex(fs.dstSkel, key)
	if okS && okD {
		f.src = rigid{rot: fs.srcSkel.rot[i], pos: fs.srcSkel.pos[i]}
		f.dst = rigid{rot: fs.dstSkel.rot[j], pos: fs.dstSkel.pos[j]}
		f.bound = true
	}
	allAxes := partScalesAllAxes[slot]
	if !allAxes {
		if a, ok := skeletonBoneAxis(fs.srcSkel, fs.dstSkel, key); ok {
			f.ax = a
		} else if slot != "" {
			fs.note("skeleton "+fs.srcPrefix+" or "+fs.target+" not found, or "+key+" has no child joint in it", ": the part keeps its length along "+key)
		}
	}
	if slot != "" {
		na, nb := basePart(fs.srcPrefix, slot, fs.locs), basePart(fs.target, slot, fs.locs)
		ca, sa, okA := partStats(na, fs.locs)
		cb, sb, okB := partStats(nb, fs.locs)
		if !okA || !okB {
			var missing []string
			if !okA {
				missing = append(missing, na)
			}
			if !okB {
				missing = append(missing, nb)
			}
			fs.note("base body part "+strings.Join(missing, " and ")+" not found", ": the part keeps its width there")
		} else {
			for k := range f.g {
				if (k < 2 || allAxes) && sa[k] > 1e-4 {
					f.g[k] = 1 + fs.strength*(sb[k]/sa[k]-1)
					f.ca[k], f.cb[k] = ca[k], ca[k]+fs.strength*(cb[k]-ca[k])
				}
			}
		}
	}
	if !allAxes {
		f.ax.ratio *= fs.length
	}
	for k := range f.g {
		if k < 2 || allAxes {
			f.g[k] *= fs.width
		}
	}
	fs.cache[key] = f
	return f
}

func (fs *boneFits) note(what, then string) {
	if fs.warned == nil {
		fs.warned = map[string]bool{}
	}
	if !fs.warned[what] {
		fs.warned[what] = true
		fs.warn(what + then)
	}
}

// slotOfBone returns the body part slot that hangs from a skeleton bone
// (chest for torso_g), or "" for a bone no part hangs from.
func slotOfBone(bone string) string {
	for slot, b := range partAttachNodes {
		if b == bone {
			return slot
		}
	}
	return ""
}

// skeletonIndex returns a node's tree position in sk.
func skeletonIndex(sk *skeleton, name string) (int, bool) {
	if sk == nil {
		return 0, false
	}
	i, ok := sk.index[strings.ToLower(name)]
	return i, ok
}

// skeletonBoneAxis returns a bone's stretch from the race skeletons: the
// direction to its child joints (isBodyJoint) and how much longer the
// target's offset to them is. ok is false when a skeleton is missing or the
// bone has no child joint in both.
func skeletonBoneAxis(srcSkel, dstSkel *skeleton, bone string) (boneAxis, bool) {
	if srcSkel == nil || dstSkel == nil {
		return boneAxis{}, false
	}
	var sa, sb [3]float64
	n := 0
	for _, c := range srcSkel.model.Nodes {
		if !strings.EqualFold(c.Parent, bone) || !isBodyJoint(c.Name) {
			continue
		}
		if i, ok := dstSkel.index[strings.ToLower(c.Name)]; ok {
			sa, sb = add3(sa, vec3d(c.Position)), add3(sb, vec3d(dstSkel.order[i].Position))
			n++
		}
	}
	if n == 0 || len3(sa) < 1e-4*float64(n) {
		return boneAxis{}, false
	}
	return boneAxis{dir: norm3(sa), ratio: len3(sb) / len3(sa)}, true
}
