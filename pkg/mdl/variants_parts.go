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
// the body target. See the comment above.
func partVariant(src *Model, srcPrefix, target string, locs []string, warn func(string)) (*Model, error) {
	for _, n := range src.Nodes {
		if n.Skin != nil {
			return nil, fmt.Errorf("%s has skinned meshes; variants of skinned body parts are not supported yet", src.Name)
		}
	}
	slot := partKind(src.Name)
	bone := partAttachNode(src.Name)
	allAxes := partScalesAllAxes[slot]

	ax := boneAxis{dir: [3]float64{0, 0, 1}, ratio: 1}
	if !allAxes {
		srcSkel, _ := loadSkeleton(srcPrefix, locs)
		dstSkel, _ := loadSkeleton(target, locs)
		if a, ok := skeletonBoneAxis(srcSkel, dstSkel, bone); ok {
			ax = a
		} else {
			warn(fmt.Sprintf("skeleton %s or %s not found, or %s has no child bone in it: the part keeps its length", srcPrefix, target, bone))
		}
	}

	// Scaling across the bone: p -> (p - ca) * g + cb on the scaled axes.
	g := [3]float64{1, 1, 1}
	base := slot + "001"
	ca, sa, okA := partStats(srcPrefix+"_"+base, locs)
	cb, sb, okB := partStats(target+"_"+base, locs)
	if !okA || !okB {
		warn(fmt.Sprintf("base body part %s_%s or %s_%s not found: the part keeps its width", srcPrefix, base, target, base))
		ca, cb = [3]float64{}, [3]float64{}
	} else {
		for k := range g {
			if (k < 2 || allAxes) && sa[k] > 1e-4 {
				g[k] = sb[k] / sa[k]
			} else {
				ca[k], cb[k] = 0, 0
			}
		}
	}

	out := cloneForBody(src, srcPrefix, target)
	w := newWorldTransforms(src)
	for i, n := range src.Nodes {
		if n.Mesh == nil || len(n.Mesh.Verts) == 0 {
			continue
		}
		x := w.of(n.Name)
		dst := out.Nodes[i]
		verts := make([]Vec3, len(n.Mesh.Verts))
		for vi, v := range n.Mesh.Verts {
			p := x.apply(vec3d(v))
			if d := dot3(p, ax.dir); d > 0 {
				p = add3(p, scale3(ax.dir, (ax.ratio-1)*d))
			}
			for k := range p {
				p[k] = (p[k]-ca[k])*g[k] + cb[k]
			}
			verts[vi] = vec3f(x.unapply(p))
		}
		dst.Mesh.Verts = verts
		if len(n.Mesh.Normals) == len(n.Mesh.Verts) {
			normals := make([]Vec3, len(n.Mesh.Normals))
			for vi, nv := range n.Mesh.Normals {
				l := quatRotate(x.rot, vec3d(nv))
				l = add3(l, scale3(ax.dir, (1/ax.ratio-1)*dot3(l, ax.dir)))
				l = norm3([3]float64{l[0] / g[0], l[1] / g[1], l[2] / g[2]})
				normals[vi] = vec3f(norm3(quatRotate(quatConj(x.rot), l)))
			}
			dst.Mesh.Normals = normals
		}
	}
	return out, nil
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
