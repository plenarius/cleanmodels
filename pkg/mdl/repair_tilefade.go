// Tile-fade repair: slice trimesh/skin geometry at a world Z plane so upper
// portions can use tilefade=1 independently of the base mesh (tilefade=0).
//
// Ref: plenarius/cleanmodels/tilefade.pl — predicate t_slice_trimesh/6 and
// related helpers for edge/plane intersection, per-face classification, and
// splitting nodes by tilefade group.
package mdl

import (
	"fmt"
	"math"
	"sort"
	"strings"
)

const tilefadeZEpsilon = float32(1e-4)

// SliceTileFade processes a TILE model's mesh nodes, splitting geometry at the
// given Z height to separate tile-fadeable portions from non-fadeable ones.
// Only operates on TILE classification models.
// Ref: tilefade.pl t_slice_trimesh/6
func SliceTileFade(model *Model, sliceZ float32) []string {
	var msgs []string
	if model == nil {
		return append(msgs, "slice tilefade: nil model")
	}
	if !strings.EqualFold(model.Classification, "TILE") {
		return msgs
	}

	for i := 0; i < len(model.Nodes); i++ {
		n := model.Nodes[i]
		if n == nil || n.Mesh == nil {
			continue
		}
		if n.AnimMesh != nil || n.Dangly != nil || n.Aabb != nil || n.Emitter != nil || n.Light != nil || n.Reference != nil {
			continue
		}
		// trimesh or skin (both carry MeshData on Mesh)

		if !meshCrossesTileFadeZ(model, n, n.Mesh, sliceZ) {
			continue
		}

		above, below, skAbove, skBelow, err := splitMeshAtWorldZ(model, n, n.Mesh, sliceZ)
		if err != nil {
			msgs = append(msgs, fmt.Sprintf("slice tilefade: node %q — %v", n.Name, err))
			continue
		}

		baseName := n.Name
		name0 := uniqueMeshNodeName(model, baseName+"__tf0")
		name1 := uniqueMeshNodeName(model, baseName+"__tf1")

		nodeBelow := cloneNodeForTileFadeSplit(n, name0, below, skBelow, 0)
		nodeAbove := cloneNodeForTileFadeSplit(n, name1, above, skAbove, 1)

		reparentNodes(model, baseName, name0)

		model.Nodes[i] = nodeBelow
		model.Nodes = append(model.Nodes[:i+1], append([]*Node{nodeAbove}, model.Nodes[i+1:]...)...)

		msgs = append(msgs, fmt.Sprintf(
			"slice tilefade: node %q split at z=%g into %q (tilefade=0, %d faces) and %q (tilefade=1, %d faces)",
			baseName, sliceZ, name0, len(below.Faces), name1, len(above.Faces),
		))
		i++ // skip the inserted node we just processed
	}

	return msgs
}

func uniqueMeshNodeName(m *Model, base string) string {
	if m.FindNode(base) == nil {
		return base
	}
	for k := 2; k < 10000; k++ {
		cand := fmt.Sprintf("%s_%d", base, k)
		if m.FindNode(cand) == nil {
			return cand
		}
	}
	return base + "_n"
}

func reparentNodes(m *Model, oldParent, newParent string) {
	for _, x := range m.Nodes {
		if x != nil && strings.EqualFold(x.Parent, oldParent) {
			x.Parent = newParent
		}
	}
	for ai := range m.Animations {
		for ni := range m.Animations[ai].Nodes {
			an := &m.Animations[ai].Nodes[ni]
			if strings.EqualFold(an.Parent, oldParent) {
				an.Parent = newParent
			}
		}
	}
}

func cloneNodeForTileFadeSplit(src *Node, newName string, mesh *MeshData, skin *SkinData, tileFade int32) *Node {
	if mesh != nil {
		mesh.TileFade = tileFade
	}
	dst := &Node{
		Name:         newName,
		Parent:       src.Parent,
		PartNumber:   src.PartNumber,
		Position:     src.Position,
		Orientation:  src.Orientation,
		Scale:        src.Scale,
		InheritColor: src.InheritColor,
		WireColor:    src.WireColor,
		Mesh:         mesh,
		Skin:         skin,
	}
	return dst
}

// --- world / local transform (tile-relative: node position + orientation + scale) ---

// ancestorChain returns the chain of nodes from n up to root, with cycle detection.
func ancestorChain(m *Model, n *Node) []*Node {
	var chain []*Node
	visited := make(map[string]bool)
	for nd := n; nd != nil; {
		key := strings.ToLower(nd.Name)
		if visited[key] {
			break
		}
		visited[key] = true
		chain = append(chain, nd)
		if nd.Parent == "" || strings.EqualFold(nd.Parent, "NULL") {
			break
		}
		nd = m.FindNode(nd.Parent)
	}
	return chain
}

// localToParent maps a point from this node's local space into its parent's space.
func localToParent(n *Node, v Vec3) Vec3 {
	s := n.Scale
	if s == 0 {
		s = 1
	}
	sv := Vec3{X: v.X * s, Y: v.Y * s, Z: v.Z * s}
	q := axisAngleToQuat(n.Orientation)
	rv := quatRotateVec(q, sv)
	return Vec3{X: rv.X + n.Position.X, Y: rv.Y + n.Position.Y, Z: rv.Z + n.Position.Z}
}

// parentToLocal maps a point from this node's parent space into this node's local space.
func parentToLocal(n *Node, p Vec3) Vec3 {
	t := Vec3{X: p.X - n.Position.X, Y: p.Y - n.Position.Y, Z: p.Z - n.Position.Z}
	q := axisAngleToQuat(n.Orientation)
	rv := quatRotateVec(quatConj(q), t)
	s := n.Scale
	if s == 0 {
		s = 1
	}
	return Vec3{X: rv.X / s, Y: rv.Y / s, Z: rv.Z / s}
}

func localMeshVertexToWorld(m *Model, n *Node, v Vec3) Vec3 {
	chain := ancestorChain(m, n)
	cur := v
	for _, nd := range chain {
		cur = localToParent(nd, cur)
	}
	return cur
}

func worldToMeshVertexLocal(m *Model, n *Node, w Vec3) Vec3 {
	chain := ancestorChain(m, n)
	p := w
	for i := len(chain) - 1; i >= 0; i-- {
		p = parentToLocal(chain[i], p)
	}
	return p
}

func localMeshNormalToWorld(m *Model, n *Node, nl Vec3) Vec3 {
	chain := ancestorChain(m, n)
	nw := nl
	for _, nd := range chain {
		q := axisAngleToQuat(nd.Orientation)
		nw = quatRotateVec(q, nw)
	}
	return vecNormalize(nw)
}

func worldNormalToMeshLocal(m *Model, n *Node, nw Vec3) Vec3 {
	chain := ancestorChain(m, n)
	nl := nw
	for i := len(chain) - 1; i >= 0; i-- {
		q := axisAngleToQuat(chain[i].Orientation)
		nl = quatRotateVec(quatConj(q), nl)
	}
	return vecNormalize(nl)
}

func meshCrossesTileFadeZ(m *Model, n *Node, mesh *MeshData, sliceZ float32) bool {
	for fi := range mesh.Faces {
		f := &mesh.Faces[fi]
		z0 := localMeshVertexToWorld(m, n, meshVert(mesh, f.Verts[0])).Z
		z1 := localMeshVertexToWorld(m, n, meshVert(mesh, f.Verts[1])).Z
		z2 := localMeshVertexToWorld(m, n, meshVert(mesh, f.Verts[2])).Z
		a0 := zAboveSlice(z0, sliceZ)
		a1 := zAboveSlice(z1, sliceZ)
		a2 := zAboveSlice(z2, sliceZ)
		if a0 != a1 || a1 != a2 || a0 != a2 {
			return true
		}
	}
	return false
}

func meshVert(mesh *MeshData, idx int32) Vec3 {
	if idx < 0 || int(idx) >= len(mesh.Verts) {
		return Vec3{}
	}
	return mesh.Verts[idx]
}

func zAboveSlice(worldZ, sliceZ float32) bool {
	return worldZ > sliceZ+tilefadeZEpsilon
}

func quatConj(q Vec4) Vec4 {
	return Vec4{X: -q.X, Y: -q.Y, Z: -q.Z, W: q.W}
}

// quatRotateVec applies a unit quaternion (x,y,z,w) to vector v.
func quatRotateVec(q Vec4, v Vec3) Vec3 {
	bx, by, bz, bw := q.X, q.Y, q.Z, q.W
	tx := float32(2) * (by*v.Z - bz*v.Y)
	ty := float32(2) * (bz*v.X - bx*v.Z)
	tz := float32(2) * (bx*v.Y - by*v.X)
	return Vec3{
		X: v.X + bw*tx + (by*tz - bz*ty),
		Y: v.Y + bw*ty + (bz*tx - bx*tz),
		Z: v.Z + bw*tz + (bx*ty - by*tx),
	}
}

// --- bundled vertex (per-corner data while clipping) ---

type tileFadeVert struct {
	Pos    Vec3
	Norm   Vec3
	UV     Vec3
	UV1    Vec3
	UV2    Vec3
	UV3    Vec3
	Color  Vec3
	Tan    Vec4
	Weight VertexWeight
}

func readTileFadeVert(mesh *MeshData, skin *SkinData, vi, uvi int32) tileFadeVert {
	v := tileFadeVert{
		Norm: Vec3{0, 0, 1},
		UV:   Vec3{},
		Tan:  Vec4{0, 0, 1, 1},
	}
	if vi >= 0 && int(vi) < len(mesh.Verts) {
		v.Pos = mesh.Verts[vi]
	}
	if vi >= 0 && int(vi) < len(mesh.Normals) {
		v.Norm = mesh.Normals[vi]
	}
	if uvi >= 0 && int(uvi) < len(mesh.TVerts) {
		v.UV = mesh.TVerts[uvi]
	}
	if uvi >= 0 && int(uvi) < len(mesh.TVerts1) {
		v.UV1 = mesh.TVerts1[uvi]
	}
	if uvi >= 0 && int(uvi) < len(mesh.TVerts2) {
		v.UV2 = mesh.TVerts2[uvi]
	}
	if uvi >= 0 && int(uvi) < len(mesh.TVerts3) {
		v.UV3 = mesh.TVerts3[uvi]
	}
	if vi >= 0 && int(vi) < len(mesh.Colors) {
		v.Color = mesh.Colors[vi]
	}
	if vi >= 0 && int(vi) < len(mesh.Tangents) {
		v.Tan = mesh.Tangents[vi]
	}
	if skin != nil && vi >= 0 && int(vi) < len(skin.Weights) {
		v.Weight = CloneVertexWeight(skin.Weights[vi])
	}
	return v
}


func worldZOf(m *Model, n *Node, v tileFadeVert) float32 {
	return localMeshVertexToWorld(m, n, v.Pos).Z
}

// edgeSlice builds a new vertex on the edge v0—v1 where world Z == sliceZ.
// Ref: tilefade.pl — plane/edge intersection then attribute interpolation.
func edgeSlice(m *Model, n *Node, v0, v1 tileFadeVert, sliceZ float32) (tileFadeVert, bool) {
	w0 := localMeshVertexToWorld(m, n, v0.Pos)
	w1 := localMeshVertexToWorld(m, n, v1.Pos)
	dz := w1.Z - w0.Z
	if math.Abs(float64(dz)) < float64(tilefadeZEpsilon) {
		return tileFadeVert{}, false
	}
	t := (sliceZ - w0.Z) / dz
	if t <= tilefadeZEpsilon || t >= 1.0-tilefadeZEpsilon {
		return tileFadeVert{}, false
	}
	pw := vecLerp3(w0, w1, t)
	out := tileFadeVert{
		Pos: worldToMeshVertexLocal(m, n, pw),
		Norm: worldNormalToMeshLocal(m, n, vecNormalize(vecLerp3(
			localMeshNormalToWorld(m, n, v0.Norm),
			localMeshNormalToWorld(m, n, v1.Norm),
			t,
		))),
		UV:   vecLerp3(v0.UV, v1.UV, t),
		UV1:  vecLerp3(v0.UV1, v1.UV1, t),
		UV2:  vecLerp3(v0.UV2, v1.UV2, t),
		UV3:  vecLerp3(v0.UV3, v1.UV3, t),
		Color: vecLerp3(v0.Color, v1.Color, t),
		Tan:   vecLerp4(v0.Tan, v1.Tan, t),
		Weight: lerpVertexWeight(v0.Weight, v1.Weight, t),
	}
	return out, true
}

func lerpVertexWeight(a, b VertexWeight, t float32) VertexWeight {
	if len(a.Bones) == 0 && len(b.Bones) == 0 {
		return VertexWeight{}
	}
	// Merge bone influences (small N, typical for game skinning).
	acc := map[string]float32{}
	for i := range a.Bones {
		if i < len(a.Weights) {
			acc[strings.ToLower(a.Bones[i])] += a.Weights[i] * (1 - t)
		}
	}
	for i := range b.Bones {
		if i < len(b.Weights) {
			acc[strings.ToLower(b.Bones[i])] += b.Weights[i] * t
		}
	}
	bones := make([]string, 0, len(acc))
	for bone, w := range acc {
		if w > 1e-6 {
			bones = append(bones, bone)
		}
	}
	sort.Strings(bones)
	out := VertexWeight{}
	for _, bone := range bones {
		out.Bones = append(out.Bones, bone)
		out.Weights = append(out.Weights, acc[bone])
	}
	renormalizeWeights(&out)
	return out
}

func renormalizeWeights(w *VertexWeight) {
	var sum float32
	for i := range w.Weights {
		sum += w.Weights[i]
	}
	if sum <= 0 {
		return
	}
	for i := range w.Weights {
		w.Weights[i] /= sum
	}
}

type meshBuilder struct {
	mesh *MeshData
}

func newMeshBuilderLike(template *MeshData) *meshBuilder {
	m := &MeshData{
		Diffuse:          template.Diffuse,
		Ambient:          template.Ambient,
		Specular:         template.Specular,
		Shininess:        template.Shininess,
		Bitmap:           template.Bitmap,
		Texture1:         template.Texture1,
		Texture2:         template.Texture2,
		MaterialName:     template.MaterialName,
		RenderHint:       template.RenderHint,
		Shadow:           template.Shadow,
		Beaming:          template.Beaming,
		Render:           template.Render,
		TransparencyHint: template.TransparencyHint,
		Alpha:            template.Alpha,
		SelfIllumColor:   template.SelfIllumColor,
		RotateTexture:    template.RotateTexture,
		LightMapped:      template.LightMapped,
		Multimaterial:    append([]string(nil), template.Multimaterial...),
	}
	return &meshBuilder{mesh: m}
}

func (b *meshBuilder) appendSkinWeight(skin *SkinData, w VertexWeight) {
	if skin == nil {
		return
	}
	skin.Weights = append(skin.Weights, w)
}

func (b *meshBuilder) addVertWithSkin(tv tileFadeVert, template *MeshData, skin *SkinData) (vidx, uvidx int32) {
	vidx = int32(len(b.mesh.Verts))
	uvidx = vidx
	b.mesh.Verts = append(b.mesh.Verts, tv.Pos)
	if len(template.Normals) > 0 {
		b.mesh.Normals = append(b.mesh.Normals, tv.Norm)
	}
	if len(template.TVerts) > 0 {
		b.mesh.TVerts = append(b.mesh.TVerts, tv.UV)
	}
	if len(template.TVerts1) > 0 {
		b.mesh.TVerts1 = append(b.mesh.TVerts1, tv.UV1)
	}
	if len(template.TVerts2) > 0 {
		b.mesh.TVerts2 = append(b.mesh.TVerts2, tv.UV2)
	}
	if len(template.TVerts3) > 0 {
		b.mesh.TVerts3 = append(b.mesh.TVerts3, tv.UV3)
	}
	if len(template.Colors) > 0 {
		b.mesh.Colors = append(b.mesh.Colors, tv.Color)
	}
	if len(template.Tangents) > 0 {
		b.mesh.Tangents = append(b.mesh.Tangents, tv.Tan)
	}
	if skin != nil {
		b.appendSkinWeight(skin, tv.Weight)
	}
	return vidx, uvidx
}

func (b *meshBuilder) triangleNormal(v0, v1, v2 int32) Vec3 {
	return triangleNormalFromIndices(b.mesh.Verts, v0, v1, v2)
}

func triangleNormalFromIndices(verts []Vec3, v0, v1, v2 int32) Vec3 {
	if verts == nil || int(v0) >= len(verts) || int(v1) >= len(verts) || int(v2) >= len(verts) {
		return Vec3{0, 0, 1}
	}
	a := verts[v0]
	b := verts[v1]
	c := verts[v2]
	return vecNormalize(vecCross(vecSub(b, a), vecSub(c, a)))
}

func (b *meshBuilder) emitTri(v0, v1, v2 int32, uv0, uv1, uv2 int32, src *Face, useTex0, useTex1, useTex2, useTex3 bool, tex0, tex1, tex2, tex3 [3]int32) {
	n := b.triangleNormal(v0, v1, v2)
	f := Face{
		Verts:       [3]int32{v0, v1, v2},
		SmoothGroup: src.SmoothGroup,
		UVs:         [3]int32{uv0, uv1, uv2},
		Material:    src.Material,
		Normal:      n,
		PlaneD:      0,
	}
	b.mesh.Faces = append(b.mesh.Faces, f)
	if useTex0 {
		b.mesh.TexIndices0 = append(b.mesh.TexIndices0, tex0)
	}
	if useTex1 {
		b.mesh.TexIndices1 = append(b.mesh.TexIndices1, tex1)
	}
	if useTex2 {
		b.mesh.TexIndices2 = append(b.mesh.TexIndices2, tex2)
	}
	if useTex3 {
		b.mesh.TexIndices3 = append(b.mesh.TexIndices3, tex3)
	}
}

// orientTriangleForPlaneCut reorders corners so va is the lone vertex on one
// side of the slice plane; vb and vc follow CCW order along the opposite edge.
func orientTriangleForPlaneCut(a0, a1, a2 bool, v0, v1, v2 tileFadeVert) (va, vb, vc tileFadeVert, loneAbove, loneBelow bool) {
	switch {
	case a0 && !a1 && !a2:
		return v0, v1, v2, true, false
	case !a0 && a1 && !a2:
		return v1, v2, v0, true, false
	case !a0 && !a1 && a2:
		return v2, v0, v1, true, false
	case !a0 && a1 && a2:
		return v0, v1, v2, false, true
	case a0 && !a1 && a2:
		return v1, v2, v0, false, true
	case a0 && a1 && !a2:
		return v2, v0, v1, false, true
	default:
		return v0, v1, v2, false, false
	}
}

func splitMeshAtWorldZ(m *Model, n *Node, template *MeshData, sliceZ float32) (above, below *MeshData, skinAbove, skinBelow *SkinData, err error) {
	skin := n.Skin
	ab := newMeshBuilderLike(template)
	bl := newMeshBuilderLike(template)
	var skAbove, skBelow *SkinData
	if skin != nil {
		skAbove = &SkinData{}
		skBelow = &SkinData{}
	}

	bAbove := &meshBuilderWithSkin{meshBuilder: ab, skin: skAbove}
	bBelow := &meshBuilderWithSkin{meshBuilder: bl, skin: skBelow}

	for fi := range template.Faces {
		f := &template.Faces[fi]
		useTI0 := fi < len(template.TexIndices0)
		useTI1 := fi < len(template.TexIndices1)
		useTI2 := fi < len(template.TexIndices2)
		useTI3 := fi < len(template.TexIndices3)
		var tex0, tex1, tex2, tex3 [3]int32
		if useTI0 {
			tex0 = template.TexIndices0[fi]
		}
		if useTI1 {
			tex1 = template.TexIndices1[fi]
		}
		if useTI2 {
			tex2 = template.TexIndices2[fi]
		}
		if useTI3 {
			tex3 = template.TexIndices3[fi]
		}

		v0 := readTileFadeVert(template, skin, f.Verts[0], f.UVs[0])
		v1 := readTileFadeVert(template, skin, f.Verts[1], f.UVs[1])
		v2 := readTileFadeVert(template, skin, f.Verts[2], f.UVs[2])

		z0 := worldZOf(m, n, v0)
		z1 := worldZOf(m, n, v1)
		z2 := worldZOf(m, n, v2)
		a0 := zAboveSlice(z0, sliceZ)
		a1 := zAboveSlice(z1, sliceZ)
		a2 := zAboveSlice(z2, sliceZ)

		if a0 == a1 && a1 == a2 {
			if a0 {
				bAbove.emitFace(template, f, v0, v1, v2, useTI0, useTI1, useTI2, useTI3, tex0, tex1, tex2, tex3)
			} else {
				bBelow.emitFace(template, f, v0, v1, v2, useTI0, useTI1, useTI2, useTI3, tex0, tex1, tex2, tex3)
			}
			continue
		}

		va, vb, vc, loneAbove, loneBelow := orientTriangleForPlaneCut(a0, a1, a2, v0, v1, v2)
		if !loneAbove && !loneBelow {
			return nil, nil, nil, nil, fmt.Errorf("face %d: unexpected plane/triangle configuration", fi)
		}
		if loneAbove {
			if err = splitOneAbove(m, n, template, sliceZ, bAbove, bBelow, f, va, vb, vc, useTI0, useTI1, useTI2, useTI3, tex0, tex1, tex2, tex3); err != nil {
				return nil, nil, nil, nil, err
			}
		} else {
			if err = splitOneAbove(m, n, template, sliceZ, bBelow, bAbove, f, va, vb, vc, useTI0, useTI1, useTI2, useTI3, tex0, tex1, tex2, tex3); err != nil {
				return nil, nil, nil, nil, err
			}
		}
	}

	return bAbove.mesh, bBelow.mesh, skAbove, skBelow, nil
}

type meshBuilderWithSkin struct {
	*meshBuilder
	skin *SkinData
}

func (b *meshBuilderWithSkin) emitFace(template *MeshData, src *Face, v0, v1, v2 tileFadeVert, useTI0, useTI1, useTI2, useTI3 bool, tex0, tex1, tex2, tex3 [3]int32) {
	i0, u0 := b.addVertWithSkin(v0, template, b.skin)
	i1, u1 := b.addVertWithSkin(v1, template, b.skin)
	i2, u2 := b.addVertWithSkin(v2, template, b.skin)
	b.emitTri(i0, i1, i2, u0, u1, u2, src, useTI0, useTI1, useTI2, useTI3, tex0, tex1, tex2, tex3)
}

// splitOneAbove clips a triangle whose apex va lies strictly on one side of
// the world-space Z=sliceZ plane and base corners vb, vc lie on the other.
// The first mesh builder receives the small corner triangle; the second
// receives the two fan triangles on the opposite side.
// Ref: tilefade.pl — two-triangle fan + corner triangle at the slice.
func splitOneAbove(m *Model, n *Node, template *MeshData, sliceZ float32, triA, triB *meshBuilderWithSkin, src *Face, va, vb, vc tileFadeVert, useTI0, useTI1, useTI2, useTI3 bool, tex0, tex1, tex2, tex3 [3]int32) error {
	iab, okAB := edgeSlice(m, n, va, vb, sliceZ)
	iac, okAC := edgeSlice(m, n, va, vc, sliceZ)
	if !okAB || !okAC {
		return fmt.Errorf("degenerate slice intersection (parallel edge)")
	}
	ia, ua := triA.addVertWithSkin(va, template, triA.skin)
	ib, ub := triA.addVertWithSkin(iab, template, triA.skin)
	ic, uc := triA.addVertWithSkin(iac, template, triA.skin)
	triA.emitTri(ia, ib, ic, ua, ub, uc, src, useTI0, useTI1, useTI2, useTI3, tex0, tex1, tex2, tex3)

	b0, t0 := triB.addVertWithSkin(vb, template, triB.skin)
	b1, t1 := triB.addVertWithSkin(vc, template, triB.skin)
	b2, t2 := triB.addVertWithSkin(iac, template, triB.skin)
	b3, t3 := triB.addVertWithSkin(iab, template, triB.skin)
	triB.emitTri(b0, b1, b2, t0, t1, t2, src, useTI0, useTI1, useTI2, useTI3, tex0, tex1, tex2, tex3)
	triB.emitTri(b0, b2, b3, t0, t2, t3, src, useTI0, useTI1, useTI2, useTI3, tex0, tex1, tex2, tex3)
	return nil
}

// --- small vector helpers ---

