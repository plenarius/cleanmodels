package mdl

import (
	"fmt"
	"sort"
	"strings"
)

// Variant previews. To judge a variant's fit, a viewer needs it where the
// game shows it, on the body it was made for. VariantPreview bakes both into
// the skeleton's space at rest, as plain meshes under one root: no skins, no
// part hierarchy and no animations, so any viewer draws them in place.

// VariantPreview returns two static models for looking at the variant v (a
// robe, cloak or body part made by Variant for the body target): fit, v
// placed where the game shows it on that body, and body, the body's base
// parts as the game picks them (part 001 of every slot but the belt and
// shoulders, and head 001; see basePart) on its skeleton (bodySkeleton), with
// the skeleton's own meshes, without the slot v replaces, or for a robe, without the parts
// its row in parts_robe.2da hides. notes says what could not be shown that
// way (a base part or the robe's row not found).
//
// When textureDir is set, the variant is textured as in game: its palette
// texture (found by its name, as the game finds it; see partTextureNames) is
// coloured with fixed preview colours and written there as <texture>.tga,
// and the fit's meshes name it. The body is left untextured.
func VariantPreview(v *Model, target string, locs []string, textureDir string) (fit, body *Model, notes []string, err error) {
	target = strings.ToLower(strings.TrimSpace(target))
	sk, err := loadBodySkeleton(target, locs)
	if err != nil {
		return nil, nil, nil, err
	}
	if sk == nil {
		return nil, nil, nil, fmt.Errorf("skeleton %s not found; pass the game install or the hak that has it", target)
	}
	// Where each skeleton node is drawn. A robe or cloak carries its own
	// copies of the bones, in the pose it is fitted in; the skeleton files
	// do not always rest in that pose (a dwarf's root is as high as a
	// human's, a mounted one rests as a rider). So for those, the body is
	// posed on the model's copies: a node it has takes the copy's place, the
	// others keep their skeleton offset from their parent.
	pose := make([]rigid, len(sk.order))
	robeLike := partAttachNode(v.Name) == ""
	vw := newWorldTransforms(v)
	for i, n := range sk.order {
		own := rigid{rot: sk.rot[i], pos: sk.pos[i]}
		pose[i] = own
		if !robeLike {
			continue
		}
		if r, ok := vw.lookup(n.Name); ok {
			pose[i] = r
			continue
		}
		if p, ok := sk.index[strings.ToLower(n.Parent)]; ok && p < i {
			parent := rigid{rot: sk.rot[p], pos: sk.pos[p]}
			pose[i] = pose[p].compose(parent.inverse().compose(own))
		}
	}
	at := func(node string) (rigid, bool) {
		i, ok := sk.attachIndex(node)
		if !ok {
			return rigid{}, false
		}
		return pose[i], true
	}

	fit = newPreviewModel(v.Name + "_fit")
	place := rigid{rot: [4]float64{0, 0, 0, 1}}
	if node := partAttachNode(v.Name); node != "" {
		r, ok := at(node)
		if !ok {
			return nil, nil, nil, fmt.Errorf("skeleton %s has no %s for %s", target, node, v.Name)
		}
		place = r
	}
	bakeInto(fit, v, place)
	// Textured where its palette texture is found; mid grey otherwise (the
	// source's white would hide its shape under the viewer's light).
	written := map[string]bool{}
	for _, n := range fit.Nodes[1:] {
		tex := ""
		if textureDir != "" {
			tex = previewTexture(partTextureNames(v.Name, n.Mesh.Bitmap), locs, textureDir, written)
		}
		if tex != "" {
			n.Mesh.Bitmap = tex
			n.Mesh.Diffuse, n.Mesh.Ambient = Vec3{1, 1, 1}, Vec3{0.8, 0.8, 0.8}
		} else {
			n.Mesh.Bitmap = "null"
			n.Mesh.Diffuse, n.Mesh.Ambient = Vec3{0.6, 0.6, 0.6}, Vec3{0.6, 0.6, 0.6}
		}
		n.Mesh.Texture1, n.Mesh.Texture2, n.Mesh.MaterialName = "", "", ""
	}

	body = newPreviewModel(target + "_body")
	skip := partKind(v.Name)
	hidden, note := robeHides(v.Name, locs)
	if note != "" {
		notes = append(notes, note)
	}
	var missing []string
	// A skeleton can carry meshes of its own (a wemic's lion body).
	bakeNodes(body, sk.model, func(name string) rigid {
		if i, ok := sk.index[strings.ToLower(name)]; ok {
			return pose[i]
		}
		return rigid{rot: [4]float64{0, 0, 0, 1}}
	})
	slots := make([]string, 0, len(partAttachNodes)+1)
	for slot := range partAttachNodes {
		if !previewAccessory[slot] {
			slots = append(slots, slot)
		}
	}
	sort.Strings(slots)
	slots = append(slots, "head")
	for _, slot := range slots {
		if slot == skip || hidden[slot] {
			continue
		}
		node := partAttachNodes[slot]
		if slot == "head" {
			node = "head_g"
		}
		name := basePart(target, slot, locs)
		ref := findModelResource(name, locs)
		r, ok := at(node)
		if ref == nil || !ok {
			missing = append(missing, name)
			continue
		}
		part, err := readModelRef(ref)
		if err != nil {
			missing = append(missing, name)
			continue
		}
		bakeInto(body, part, r)
	}
	if len(missing) > 0 {
		notes = append(notes, "preview body without "+strings.Join(missing, ", ")+" (not found)")
	}
	return fit, body, notes, nil
}

// robeHides returns the body part slots the game hides under a robe (name
// pXXX_robeNNN), from row NNN of parts_robe.2da, or nil for a model that is
// not a robe. note is set when the table or the row is not found.
func robeHides(name string, locs []string) (hidden map[string]bool, note string) {
	if strings.TrimRight(partKind(name), "_") != "robe" {
		return nil, ""
	}
	num := strings.TrimLeft(strings.TrimLeftFunc(strings.ToLower(name)[len(partSkeletonName(name))+1:], func(r rune) bool { return r < '0' || r > '9' }), "0")
	if num == "" {
		num = "0"
	}
	ref := findResource("parts_robe", resType2DA, ".2da", locs)
	if ref == nil {
		return nil, "parts_robe.2da not found: the preview body shows every part, though the game hides some under a robe"
	}
	data, err := ref.read()
	if err != nil {
		return nil, fmt.Sprintf("%s: %v", ref.id, err)
	}
	rows, err := parse2DA(data)
	if err != nil {
		return nil, fmt.Sprintf("%s: %v", ref.id, err)
	}
	for _, r := range rows {
		if r[rowKey] != num {
			continue
		}
		hidden = map[string]bool{}
		for col, v := range r {
			if slot, ok := strings.CutPrefix(col, "hide"); ok && v == "1" {
				hidden[slot] = true
			}
		}
		return hidden, ""
	}
	return nil, fmt.Sprintf("%s has no row %s: the preview body shows every part, though the game hides those the row will name", ref.id, num)
}

// previewAccessory are the slots a bare body has nothing in (part 000), so the
// preview body leaves them out.
var previewAccessory = map[string]bool{"belt": true, "shol": true, "shor": true}

func newPreviewModel(name string) *Model {
	return &Model{
		Name:           name,
		SuperModel:     "NULL",
		Classification: "character",
		AnimationScale: 1,
		Nodes:          []*Node{{Name: name, Parent: "NULL", Orientation: Vec4{W: 1}, Scale: 1}},
	}
}

// bakeInto adds every rendered mesh of m (walkmeshes aside) to dst as a
// plain mesh under dst's root, its vertices and normals moved by the node's
// rest transform in m and then by place.
func bakeInto(dst, m *Model, place rigid) {
	w := newWorldTransforms(m)
	bakeNodes(dst, m, func(name string) rigid { return place.compose(w.of(name)) })
}

// bakeNodes is bakeInto with each node's transform given by world.
func bakeNodes(dst, m *Model, world func(name string) rigid) {
	root := dst.Nodes[0].Name
	for _, n := range m.Nodes {
		if n.Mesh == nil || len(n.Mesh.Verts) == 0 || len(n.Mesh.Faces) == 0 || n.Aabb != nil || n.Mesh.Render == 0 {
			continue
		}
		x := world(n.Name)
		mesh := *n.Mesh
		mesh.Verts = make([]Vec3, len(n.Mesh.Verts))
		for i, v := range n.Mesh.Verts {
			mesh.Verts[i] = vec3f(x.apply(vec3d(v)))
		}
		if len(n.Mesh.Normals) > 0 {
			mesh.Normals = make([]Vec3, len(n.Mesh.Normals))
			for i, nv := range n.Mesh.Normals {
				mesh.Normals[i] = vec3f(quatRotate(x.rot, vec3d(nv)))
			}
		}
		mesh.CornerNormals, mesh.Tangents = nil, nil
		dst.Nodes = append(dst.Nodes, &Node{
			Name:        fmt.Sprintf("%s_%d", root, len(dst.Nodes)),
			Parent:      root,
			Orientation: Vec4{W: 1},
			Scale:       1,
			Mesh:        &mesh,
		})
	}
}
