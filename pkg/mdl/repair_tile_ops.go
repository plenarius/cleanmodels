package mdl

import (
	"fmt"
	"strings"
)

// RetileUVs remaps UV coordinates for mesh nodes whose bitmap contains key.
// The tile coordinate system spans -5 to 5 in X and Y (10x10 units).
// tileSize controls the repetition factor: U = (vertX + 5) * tileSize * 0.1,
// V = (vertY + 5) * tileSize * 0.1, W = 0. Face UV indices are set to match
// vertex indices (1:1 mapping).
func RetileUVs(model *Model, key string, tileSize int) []string {
	var out []string
	if key == "" || tileSize <= 0 {
		return out
	}
	lk := strings.ToLower(key)
	n := float32(tileSize)
	for _, nd := range model.Nodes {
		if nd == nil || nd.Mesh == nil {
			continue
		}
		if !strings.Contains(strings.ToLower(nd.Mesh.Bitmap), lk) {
			continue
		}
		mesh := nd.Mesh
		if len(mesh.Verts) == 0 {
			continue
		}
		mesh.TVerts = make([]Vec3, len(mesh.Verts))
		for i, v := range mesh.Verts {
			mesh.TVerts[i] = Vec3{
				X: (v.X + 5) * n * 0.1,
				Y: (v.Y + 5) * n * 0.1,
				Z: 0,
			}
		}
		for i := range mesh.Faces {
			mesh.Faces[i].UVs = mesh.Faces[i].Verts
		}
		out = append(out, fmt.Sprintf("retiled UVs on node %q (tileSize=%d)", nd.Name, tileSize))
	}
	return out
}

// SetRotateTexture sets the RotateTexture property on mesh nodes whose bitmap
// contains key.
func SetRotateTexture(model *Model, key string, value int32) []string {
	var out []string
	if key == "" {
		return out
	}
	lk := strings.ToLower(key)
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil {
			continue
		}
		if !strings.Contains(strings.ToLower(n.Mesh.Bitmap), lk) {
			continue
		}
		if n.Mesh.RotateTexture != value {
			n.Mesh.RotateTexture = value
			out = append(out, fmt.Sprintf("set rotatetexture=%d on node %q", value, n.Name))
		}
	}
	return out
}

// ReparentToModela reparents mesh nodes whose bitmap contains key to a node
// named "Modela" (creating it if needed). This is used for foliage/splotch
// animate operations.
func ReparentToModela(model *Model, key string, nodeType string) []string {
	var out []string
	if key == "" {
		return out
	}
	lk := strings.ToLower(key)
	var modela *Node
	for _, n := range model.Nodes {
		if n != nil && strings.EqualFold(n.Name, "Modela") {
			modela = n
			break
		}
	}

	var matched []*Node
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil {
			continue
		}
		if !strings.Contains(strings.ToLower(n.Mesh.Bitmap), lk) {
			continue
		}
		matched = append(matched, n)
	}
	if len(matched) == 0 {
		return out
	}

	if modela == nil {
		root := model.RootNode()
		parentName := "NULL"
		if root != nil {
			parentName = root.Name
		}
		modela = &Node{
			Name:   "Modela",
			Parent: parentName,
		}
		model.Nodes = append(model.Nodes, modela)
		out = append(out, fmt.Sprintf("created dummy node \"Modela\" under %q for %s reparenting", parentName, nodeType))
	}

	for _, n := range matched {
		old := n.Parent
		n.Parent = "Modela"
		out = append(out, fmt.Sprintf("reparented %s node %q from %q to \"Modela\"", nodeType, n.Name, old))
	}
	return out
}

// RaiseLowerTile shifts tile geometry vertically. direction is "raise" or "lower".
// amount is always positive; negative shift is applied for "lower".
// Adjusts node Position.Z, AABB vertex Z coordinates, and animation position keys.
func RaiseLowerTile(model *Model, direction string, amount float32) []string {
	var out []string
	if amount == 0 {
		return out
	}
	dz := amount
	if strings.EqualFold(direction, "lower") {
		dz = -amount
	}

	for _, n := range model.Nodes {
		if n == nil {
			continue
		}
		n.Position.Z += dz
		if n.Mesh != nil {
			for i := range n.Mesh.Verts {
				n.Mesh.Verts[i].Z += dz
			}
		}
	}

	for ai := range model.Animations {
		for ni := range model.Animations[ai].Nodes {
			an := &model.Animations[ai].Nodes[ni]
			for ki := range an.PositionKeys {
				an.PositionKeys[ki].Value.Z += dz
			}
		}
	}

	out = append(out, fmt.Sprintf("tile %s by %.4f units (dz=%.4f)", direction, amount, dz))
	return out
}
