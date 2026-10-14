package mdl

import (
	"fmt"
	"strings"
)

// RetileUVs remaps UV coordinates for mesh nodes whose bitmap contains key.
// The tile coordinate system spans -5 to 5 in X and Y (10x10 units).
// tileSize controls the repetition factor: U = (vertX + 5) * tileSize * 0.1,
// V = (vertY + 5) * tileSize * 0.1, W = 0, where vertX and vertY are the
// vertex's position in the tile (through the node's position and orientation and
// its parents'), as the original CleanModels used (abs_vertex), not the mesh's
// local coordinates: a ground mesh that is offset or rotated in the tile would
// otherwise get UVs that do not line up with its neighbours. Face UV indices
// are set to match vertex indices (1:1 mapping).
func RetileUVs(model *Model, key string, tileSize int) []string {
	var out []string
	if key == "" || tileSize <= 0 {
		return out
	}
	lk := strings.ToLower(key)
	n := float32(tileSize)
	idx := nodeIndex(model)
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
		world := WorldVerticesCached(idx, nd)
		if len(world) != len(mesh.Verts) {
			continue
		}
		mesh.TVerts = make([]Vec3, len(mesh.Verts))
		for i, v := range world {
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

// RaiseLowerTile moves a tile up or down by amount, in the model's units
// (metres). direction is "raise" or "lower"; amount is always positive.
//
// Every vertex of the tile must end up exactly amount higher (or lower) in world
// space, so each piece of geometry is moved once, in the one place that moves it,
// as the original CleanModels did:
//
//   - nodes hanging directly off the root (the tile's meshes, lights, emitters)
//     get their position shifted; whatever hangs below them follows its parent;
//   - the walkmesh (aabb) has its vertices and its tree's bounding boxes shifted,
//     and its own position left alone;
//   - the children of the animation parent "<model>a" are shifted the same way,
//     with their animation position keys; an animmesh has no position of its own
//     to move, so its vertices and animation vertices are shifted instead.
//
// This used to add the amount to every node's position and to every vertex, so a
// vertex under a root and a mesh node moved by three times the amount (5 m
// became 15 m).
func RaiseLowerTile(model *Model, direction string, amount float32) []string {
	var out []string
	root := model.RootNode()
	if amount == 0 || root == nil {
		return out
	}
	dz := amount
	if strings.EqualFold(direction, "lower") {
		dz = -amount
	}
	modela := root.Name + "a"

	shiftVerts := func(vs []Vec3) {
		for i := range vs {
			vs[i].Z += dz
		}
	}
	for _, n := range model.Nodes {
		if n == nil || n == root {
			continue
		}
		switch {
		case n.Aabb != nil || n.NodeType() == "aabb":
			if n.Mesh != nil {
				shiftVerts(n.Mesh.Verts)
			}
			if n.Aabb != nil {
				for i := range n.Aabb.Entries {
					n.Aabb.Entries[i].BoundMin.Z += dz
					n.Aabb.Entries[i].BoundMax.Z += dz
				}
			}
		case strings.EqualFold(n.Parent, root.Name) && !strings.EqualFold(n.Name, modela):
			n.Position.Z += dz
		case strings.EqualFold(n.Parent, modela):
			if n.AnimMesh != nil {
				if n.Mesh != nil {
					shiftVerts(n.Mesh.Verts)
				}
				shiftVerts(n.AnimMesh.AnimVerts)
			} else {
				n.Position.Z += dz
			}
			for ai := range model.Animations {
				for ni := range model.Animations[ai].Nodes {
					an := &model.Animations[ai].Nodes[ni]
					if !strings.EqualFold(an.Name, n.Name) {
						continue
					}
					if n.AnimMesh != nil {
						if an.AnimMesh != nil {
							shiftVerts(an.AnimMesh.AnimVerts)
						}
						continue
					}
					for ki := range an.PositionKeys {
						an.PositionKeys[ki].Value.Z += dz
					}
				}
			}
		}
	}

	out = append(out, fmt.Sprintf("tile %s by %.4f units (dz=%.4f)", direction, amount, dz))
	return out
}
