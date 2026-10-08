package mdl

import (
	"errors"
	"fmt"
	"sort"
)

// ErrDegenerateAABBTree is returned when face centroids cannot be partitioned
// along any axis (overlapping or collinear in a way that leaves one side empty).
// Ref: rebuild_wok.pl w_rebuild_aabb/4 (failure branch when no split axis works).
var ErrDegenerateAABBTree = errors.New("mdl: cannot rebuild AABB tree: degenerate centroid arrangement")

// RebuildAABB rebuilds the AABB tree for a walkmesh node from its face geometry.
// The node must have Mesh and Aabb data. The existing Aabb.Entries are replaced.
// Entries are stored depth-first pre-order (root, left subtree, right subtree),
// matching the order produced by rebuild_wok.pl / binary decompilation.
//
// Leaf nodes have LeafFace >= 0 (face index); interior nodes use LeafFace == -1.
// Plane is 0 for leaves; for interior nodes it records the split axis (0=X, 1=Y, 2=Z).
//
// Ref: rebuild_wok.pl w_rebuild_aabb/4
func RebuildAABB(node *Node) error {
	if node == nil || node.Mesh == nil || node.Aabb == nil {
		return fmt.Errorf("mdl: RebuildAABB requires non-nil node, Mesh, and Aabb")
	}
	mesh := node.Mesh
	if len(mesh.Faces) == 0 {
		return fmt.Errorf("mdl: RebuildAABB requires at least one face")
	}
	saved := append([]AabbEntry(nil), node.Aabb.Entries...)
	node.Aabb.Entries = node.Aabb.Entries[:0]

	all := make([]int32, len(mesh.Faces))
	for i := range mesh.Faces {
		all[i] = int32(i)
	}
	rb := aabbRebuilder{mesh: mesh, out: &node.Aabb.Entries}
	if err := rb.rebuild(all); err != nil {
		node.Aabb.Entries = saved
		return err
	}
	return nil
}

type aabbRebuilder struct {
	mesh *MeshData
	out  *[]AabbEntry
}

func (r *aabbRebuilder) rebuild(faceIdxs []int32) error {
	if len(faceIdxs) == 0 {
		return fmt.Errorf("mdl: internal AABB rebuild with empty face set")
	}
	minB, maxB, err := boundsForFaces(r.mesh, faceIdxs)
	if err != nil {
		return err
	}
	if len(faceIdxs) == 1 {
		r.pushLeaf(minB, maxB, faceIdxs[0])
		return nil
	}
	left, right, axis, err := partitionFaces(r.mesh, faceIdxs, minB, maxB)
	if err != nil {
		return err
	}
	r.pushInterior(minB, maxB, axis)
	if err := r.rebuild(left); err != nil {
		return err
	}
	return r.rebuild(right)
}

func (r *aabbRebuilder) pushLeaf(minB, maxB Vec3, face int32) {
	*r.out = append(*r.out, AabbEntry{
		BoundMin: minB,
		BoundMax: maxB,
		LeafFace: face,
		Plane:    0,
	})
}

func (r *aabbRebuilder) pushInterior(minB, maxB Vec3, axis int) {
	*r.out = append(*r.out, AabbEntry{
		BoundMin: minB,
		BoundMax: maxB,
		LeafFace: -1,
		Plane:    uint32(axis),
	})
}

func boundsForFaces(mesh *MeshData, faces []int32) (Vec3, Vec3, error) {
	minB, maxB, err := faceBounds(mesh, faces[0])
	if err != nil {
		return Vec3{}, Vec3{}, err
	}
	for _, fi := range faces[1:] {
		fmin, fmax, err := faceBounds(mesh, fi)
		if err != nil {
			return Vec3{}, Vec3{}, err
		}
		minB = vecMin(minB, fmin)
		maxB = vecMax(maxB, fmax)
	}
	return minB, maxB, nil
}

func faceBounds(mesh *MeshData, fi int32) (Vec3, Vec3, error) {
	f := mesh.Faces[fi]
	nv := int32(len(mesh.Verts))
	for _, vi := range f.Verts {
		if vi < 0 || vi >= nv {
			return Vec3{}, Vec3{}, fmt.Errorf("mdl: face %d references invalid vertex index %d", fi, vi)
		}
	}
	v0 := mesh.Verts[f.Verts[0]]
	v1 := mesh.Verts[f.Verts[1]]
	v2 := mesh.Verts[f.Verts[2]]
	return vecMin(vecMin(v0, v1), v2), vecMax(vecMax(v0, v1), v2), nil
}



func faceCentroid(mesh *MeshData, fi int32) Vec3 {
	f := mesh.Faces[fi]
	nv := int32(len(mesh.Verts))
	for _, vi := range f.Verts {
		if vi < 0 || vi >= nv {
			return Vec3{}
		}
	}
	p0 := mesh.Verts[f.Verts[0]]
	p1 := mesh.Verts[f.Verts[1]]
	p2 := mesh.Verts[f.Verts[2]]
	return Vec3{
		X: (p0.X + p1.X + p2.X) / 3,
		Y: (p0.Y + p1.Y + p2.Y) / 3,
		Z: (p0.Z + p1.Z + p2.Z) / 3,
	}
}

func centroidAverage(mesh *MeshData, faces []int32) Vec3 {
	var sum Vec3
	for _, fi := range faces {
		c := faceCentroid(mesh, fi)
		sum.X += c.X
		sum.Y += c.Y
		sum.Z += c.Z
	}
	n := float32(len(faces))
	return Vec3{X: sum.X / n, Y: sum.Y / n, Z: sum.Z / n}
}

func axisExtent(minB, maxB Vec3, axis int) float32 {
	switch axis {
	case 0:
		return maxB.X - minB.X
	case 1:
		return maxB.Y - minB.Y
	default:
		return maxB.Z - minB.Z
	}
}


// axesByExtent returns {0,1,2} sorted by bounding-box extent on that axis, descending.
func axesByExtent(minB, maxB Vec3) []int {
	axes := []int{0, 1, 2}
	sort.Slice(axes, func(i, j int) bool {
		ei := axisExtent(minB, maxB, axes[i])
		ej := axisExtent(minB, maxB, axes[j])
		if ei != ej {
			return ei > ej
		}
		return axes[i] < axes[j]
	})
	return axes
}

func partitionFaces(mesh *MeshData, faces []int32, minB, maxB Vec3) (left, right []int32, axis int, err error) {
	avg := centroidAverage(mesh, faces)
	for _, ax := range axesByExtent(minB, maxB) {
		avgC := avg.Index(ax)
		left, right = splitByAxis(mesh, faces, ax, avgC)
		if len(left) > 0 && len(right) > 0 {
			return left, right, ax, nil
		}
	}
	return nil, nil, 0, ErrDegenerateAABBTree
}

// RemapAABBMaterial swaps face Material IDs on all AABB (walkmesh) nodes.
// from and to are the material IDs to remap.
func RemapAABBMaterial(model *Model, from, to int) []string {
	var out []string
	for _, n := range model.Nodes {
		if n == nil || n.Aabb == nil || n.Mesh == nil {
			continue
		}
		count := 0
		fromID := int32(from)
		toID := int32(to)
		for i := range n.Mesh.Faces {
			if n.Mesh.Faces[i].Material == fromID {
				n.Mesh.Faces[i].Material = toID
				count++
			}
		}
		if count > 0 {
			out = append(out, fmt.Sprintf("node %q: remapped %d walkmesh faces from material %d to %d", n.Name, count, from, to))
		}
	}
	return out
}

func splitByAxis(mesh *MeshData, faces []int32, axis int, avgCoord float32) (left, right []int32) {
	for _, fi := range faces {
		c := faceCentroid(mesh, fi).Index(axis)
		if c < avgCoord {
			left = append(left, fi)
		} else {
			right = append(right, fi)
		}
	}
	return left, right
}
