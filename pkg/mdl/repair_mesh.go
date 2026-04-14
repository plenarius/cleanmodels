package mdl

import (
	"math"
	"strings"
)

// ForceRender sets Render=1 or Render=0 on all mesh nodes.
// mode should be "all" (render=1) or "none" (render=0).
func ForceRender(model *Model, mode string) int {
	val := int32(1)
	if strings.EqualFold(mode, "none") {
		val = 0
	}
	count := 0
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil {
			continue
		}
		if n.Mesh.Render != val {
			n.Mesh.Render = val
			count++
		}
	}
	return count
}

// ForceShadow sets Shadow=1 or Shadow=0 on all mesh nodes.
func ForceShadow(model *Model, mode string) int {
	val := int32(1)
	if strings.EqualFold(mode, "none") {
		val = 0
	}
	count := 0
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil {
			continue
		}
		if n.Mesh.Shadow != val {
			n.Mesh.Shadow = val
			count++
		}
	}
	return count
}

// ForceWhiteAmbientDiffuse sets ambient and diffuse to (1,1,1) on all mesh nodes.
func ForceWhiteAmbientDiffuse(model *Model) int {
	white := Vec3{X: 1, Y: 1, Z: 1}
	count := 0
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil {
			continue
		}
		changed := false
		if n.Mesh.Ambient != white {
			n.Mesh.Ambient = white
			changed = true
		}
		if n.Mesh.Diffuse != white {
			n.Mesh.Diffuse = white
			changed = true
		}
		if changed {
			count++
		}
	}
	return count
}

// CullInvisibleMeshes converts non-animated mesh nodes with render=0 and shadow=0
// to dummy nodes by removing their mesh data.
func CullInvisibleMeshes(model *Model) int {
	count := 0
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil {
			continue
		}
		if n.Mesh.Render != 0 || n.Mesh.Shadow != 0 {
			continue
		}
		nt := n.NodeType()
		if nt == "animmesh" || nt == "skin" || nt == "danglymesh" {
			continue
		}
		n.Mesh = nil
		n.Aabb = nil
		count++
	}
	return count
}

// meshBitmapMergeSig captures which optional mesh channels are present so we
// only merge nodes with identical layouts.
func meshBitmapMergeSig(m *MeshData) uint16 {
	var s uint16
	if len(m.TVerts) > 0 {
		s |= 1 << 0
	}
	if len(m.TVerts1) > 0 {
		s |= 1 << 1
	}
	if len(m.TVerts2) > 0 {
		s |= 1 << 2
	}
	if len(m.TVerts3) > 0 {
		s |= 1 << 3
	}
	if len(m.Normals) > 0 {
		s |= 1 << 4
	}
	if len(m.Colors) > 0 {
		s |= 1 << 5
	}
	if len(m.Tangents) > 0 {
		s |= 1 << 6
	}
	if len(m.TexIndices0) > 0 {
		s |= 1 << 7
	}
	if len(m.TexIndices1) > 0 {
		s |= 1 << 8
	}
	if len(m.TexIndices2) > 0 {
		s |= 1 << 9
	}
	if len(m.TexIndices3) > 0 {
		s |= 1 << 10
	}
	return s
}

func meshBitmapMergeCompatible(m *MeshData) bool {
	if len(m.CornerNormals) > 0 {
		return false
	}
	if len(m.TVerts) == 0 {
		if len(m.TVerts1) > 0 || len(m.TVerts2) > 0 || len(m.TVerts3) > 0 {
			return false
		}
	}
	tvLen := len(m.TVerts)
	tvExtraOK := func(extra []Vec3) bool {
		return len(extra) == 0 || (tvLen > 0 && len(extra) == tvLen)
	}
	if !tvExtraOK(m.TVerts1) || !tvExtraOK(m.TVerts2) || !tvExtraOK(m.TVerts3) {
		return false
	}
	perFace := func(rows [][3]int32) bool {
		return len(rows) == 0 || len(rows) == len(m.Faces)
	}
	if !perFace(m.TexIndices0) || !perFace(m.TexIndices1) || !perFace(m.TexIndices2) || !perFace(m.TexIndices3) {
		return false
	}
	nv := len(m.Verts)
	if len(m.Tangents) > 0 && len(m.Tangents) != nv {
		return false
	}
	if len(m.Normals) > 0 && len(m.Normals) != nv {
		return false
	}
	if len(m.Colors) > 0 && len(m.Colors) != nv {
		return false
	}
	return true
}

func meshesBitmapMergeCompatible(nodes []*Node) bool {
	sig := meshBitmapMergeSig(nodes[0].Mesh)
	for _, n := range nodes {
		if n.AnimMesh != nil {
			return false
		}
		m := n.Mesh
		if meshBitmapMergeSig(m) != sig || !meshBitmapMergeCompatible(m) {
			return false
		}
	}
	return true
}

func appendTexIndicesFace(dst *[][3]int32, src [][3]int32, faceIdx int, off int32) {
	if faceIdx >= len(src) {
		return
	}
	row := src[faceIdx]
	*dst = append(*dst, [3]int32{row[0] + off, row[1] + off, row[2] + off})
}

// MergeByBitmap merges sibling trimesh nodes that share the same bitmap.
// Returns the number of merge groups processed.
func MergeByBitmap(model *Model) int {
	// Group children by parent, then by bitmap
	type key struct {
		parent string
		bitmap string
	}
	groups := map[key][]*Node{}
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil {
			continue
		}
		nt := n.NodeType()
		if nt != "trimesh" {
			continue
		}
		k := key{parent: n.Parent, bitmap: strings.ToLower(n.Mesh.Bitmap)}
		groups[k] = append(groups[k], n)
	}

	merged := 0
	for _, nodes := range groups {
		if len(nodes) < 2 {
			continue
		}
		dst := nodes[0]
		if dst.AnimMesh != nil || !meshesBitmapMergeCompatible(nodes) {
			continue
		}
		m := dst.Mesh
		mergedAny := false
		for _, srcNode := range nodes[1:] {
			src := srcNode.Mesh
			vertOff := int32(len(m.Verts))
			tvertOff := int32(len(m.TVerts))
			tvert1Off := int32(len(m.TVerts1))
			tvert2Off := int32(len(m.TVerts2))
			tvert3Off := int32(len(m.TVerts3))

			m.Verts = append(m.Verts, src.Verts...)
			if len(m.Normals) > 0 && len(src.Normals) > 0 {
				m.Normals = append(m.Normals, src.Normals...)
			}
			if len(m.TVerts) > 0 && len(src.TVerts) > 0 {
				m.TVerts = append(m.TVerts, src.TVerts...)
			}
			if len(m.TVerts1) > 0 && len(src.TVerts1) > 0 {
				m.TVerts1 = append(m.TVerts1, src.TVerts1...)
			}
			if len(m.TVerts2) > 0 && len(src.TVerts2) > 0 {
				m.TVerts2 = append(m.TVerts2, src.TVerts2...)
			}
			if len(m.TVerts3) > 0 && len(src.TVerts3) > 0 {
				m.TVerts3 = append(m.TVerts3, src.TVerts3...)
			}
			if len(m.Colors) > 0 && len(src.Colors) > 0 {
				m.Colors = append(m.Colors, src.Colors...)
			}
			if len(m.Tangents) > 0 && len(src.Tangents) > 0 {
				m.Tangents = append(m.Tangents, src.Tangents...)
			}
			srcHasTV := len(src.TVerts) > 0
			for fi, f := range src.Faces {
				nf := f
				nf.Verts[0] += vertOff
				nf.Verts[1] += vertOff
				nf.Verts[2] += vertOff
				if srcHasTV {
					nf.UVs[0] += tvertOff
					nf.UVs[1] += tvertOff
					nf.UVs[2] += tvertOff
				}
				m.Faces = append(m.Faces, nf)
				appendTexIndicesFace(&m.TexIndices0, src.TexIndices0, fi, tvertOff)
				appendTexIndicesFace(&m.TexIndices1, src.TexIndices1, fi, tvert1Off)
				appendTexIndicesFace(&m.TexIndices2, src.TexIndices2, fi, tvert2Off)
				appendTexIndicesFace(&m.TexIndices3, src.TexIndices3, fi, tvert3Off)
			}
			srcNode.Mesh = nil
			mergedAny = true
		}
		if mergedAny {
			merged++
		}
	}
	return merged
}

// SnapVertices snaps vertex positions to a grid.
// mode: "binary" (1/128), "decimal" (0.01), "fine" (0.001)
func SnapVertices(model *Model, mode string) {
	var grid float32
	switch strings.ToLower(mode) {
	case "binary":
		grid = 1.0 / 128.0
	case "decimal":
		grid = 0.01
	case "fine":
		grid = 0.001
	default:
		return
	}
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil {
			continue
		}
		for i := range n.Mesh.Verts {
			n.Mesh.Verts[i] = snapVec3(n.Mesh.Verts[i], grid)
		}
	}
}

func snapVec3(v Vec3, grid float32) Vec3 {
	return Vec3{
		X: snapFloat(v.X, grid),
		Y: snapFloat(v.Y, grid),
		Z: snapFloat(v.Z, grid),
	}
}

func snapFloat(f, grid float32) float32 {
	if grid <= 0 {
		return f
	}
	return float32(math.Round(float64(f)/float64(grid))) * grid
}
