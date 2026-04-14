// compiler_node.go — geometry node writing for the binary MDL compiler.
// Every write mirrors the exact read order in binary.go readNode and sub-readers.
package mdl

import (
	"math"
	"strings"
)

// writeNode writes a geometry node and all its children recursively.
// parentOff is the core offset of the parent node (0 for root).
// count is incremented once per node.
// Cycle safety: if n has already been written (its offset is in c.nodeOffsets),
// we return the existing offset without re-writing, breaking any parent cycles
// present in malformed ASCII source files.
func (c *compiler) writeNode(n *Node, parentOff int32, count *int32) int32 {
	nodeKey := strings.ToLower(n.Name)
	if off, already := c.nodeOffsets[nodeKey]; already {
		return off
	}
	nodeOff := int32(c.core.len())
	c.nodeOffsets[nodeKey] = nodeOff
	*count++

	partNum := c.nodeIDs[strings.ToLower(n.Name)]

	// ---- header_node (112 bytes) ----
	// Ref: binary.go readNodeDepth lines 484-511
	c.core.zeros(24)              // p_func1..p_func6 (6×4 = 24 bytes)
	c.core.i32le(n.InheritColor) // inheritColor
	c.core.i32le(partNum)        // node_number / m_ID
	c.core.fixedStr(n.Name, 32)  // char node_name[32]
	c.core.i32le(0)              // p_geometry  (tree ptr — engine fills at runtime)
	c.core.i32le(parentOff)      // p_parent_node

	// array_definition children {ptr, count, alloc}
	childListPtrPos := c.core.placeholder()
	childListNumPos := c.core.placeholder()
	childListAlcPos := c.core.placeholder()

	// array_definition controller_keys
	ctrlKeysPtrPos := c.core.placeholder()
	ctrlKeysNumPos := c.core.placeholder()
	ctrlKeysAlcPos := c.core.placeholder()

	// array_definition controller_data
	ctrlDataPtrPos := c.core.placeholder()
	ctrlDataNumPos := c.core.placeholder()
	ctrlDataAlcPos := c.core.placeholder()

	contentBits := n.NodeTypeFlag()
	c.core.u32le(contentBits) // content_node bitfield

	hasLight := contentBits&0x02 != 0
	hasEmitter := contentBits&0x04 != 0
	hasReference := contentBits&0x10 != 0
	hasMesh := contentBits&0x20 != 0
	hasSkin := contentBits&0x40 != 0
	hasAnimMesh := contentBits&0x80 != 0
	hasDangly := contentBits&0x100 != 0
	hasAABB := contentBits&0x200 != 0

	// Type-specific headers — same order as binary.go lines 548-591.
	if hasLight {
		c.writeLightHeader(n)
	}
	if hasEmitter {
		c.writeEmitterHeader(n)
	}
	// camera (bit 3): no extra header data
	if hasReference {
		c.writeReferenceHeader(n)
	}

	// Type-specific headers — all fixed-size, written contiguously.
	var animVertsPtrPos, animTVertsPtrPos int
	var animNVertSets, animNTVertSets int

	var expanded *expandedMesh
	if hasMesh {
		expanded = c.writeMeshHeaderFull(n)
	}
	if hasSkin {
		c.writeSkinHeader(n, expanded)
	}
	if hasAnimMesh {
		animVertsPtrPos, animTVertsPtrPos, animNVertSets, animNTVertSets = c.writeAnimMeshHeader(n, nil)
	}
	if hasDangly {
		c.writeDanglyHeader(n)
	}
	if hasAABB {
		c.writeAABBHeader(n)
	}

	// Variable-length data pointed to by headers above. Written after all
	// fixed-size headers so that header offsets are contiguous — matching
	// the layout the decompiler reads sequentially.
	if hasMesh {
		c.writeMeshFaceData(n.Mesh, expanded)
	}
	if hasDangly {
		c.writeDanglyConstraints(n)
	}
	if hasAnimMesh && n.AnimMesh != nil {
		if animNVertSets > 0 {
			off := int32(c.core.len())
			c.core.patchU32(animVertsPtrPos, uint32(off))
			for _, v := range n.AnimMesh.AnimVerts {
				c.core.vec3(v)
			}
		}
		if animNTVertSets > 0 {
			off := int32(c.core.len())
			c.core.patchU32(animTVertsPtrPos, uint32(off))
			for _, v := range n.AnimMesh.AnimTVerts {
				c.core.f32le(v.X)
				c.core.f32le(v.Y)
			}
		}
	}

	// ---- Controllers ----
	ctrlKeys, timeArr, dataArr := c.encodeGeomNodeControllers(n)
	c.writeCtrlBlock(ctrlKeys, timeArr, dataArr,
		ctrlKeysPtrPos, ctrlKeysNumPos, ctrlKeysAlcPos,
		ctrlDataPtrPos, ctrlDataNumPos, ctrlDataAlcPos)

	// ---- Children ----
	children := c.childrenOf(n)

	childArrayOff := int32(c.core.len())
	childPtrOffs := make([]int, len(children))
	for i := range children {
		childPtrOffs[i] = c.core.placeholder()
	}

	if len(children) > 0 {
		c.core.patchU32(childListPtrPos, uint32(childArrayOff))
	}
	c.core.patchU32(childListNumPos, uint32(len(children)))
	c.core.patchU32(childListAlcPos, uint32(len(children)))

	for i, child := range children {
		childOff := c.writeNode(child, nodeOff, count)
		c.core.patchU32(childPtrOffs[i], uint32(childOff))
	}

	return nodeOff
}

// writeLightHeader writes header_light (92 bytes).
// Ref: binary.go readLightHeader (lines 618-681)
// Layout: flareRadius(4) + unknown_list(12) + 4×ProxyList(48) + 7×int32(28) = 92 bytes
func (c *compiler) writeLightHeader(n *Node) {
	l := n.Light
	if l == nil {
		c.core.zeros(92)
		return
	}
	c.core.f32le(l.FlareRadius)
	c.core.proxyListEmpty() // unknown array_definition
	// flare_sizes
	flareSizesPtrPos := c.core.placeholder()
	c.core.u32le(uint32(len(l.FlareSizes)))
	c.core.u32le(uint32(len(l.FlareSizes)))
	// flare_positions
	flarePosPtrPos := c.core.placeholder()
	c.core.u32le(uint32(len(l.FlarePositions)))
	c.core.u32le(uint32(len(l.FlarePositions)))
	// flare_color_shifts
	flareColorPtrPos := c.core.placeholder()
	c.core.u32le(uint32(len(l.FlareColorShifts)))
	c.core.u32le(uint32(len(l.FlareColorShifts)))
	// flare_textures
	flareTexPtrPos := c.core.placeholder()
	c.core.u32le(uint32(len(l.TextureNames)))
	c.core.u32le(uint32(len(l.TextureNames)))

	c.core.i32le(l.LightPriority)
	c.core.i32le(l.AmbientOnly)
	c.core.i32le(l.NDynamicType)
	c.core.i32le(l.AffectDynamic)
	c.core.i32le(l.Shadow)
	c.core.i32le(l.GenerateFlare)
	c.core.i32le(l.FadingLight)
	// 92 bytes ✓

	// Write flare data inline after the header.
	if len(l.FlareSizes) > 0 {
		off := int32(c.core.len())
		c.core.patchU32(flareSizesPtrPos, uint32(off))
		for _, v := range l.FlareSizes {
			c.core.f32le(v)
		}
	}
	if len(l.FlarePositions) > 0 {
		off := int32(c.core.len())
		c.core.patchU32(flarePosPtrPos, uint32(off))
		for _, v := range l.FlarePositions {
			c.core.f32le(v)
		}
	}
	if len(l.FlareColorShifts) > 0 {
		off := int32(c.core.len())
		c.core.patchU32(flareColorPtrPos, uint32(off))
		for _, v := range l.FlareColorShifts {
			c.core.vec3(v)
		}
	}
	if len(l.TextureNames) > 0 {
		// Texture names are stored as pointer array → each points to a 64-byte name.
		ptrArrayOff := int32(c.core.len())
		c.core.patchU32(flareTexPtrPos, uint32(ptrArrayOff))
		namePtrOffs := make([]int, len(l.TextureNames))
		for i := range l.TextureNames {
			namePtrOffs[i] = c.core.placeholder()
		}
		for i, name := range l.TextureNames {
			nameOff := int32(c.core.len())
			c.core.patchU32(namePtrOffs[i], uint32(nameOff))
			c.core.fixedStr(name, 64)
		}
	}
}

// writeEmitterHeader writes header_emitter (216 bytes).
// Ref: binary.go readEmitterHeader (lines 684-723)
// 4*3 + 4*3 + 32*3 + 64 + 16 + 4*2 + 2 + 2 + 4 = 216 bytes
func (c *compiler) writeEmitterHeader(n *Node) {
	em := n.Emitter
	if em == nil {
		c.core.zeros(216)
		return
	}
	c.core.f32le(em.DeadSpace)
	c.core.f32le(em.BlastRadius)
	c.core.f32le(em.BlastLength)
	c.core.u32le(uint32(em.XGrid))
	c.core.u32le(uint32(em.YGrid))
	c.core.u32le(uint32(em.SpawnType))
	c.core.fixedStr(em.Update, 32)
	c.core.fixedStr(em.Render, 32)
	c.core.fixedStr(em.Blend, 32)
	c.core.fixedStr(em.Texture, 64)
	chunk := em.ChunkName
	if chunk == "" {
		chunk = "CHUNK"
	}
	c.core.fixedStr(chunk, 16)
	c.core.u32le(uint32(em.TwoSidedTex))
	c.core.u32le(uint32(em.Loop))
	c.core.u16le(uint16(em.RenderOrder))
	c.core.zeros(2) // padding
	// Pack flags bitfield
	flags := uint32(0)
	flags |= boolBit(em.P2P, 0)
	flags |= boolBit(em.P2PSel, 1)
	flags |= boolBit(em.AffectedByWind, 2)
	flags |= boolBit(em.IsTinted, 3)
	flags |= boolBit(em.Bounce, 4)
	flags |= boolBit(em.Random, 5)
	flags |= boolBit(em.Inherit, 6)
	flags |= boolBit(em.InheritVel, 7)
	flags |= boolBit(em.InheritLocal, 8)
	flags |= boolBit(em.Splat, 9)
	flags |= boolBit(em.InheritPart, 10)
	c.core.u32le(flags)
}

func boolBit(v int32, bit uint) uint32 {
	if v != 0 {
		return 1 << bit
	}
	return 0
}

// writeReferenceHeader writes header_reference (68 bytes).
// Ref: binary.go readReferenceHeader (lines 726-730)
func (c *compiler) writeReferenceHeader(n *Node) {
	ref := n.Reference
	if ref == nil {
		c.core.zeros(68)
		return
	}
	c.core.fixedStr(ref.RefModel, 64)
	c.core.i32le(ref.Reattachable)
}

// writeSkinHeader writes header_skin (100 bytes) and skin MDX data.
// Ref: binary.go readSkinHeader (lines 902-972)
// Layout: weights_ProxyList(12) + 4×int32(16) + 3×ProxyList(36) + 17×int16+spare(36) = 100 bytes
//
// SkinData.Weights[i] is indexed by original mesh vertex. The binary format
// expects expanded GPU vertices (N_faces*3), so we expand skin data using face indices.
func (c *compiler) writeSkinHeader(n *Node, exp *expandedMesh) {
	skin := n.Skin
	mesh := n.Mesh
	if skin == nil || mesh == nil {
		c.core.zeros(192)
		return
	}

	boneIndexMap := make(map[string]int)
	var boneNames []string
	addBone := func(name string) int {
		key := strings.ToLower(name)
		if idx, ok := boneIndexMap[key]; ok {
			return idx
		}
		if len(boneNames) >= 64 {
			return -1
		}
		idx := len(boneNames)
		boneNames = append(boneNames, name)
		boneIndexMap[key] = idx
		return idx
	}
	for _, vw := range skin.Weights {
		for _, b := range vw.Bones {
			if b != "" {
				addBone(b)
			}
		}
	}

	var bonePartNums [64]int16
	for i := range bonePartNums {
		bonePartNums[i] = -1
	}
	for i, name := range boneNames {
		if i >= 64 {
			break
		}
		if id, ok := c.nodeIDs[strings.ToLower(name)]; ok {
			bonePartNums[i] = int16(id)
		}
	}

	// GPU vertex count matches the deduplicated count from buildExpandedMesh.
	nGPUVerts := len(mesh.Faces) * 3
	if exp != nil {
		nGPUVerts = len(exp.positions)
	}

	// MDX offsets for weight and bone-ref arrays.
	wgtMdxOff := int32(c.vol.len())
	boneRefMdxOff := int32(c.vol.len() + nGPUVerts*16) // after N×4 floats

	c.core.proxyListEmpty() // weights ProxyList (legacy)
	c.core.i32le(wgtMdxOff)
	c.core.i32le(boneRefMdxOff)
	c.core.i32le(0) // boneindexarray
	c.core.i32le(0) // boneindexarraysize
	c.core.proxyListEmpty() // qbone_ref_inv
	c.core.proxyListEmpty() // tbone_ref_inv
	c.core.proxyListEmpty() // boneconstantindices
	for i := 0; i < 64; i++ {
		c.core.u16le(uint16(bonePartNums[i]))
	}

	// Build per-GPU-vertex weight/bone-ref arrays using the origVert mapping.
	wgts := make([][4]float32, nGPUVerts)
	brefs := make([][4]int16, nGPUVerts)
	for gpuIdx := 0; gpuIdx < nGPUVerts; gpuIdx++ {
		origIdx := -1
		if exp != nil && gpuIdx < len(exp.origVert) {
			origIdx = int(exp.origVert[gpuIdx])
		} else if gpuIdx/3 < len(mesh.Faces) {
			origIdx = int(mesh.Faces[gpuIdx/3].Verts[gpuIdx%3])
		}
		if origIdx >= 0 && origIdx < len(skin.Weights) {
			vw := skin.Weights[origIdx]
			for bi := 0; bi < 4 && bi < len(vw.Bones); bi++ {
				w := float32(0)
				if bi < len(vw.Weights) {
					w = vw.Weights[bi]
				}
				wgts[gpuIdx][bi] = w
				boneIdx := int16(-1)
				if vw.Bones[bi] != "" {
					if idx, ok := boneIndexMap[strings.ToLower(vw.Bones[bi])]; ok {
						boneIdx = int16(idx)
					}
				}
				brefs[gpuIdx][bi] = boneIdx
			}
		}
	}

	// Write weight data to MDX: nGPUVerts × 4 floats
	for i := 0; i < nGPUVerts; i++ {
		c.vol.f32le(wgts[i][0])
		c.vol.f32le(wgts[i][1])
		c.vol.f32le(wgts[i][2])
		c.vol.f32le(wgts[i][3])
	}
	// Write bone-ref data to MDX: nGPUVerts × 4 int16
	for i := 0; i < nGPUVerts; i++ {
		c.vol.u16le(uint16(brefs[i][0]))
		c.vol.u16le(uint16(brefs[i][1]))
		c.vol.u16le(uint16(brefs[i][2]))
		c.vol.u16le(uint16(brefs[i][3]))
	}
}

// writeDanglyHeader writes header_dangly (24 bytes).
// Constraint data is deferred to writeDanglyConstraints so that subsequent
// headers (aabb) are at the correct offset.
// Ref: binary.go readDanglyHeader (lines 1001-1015)
// Layout: constraints_ProxyList(12) + 3×float32(12) = 24 bytes
func (c *compiler) writeDanglyHeader(n *Node) {
	dangly := n.Dangly
	if dangly == nil {
		c.core.zeros(24)
		return
	}
	c.danglyConstraintsPtrPos = c.core.placeholder()
	c.core.u32le(uint32(len(dangly.Constraints)))
	c.core.u32le(uint32(len(dangly.Constraints)))
	c.core.f32le(dangly.Displacement)
	c.core.f32le(dangly.Tightness)
	c.core.f32le(dangly.Period)
	// 24 bytes ✓
}

// writeDanglyConstraints writes constraint float data to core (deferred from writeDanglyHeader).
func (c *compiler) writeDanglyConstraints(n *Node) {
	dangly := n.Dangly
	if dangly == nil || len(dangly.Constraints) == 0 {
		return
	}
	constraintsOff := int32(c.core.len())
	c.core.patchU32(c.danglyConstraintsPtrPos, uint32(constraintsOff))
	for _, v := range dangly.Constraints {
		c.core.f32le(v)
	}
}

// writeAABBHeader writes header_aabb (4 bytes) and inline AABB tree.
// Ref: binary.go readAABBHeader (lines 1018-1024)
func (c *compiler) writeAABBHeader(n *Node) int32 {
	aabb := n.Aabb
	if aabb == nil || n.Mesh == nil || len(n.Mesh.Faces) == 0 {
		c.core.zeros(4)
		return 0
	}

	aabbRootPtrPos := c.core.placeholder() // pAABB

	// Build and write AABB tree.
	tree := buildAABBTree(n.Mesh)
	if len(tree) == 0 {
		return 0
	}

	// Write all nodes; record their core offsets.
	nodeBaseOff := int32(c.core.len())
	offsets := make([]int32, len(tree))
	for i := range tree {
		offsets[i] = nodeBaseOff + int32(i)*40
	}

	c.core.patchU32(aabbRootPtrPos, uint32(offsets[0]))

	for _, entry := range tree {
		c.core.vec3(entry.BoundMin)
		c.core.vec3(entry.BoundMax)
		if entry.Left >= 0 && entry.Left < len(tree) {
			c.core.u32le(uint32(offsets[entry.Left]))
		} else {
			c.core.u32le(0)
		}
		if entry.Right >= 0 && entry.Right < len(tree) {
			c.core.u32le(uint32(offsets[entry.Right]))
		} else {
			c.core.u32le(0)
		}
		c.core.i32le(int32(entry.LeafFace))
		c.core.u32le(entry.Plane)
	}

	return offsets[0]
}

// aabbTreeEntry is an internal AABB tree node used during compilation.
type aabbTreeEntry struct {
	BoundMin Vec3
	BoundMax Vec3
	LeafFace int  // -1 for internal
	Plane    uint32
	Left     int // index into tree slice, -1 if absent
	Right    int
}

// buildAABBTree builds a simple median-split AABB tree from mesh faces.
// Each entry is 40 bytes in binary: bmin(12)+bmax(12)+left(4)+right(4)+face(4)+plane(4).
func buildAABBTree(mesh *MeshData) []aabbTreeEntry {
	if mesh == nil || len(mesh.Faces) == 0 {
		return nil
	}
	faceIdxs := make([]int, len(mesh.Faces))
	for i := range faceIdxs {
		faceIdxs[i] = i
	}
	var out []aabbTreeEntry
	buildAABBNode(mesh, faceIdxs, &out)
	return out
}

func buildAABBNode(mesh *MeshData, faces []int, out *[]aabbTreeEntry) int {
	bmin, bmax := facesAABB(mesh, faces)
	idx := len(*out)
	*out = append(*out, aabbTreeEntry{BoundMin: bmin, BoundMax: bmax, LeafFace: -1, Left: -1, Right: -1})

	if len(faces) == 1 {
		(*out)[idx].LeafFace = faces[0]
		(*out)[idx].Plane = axisPlane(bmin, bmax)
		return idx
	}

	axis := longestAABBAxis(bmin, bmax)
	(*out)[idx].Plane = uint32(1 << uint(axis))

	// Median split along chosen axis.
	mid := len(faces) / 2
	sortFacesByAxis(mesh, faces, axis)

	left := buildAABBNode(mesh, faces[:mid], out)
	right := buildAABBNode(mesh, faces[mid:], out)
	(*out)[idx].Left = left
	(*out)[idx].Right = right
	return idx
}

func facesAABB(mesh *MeshData, faceIdxs []int) (bmin, bmax Vec3) {
	bmin = Vec3{X: math.MaxFloat32, Y: math.MaxFloat32, Z: math.MaxFloat32}
	bmax = Vec3{X: -math.MaxFloat32, Y: -math.MaxFloat32, Z: -math.MaxFloat32}
	for _, fi := range faceIdxs {
		f := mesh.Faces[fi]
		for _, vi := range f.Verts {
			if vi < 0 || int(vi) >= len(mesh.Verts) {
				continue
			}
			v := mesh.Verts[vi]
			if v.X < bmin.X { bmin.X = v.X }
			if v.Y < bmin.Y { bmin.Y = v.Y }
			if v.Z < bmin.Z { bmin.Z = v.Z }
			if v.X > bmax.X { bmax.X = v.X }
			if v.Y > bmax.Y { bmax.Y = v.Y }
			if v.Z > bmax.Z { bmax.Z = v.Z }
		}
	}
	return
}

func longestAABBAxis(bmin, bmax Vec3) int {
	dx := bmax.X - bmin.X
	dy := bmax.Y - bmin.Y
	dz := bmax.Z - bmin.Z
	if dx >= dy && dx >= dz {
		return 0
	}
	if dy >= dz {
		return 1
	}
	return 2
}

func axisPlane(bmin, bmax Vec3) uint32 {
	return uint32(1 << uint(longestAABBAxis(bmin, bmax)))
}

func faceCentroidAxis(mesh *MeshData, fi, axis int) float32 {
	f := mesh.Faces[fi]
	var sum float32
	count := 0
	for _, vi := range f.Verts {
		if vi >= 0 && int(vi) < len(mesh.Verts) {
			sum += mesh.Verts[vi].Index(axis)
			count++
		}
	}
	if count == 0 {
		return 0
	}
	return sum / float32(count)
}

func sortFacesByAxis(mesh *MeshData, faces []int, axis int) {
	// Simple insertion sort (face counts are rarely huge for AABB meshes).
	for i := 1; i < len(faces); i++ {
		key := faces[i]
		keyC := faceCentroidAxis(mesh, key, axis)
		j := i - 1
		for j >= 0 && faceCentroidAxis(mesh, faces[j], axis) > keyC {
			faces[j+1] = faces[j]
			j--
		}
		faces[j+1] = key
	}
}
