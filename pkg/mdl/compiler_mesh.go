// compiler_mesh.go — 512-byte mesh header and MDX vertex data writing.
// Every write field mirrors the read in binary.go readMeshHeader exactly.
package mdl

import (
	"fmt"
	"math"
)

// vertKey is a bit-exact key for vertex deduplication.
// Using uint32 bit patterns avoids float comparison ambiguity.
type vertKey struct {
	px, py, pz     uint32 // position bits
	ux, uy         uint32 // UV0 bits
	u1x, u1y       uint32 // UV1 bits
	u2x, u2y       uint32 // UV2 bits
	u3x, u3y       uint32 // UV3 bits
	nx, ny, nz     uint32 // normal bits
	cr, cg, cb     uint32 // color bits
}

func f32bits(f float32) uint32 { return math.Float32bits(f) }

// writeMeshHeaderFull writes the full 512-byte header_mesh and any inline data
// (faces array), then appends vertex data to the MDX (volatile) buffer.
// Returns: (faceArrayOff, mdxVertOff, countVerts, mdxTex0Off) for patching.
func (c *compiler) writeMeshHeaderFull(n *Node) *expandedMesh {
	mesh := n.Mesh
	if mesh == nil {
		c.core.zeros(meshHeaderSize)
		return nil
	}
	c.writeMeshHeaderInner(mesh, n)
	return c.lastExpanded
}

// writeMeshHeaderInner writes exactly 512 bytes of header_mesh.
// It also writes the face array to core and vertex data to MDX.
// Returns: (facesPtrField, mdxVertOff, countVerts, mdxTex0Off) — all core/MDX offsets.
func (c *compiler) writeMeshHeaderInner(mesh *MeshData, n *Node) (facesPtrField, mdxVertOff int32, countVerts uint16, mdxTex0Off int32) {
	// Generate normals and default colors if not present in the ASCII source.
	generateNormals(mesh)
	// NOTE: nwnmdlcomp generates default white colors, but the NWN:EE game
	// compiler does NOT — it only writes colors when the ASCII source has
	// explicit color data. We match the game's behavior.

	// Build expanded vertex data (one GPU vertex per face-vertex reference).
	expanded, err := buildExpandedMesh(mesh)
	if err != nil {
		c.err = err
		c.core.zeros(meshHeaderSize)
		return -1, -1, 0, -1
	}
	c.lastExpanded = &expanded

	// Resolve tangents up-front so the header can decide whether to emit a
	// real MDX pointer or leave -1 ("not present"). Generation needs the
	// expanded mesh's positions, normals, and UV0 — meshes without UVs
	// (e.g. AABB walkmesh) keep both pointers at -1.
	tangentsOut, bitangentsOut := resolveTangents(mesh, &expanded)

	// uint32 p_func1, p_func2 (8 bytes) — engine fills at load
	c.core.zeros(8)

	// array_definition faces {offset, count, alloc}
	facesPtrPos := c.core.placeholder() // will be patched after faces are written
	c.core.u32le(uint32(len(mesh.Faces)))
	c.core.u32le(uint32(len(mesh.Faces)))

	// bmin, bmax (24 bytes)
	bmin, bmax, center, radius := computeBounds(expanded.positions)
	c.core.vec3(bmin)
	c.core.vec3(bmax)

	// float radius (4)
	c.core.f32le(radius)

	// vertex average / center (12)
	c.core.vec3(center)

	// diffuse, ambient, specular (36)
	c.core.vec3(mesh.Diffuse)
	c.core.vec3(mesh.Ambient)
	c.core.vec3(mesh.Specular)

	// float shininess (4)
	c.core.f32le(mesh.Shininess)

	// shadow, beaming, render, transparencyhint (16)
	c.core.i32le(mesh.Shadow)
	c.core.i32le(mesh.Beaming)
	c.core.i32le(mesh.Render)
	c.core.i32le(mesh.TransparencyHint)

	// uint32 unknown1 / renderHint (4)
	renderHintVal := uint32(0)
	switch mesh.RenderHint {
	case "NormalAndSpecMapped":
		renderHintVal = 2
	}
	c.core.u32le(renderHintVal)

	// char texture0[64], texture1[64], texture2[64], materialName[64] (256)
	c.core.fixedStr(mesh.Bitmap, 64)
	c.core.fixedStr(mesh.Texture1, 64)
	c.core.fixedStr(mesh.Texture2, 64)
	c.core.fixedStr(mesh.MaterialName, 64)

	// uint32 tile_fade (4)
	c.core.i32le(mesh.TileFade)

	// array_definition vertex_indices (deprecated, always empty) (12)
	c.core.proxyListEmpty()
	// array_definition face_leftover (deprecated, always empty) (12)
	c.core.proxyListEmpty()
	// array_definition vertex_indices_count (deprecated, always empty) (12)
	c.core.proxyListEmpty()
	// array_definition vertex_indices_offset / m_listVertexTokenIndices (12)
	c.core.proxyListEmpty()

	// int32 p_mdx_unknown1 / m_nLeftOverFacesToken (4)
	c.core.u32le(0xFFFFFFFF)
	// uint32 unknown2 / m_nLeftOverFacesCount (4)
	c.core.u32le(0)
	// mesh_type type / m_nMode (4)
	c.core.u32le(0)
	// int32 p_start_mdx / m_pPostProcessInfo (4)
	c.core.i32le(0)

	// int32 p_mdx_vertex — MDX pointer to vertex positions (4)
	mdxVertOff = int32(c.vol.len())
	mdxVertPtrPos := c.core.len()
	c.core.i32le(0) // placeholder — we patch this below
	// (must be i32: value is written as int32, read as int32 in binary.go)

	// uint16 count_vertexes, count_textures (4)
	countVerts = uint16(len(expanded.positions))
	c.core.u16le(countVerts)
	nTexStages := uint16(expanded.texStages)
	c.core.u16le(nTexStages)

	// int32 p_mdx_texture0..3 (16)
	mdxTex0Off = int32(-1)
	mdxTex0PtrPos := -1
	mdxTex1PtrPos := -1
	mdxTex2PtrPos := -1
	mdxTex3PtrPos := -1
	if expanded.texStages > 0 {
		mdxTex0PtrPos = c.core.len()
		c.core.i32le(0)
	} else {
		c.core.i32le(-1)
	}
	if expanded.texStages > 1 {
		mdxTex1PtrPos = c.core.len()
		c.core.i32le(0)
	} else {
		c.core.i32le(-1)
	}
	if expanded.texStages > 2 {
		mdxTex2PtrPos = c.core.len()
		c.core.i32le(0)
	} else {
		c.core.i32le(-1)
	}
	if expanded.texStages > 3 {
		mdxTex3PtrPos = c.core.len()
		c.core.i32le(0)
	} else {
		c.core.i32le(-1)
	}

	// int32 p_mdx_vertex_normals (4)
	mdxNormalPtrPos := -1
	if len(expanded.normals) > 0 {
		mdxNormalPtrPos = c.core.len()
		c.core.i32le(0) // placeholder
	} else {
		c.core.i32le(-1)
	}

	// int32 p_mdx_vertex_colors (4)
	mdxColorPtrPos := -1
	if len(expanded.colors) > 0 {
		mdxColorPtrPos = c.core.len()
		c.core.i32le(0) // placeholder
	} else {
		c.core.i32le(-1)
	}

	// int32 p_mdx_tex_anim0,1,2 (12 bytes, deprecated, write -1)
	c.core.i32le(-1)
	c.core.i32le(-1)
	c.core.i32le(-1)
	// int32 p_mdx_tangent / tex_anim3 (4) — EE tangents
	mdxTangentPtrPos := -1
	if len(tangentsOut) > 0 {
		mdxTangentPtrPos = c.core.len()
		c.core.i32le(0) // placeholder, patched after MDX tangent write
	} else {
		c.core.i32le(-1)
	}
	// int32 p_mdx_tex_anim4 (4, deprecated)
	c.core.i32le(-1)
	// int32 p_mdx_bitangent / tex_anim5 (4) — EE bitangent
	mdxBitangentPtrPos := -1
	if len(bitangentsOut) > 0 {
		mdxBitangentPtrPos = c.core.len()
		c.core.i32le(0) // placeholder, patched after MDX bitangent write
	} else {
		c.core.i32le(-1)
	}

	// byte light_mapped, rotate_texture (2)
	c.core.u8(byte(mesh.LightMapped))
	c.core.u8(byte(mesh.RotateTexture))
	c.core.zeros(2) // uint16 padding

	// float vertex_normal_sum / m_nLocalSurfaceArea (4)
	c.core.f32le(0)
	// uint32 unknown3 / m_nRootedSurfaceArea (4)
	c.core.u32le(0)

	// 512 bytes of header complete. Face data is written later by
	// writeMeshFaceData so that subsequent type-specific headers (skin,
	// dangly, aabb) are contiguous in the core buffer — matching the
	// layout the decompiler expects.
	expanded.facesPtrPos = facesPtrPos

	// ---- Write vertex data to MDX (volatile) ----
	// Write positions
	posStart := int32(c.vol.len())
	for _, pos := range expanded.positions {
		c.vol.vec3(pos)
	}
	// Patch pMdxVertex
	c.core.patchU32(mdxVertPtrPos, uint32(posStart))

	// Write UV coords (8 bytes each: 2× float32) per texture stage
	writeUVs := func(uvs []Vec3, ptrPos int) {
		if len(uvs) == 0 || ptrPos < 0 {
			return
		}
		start := int32(c.vol.len())
		for _, uv := range uvs {
			c.vol.f32le(uv.X)
			c.vol.f32le(uv.Y)
		}
		c.core.patchU32(ptrPos, uint32(start))
	}
	writeUVs(expanded.uvs, mdxTex0PtrPos)
	writeUVs(expanded.uvs1, mdxTex1PtrPos)
	writeUVs(expanded.uvs2, mdxTex2PtrPos)
	writeUVs(expanded.uvs3, mdxTex3PtrPos)

	// Write a slice of Vec3 vertex attributes to the volatile MDX block and
	// patch the corresponding header pointer. Mirrors the writeUVs closure
	// above; centralising it keeps the guard semantics consistent across
	// normals, tangents, and bitangents.
	writeVec3Stream := func(data []Vec3, ptrPos int) {
		if len(data) == 0 || ptrPos < 0 {
			return
		}
		start := int32(c.vol.len())
		for _, v := range data {
			c.vol.vec3(v)
		}
		c.core.patchU32(ptrPos, uint32(start))
	}

	writeVec3Stream(expanded.normals, mdxNormalPtrPos)

	// Write colors as 4-byte RGBA (matching binary.go readMDXColors)
	if len(expanded.colors) > 0 && mdxColorPtrPos >= 0 {
		colorStart := int32(c.vol.len())
		for _, col := range expanded.colors {
			c.vol.u8(clampByte(col.X))
			c.vol.u8(clampByte(col.Y))
			c.vol.u8(clampByte(col.Z))
			c.vol.u8(0xFF) // alpha
		}
		c.core.patchU32(mdxColorPtrPos, uint32(colorStart))
	}

	// The decompiler reads both arrays back and reconstructs the per-vertex
	// Vec4 W handedness from sign(dot(cross(normal, tangent), bitangent)) —
	// see binary.go readMDXTangents.
	writeVec3Stream(tangentsOut, mdxTangentPtrPos)
	writeVec3Stream(bitangentsOut, mdxBitangentPtrPos)

	return
}

// writeMeshFaceData writes the face array to core and patches the faces pointer.
// Called after all type-specific headers are written so that variable-length face
// data doesn't shift subsequent headers to wrong offsets.
func (c *compiler) writeMeshFaceData(mesh *MeshData, exp *expandedMesh) {
	if mesh == nil || exp == nil {
		return
	}
	facesOff := int32(c.core.len())
	c.core.patchU32(exp.facesPtrPos, uint32(facesOff))

	for fi, face := range mesh.Faces {
		faceNormal := face.Normal
		var dist float32
		v0i, v1i, v2i := int(face.Verts[0]), int(face.Verts[1]), int(face.Verts[2])
		if v0i >= 0 && v0i < len(mesh.Verts) && v1i >= 0 && v1i < len(mesh.Verts) && v2i >= 0 && v2i < len(mesh.Verts) {
			p1, p2, p3 := mesh.Verts[v0i], mesh.Verts[v1i], mesh.Verts[v2i]
			e1 := Vec3{X: p2.X - p1.X, Y: p2.Y - p1.Y, Z: p2.Z - p1.Z}
			e2 := Vec3{X: p3.X - p2.X, Y: p3.Y - p2.Y, Z: p3.Z - p2.Z}
			n := Vec3{
				X: e1.Y*e2.Z - e1.Z*e2.Y,
				Y: e1.Z*e2.X - e1.X*e2.Z,
				Z: e1.X*e2.Y - e1.Y*e2.X,
			}
			length := float32(math.Sqrt(float64(n.X*n.X + n.Y*n.Y + n.Z*n.Z)))
			if length > 1e-10 {
				faceNormal = Vec3{X: n.X / length, Y: n.Y / length, Z: n.Z / length}
			}
			dist = -(faceNormal.X*p1.X + faceNormal.Y*p1.Y + faceNormal.Z*p1.Z)
		}
		c.core.vec3(faceNormal)
		c.core.f32le(dist)
		c.core.i32le(face.Material)
		c.core.u16le(0xFFFF)
		c.core.u16le(0xFFFF)
		c.core.u16le(0xFFFF)
		c.core.u16le(exp.faceVerts[fi][0])
		c.core.u16le(exp.faceVerts[fi][1])
		c.core.u16le(exp.faceVerts[fi][2])
	}
}

func clampByte(f float32) byte {
	v := int(f*255.0 + 0.5)
	if v < 0 {
		return 0
	}
	if v > 255 {
		return 255
	}
	return byte(v)
}

// expandedMesh holds the deduplicated GPU vertex arrays and per-face-corner indices.
type expandedMesh struct {
	positions  []Vec3
	uvs        []Vec3 // UV0: only X,Y used; stored as 2 floats in MDX
	uvs1       []Vec3 // UV1
	uvs2       []Vec3 // UV2
	uvs3       []Vec3 // UV3
	normals    []Vec3
	colors     []Vec3
	texStages  int
	faceVerts  [][3]uint16 // per-face GPU vertex indices (into the arrays above)
	origVert   []int32     // GPU vertex → original mesh vertex index

	facesPtrPos int // core buffer position of faces array pointer (for deferred patching)
}

// buildExpandedMesh converts an ASCII indexed mesh into GPU vertex arrays,
// deduplicating vertices that share identical position + UV + normal + color.
// Deduplication uses exact bit-level float comparison (no epsilon) so that
// vertices authored as identical in ASCII always merge.
func buildExpandedMesh(mesh *MeshData) (expandedMesh, error) {
	hasUV      := len(mesh.TVerts) > 0
	hasTexIdx0 := len(mesh.TexIndices0) > 0
	hasUV1     := len(mesh.TVerts1) > 0
	hasUV2     := len(mesh.TVerts2) > 0
	hasUV3     := len(mesh.TVerts3) > 0
	hasNormals := len(mesh.Normals) > 0
	hasColors  := len(mesh.Colors) > 0

	exp := expandedMesh{
		faceVerts: make([][3]uint16, len(mesh.Faces)),
	}
	if hasUV  { exp.texStages = 1 }
	if hasUV1 { exp.texStages = 2 }
	if hasUV2 { exp.texStages = 3 }
	if hasUV3 { exp.texStages = 4 }

	cache := make(map[vertKey]uint16, len(mesh.Faces)*3)

	for fi, face := range mesh.Faces {
		for vi := 0; vi < 3; vi++ {
			vertIdx := int(face.Verts[vi])
			uvIdx   := int(face.UVs[vi])

			var pos, uv, uv1, uv2, uv3, nor, col Vec3
			if vertIdx >= 0 && vertIdx < len(mesh.Verts) { pos = mesh.Verts[vertIdx] }
			if hasUV {
				ti := uvIdx
				if hasTexIdx0 && fi < len(mesh.TexIndices0) { ti = int(mesh.TexIndices0[fi][vi]) }
				if ti >= 0 && ti < len(mesh.TVerts) { uv = mesh.TVerts[ti] }
			}

			// Extra UV stages use TexIndices if available, else fall back to uvIdx
			if hasUV1 {
				ti := uvIdx
				if fi < len(mesh.TexIndices1) { ti = int(mesh.TexIndices1[fi][vi]) }
				if ti >= 0 && ti < len(mesh.TVerts1) { uv1 = mesh.TVerts1[ti] }
			}
			if hasUV2 {
				ti := uvIdx
				if fi < len(mesh.TexIndices2) { ti = int(mesh.TexIndices2[fi][vi]) }
				if ti >= 0 && ti < len(mesh.TVerts2) { uv2 = mesh.TVerts2[ti] }
			}
			if hasUV3 {
				ti := uvIdx
				if fi < len(mesh.TexIndices3) { ti = int(mesh.TexIndices3[fi][vi]) }
				if ti >= 0 && ti < len(mesh.TVerts3) { uv3 = mesh.TVerts3[ti] }
			}

			if hasNormals {
				if fi < len(mesh.CornerNormals) {
					nor = mesh.CornerNormals[fi][vi]
				} else if vertIdx >= 0 && vertIdx < len(mesh.Normals) {
					nor = mesh.Normals[vertIdx]
				}
			}
			if hasColors && vertIdx >= 0 && vertIdx < len(mesh.Colors) { col = mesh.Colors[vertIdx] }

			key := vertKey{
				px: f32bits(pos.X), py: f32bits(pos.Y), pz: f32bits(pos.Z),
				ux: f32bits(uv.X),  uy: f32bits(uv.Y),
				u1x: f32bits(uv1.X), u1y: f32bits(uv1.Y),
				u2x: f32bits(uv2.X), u2y: f32bits(uv2.Y),
				u3x: f32bits(uv3.X), u3y: f32bits(uv3.Y),
				nx: f32bits(nor.X), ny: f32bits(nor.Y), nz: f32bits(nor.Z),
				cr: f32bits(col.X), cg: f32bits(col.Y), cb: f32bits(col.Z),
			}

			gpuIdx, ok := cache[key]
			if !ok {
			if len(exp.positions) >= math.MaxUint16 {
				return exp, fmt.Errorf("mesh exceeds 65535 unique GPU vertices (%d); split the mesh", len(exp.positions))
				}
				gpuIdx = uint16(len(exp.positions))
				cache[key] = gpuIdx
				exp.positions = append(exp.positions, pos)
				exp.origVert = append(exp.origVert, int32(vertIdx))
				if hasUV      { exp.uvs  = append(exp.uvs,  uv)  }
				if hasUV1     { exp.uvs1 = append(exp.uvs1, uv1) }
				if hasUV2     { exp.uvs2 = append(exp.uvs2, uv2) }
				if hasUV3     { exp.uvs3 = append(exp.uvs3, uv3) }
				if hasNormals { exp.normals = append(exp.normals, nor) }
				if hasColors  { exp.colors  = append(exp.colors,  col) }
			}
			exp.faceVerts[fi][vi] = gpuIdx
		}
	}

	return exp, nil
}

// computeBounds returns the AABB, center, and bounding sphere radius for a vertex list.
func computeBounds(verts []Vec3) (bmin, bmax, center Vec3, radius float32) {
	if len(verts) == 0 {
		return
	}
	bmin = Vec3{X: math.MaxFloat32, Y: math.MaxFloat32, Z: math.MaxFloat32}
	bmax = Vec3{X: -math.MaxFloat32, Y: -math.MaxFloat32, Z: -math.MaxFloat32}
	for _, v := range verts {
		if v.X < bmin.X { bmin.X = v.X }
		if v.Y < bmin.Y { bmin.Y = v.Y }
		if v.Z < bmin.Z { bmin.Z = v.Z }
		if v.X > bmax.X { bmax.X = v.X }
		if v.Y > bmax.Y { bmax.Y = v.Y }
		if v.Z > bmax.Z { bmax.Z = v.Z }
	}
	// Center is vertex centroid average, matching nwnmdlcomp (not AABB midpoint).
	var sx, sy, sz float64
	for _, v := range verts {
		sx += float64(v.X)
		sy += float64(v.Y)
		sz += float64(v.Z)
	}
	n := float64(len(verts))
	center = Vec3{
		X: float32(sx / n),
		Y: float32(sy / n),
		Z: float32(sz / n),
	}
	for _, v := range verts {
		dx := v.X - center.X
		dy := v.Y - center.Y
		dz := v.Z - center.Z
		r := float32(math.Sqrt(float64(dx*dx + dy*dy + dz*dz)))
		if r > radius {
			radius = r
		}
	}
	return
}
