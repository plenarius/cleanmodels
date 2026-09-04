// compiler_mesh.go — 512-byte mesh header and MDX vertex data writing.
// Every write field mirrors the read in binary.go readMeshHeader exactly.
package mdl

import (
	"fmt"
	"math"
)

// vertKey is a bit-exact key for vertex deduplication.
// Using uint32 bit patterns avoids float comparison ambiguity.
//
// The game compiler splits at material and smoothing-group boundaries
// even when those values would otherwise share a GPU vertex with the
// same position/UV/normal/color. We must include them in the dedup key
// to match the game's vertex count exactly. Without these, multi-
// material meshes (e.g. character body parts with 4-6 material indices)
// over-merge by 30-50%, which then perturbs Mikktspace tangents at
// material/SG boundaries and is the dominant cause of the ~1-3% bad-
// alignment corners observed in the oracle suite.
type vertKey struct {
	px, py, pz     uint32 // position bits
	ux, uy         uint32 // UV0 bits
	u1x, u1y       uint32 // UV1 bits
	u2x, u2y       uint32 // UV2 bits
	u3x, u3y       uint32 // UV3 bits
	nx, ny, nz     uint32 // normal bits
	cr, cg, cb     uint32 // color bits
	mat            int32  // face material index
	sg             int32  // face smoothing group
}

func f32bits(f float32) uint32 { return math.Float32bits(f) }

// writeMeshHeaderFull writes the full 512-byte header_mesh and any inline data
// (faces array), then appends vertex data to the MDX (volatile) buffer.
// Returns: (faceArrayOff, mdxVertOff, countVerts, mdxTex0Off) for patching.
func (c *compiler) writeMeshHeaderFull(n *Node) *expandedMesh {
	mesh := n.Mesh
	if mesh == nil {
		c.writeEmptyMeshHeader()
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
		c.writeEmptyMeshHeader()
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

	// array_definition vertex_indices_count / vertextokenindices —
	// m_listVertexTokenIndices. NOT deprecated: this is the actual GPU index
	// buffer the engine draws from. The "faces" array's embedded per-face
	// vertex indices are apparently only consulted by CPU-side systems
	// (picking/mouse-hover highlighting) — the real render path needs a flat
	// uint16 index buffer written into the MDX/volatile block, referenced by
	// exactly one element each in these two lists: vertexindicescount's
	// element holds the total index count (faceCount*3), and
	// vertextokenindices' element holds the MDX-relative offset of that
	// buffer. Verified against the retail helm_010.mdl: both point at
	// single uint32 "arrays" holding 438 (=146 faces*3) and an MDX offset
	// whose data matches the faces array's vertex indices exactly.
	//
	// The compiler always left these empty, so the GPU received a
	// zero-length index buffer for every mesh it ever compiled — geometry,
	// mesh header, and every controller could be perfectly correct and the
	// mesh would still draw nothing, which is exactly what issue #12 saw.
	// The actual element values aren't known until the index buffer itself
	// is written (writeMeshFaceData, after all type-specific headers), so
	// these two are placeholders patched there — mirroring how facesPtrPos
	// is deferred already.
	if len(mesh.Faces) > 0 {
		expanded.indexCountListPtrPos = c.core.placeholder()
		c.core.u32le(1) // count
		c.core.u32le(1) // alloc
		expanded.indexOffsetListPtrPos = c.core.placeholder()
		c.core.u32le(1) // count
		c.core.u32le(1) // alloc
	} else {
		expanded.indexCountListPtrPos = -1
		expanded.indexOffsetListPtrPos = -1
		c.core.proxyListEmpty()
		c.core.proxyListEmpty()
	}

	// int32 p_mdx_unknown1 / m_nLeftOverFacesToken (4)
	c.core.u32le(0xFFFFFFFF)
	// uint32 unknown2 / m_nLeftOverFacesCount (4)
	c.core.u32le(0)
	// mesh_type / m_nMode (4) — AuroraPrimitiveTypes: the GPU primitive type
	// the vertex/index buffers below should be drawn as. 3 = triangle list,
	// the only kind cleanmodels ever emits (we never generate strips).
	//
	// binary.go's decompiler skips this field outright (d.skip(4)) instead of
	// reading it into the Model struct, so there has never been an ASCII
	// representation of it, a way to round-trip it, or a test that could
	// catch it being wrong. The compiler wrote a hardcoded 0 here — not a
	// valid AuroraPrimitiveTypes value — for every mesh it ever compiled.
	// Every real binary we inspected (retail helm_010.mdl, vdr_magearmor2.mdl)
	// has 3 here for every node that actually carries geometry. Without a
	// valid primitive type the engine has no way to know how to interpret
	// the index buffer for drawing at all, even though the vertex/face data
	// itself, the mesh header, and every other field are otherwise correct
	// — which is exactly why every mesh we compiled rendered invisibly
	// regardless of alpha, animation, classification, or bounds (issue #12).
	c.core.u32le(3)
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
	// int32 m_hHandednessToken (4) — NWN:EE. One float per vertex (±1), not a
	// bitangent vector; see the handedness write below.
	mdxHandednessPtrPos := -1
	if len(tangentsOut) > 0 {
		mdxHandednessPtrPos = c.core.len()
		c.core.i32le(0) // placeholder, patched after the handedness write
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

	// GPU index buffer. Written here, after the vertex attribute streams and
	// crucially BEFORE the tangent streams: real binaries lay the volatile
	// block out as vertices → UVs → normals → index buffer → tangents →
	// handedness (verified on retail tdc01_g02_01, e.g. normals 78660 →
	// index 78828 → tangent 78864 → handedness 79032). Only the volatile
	// data goes here; the two core-side list elements that point at it are
	// appended by writeMeshFaceData, since core must stay contiguous through
	// the remaining fixed-size headers.
	expanded.indexBufOff = -1
	if expanded.indexCountListPtrPos >= 0 {
		expanded.indexBufOff = int32(c.vol.len())
		for _, fv := range expanded.faceVerts {
			c.vol.u16le(fv[0])
			c.vol.u16le(fv[1])
			c.vol.u16le(fv[2])
		}
	}

	writeVec3Stream(tangentsOut, mdxTangentPtrPos)

	// Handedness — ONE float per vertex, not a bitangent vector. The field at
	// +496 is m_hHandednessToken: real binaries store exactly ±1.0 per vertex
	// there (verified across the game-compiled tangent corpus, e.g. 40 floats
	// all +1.0 on TIN01_D01_09's Plane334, a ±1.0 mix on its Object1376).
	// We used to write the bitangent vectors themselves — three floats per
	// vertex of unit-vector components — which is both 3x too much data and
	// the wrong values. The bitangent is reconstructible at runtime as
	// cross(normal, tangent) * w, so only the sign needs storing.
	writeFloatStream := func(data []float32, ptrPos int) {
		if len(data) == 0 || ptrPos < 0 {
			return
		}
		start := int32(c.vol.len())
		for _, v := range data {
			c.vol.f32le(v)
		}
		c.core.patchU32(ptrPos, uint32(start))
	}
	writeFloatStream(handednessOf(expanded.normals, tangentsOut, bitangentsOut), mdxHandednessPtrPos)

	return
}

// handednessOf reduces a tangent/bitangent basis to the per-vertex sign the
// binary format stores: +1 when the bitangent matches cross(normal, tangent),
// -1 when it is mirrored (a flipped UV chart). Returns nil when there is no
// basis to describe, so the caller leaves the token unset.
func handednessOf(normals, tangents, bitangents []Vec3) []float32 {
	if len(tangents) == 0 {
		return nil
	}
	out := make([]float32, len(tangents))
	for i := range tangents {
		w := float32(1)
		if i < len(normals) && i < len(bitangents) {
			if vecDot(vecCross(normals[i], tangents[i]), bitangents[i]) < 0 {
				w = -1
			}
		}
		out[i] = w
	}
	return out
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

	// Point vertexindicescount/vertextokenindices at the GPU index buffer.
	// The buffer itself already went into the volatile block during
	// writeMeshHeaderInner (it has to precede the tangent streams); all that
	// is left is to append each list's single uint32 element to core, which
	// can only happen now that the fixed-size headers are behind us.
	// Without these lists the mesh has a zero-length index buffer and draws
	// nothing — issue #12's actual root cause.
	if exp.indexCountListPtrPos >= 0 && exp.indexBufOff >= 0 {
		countElemOff := c.core.len()
		c.core.u32le(uint32(len(mesh.Faces) * 3))
		c.core.patchU32(exp.indexCountListPtrPos, uint32(countElemOff))

		offsetElemOff := c.core.len()
		c.core.u32le(uint32(exp.indexBufOff))
		c.core.patchU32(exp.indexOffsetListPtrPos, uint32(offsetElemOff))
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

	// indexCountListPtrPos/indexOffsetListPtrPos are core buffer positions
	// of the "offset" field in the vertexindicescount / vertextokenindices
	// ProxyLists (each a list of exactly one uint32 element) — patched once
	// writeMeshFaceData has appended those elements to core. -1 if the mesh
	// has no faces (both lists stay empty in that case).
	indexCountListPtrPos  int
	indexOffsetListPtrPos int

	// indexBufOff is the volatile-block offset of the GPU index buffer,
	// written during writeMeshHeaderInner so it lands before the tangent
	// streams (the order real binaries use). The core-side list elements
	// that point at it are appended later, in writeMeshFaceData.
	indexBufOff int32
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
				mat: face.Material,
				sg:  face.SmoothGroup,
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
