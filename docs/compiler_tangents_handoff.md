# Handoff: `compiler_tangents.go` — Mikktspace Tangent Generation

## Goal

Implement tangent (and bitangent) generation at compile time so that compiled binary MDL files are "complete" — the NWN:EE engine won't need to generate tangents on the fly at load time. This is the counterpart to the existing `compiler_normals.go` which generates normals from face geometry and smoothing groups.

## Why This Matters

NWN:EE models using `RenderHint NormalAndSpecMapped` require tangent data for correct normal-map lighting. Currently the compiler writes `-1` for both `p_mdx_tangent` and `p_mdx_bitangent`, meaning "not present." The engine will generate them at load time, but this adds startup latency and means compiled models aren't truly pre-baked.

---

## Architecture Overview

### Compiler Pipeline (how compilation works today)

1. `Compile()` in `compiler.go` creates a `compiler` struct with two buffers:
  - `core` — model metadata, node headers, face arrays, controllers
  - `vol` (MDX) — per-GPU-vertex data (positions, UVs, normals, colors)
2. Node tree is written via `writeNode()` in `compiler_node.go`
3. For mesh nodes, `writeMeshHeaderFull()` → `writeMeshHeaderInner()` in `compiler_mesh.go`:
  - Calls `generateNormals(mesh)` from `compiler_normals.go` (if normals are missing)
  - Calls `buildExpandedMesh(mesh)` to create deduplicated GPU vertex arrays
  - Writes the 512-byte mesh header to `core`
  - Writes vertex data (positions, UVs, normals, colors) to `vol` (MDX)

### Where Tangents Fit

In `compiler_mesh.go`, lines 189-194, the header currently writes:

```go
// int32 p_mdx_tex_anim0,1,2 (12 bytes, deprecated, write -1)
c.core.i32le(-1)
c.core.i32le(-1)
c.core.i32le(-1)
// int32 p_mdx_tangent / tex_anim3 (4) — EE tangents
c.core.i32le(-1)    // ← NEEDS TO BE A REAL MDX OFFSET
// int32 p_mdx_tex_anim4 (4, deprecated)
c.core.i32le(-1)
// int32 p_mdx_bitangent / tex_anim5 (4) — EE bitangent
c.core.i32le(-1)    // ← NEEDS TO BE A REAL MDX OFFSET
```

The tangent and bitangent data goes into the MDX (`vol`) buffer, just like positions and normals. The header fields become MDX offsets pointing to where the data was written.

---

## Required Inputs for Tangent Generation

Per GPU vertex (after `buildExpandedMesh`):

- **Position** (`expanded.positions[i]`) — `Vec3`
- **Normal** (`expanded.normals[i]`) — `Vec3`
- **UV0** (`expanded.uvs[i]`) — `Vec3` (only X,Y used)
- **Face topology** (`expanded.faceVerts`) — which GPU vertices form each triangle

You MUST have normals and UVs to compute tangents. If either is missing, skip tangent generation for that mesh (leave the pointers at `-1`).

---

## Algorithm: Mikktspace Tangent Computation

The standard algorithm (used by virtually all game engines):

### Per-triangle accumulation

For each triangle with GPU vertices `v0, v1, v2`:

```
edge1 = pos[v1] - pos[v0]
edge2 = pos[v2] - pos[v0]
deltaUV1 = uv[v1] - uv[v0]
deltaUV2 = uv[v2] - uv[v0]

r = 1.0 / (deltaUV1.x * deltaUV2.y - deltaUV2.x * deltaUV1.y)

tangent   = (edge1 * deltaUV2.y - edge2 * deltaUV1.y) * r
bitangent = (edge2 * deltaUV1.x - edge1 * deltaUV2.x) * r
```

Accumulate tangent and bitangent into per-vertex sums for `v0`, `v1`, `v2`.

Handle degenerate UVs: if `r` would be infinity/NaN (denominator ≈ 0), skip the triangle's contribution.

### Per-vertex orthonormalization (Gram-Schmidt)

For each GPU vertex:

```
t = tangent_sum[i]
n = normal[i]

// Gram-Schmidt orthogonalize
t = normalize(t - n * dot(n, t))

// Handedness: determine W sign from cross product
w = sign(dot(cross(n, t), bitangent_sum[i]))
// w is +1.0 or -1.0
```

The result is a `Vec4{t.X, t.Y, t.Z, w}` per GPU vertex (matching `MeshData.Tangents`).

The bitangent is reconstructed at runtime as `cross(normal, tangent.xyz) * tangent.w`.

### What to store in MDX

- **Tangents**: `N_gpu_verts × Vec3` (just XYZ, 12 bytes each) at the MDX offset pointed to by `p_mdx_tangent`
- **Bitangents**: `N_gpu_verts × Vec3` (12 bytes each) at the MDX offset pointed to by `p_mdx_bitangent`
- The W (handedness) sign is implicit — the decompiler reconstructs it from `cross(normal, tangent) · bitangent` (see `binary.go` `readMDXTangents`)

So you need to write BOTH tangent XYZ and bitangent XYZ to MDX. The decompiler reads them back and encodes handedness into `Vec4.W`.

---

## Implementation Plan

### 1. Create `pkg/mdl/compiler_tangents.go`

```go
// compiler_tangents.go — tangent generation for the binary MDL compiler.
//
// Computes per-GPU-vertex tangent and bitangent vectors from positions,
// normals, and UV0 coordinates using the Mikktspace algorithm.
// Called after buildExpandedMesh produces the deduplicated GPU vertex arrays.
package mdl

// generateTangents computes tangent + bitangent arrays for the expanded mesh.
// Returns (tangents []Vec3, bitangents []Vec3).
// Returns nil, nil if normals or UVs are missing.
func generateTangents(exp *expandedMesh, faces []Face) ([]Vec3, []Vec3) {
    // ...
}
```

**Key decisions:**

- Operates on `expandedMesh` (GPU vertices), not the original indexed mesh
- Uses `exp.faceVerts` for triangle topology (not `mesh.Faces`)
- Returns separate tangent and bitangent `Vec3` slices (NOT `Vec4` — the W is only for ASCII representation)

### 2. Modify `compiler_mesh.go` — `writeMeshHeaderInner`

After writing normals and colors to MDX, add:

```go
// Write tangents and bitangents to MDX
tangents, bitangents := generateTangents(&expanded, mesh.Faces)
if tangents != nil {
    tangentStart := int32(c.vol.len())
    for _, t := range tangents {
        c.vol.vec3(t)
    }
    c.core.patchU32(mdxTangentPtrPos, uint32(tangentStart))

    bitangentStart := int32(c.vol.len())
    for _, bt := range bitangents {
        c.vol.vec3(bt)
    }
    c.core.patchU32(mdxBitangentPtrPos, uint32(bitangentStart))
}
```

You'll also need to change lines 189-194 from hardcoded `-1` to placeholder/patchable positions (same pattern as normals and colors above).

### 3. Respect existing tangent data

If `mesh.Tangents` already has data (parsed from ASCII or decompiled from binary), use it directly instead of generating. This avoids overwriting hand-authored tangent data:

```go
if len(mesh.Tangents) >= len(expanded.positions) {
    // Use existing tangents — expand to GPU vertices and split into tangent/bitangent
} else if len(expanded.normals) > 0 && len(expanded.uvs) > 0 {
    // Generate tangents from geometry
    tangents, bitangents = generateTangents(...)
}
```

When using existing `mesh.Tangents` (which are `Vec4`), you need to reconstruct the bitangent:

```go
bitangent = cross(normal, tangent.xyz) * tangent.W
```

### 4. Add tests in `compiler_tangents_test.go`

Test cases:

- **Simple quad**: 4 verts, 2 tris, known UVs → verify tangent direction matches UV U-axis
- **Roundtrip preservation**: ASCII with tangents → compile → decompile → tangents match
- **Auto-generation**: ASCII without tangents but with normals+UVs → compile → decompile → tangents present
- **No UVs**: mesh without UVs → no tangents generated (pointers remain -1)
- **Degenerate UV**: triangle with zero-area UV → doesn't NaN

---

## Key Files Reference


| File                               | Role                                                                                |
| ---------------------------------- | ----------------------------------------------------------------------------------- |
| `pkg/mdl/compiler_tangents.go`     | **NEW** — tangent generation algorithm                                              |
| `pkg/mdl/compiler_mesh.go`         | Mesh header + MDX writing — needs tangent/bitangent MDX writes                      |
| `pkg/mdl/compiler_normals.go`      | **Reference** — similar pattern for normal generation                               |
| `pkg/mdl/compiler.go`              | Top-level compiler, `patchBuf`, `compiler` struct                                   |
| `pkg/mdl/compiler_node.go`         | Node writing — calls `writeMeshHeaderFull`                                          |
| `pkg/mdl/binary.go:833-845`        | Decompiler reads `p_mdx_tangent` / `p_mdx_bitangent` from header                    |
| `pkg/mdl/binary.go:1206-1244`      | `readMDXTangents` — reads Vec3 tangents + Vec3 bitangents, computes W handedness    |
| `pkg/mdl/types.go:81`              | `Tangents []Vec4` in `MeshData` — the in-memory representation                      |
| `pkg/mdl/parser.go:591`            | ASCII parser reads `tangents` keyword as `[]Vec4`                                   |
| `pkg/mdl/writer.go:278-284`        | ASCII writer outputs `tangents` as Vec4 (XYZW)                                      |
| `pkg/mdl/vec.go`                   | `vecCross`, `vecDot`, `vecNormalize`, `vecLen` helpers                              |
| `pkg/checks/parameters.go:113-139` | `tangent_validation` check — detects missing tangents on NormalAndSpecMapped meshes |


---

## Binary Format Details

### Mesh header layout (512 bytes in core buffer)

The tangent/bitangent pointers are at byte offsets ~424-448 within the header (after colors pointer):

```
...
int32 p_mdx_vertex_colors        (4)
int32 p_mdx_tex_anim0            (4)  — deprecated, write -1
int32 p_mdx_tex_anim1            (4)  — deprecated, write -1
int32 p_mdx_tex_anim2            (4)  — deprecated, write -1
int32 p_mdx_tangent (tex_anim3)  (4)  — MDX offset to tangent Vec3 array
int32 p_mdx_tex_anim4            (4)  — deprecated, write -1
int32 p_mdx_bitangent (tex_anim5)(4)  — MDX offset to bitangent Vec3 array
byte  light_mapped               (1)
byte  rotate_texture             (1)
uint16 padding                   (2)
...
```

### MDX (vol buffer) data layout per mesh

Data is appended in this order:

1. Positions: `N × Vec3` (12 bytes each)
2. UV0-3: `N × 2 floats` (8 bytes each) per texture stage
3. Normals: `N × Vec3` (12 bytes each)
4. Colors: `N × 4 bytes` (RGBA)
5. **Tangents: `N × Vec3` (12 bytes each)** ← NEW
6. **Bitangents: `N × Vec3` (12 bytes each)** ← NEW
7. Skin weights/bone-refs (if skin node)

### Decompiler read-back (for verification)

`readMDXTangents` in `binary.go` (lines 1206-1244):

- Reads `N × Vec3` tangents from MDX at `pMdxTangent`
- Reads `N × Vec3` bitangents from MDX at `pMdxBitangent`
- Reconstructs handedness W: `w = sign(dot(cross(normal, tangent), bitangent))`
- Stores as `Tangents []Vec4{X, Y, Z, W}` in `MeshData`

---

## Edge Cases to Handle

1. **Degenerate UV triangles**: `deltaUV1.x * deltaUV2.y - deltaUV2.x * deltaUV1.y ≈ 0` → skip triangle contribution
2. **Zero-length tangent sum**: after accumulation, if a vertex's tangent sum is near-zero, use a fallback (e.g., derive from face normal)
3. **Meshes without UVs**: `len(expanded.uvs) == 0` → skip entirely, leave pointers at -1
4. **Meshes without normals**: shouldn't happen after `generateNormals()`, but guard anyway
5. **Existing tangent data**: if `mesh.Tangents` is already populated, prefer it over generation
6. **meshHeaderSize constant**: verify it doesn't change (should still be 512) — tangent pointers are already allocated in the header, just currently set to -1

---

## Testing Strategy

### Unit test: known geometry

A flat quad on the XY plane with UVs mapping U→X, V→Y should produce tangents pointing along +X and bitangents along +Y:

```go
func TestGenerateTangentsSimpleQuad(t *testing.T) {
    mesh := &MeshData{
        Verts: []Vec3{{0,0,0}, {1,0,0}, {1,1,0}, {0,1,0}},
        TVerts: []Vec3{{0,0,0}, {1,0,0}, {1,1,0}, {0,1,0}},
        Normals: []Vec3{{0,0,1}, {0,0,1}, {0,0,1}, {0,0,1}},
        Faces: []Face{
            {Verts: [3]int32{0,1,2}, UVs: [3]int32{0,1,2}},
            {Verts: [3]int32{0,2,3}, UVs: [3]int32{0,2,3}},
        },
    }
    // Build expanded mesh, generate tangents, verify direction
}
```

### Roundtrip test: compile → decompile preserves tangents

Use the existing roundtrip test pattern (see `compiler_roundtrip_test.go`):

```go
func TestRoundtripTangents(t *testing.T) {
    // ASCII model with NormalAndSpecMapped + UVs + normals (no explicit tangents)
    // Compile → decompile → verify Tangents[] is populated and reasonable
}
```

### Oracle test integration

The existing `oracle_test.go` compares against known-good fixtures. After implementing tangent generation, models with `NormalAndSpecMapped` will gain tangent data that wasn't there before. Update oracle expectations or add a separate tangent-specific oracle fixture.

---

## Summary of Changes


| File                                | Change                                                                                                              |
| ----------------------------------- | ------------------------------------------------------------------------------------------------------------------- |
| `pkg/mdl/compiler_tangents.go`      | **NEW** — `generateTangents()` function                                                                             |
| `pkg/mdl/compiler_tangents_test.go` | **NEW** — unit + roundtrip tests                                                                                    |
| `pkg/mdl/compiler_mesh.go`          | Change tangent/bitangent pointer writes from `-1` to real MDX offsets; call `generateTangents` after normals/colors |
| `pkg/checks/parameters.go`          | (optional) Update `tangent_validation` description to note auto-generation is available                             |


No changes to types, parser, writer, or decompiler needed — those already handle tangents correctly.