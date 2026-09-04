# Compile pipeline

`cleanmodels compile` turns an ASCII `.mdl` file into the binary, MDX-paired
form NWN:EE loads at runtime. The intent is to produce output that the engine
treats as interchangeable with what its own internal compiler would emit from
the same source — so a hak built from `cleanmodels`-compiled binaries doesn't
need the engine to recompile anything at load time.

We don't claim byte-for-byte equality with the game compiler. We do try to
match it semantically, and we have a test harness that diffs against
game-compiled binaries to catch drift.

## CLI surface

```
cleanmodels compile [flags] <input> [output]
```

Compile is intentionally narrow: it parses ASCII, runs the binary writer, and
writes the result. It doesn't repair or validate — for that, run
`cleanmodels check` or `cleanmodels repair` first (or chain them with `--fix`
on `check` / `--all` on `repair`). Pointing it at a directory batch-compiles
every `.mdl` inside.

The compile path is also reachable as part of an end-to-end ASCII → repair →
binary flow when `compile=true` is set after a `repair` invocation; see
`[cmd/cleanmodels/process.go](cmd/cleanmodels/process.go)`.

## File layout we produce

```
[ 0 .. 11 ]                 header_file: {0, core_len, mdx_len}
[12 .. 12+core_len-1]       core block:  model header + node tree + animations
[12+core_len .. EOF]        MDX block:   per-GPU-vertex streams (positions,
                                          UVs, normals, colors, tangents,
                                          bitangents, skin weights)
```

Pointers in the file come in two flavours:

- **Core-relative** `p` → file offset `12 + p`
- **MDX-relative** `p` → file offset `12 + core_len + p`

These conventions are inherited from the format itself (xoreos `NWN1MDL.bt`
binary template) and are reflected one-to-one in the compiler:
`pkg/mdl/compiler.go` writes everything via a `patchBuf` so we can reserve a
4-byte placeholder, write the data it should point to, and patch the offset
back into the right slot. Every read in `binary.go` mirrors a write here.

## Compilation order

1. **Pre-pass** — `assignNodeIDs` walks the geometry tree iteratively (DFS,
  cycle-guarded by a visited set) and gives every node a sequential part
   number. Skin nodes need stable part numbers to encode bone references.
2. **Geometry header** — `header_geometry` (112 bytes) + `header_model` (120
  bytes) + animation pointer array. Animation list pointers are written as
   placeholders and patched once each animation's core offset is known.
3. **Geometry node tree** — `writeNode` walks DFS; each node writes its
  own 112-byte `header_node`, type-specific sub-headers (mesh, skin, dangly,
   AABB, light, emitter, reference), then deferred face/MDX data, then its
   children.
4. **Animation blocks** — `writeAnimation` per animation, each with its own
  `header_animation` and an animation-node tree. Animation nodes reuse
   geometry node IDs so bone references stay consistent.
5. **File header** — `{0, core_len, mdx_len}` is written last with the final
  buffer sizes, then `core` and `vol` are appended.

The trickiest part is ordering inside a mesh node. Type-specific sub-headers
(skin, dangly, AABB) must sit at fixed offsets the decompiler expects. To
avoid them being shifted by variable-length face data, the compiler defers
the face array write until after all sub-headers, and patches the faces
pointer back into the placeholder slot it left in the mesh header.

## Mesh compilation

`writeMeshHeaderInner` (`[compiler_mesh.go](pkg/mdl/compiler_mesh.go)`) does
the heavy lifting:

1. **Generate normals if missing** — `generateNormals` in
  `[compiler_normals.go](pkg/mdl/compiler_normals.go)` follows
   nwnmdlcomp's algorithm: sum unnormalised face normals across faces that
   share a vertex AND have overlapping smoothing-group bits, then normalise.
   We skip generation when the mesh already carries per-vertex normals.
2. **Build expanded mesh** — `buildExpandedMesh` deduplicates vertices into
  GPU vertices (one per unique attribute combination). The dedup key
   includes:
  - Position bits, UV0/UV1/UV2/UV3 bits, normal bits, color bits
  - **Material index** of the originating face
  - **Smoothing group** of the originating face
   The material/SG fields aren't obvious — they're there because the game
   compiler splits at material and smoothing-group boundaries even when
   the rest of the attributes match. Without them, multi-material body parts
   over-merge by 30–50%, which then perturbs Mikktspace tangents at the
   boundaries (see "Known divergences" below).
3. **Resolve tangents** — `resolveTangents` in
  `[compiler_tangents.go](pkg/mdl/compiler_tangents.go)` decides what to
   write into the tangent / bitangent MDX streams. Three cases:
  - `mesh.Tangents` already populated (parsed from ASCII or recovered by
  the decompiler) and covers every GPU vertex → expand and split into
  tangent/bitangent. Authored handedness is preserved.
  - Mesh declares `RenderHint NormalAndSpecMapped` and has UVs +
  normals → generate via `generateTangents` (Mikktspace, accumulated in
  float64 for stability, Gram–Schmidt orthogonalised against the vertex
  normal, with a fallback orthogonal axis when the accumulated tangent
  collapses to zero).
  - Otherwise → leave both MDX pointers at `-1` ("not present"). The
  engine still works; it just regenerates them at load.
4. **Write the 512-byte mesh header** — fixed offsets, every field mirrored
  from the read in `binary.go readMeshHeader`. Pointers to
   positions / UVs / normals / colors / tangents / bitangents are
   placeholders, patched after each block is written into MDX.
5. **Append vertex streams to MDX** — positions, then UVs (per stage),
  normals, RGBA colors (4 bytes each), tangents (Vec3), bitangents (Vec3).
   The tangent/bitangent split mirrors what the decompiler reconstructs into
   `Vec4{tangent.xyz, w=sign(dot(cross(n, t), bitangent))}`.

## Faces, AABB, skin, danglymesh

- **Faces** are written by `writeMeshFaceData` after all sub-headers are
emitted. Each face record carries the recomputed face normal and plane
distance, the material index, three legacy `0xFFFF` slots, and the three
GPU vertex indices.
- **AABB walkmesh** trees come from `RebuildAABB` (in `pkg/mdl/repair_walkmesh.go`)
if the user asked for it, or are read from ASCII as-is. Tree topology
(split order) may differ from the game's median-split, but pathing and
collision are unaffected because the same faces end up in the same leaves.
- **Skin** writes per-bone weights into MDX after positions/UVs/normals;
bone references are resolved through the part-number map populated in
the pre-pass.
- **Danglymesh** constraints are written after the mesh header so they sit at
the offset the decompiler expects.

## Tangent generation, in detail

The Mikktspace-style accumulation is straightforward:

```
For each triangle:
  e1 = p1 - p0;  e2 = p2 - p0
  du1 = uv1 - uv0;  du2 = uv2 - uv0
  r = 1 / (du1.x * du2.y - du2.x * du1.y)
  t = (e1 * du2.y - e2 * du1.y) * r
  b = (e2 * du1.x - e1 * du2.x) * r
  Accumulate t, b into each of the triangle's 3 vertices.

For each vertex:
  t = normalize(t - n * dot(n, t))                 // Gram-Schmidt
  cross_nt = cross(n, t)
  w = sign(dot(cross_nt, accumulated_b))
  bitangent = cross_nt * w
```

Edge cases handled in code: degenerate UV triangles (skipped), zero-length
accumulated tangent (fall back to an arbitrary axis perpendicular to the
normal), degenerate normal (fall back to `+X`).

Why we write the bitangent explicitly even though it's reconstructible:
because the decompiler needs it to recover `W` handedness without ambiguity.
`binary.go readMDXTangents` reads both Vec3 streams and computes
`w = sign(dot(cross(n, t), b))` to populate `Vec4.W`. Without the bitangent
stream we'd have to pick a fixed handedness convention, and any model with
mirrored UVs would render incorrectly.

## How we test it


| Layer                                              | Tests                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    | What they catch                                                            |
| -------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | -------------------------------------------------------------------------- |
| **Unit** — algorithms                              | `TestGenerateTangentsSimpleQuad`, `TestGenerateTangentsNoUVs`, `TestGenerateTangentsDegenerateUVs`, `TestCompilerHeaderSizes`, `TestMeshHeaderSize`                                                                                                                                                                                                                                                                                                                                                                      | Header byte layouts, tangent direction on known geometry, NaN/inf handling |
| **Roundtrip** — compile → decompile preserves data | `TestRoundtripDummy`, `TestRoundtripTrimesh`, `TestRoundtripAnimation`, `TestRoundtripLight`, `TestRoundtripEmitter`, `TestRoundtripDanglymesh`, `TestRoundtripSkin`, `TestRoundtripReference`, `TestRoundtripAlphaZero`, `TestRoundtripAlphaDefault`, `TestRoundtripAnimationControllers`, `TestRoundtripEmitterControllerOrder`, `TestRoundtripNodeNameCase`, `TestRoundtripTangentsNormalSpecMapped`, `TestRoundtripNoTangentsWithoutUVs`, `TestRoundtripPreservesAuthoredTangents`, `TestRoundtripGeometryPreserved` | We can read back what we wrote without losing fields                       |
| **Batch** — compile a folder                       | `TestBatchCompilePlaceables`, `TestBatchCompileDecompiled`, `TestOracleCompileAll`                                                                                                                                                                                                                                                                                                                                                                                                                                       | Real-world ASCII files don't crash the compiler; output is decodable       |
| **Oracle** — diff against game-compiled binaries   | `TestOracleCompare`, `TestOracleVertexParity`, `TestOracleTangents`                                                                                                                                                                                                                                                                                                                                                                                                                                                      | Our output matches what NWN:EE itself produces                             |


The oracle suite is the strongest signal we have. It compares our binary
output against binaries the game compiler produced from the same ASCII
sources. The fixtures and methodology live in
`[tests/fixtures/oracle/README.md](tests/fixtures/oracle/README.md)`.

### Oracle coverage today

26 fixture pairs in `ascii/` ↔ `game_binary/`, plus 18 tangent pairs.
Coverage spans:

- Static placeables and UI assets (perfect-parity baselines)
- Tileset tiles with AABB walkmeshes
- Character body parts (head, neck, torso) — exercises skin
- Dangly meshes (`squid`, `c_marilithe`, `plc_guillo2`, `c_marilith2`)
- Emitters (`zlc_o23`, `c_elemashm`, `PLC_D01`, `c_marilith2`)
- Lights (`plc_dd27`, `plc_nc03`)
- Animations (1 anim → 67 anims across the suite)
- Hak-sourced ASCII overrides (Layonara `wsf10` tiles, `tin01` stair tiles)

What `TestOracleVertexParity` enforces: per-mesh-node GPU vertex count
matches the game **exactly** unless the divergence is recorded in
`vertParityBaseline`. New fixtures default to strict equality, so a
regression in dedup behaviour fails loudly. Stale baseline entries also fail
the test, which keeps the baseline honest.

What `TestOracleTangents` enforces: mean `|dot(ourTangent, gameTangent)|`
across all matched face corners must clear **0.96**, and the bad-corner
ratio (`|dot| < 0.5`) must stay below **5%**. The empirical floor across
the current fixture set is 0.969 mean alignment.

## Known divergences (intentional, not bugs)

These show up as documented baseline entries in `vertParityBaseline` or as
notes in the fixture README:

- **Vertex deduplication philosophy on body parts.** The game compiler
doesn't bit-deduplicate identical face-corner GPU vertices the way we do.
On character body parts (`Red_M_Torso.mdl`, `pmh0_head001.mdl`) the game
binary ends up with more unique GPU verts per node than ours, even though
every face's per-corner attributes are bit-identical. Both produce
identical render output — this is purely a count difference in the GPU
vertex array. We could mirror the game's behaviour to close the gap, but
the cost is wasted MDX bytes for no visual benefit.
- **AABB tree split order.** Median-split may pick a different splitting
axis than the game; pathing and collision are driven by face-to-leaf
assignments, which match.
- **Tangent / bitangent values.** Both compilers run independent
tangent-generation passes; we use Mikktspace, the game uses an
unspecified algorithm. We validate alignment direction (mean ≥0.96, bad
corners ≤5%), not bit equality.
- **Degenerate face elimination.** The game compiler silently strips faces
with duplicate vertex indices (e.g. `18 0 9` where two refs collapse).
We keep them. `tdc01_g02_03.mdl` exercises this; the ASCII has 19 faces
on `object01`, the game binary has 16.

## Known gaps

- **MDL classification drift.** A few sources we received had ASCII
classifications that don't match the binary the game shipped (e.g.
`c_nightmare.mdl` ASCII `Tile`, binary `Character`). These are excluded
from the structural diff because no compiler choice can make them match.
- **Hak-only binaries with no surviving ASCII.** Some shipped haks include
pre-compiled binaries whose ASCII source was never re-emitted. We skip
these in the oracle suite (`hakBinaryOnlySkipList` in `oracle_test.go`)
because there's no apples-to-apples comparison to make.
- **Material/SG dedup matching is empirical.** We worked back to the
current `vertKey` shape from observed behaviour against the game's
output. We don't have access to the game's compiler source; if there's
another field the game's dedup considers (e.g. a per-corner attribute
we're not aware of), we'd see it as a parity delta on a fixture we don't
yet have.
- **No fuzz harness.** Parsing untrusted ASCII is bounded by alloc tracking
and a max-depth guard, but we haven't exercised the parser/compiler with
random or adversarial inputs.
- **Bone limit and triangle limit are check-only.** The compiler will write
binaries that exceed engine limits if asked; the `bone_limit` and
`triangle_limit` checks (`pkg/checks/geometry.go`) flag them but don't
refuse compilation.

## Useful entry points


| Concern                      | File / function                                                          |
| ---------------------------- | ------------------------------------------------------------------------ |
| Top-level orchestration      | `pkg/mdl/compiler.go` — `Compile`, `writeModel`                          |
| Mesh header + MDX writes     | `pkg/mdl/compiler_mesh.go` — `writeMeshHeaderInner`, `buildExpandedMesh` |
| Tangent generation           | `pkg/mdl/compiler_tangents.go` — `resolveTangents`, `generateTangents`   |
| Normal generation            | `pkg/mdl/compiler_normals.go` — `generateNormals`                        |
| Node tree + sub-headers      | `pkg/mdl/compiler_node.go`, `compiler_controllers.go`                    |
| Format constants and offsets | `pkg/mdl/binary.go` (read side defines the layout we mirror)             |
| Oracle fixtures and workflow | `tests/fixtures/oracle/README.md`                                        |


