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
                                          UVs, normals, colors, index buffer,
                                          tangents, handedness, skin weights)
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
  `[compiler_normals.go](pkg/mdl/compiler_normals.go)` is a port of the
   game's `MdlNodeTriMesh::ProcessVerts`. For each face corner: a
   smoothing-group neighbour with a lower face index donates the normal it
   already received, unless it is nearly opposite (dot < -0.99); otherwise
   the corner starts from its own area-weighted face normal, adds the
   neighbours with a higher face index, and normalises. A zero sum falls
   back to the face's own unit normal. Smoothing group 0 never joins another
   face. The result depends on face order, as it does in the game.
   Normalisation is `gameNormalize`, a bit-exact float32 port of the game's
   `Vector::Normalize` (near-unit vectors are left alone, zero becomes
   `(1,0,0)`), which matters because a one-ulp difference splits vertices.
   We skip generation when the mesh already carries per-vertex normals.
2. **Build expanded mesh** — `buildExpandedMeshOpts` merges face corners
   into GPU vertices, like the game's `InternalGenVertices`. Two corners share
   a vertex when these match (`vertKey`, mirroring `NodeVertex::operator==`):
  - Position, UV0/UV1/UV2/UV3, normal and color, compared **by value**
   (`-0` equals `+0`, as with the game's float `==`).
  - The face's `mirrorlist` value (low byte), when the ASCII has one.
  - Skin meshes: the bone names and weights of the original vertex.
  - Danglymesh: the vertex constraint.
   Material index and smoothing group are **not** in the key. The game merges
   across both, and the EE-compiled oracle fixtures only match when we do
   too.
   Every mesh type merges against the whole vertex list (first equal vertex
   wins) except **AABB nodes**, where the game uses a different scheme and we
   follow it: a corner joins an existing vertex only through an earlier face
   that has at least two equal corners, the first such face wins, and a face
   with no such neighbour gets three new vertices.
   After merging, faces whose corners resolve to fewer than three distinct
   GPU vertices are dropped (see "Faces, AABB, skin, danglymesh"). Vertices
   those faces introduced stay in the arrays.
3. **Resolve tangents** — `resolveTangents` in
  `[compiler_tangents.go](pkg/mdl/compiler_tangents.go)` decides what to
   write into the tangent and handedness MDX streams. Three cases:
  - `mesh.Tangents` already populated (parsed from ASCII or recovered by
  the decompiler) and covers every GPU vertex → used as authored, with the
  sign of `W` as handedness.
  - The mesh is normal-mapped (`isNormalMapped`): a `NormalAndSpecMapped` or
  `NormalTangents` render hint, case-insensitive, on the mesh or in its
  material, and it has UVs + normals → generate via `generateTangents`, a
  port of the game's algorithm described below. This can **append cloned
  vertices**, so the vertex count is read after it runs.
  - Otherwise → leave both MDX pointers at `-1` ("not present"). The
  engine still works; it just regenerates them at load.
4. **Write the 512-byte mesh header** — fixed offsets, every field mirrored
  from the read in `binary.go readMeshHeader`. Pointers to
   positions / UVs / normals / colors / tangents / handedness are
   placeholders, patched after each block is written into MDX.
5. **Append vertex streams to MDX** — positions, then UVs (per stage),
  normals, RGBA colors (4 bytes each), the index buffer, tangents (Vec3),
  and handedness (one float per vertex). The decompiler reconstructs
   `Vec4{tangent.xyz, w}` from the tangent and handedness streams.

## Faces, AABB, skin, danglymesh

- **Faces** are written by `writeMeshFaceData` after all sub-headers are
emitted. Each face record carries the recomputed face normal and plane
distance, the material index, the three adjacency slots, and the three GPU
vertex indices. Face normals and plane distances match the game's
`BuildNeighborLists` on every fixture node we compare.
- **Face adjacency** (`buildFaceAdjacency`) is a port of
`MdlNodeTriMesh::BuildNeighborLists`. Slot `k` holds the lowest-numbered
other face that shares the edge from corner `k` to corner `k+1`, matched on
source `verts` indices (not coordinates, not GPU vertices) in either
direction, or `0xFFFF`. The game runs it after degenerate faces were removed
but still reads the *source* face list by position, so on a mesh that lost
faces the numbers refer to source numbering and only the first n source faces
are searched. We reproduce that, and `TestCompiledFaceAdjacencyMatchesGame`
checks the bytes we write against the game's on 60 nodes.
- **Degenerate faces** are removed for every mesh except AABB walkmeshes.
After the vertex merge, a face with two corners on the same GPU vertex is
dropped, exactly like the game (`tdc01_g02_03` `object01`: 19 ASCII faces,
16 in the game binary and in ours). Face counts, the index buffer and the
face records all use the surviving faces. AABB nodes are skipped because
their authored tree indexes faces by position; see "Known gaps".
- **AABB walkmesh** trees are authored data. The game parses the tree from
the ASCII as written (`Parse(char const*, AABB*&)`) and never builds one;
across all 20 AABB nodes in the oracle fixtures the game's tree equals the
authored tree entry for entry. We read it the same way. `RebuildAABB` (in
`pkg/mdl/repair_walkmesh.go`) is a repair the user asks for and has no
game equivalent.
- **Skin** writes per-bone weights into MDX after positions/UVs/normals;
bone references are resolved through the part-number map populated in
the pre-pass.
- **Danglymesh** constraints are written after the mesh header so they sit at
the offset the decompiler expects.

## Tangent generation, in detail

`generateTangents` ports the game's `CalculateTangentSpaceBasis`. It runs
after the vertex merge, and the game only runs it for normal-mapped materials
when the file carries no tangents.

```
For each triangle (p0,p1,p2 positions; (u,v) texture coordinates):
  dv1 = v1 - v0;  dv2 = v2 - v0
  det = (u1 - u0) * dv2 - (u2 - u0) * dv1
  w   = +1 if det >= 0 else -1
  if det != 0:
    t = (dv2 * (p1 - p0) - dv1 * (p2 - p0)) / det       // not normalised
  else:
    t = (n0 + n1 + n2) / 3 with components rotated (y, z, x), w = +1

For each triangle corner, at vertex v:
  first triangle to reach v:    tangent[v] = t, handed[v] = w
  later triangle, same w:       tangent[v] += t
  later triangle, other w:      clone v (once per source vertex and w),
                                clone.tangent += t, re-point the corner
  animmesh nodes:               every triangle overwrites

For each vertex (clones included):
  t = normalize(t - n * dot(n, t))
```

The unnormalised `t` means larger triangles weigh more. A zero-determinant
triangle is not skipped; it gets a tangent from its normals. A clone copies
the UVs, normals, color and original-vertex index of its source, so skin
weights and dangly constraints follow it. A vertex no triangle touches keeps
handedness 0, as in the game.

We write the tangent as a vec3 stream and the handedness as one float per
vertex (`m_hHandednessToken`), exactly what the game stores; there is no
bitangent stream. `binary.go readMDXTangents` combines them back into
`Vec4{tangent.xyz, w}`.

## How we test it


| Layer                                              | Tests                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    | What they catch                                                            |
| -------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | -------------------------------------------------------------------------- |
| **Unit** — algorithms                              | `TestGenerateTangentsSimpleQuad`, `TestGenerateTangentsNoUVs`, `TestGenerateTangentsDegenerateUVs`, `TestGenerateTangentsZeroDeterminantUsesNormals`, `TestGenerateTangentsClonesVertexAtMirroredUVs`, `TestGenerateNormals*`, `TestBuildExpandedMesh*`, `TestBuildFaceAdjacency*`, `TestCompilerHeaderSizes`, `TestMeshHeaderSize`                                                                                                                                                                                                                                                                                                                                                                      | Header byte layouts, tangent direction on known geometry, NaN/inf handling |
| **Roundtrip** — compile → decompile preserves data | `TestRoundtripDummy`, `TestRoundtripTrimesh`, `TestRoundtripAnimation`, `TestRoundtripLight`, `TestRoundtripEmitter`, `TestRoundtripDanglymesh`, `TestRoundtripSkin`, `TestRoundtripReference`, `TestRoundtripAlphaZero`, `TestRoundtripAlphaDefault`, `TestRoundtripAnimationControllers`, `TestRoundtripEmitterControllerOrder`, `TestRoundtripNodeNameCase`, `TestRoundtripTangentsNormalSpecMapped`, `TestRoundtripNoTangentsWithoutUVs`, `TestRoundtripPreservesAuthoredTangents`, `TestRoundtripGeometryPreserved` | We can read back what we wrote without losing fields                       |
| **Batch** — compile a folder                       | `TestBatchCompilePlaceables`, `TestBatchCompileDecompiled`, `TestOracleCompileAll`                                                                                                                                                                                                                                                                                                                                                                                                                                       | Real-world ASCII files don't crash the compiler; output is decodable       |
| **Oracle** — diff against game-compiled binaries   | `TestOracleCompare`, `TestOracleVertexParity`, `TestOracleTangents`, `TestOracleFaceAdjacency`, `TestCompiledFaceAdjacencyMatchesGame`                                                                                                                                                                                                                                                                                                                                                                                                                                                      | Our output matches what NWN:EE itself produces                             |


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
across all matched face corners must clear **0.99**, and the bad-corner
ratio (`|dot| < 0.5`) must stay below **1%**. Because we port the game's
algorithm, the core-game fixtures match to 1.0000 and the worst fixture (a
hak tile whose ASCII is a different revision of the binary) is 0.9974.

What the adjacency tests enforce: for the fixtures whose binary was compiled
from exactly this ASCII by the EE compiler, the adjacency bytes we write equal
the game's on every node (60 nodes). Other fixtures are base-game assets built
by the legacy tool, whose adjacency was computed before faces were removed, or
ASCII/binary pairs from different revisions, so they cannot be compared.

## Known divergences (intentional, not bugs)

These show up as documented baseline entries in `vertParityBaseline` or as
notes in the fixture README:

- **Legacy-tool binaries.** `Red_M_Torso.mdl`, `pmh0_head001.mdl` and
`ctl_compass.mdl` are shipped base-game assets with more GPU vertices than we
emit. In every case the difference equals the number of exact-duplicate
vertices (identical position, UV, normal and color) in the game binary:
`torso_g` 62, `pmh0_head001g` 119, `ctl_compass` `box01` 104 and
`cmp_pointer` 4. The game's own merge cannot leave those behind, so these
binaries were not produced by the EE compile path and nothing in the engine
can be mirrored to match them. Both versions render identically. They are
the only entries left in `vertParityBaseline`.
- **Legacy adjacency.** In the same kind of binary (`PLC_D01`,
`abp_weaprack_1`, `c_marilith2`, `plc_guillo2`, `plc_nc03`, `tdc01_a01_03`)
the stored adjacency can hold numbers at or above the face count, because
the legacy tool computed it before faces were removed. We write what the EE
compiler would write.
- **Drifted fixtures.** A few `tin01_*` hak tiles pair an ASCII with a
different revision of the binary, so tangents on some corners differ in sign
or handedness: 0.65% of corners across the 18 `tangent_pairs` models, and the
worst model (`tin01_a02_03`) is 97.3% exact. The core-game tangent fixtures
are exact.

## Known gaps

- **MDL classification drift.** A few sources we received had ASCII
classifications that don't match the binary the game shipped (e.g.
`c_nightmare.mdl` ASCII `Tile`, binary `Character`). These are excluded
from the structural diff because no compiler choice can make them match.
- **Hak-only binaries with no surviving ASCII.** Some shipped haks include
pre-compiled binaries whose ASCII source was never re-emitted. We skip
these in the oracle suite (`hakBinaryOnlySkipList` in `oracle_test.go`)
because there's no apples-to-apples comparison to make.
- **Degenerate faces on AABB nodes.** The game's delete loop runs for AABB
nodes too, but no fixture has an AABB node that lost faces, so we cannot see
how the game keeps the authored tree's face indices valid afterwards. We keep
every face on AABB nodes.
- **Render hint inference needs the materials.** With no `renderhint` on the
mesh, the game takes the hint from the mesh's `.mtr` material. We do the same
(`isNormalMapped`), but only when `--resource-dir` or `--game-dir` can reach
that material. Without it only an explicit `renderhint` counts, and the engine
regenerates tangents at load.
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


