# Decompile pipeline

`cleanmodels decompile` reads a binary `.mdl` (the format the engine loads)
and emits an ASCII `.mdl` you can edit, diff, or feed back through the
compiler. It's the inverse of `cleanmodels compile`, and the two share the
same in-memory `Model` type so a clean roundtrip means no data was lost in
either direction.

## CLI surface

```
cleanmodels decompile [flags] <input> [output]
```

Flags worth knowing:

- `--force` / `-f` — treat input as binary even if auto-detection fails
(we sniff for the all-zero file-header `{0, ...}` signature; some odd
inputs slip past).
- `-o` directory — write decompiled `.mdl` files alongside their inputs in
batch mode.

In batch mode (`<input>` is a directory) every `.mdl` inside is processed.
ASCII files are passed through; binary files are decompiled.

## What "decompile" means here

The output is intended to be:

1. **Human-readable.** The writer follows `output_models.pl`'s parameter
  ordering so the result looks like ASCII files authored in toolset.
2. **Re-compilable.** Anything we read in, we can write back out as ASCII
  that the same compiler (and the game's compiler) accepts.
3. **Lossless for the fields we model.** A field that exists in our
  `MeshData` / `Node` / `Animation` types makes it through both directions
   intact. Fields the binary stores but we don't model — engine-managed
   pointers, padding, deprecated proxy lists — are not preserved (and
   shouldn't be; the compiler regenerates them).

## File layout we read

```
[ 0 .. 11 ]                 header_file: {0, p_start_mdx, size_mdx}
[12 .. 12+core_len-1]       core block:  model header + node tree + animations
[12+core_len .. EOF]        MDX block:   per-vertex streams
```

Pointer conventions, mirroring the format itself (xoreos `NWN1MDL.bt`):

- **Core-relative** `p` → `goToPointer(p)` = `12 + p`
- **MDX-relative** `p` → `goToMDXPointer(p)` = `12 + mdxOffset + p`

Negative MDX pointers (`-1`) mean "stream not present" — the engine would
generate it at load time. We preserve that signal: on the way back out, the
compiler only emits MDX data the source actually carried (or that the
compiler chose to bake).

## Walk order

`Decompile` (`[pkg/mdl/binary.go](pkg/mdl/binary.go)`) is structured
top-down:

1. `**readFileHeader`** — read `{p_start_mdx, size_mdx}`, set up alloc
  tracking. We cap total bytes read at `fileSize * 16` to defuse hostile
   inputs that try to make us allocate gigabytes from a small file.
2. `**readModelHeader**` — model name, classification, supermodel, animation
  scale, list of animation pointers, root node pointer. If the layout is
   non-standard but the model name is recoverable, we emit a
   `WarnTruncatedData` warning and return whatever geometry we can.
3. `**readNode` (depth-first)** — for each node, read the 112-byte
  `header_node` (parent pointer, child list, controller blocks, content
   bits indicating which type-specific sub-headers follow), then dispatch to
   `readMeshHeader` / `readSkinHeader` / `readDanglyHeader` /
   `readAABBHeader` / `readLightHeader` / `readEmitterHeader` /
   `readReferenceHeader` based on the bits.
4. **Animations** — each animation pointer is followed once (`seenAnims`
  guards against repeats). Each animation has its own node tree which
   reuses geometry node IDs.
5. **Skin resolution** — bone references in skin meshes are stored as part
  numbers in the binary; after the geometry tree is read, we walk
   `pendingSkins` and resolve them to node names.

The decompiler is **non-fatal on errors**. Truncated data, out-of-bounds
pointers, and unknown controller types are recorded as
`DecompileWarning`s on the resulting model but never abort the read. The
caller decides how to surface them (the CLI prints them; tests can assert
on them). Warning kinds:

- `WarnTruncatedData` — pointer ran past EOF, or a fixed-size record
couldn't be fully read.
- `WarnPointerOutOfBounds` — a pointer was out of range and the read was
skipped.
- `WarnUnknownController` — a controller type ID we don't have a mapping
for; the controller is dropped with its type ID logged so we can decide
whether to extend coverage.
- `WarnGeneral` — anything else worth surfacing.

## Mesh decompilation

`readMeshHeader` reads exactly 512 bytes of fixed-layout header, then seeks
to MDX pointers to pull each per-vertex stream into the in-memory mesh:


| MDX stream               | Format                                                 | Read by                         |
| ------------------------ | ------------------------------------------------------ | ------------------------------- |
| Positions                | `count_vertexes × Vec3`                                | `readMDXVec3Array`              |
| UV0 / UV1 / UV2 / UV3    | `count_vertexes × 2 floats`                            | `readMDXTexCoords`              |
| Normals                  | `count_vertexes × Vec3`                                | `readMDXVec3Array`              |
| Colors                   | `count_vertexes × RGBA bytes` → unpacked to `Vec3` 0–1 | `readMDXColors`                 |
| Tangents + bitangents    | `count_vertexes × Vec3` each                           | `readMDXTangents`               |
| Skin weights / bone refs | per-vertex `4 × float + 4 × int16`                     | `resolveSkinWeights` (deferred) |


After reading per-vertex data, we `seek` back to the position right after
the 512-byte mesh header so subsequent type-specific sub-headers (skin,
dangly, AABB) are read from the right offset. The decompiler enforces this
with a `defer d.seek(headerEnd)` so an early return inside MDX reads can't
desynchronise the cursor.

### Tangent reconstruction

The binary stores tangent and bitangent as two separate `Vec3` MDX streams.
We read both, then for each vertex compute:

```
w = sign(dot(cross(normal, tangent), bitangent))
mesh.Tangents[i] = Vec4{tangent.xyz, w}
```

That's the inverse of what the compiler does, and means the in-memory
representation is the more compact `Vec4` (XYZ + handedness) the ASCII
format also uses. If the bitangent stream is missing or corrupt, `W`
defaults to `+1`, which is the correct fallback for the (common) case where
no UV mirroring is involved.

If the binary doesn't carry tangents at all (`p_mdx_tangent = -1`),
`mesh.Tangents` stays empty, the ASCII writer doesn't emit a `tangents`
block, and the next compile will either bake tangents (for normal-mapped
meshes) or leave the pointers unset for the engine to generate at load
time.

## Skin meshes

Skin nodes carry a per-vertex bone weight table and a per-mesh
`bone_part_numbers` array indexing into the geometry node tree. Because the
bone references resolve to node names that may not have been read yet
(forward references in DFS order are possible), `readSkinHeader` queues a
`pendingSkin` record. After the whole geometry tree has been walked,
`resolveSkinWeights` matches part numbers back to node names and fills out
the in-memory skin's `BoneRefs` field.

## AABB walkmeshes

`readAABBHeader` reads the root pointer, then `readAABBTree` walks the
binary tree iteratively (not recursively — these can be deep) and flattens
it into a list of `AabbEntry` records. Tree topology is preserved as-is for
diagnostic purposes; on the way back out, the compiler can either re-emit
the same tree or rebuild it with `RebuildAABB`. Either way pathing and
collision are unaffected because the same faces end up in the same leaves.

## Animations

Each animation block has its own `header_geometry` (animation name,
length, transition time, animation root, events, root anim node) and its
own node tree mirroring the geometry tree's shape. Animation nodes carry
content bits indicating which type-specific data the animation overrides
(positions, controllers, etc.).

`readAnimNode` decodes the same 112-byte node header layout as geometry
nodes, then dispatches the type-specific sub-headers based on content
bits. Controllers are read in column-format (a single shared time/data
buffer per node, with per-controller stride / column-count metadata).

`readControllers` (`[binary.go:1418](pkg/mdl/binary.go)`) handles
type-specific dispatch: 60+ controller IDs across light, emitter, mesh,
node, skin, and AABB nodes, each with their own value layout
(scalar / Vec3 / Vec4 / quaternion / RGB / sampling-period). When we
encounter an ID we don't have a mapping for, we emit
`WarnUnknownController` with the ID so we can extend coverage; the
controller itself is dropped (no guess about what its data means).

## Safety / defensive reads

Decompilation runs on untrusted input — model files arrive from haks of
unknown provenance. The decompiler:

- Tracks total allocations against `maxAlloc = fileSize * 16`. Any
allocation that would push past the cap is silently dropped (with a
warning). This kills "1 KB file claims to have 2 GB of vertices"
scenarios.
- Bounds-checks every pointer against `fileSize` before seeking. Pointers
past EOF or negative are skipped.
- Caps node-tree depth at 10 000 to defuse cyclic / pathological parent
pointers.
- Detects revisits via `visitedPtrs` so a node pointing back to its own
parent doesn't infinite-loop.
- Recovers gracefully from a non-standard header by trying to extract the
model name with a heuristic scan and returning a model with no geometry
(rather than panicking).

## How we test it


| Layer                               | Tests                                                                                                                                               | What they catch                                                                                                |
| ----------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------- |
| **Unit**                            | `TestDecompileFileHeader`, `TestDecompileNodeTypes`                                                                                                 | Header parsing, dispatch on each node type                                                                     |
| **Bulk fixture sweep**              | `TestDecompileAllFixtures` (`tests/fixtures/clean/`, `tests/fixtures/broken/`)                                                                      | Real-world files don't crash the decompiler                                                                    |
| **Roundtrip — ASCII → bin → ASCII** | `TestRoundtripFixtures`, `TestASCIIRoundtripFixtures`, `TestWriteAndParse`                                                                          | The two pipelines compose cleanly on the fixture corpus                                                        |
| **Roundtrip — bin → ASCII → bin**   | `TestRoundtripGeometryPreserved`, `TestRoundtripPreservesAuthoredTangents` and the 11 type-specific roundtrip tests in `compiler_roundtrip_test.go` | Fields survive the trip, including alpha=0, controller order, node-name case                                   |
| **Oracle** (cross-compiler)         | `TestOracleCompare`, `TestOracleVertexParity`                                                                                                       | Decompiling our binary produces the same `Model` as decompiling the game's binary, modulo documented baselines |
| **Regression**                      | `TestRegression_AlphaZeroPreserved`, `TestRegression_AABBRestoreOnError`, `TestRegression_SnapFloatNegative`                                        | Specific bugs we've hit before, locked down so they don't recur                                                |


The decompiler also gets exercised indirectly by every compile test: each
roundtrip test compiles a model and then decompiles the binary back to
verify the round trip. So the decompile path runs on every PR even when
the changes were nominally compile-only.

### `tests/fixtures/broken/`

A growing corpus of binaries known to trigger non-fatal warnings or to hit
recovery paths. The decompiler is expected to read these without panicking
and emit a model with appropriate `Warnings` populated.

## Known divergences (intentional, not bugs)

- **Engine-managed fields aren't preserved.** Fields like `p_geometry`
(filled by the engine at load), padding bytes, and deprecated proxy lists
are not surfaced into the in-memory `Model`. A bin → ASCII → bin
roundtrip will not be byte-identical because the compiler regenerates
these from a known template.
- **Vertex deduplication is not inferred.** When we read a binary mesh, we
read the GPU vertex array as-is. We don't try to reconstruct the
pre-deduplication ASCII vertex/UV indexing the original author may have
used. The ASCII output therefore has one vertex/UV per face corner. This
is the same layout `nwnmdlcomp` and the game compiler would accept and
re-emit.
- **AABB tree is preserved verbatim.** We don't recompute the tree on read;
whatever split the original compiler chose is what shows up in the
ASCII. The compiler's `RebuildAABB` is a separate, opt-in step.

## Known gaps

- **Unknown controllers are dropped.** When we see a controller type ID
we don't recognise, we log a warning and skip it. We've covered every
controller type we've encountered in the fixture corpus, but engine
versions or community extensions could introduce new ones we'd miss
silently apart from the warning.
- **No checksum / signature validation.** The binary format has no
built-in integrity field, so we have no way to detect a partially
corrupted file beyond "pointer out of bounds" warnings. A binary that's
corrupted in a way that keeps pointers in-bounds will read as a model
with subtly wrong data.
- **Limited fuzz coverage.** Decompiler defensive paths (bounds checks,
alloc tracking, depth caps) are exercised by the `broken/` fixture set
and by hand-crafted truncated inputs in `TestDecompileFileHeader`, but
there's no continuous fuzz harness.
- **Hak-only binaries with no surviving ASCII can't be cross-validated.**
We can decompile them, but we can't compare against an ASCII baseline
to know whether what we recovered matches author intent. The oracle
suite skips these (see `hakBinaryOnlySkipList` in
`pkg/mdl/oracle_test.go`).
- **Single-file output destination.** When decompiling to a fixed output
path (rather than a directory), batch mode will overwrite the same
destination file once per input. Use directory output for batch mode.

## Useful entry points


| Concern                        | File / function                                                                                                                             |
| ------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------- |
| Top-level orchestration        | `pkg/mdl/binary.go` — `Decompile`, `readModelHeader`                                                                                        |
| Mesh + MDX reads               | `pkg/mdl/binary.go` — `readMeshHeader`, `readMDXVec3Array`, `readMDXTangents`, `readMDXColors`                                              |
| Type-specific node sub-headers | `pkg/mdl/binary.go` — `readSkinHeader`, `readDanglyHeader`, `readAABBHeader`, `readLightHeader`, `readEmitterHeader`, `readReferenceHeader` |
| Animations + controllers       | `pkg/mdl/binary.go` — `readAnimation`, `readAnimNode`, `readControllers`, `readAnimControllers`                                             |
| ASCII output                   | `pkg/mdl/writer.go` — follows `output_models.pl` parameter ordering                                                                         |
| Warnings model                 | `pkg/mdl/types.go` — `DecompileWarning`, `DecompileWarnKind`                                                                                |


