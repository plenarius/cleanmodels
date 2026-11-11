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
   `pendingSkins` and resolve them to node names (from the creature
   skeleton for a skinned body part; see "Skin meshes").

### Declaration order

The game's compiler numbers nodes in declaration order and sorts a node's
children where one is a mesh with transparencyhint above 0 (std::sort, not
stable past 16 children). So the decompiler picks the order the nodes are
declared in (`declarationOrder`, `binary_order.go`): ascending part number,
when the model has no supermodel and that order rebuilds the stored numbers
and tree; else, for each node whose children get sorted, the order the sort
turns back into the stored one; else the stored order. A candidate is taken
only when a compile of it rebuilds the stored tree.

- **Sort keys.** A donor restored from a merged render list takes its
  receiver's transparencyhint (see "Merged render lists"), and the ASCII
  carries that one, so the candidates are planned with it first: an order
  found that way rebuilds the stored tree as a compile sorts it. Only when
  none does, and the stored order does not either, are they planned with the
  stored hints, which the stored tree was sorted by; the compile then sorts
  the donor among its siblings by its new hint (`plc_tn1_28`). Planning with
  the stored hints first moved the equal-hint siblings of a donor whose
  stored hint was the only one above 0 (`trlk1_o08_01`): the compile, sorting
  nothing, kept the order a sort would have turned back.
- **Duplicate names.** A parent is found by name: the nearest node of that
  name declared before it. A name used twice by nodes that are no parent
  changes nothing. Where a name used twice is some node's parent (a child
  dummy named like the root, `eld02_p18_01`), which node a child goes under
  depends on the order: such a model gets no part-number order, but the
  inverse sort, checked against the stored parents, still applies. The writer
  finds each node's children the same way when names repeat, so it writes
  that order back.

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

The same pointer is water's bump-map animation stream in 1.69 models, which
is not tangents (582 of the game's binary meshes, e.g. stock `tcn01_a15_01`
node `plane452`: `3.080194e-36 ...`); those meshes also set texture animation
slots 0, 1, 2 and 4, which EE's compiler never writes. So the stream is read
as tangents (nwn-mdl-spec MDL-BIN-048) when:

- the mesh's render hint is normal-mapped (2 `NormalAndSpecMapped`, 3
  `NormalTangents`); or
- every vector is unit length (squared length within 0.02 of 1). The engine's
  compiler writes hint 0 and real tangents for meshes whose hint comes from
  their material (all 393 tangent meshes in
  `tests/fixtures/oracle/tangent_pairs`), so the field alone can't decide; or
- slots 0, 1, 2 and 4 are unset, at least one vector is unit length, and
  every finite, non-zero vector is. This keeps the streams other tools wrote
  with some NaN or zero vectors among real tangents (251 meshes in 169 of
  Layonara's models, 96.6% unit vectors), while the 1.69 streams stay refused:
  all zeros (Layonara's `tcn01` `water01`), tiny values (`plane452`), or one
  vector of length 1 by chance among zeros (stock `tcn01_l03_01` `plane211`,
  `tts01_c02_01` `plane95`, `6.1e-33 3.1e-36 1.00106`, with slots 0-2 and 4
  set).

A stream is read as raw bits, so a NaN is told from a zero. In a stream that
is not read as tangents, a NaN or infinite float is reported as in any other
list ("NaN/Inf floats in tangents replaced with 0").

**Bad tangents are filled** (`binary_tangents.go`). In a stream that is read
as tangents, a vector that is NaN or infinite, zero, or (with a normal-mapped
hint) not unit length is replaced with the tangent the game would generate
for that vertex: `generateTangents` (MDL-MESH-043) run on the binary's own
vertex list and index list (MDL-BIN-049), with its handedness. A vertex the
game would clone for triangles of the other handedness keeps the tangent of
the first handedness to reach it, as the game's vertex does. The good vectors
stay exactly as stored. A vertex the generation gives no tangent (no UVs or
normals in the mesh, or no triangle reaches it) gets the one the game gives a
triangle without UV area: its normal with the components rotated to
`(y, z, x)`, made perpendicular to the normal and normalised, or the X axis,
then the Y axis, where that is parallel to the normal; handedness +1. One
warning per mesh says how many were filled and what they were:

```
mesh "trunk017": 98 of 1860 stored tangents are unusable (98 NaN or infinite); filled with the tangents the game generates for those vertices, the others kept as stored
```

Measured on Layonara's haks: 527 meshes in 189 models keep their stream
now, with 10,367 vectors filled. On 238 of the 251 meshes an earlier audit
listed (the other 13 lose a degenerate triangle on compile, so their corners
can't be compared in order), the round trip keeps the stored tangent at all
524,060 drawn corners whose tangent is unit length (165,654 of them changed
before) and fills the 17,035 NaN or zero ones. On all 25,597 binary models of
the base game the decompiled ASCII, warnings and notes are unchanged.

The render hint field holds what the engine's
`SharedMaterial::ExtractRenderHint` gives: 0 no hint, 1 `None`, 2
`NormalAndSpecMapped`, 3 `NormalTangents`.

If the binary doesn't carry tangents at all (`p_mdx_tangent = -1`),
`mesh.Tangents` stays empty, the ASCII writer doesn't emit a `tangents`
block, and the next compile will either bake tangents (for normal-mapped
meshes) or leave the pointers unset for the engine to generate at load
time.

### Tangent clones

When the game generates tangents, a vertex reached by triangles of both
handedness is cloned: the clone goes to the end of the vertex list and only
the index list is rewritten to use it; the face records keep the original
(nwn-mdl-spec MDL-MESH-043). The engine draws the index list (MDL-BIN-049).
`applyTangentClones` (`binary_clones.go`) therefore points each face at the
vertices of its triangle in the index list when they are the face's own
vertices or clones of them (the same position and UV), so the decompiled
faces draw the clones' tangent and handedness and no clone is left unused.
A list triangle that is some other triangle leaves the face as read.
`TestDecompileDrawsTangentClones` checks this against the game's compile of
`zspec_tanmirror`.

**Generated tangents are left out when a compile makes them again**
(`binary_gentangents.go`). The clones sit at the end of the vertex list, but
tangents written in the ASCII are part of the compile's merge key
(MDL-MESH-010, MDL-MESH-044), and a compile numbers each vertex where a face
first uses it. A clone used before the last original vertex's first use
therefore moves, and every vertex after it with it: no ASCII with tangents
compiles back to the binary's vertex order. On Layonara's haks this made 713
models (cleanmodels v4.17.5) differ in vertex order between a compile and the
compile of its own decompile. So, when the mesh is normal-mapped (its own
render hint, or its material found in `DecompileOptions.MaterialDirs`, which
the CLI fills with the same locations as compile's resource dirs), the
decompiler folds each
clone back into the vertex it was cloned from, drops the tangents, and runs
the compile's merge and tangent generation on the result. Only if that gives
the binary's vertices back exactly (count, order, positions, UVs, normals,
colours, skin weights, constraints, tangents and handedness bit for bit, and
the index list) is the mesh written that way, with a note:

```
mesh "box1199": tangents left out: a compile generates the same ones (the mesh is normal-mapped), and only a generating compile keeps their 22 cloned vertices at the end of the vertex list
```

(without the clause about clones when there are none). That includes a mesh
whose index list already uses its vertices in first-use order, which a
compile with the tangents written would number the same: its face records
would then name the clones (a generating compile keeps the vertex each was
cloned from in them), and two vertices on a UV seam whose generated tangents
differ could not be written as one position, so a compile would link no faces
across the seam. On Layonara's haks that left 168 models unstable
(cleanmodels v4.17.5); leaving the tangents out makes 158 of them
byte-stable and the other 10 differ only in face neighbours.

Anything else keeps its stored tangents, as do meshes whose stream had bad
vectors filled, and `KeepVertexSplits` (which writes one vertex per binary
vertex). The game's own `zspec_tanmirror` decompiles this way too, and
compiles back to the game's vertices and index list
(`TestGeneratedTangentsGameGolden`).

A binary vertex no face uses (the game, and cleanmodels, keep the vertices a
degenerate face added before dropping the face) still can't be written: the
ASCII has no faces that would make it, so a compile of the decompile loses
it, and every vertex after it moves up.

### Merged render lists

Some tools compile a model with the meshes of one texture drawn as one: the
receiver's index list (MDL-BIN-049) holds its own faces' triangles followed by
copies of other meshes' triangles, in its own frame and with its own appended
vertices; each donor keeps its faces, with render 0 and an empty index list.
The game never draws a render-0 mesh (`PartTriMesh::IsPartRenderable`), and it
builds shadows from face lists whatever the render flag. ASCII has only faces,
so `unmergeRenderLists` (`binary_merged.go`) matches each donor's faces to
copies in a receiver's list (texture coordinates, side lengths, and the copy
that lies where the donor does in world space first; every receiver first takes
only donors whose copies it holds in place, so of two receivers of one texture
neither takes the other's) and writes what the game drew, in one of two ways:

- **Restored** (usual): the donor gets render 1 and draws on its own. It takes
  what its copies were drawn with, not its own values, which were never drawn:
  the receiver's drawing settings (diffuse, ambient, specular, shininess,
  texture 1 and 2, material, render hint, transparency hint, tile fade,
  rotatetexture, alpha, self-illumination; a note names those that differed),
  and the copies' normals, tangents and handedness. A donor has no index list,
  so its tangent clones reach none of its faces: where its copies disagree on a
  vertex, the corners of each other group move to the donor's own unused clone
  of it, or to a new copy of the vertex.
- **Folded**: when the donor restored would draw something other than its
  copies, the copies are written as faces of the receiver (in index-list
  order, with the donor face's smoothing group, material and stored
  neighbours), and the donor keeps render 0. The rule: the restored donor must
  draw where its copies were drawn in the rest pose and in every animation,
  and look as they did. So a donor folds when its vertices, placed in world
  space, miss its copies by more than 1 mm (the merging tool baked them
  without the donor's `scale`, or `plc_candle3`'s copy, 1.3 mm off; 1 mm is
  the distance nwn-mdl-spec's round-trip checker pairs triangles at), when an animation moves a node on the donor's or the receiver's
  parent chain below the nodes they share (cabinet and safe doors the game drew
  still, as part of the cabinet),
  or when an animation changes the alpha or self-illumination of either away
  from the receiver's. If both cast shadows, the donor gets `shadow 0`, since
  the receiver's copies now cast it.

Either way, the receiver's vertices that only the copies used are dropped: the
compile computes bounds over every vertex (MDL-MESH-050), so unused ones moved
the model's bounding box between a compile and the compile of its decompile.

### Shared positions after dropped faces

A binary vertex is one per distinct corner, so the decompiler gives vertices
split only by a UV seam one position again, following the stored face
neighbours (`sharePositions`, nwn-mdl-spec MDL-MESH-025). When the compile
dropped a degenerate face, those neighbours are not in binary numbering: the
game, and cleanmodels, write face record k with source face k's neighbours,
numbered as source faces (MDL-MESH-021, MDL-MESH-022). `storedNeighbours`
(`share_neighbours.go`) finds where faces were dropped from the neighbours
themselves (the numbering in which each record's links share their edge by
position) and renumbers them before positions are shared; without that, eight
of Layonara's models shared almost no position (`eld02_g02_05`
`meshTEC0505`: 62 positions written for the source's 41). The stored
neighbours themselves (`MeshData.StoredAdjacency`) stay as read, since
`OpenMultiEdgesBinary` patches the file by record.

## Skin meshes

Skin nodes carry a per-vertex bone weight table and a per-mesh
`bone_part_numbers` array indexing into the geometry node tree. Because the
bone references resolve to node names that may not have been read yet
(forward references in DFS order are possible), `readSkinHeader` queues a
`pendingSkin` record. After the whole geometry tree has been walked,
`resolveSkinWeights` matches part numbers back to node names and fills out
the in-memory skin's `BoneRefs` field.

A skinned body part (`pmh0_neck001`) stores its bones as tree positions in
the creature skeleton (`pmh0`), not in its own file: the engine's HD neck has
3 nodes, a `boneindexarray` of 56 entries and the bone map `4 5 2`. When a
skin's `boneindexarray` is longer than the file's tree, or a weighted bone
sits past its last node, the decompiler names the bones from the skeleton
(`DecompileOptions.SkeletonDirs`; the CLI searches the same places as
compile, game install included). Without the skeleton it leaves those bones
unnamed and warns, rather than naming them after the part's own nodes. It also warns when a skin has vertices with no valid bone influence
(all weights zero, or references the file does not map, such as the
uninitialised slots an engine compile can leave): in game such a vertex is
drawn at the origin.

A 100-byte skin header (BioWare files and some EE ones) holds 17 bone list
entries and one more word; the game copies 64 entries whatever the layout, so
later entries are whatever follows the header. Where a weighted bone's list
entry lies outside its tree, the bone is named from the `boneindexarray`
instead, with a warning (stock `tni_udoor_19`: bone 17 reads 109 in a 46-node
tree; finding 47).

## Animmesh sets

The binary stores an animmesh's `animverts` and `animtverts` vertex-major:
each vertex in every set before the next vertex. The ASCII lists one whole set
after another, so `readAnimMeshHeader` transposes them (`animSetMajor`), and
the compiler transposes them back over its GPU vertices (finding 44).

An animation's animmesh node carries a mesh of its own (verts, faces, tverts
and normals). Its `normals` are read, written in the animation node and
parsed there: the game's compiler stores them as given and keeps the vertex
count, and without them computes per-face normals from the smoothing groups
and splits the vertices, so the vertex sets no longer line up (nwn-mdl-spec
MDL-MESH-065, probe `zspec_animnrm`).

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

A controller id given twice on one animation node (two one-key `orientation`
controllers, `tcn01_p02_01`) is two controllers to the engine, applied in
stored order, each overwriting the field (nwn-mdl-spec MDL-CTRL-036). ASCII
holds one key list per controller, so the last one's keys are written; they
were appended to the first one's before, which made one controller of both.

An animation node takes its geometry node's type. A node the geometry has no
node of that name for is written with the type word its stored flags give
(`AnimNode.Type`): `trimesh` for 0x21, `light` for 0x3, `emitter` for 0x5,
`dummy` otherwise. The game's compiler gives such a node the type of its type
word (nwn-mdl-spec MDL-NODE-044), so a compile gets the flags back, and with
them the keys only that type reads (`tvd_udoor_09`'s `09_DWK_wg_closed` and
its `alphakey` rows).

Emitter IDs 464 and 468 mean different controllers to BioWare's compiler
(percentstart, sizemid) and to nwnmdlcomp (alphamid, colormid). The compiler is
told by the first key that only one of them uses (448/452 BioWare, 480–488
nwnmdlcomp, or 468 with three columns, nwnmdlcomp's colormid), looking at all of
an emitter's keys before naming any. Undecided, BioWare's names are used.

**Bezier controllers** (0x10 on the column count) store each key as the value,
then a handle in, then a handle out, each as wide as the value. The game's
compiler writes `0x10 | columns` (0x13 for position, 0x11 for a float);
nwnmdlcomp writes the row's width (0x19), so the column count is taken from the
controller, not the byte. Position, float and colour controllers are read with
their handles and written back as `<name>bezierkey`. On a geometry node the
value is kept and the handles dropped (a geometry node has one value).
Orientation has no bezier form that can be stored (the game's compiler keeps
`orientationbezierkey` rows unconverted), so a bezier orientation controller is
dropped with a warning; in ASCII, `orientationbezierkey` is read as linear
`orientationkey` rows (time and axis-angle). Ground truth:
`tests/fixtures/bezier/plc_a01.mdl` (position) and `cmbez01.mdl` (every float
and colour kind on a mesh, light and emitter, plus orientation), both written by
the game's own compiler (`nwmain-linux compilemodel`); see NWMAIN_FINDINGS.md
33–36.

## Orientations

The binary stores an orientation (a geometry node's, or an animation's
`orientationkey`) as a quaternion; ASCII writes it as axis-angle.
`quaternionToAngleAxis` (`binary.go`) takes the angle as `2·atan2(|xyz|, w)`
and the axis as `xyz / |xyz|`, all in float32. That is not an exact inverse of
the compile's conversion (`axisAngleToQuat`, `vec.go`: the axis through
engineNormalize, times `sinf(angle/2)` in float32, with `cosf(angle/2)` as w):
`|xyz|` is a float32 square root, not the sine the compile takes, so about one
quaternion in five came back 1 or 2 float32 steps off (about 1e-7 rad). Nothing
drawn changed, but each decompile and compile could move it again, so the bytes
of some models never settled (`tde01_p15_01`).

So `exactAxisAngle` checks the result with the compiler's own conversion, and
when it does not give back the stored quaternion bit for bit, looks for one
that does a few float32 steps away. The components separate: the angle alone
decides w, so the angle is stepped first (0, +1, −1, … up to ±4 steps) until w
matches; for that angle each axis component alone decides its quaternion
component (while the axis is within engineNormalize's tolerance), so each is
stepped the same way. A candidate is written only if the whole quaternion
matches. A quaternion compiled from an angle past ±2π often has no such
preimage with an angle in [0, 2π] (w is rounded on another grid there), so the
same rotation with the axis negated and 4π minus the angle is searched next.
If neither search finds one (a quaternion no float32 axis-angle reaches, such
as one written by another compiler or not of unit length), the plain
conversion is written, as before.

Every quaternion `axisAngleToQuat` gives for an angle in (−4π, 4π) is found
(`exact_axis_angle_test.go`, 50,000 random cases), so a decompiled model
compiles to the same orientation bytes every time: compile → decompile →
compile is byte-stable (`tests/fixtures/quat/zcm_quat.mdl`). Bezier
orientation controllers are dropped (see above), so this is the only path.

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
- **Junk emitter texture names are left out.** An emitter whose texture
field holds bytes that are not printable ASCII before its first NUL
(uninitialised memory some old compilers left on chunk emitters, which draw
their chunk, not the texture: `ff ff ff ff`, `cd cc 4c 3e`) gets no
`texture` line, with a warning, instead of a name made of what printable
bytes there are (`l>`).
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

## Python port

`python/cleanmodels` mirrors this pipeline (`binary.py`, `writer.py`) for
tools that can't ship the Go binary. Its output is checked byte for byte
against `cleanmodels decompile` in CI (`python/parity_test.py`): every binary
fixture, 40 corruptions of each, and float formatting. Changes here need the
same change there; see `python/README.md`. The parser and compiler are ported
too; see COMPILE.md.

## Useful entry points


| Concern                        | File / function                                                                                                                             |
| ------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------- |
| Top-level orchestration        | `pkg/mdl/binary.go` — `Decompile`, `readModelHeader`                                                                                        |
| Mesh + MDX reads               | `pkg/mdl/binary.go` — `readMeshHeader`, `readMDXVec3Array`, `readMDXTangents`, `readMDXColors`                                              |
| Type-specific node sub-headers | `pkg/mdl/binary.go` — `readSkinHeader`, `readDanglyHeader`, `readAABBHeader`, `readLightHeader`, `readEmitterHeader`, `readReferenceHeader` |
| Animations + controllers       | `pkg/mdl/binary.go` — `readAnimation`, `readAnimNode`, `readControllers`, `readAnimControllers`                                             |
| ASCII output                   | `pkg/mdl/writer.go` — follows `output_models.pl` parameter ordering                                                                         |
| Warnings model                 | `pkg/mdl/types.go` — `DecompileWarning`, `DecompileWarnKind`                                                                                |


