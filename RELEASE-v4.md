# cleanmodels v4 + cleanmodels-qt v1.0

Two coordinated releases of the NWN MDL toolchain.

- **cleanmodels v4** — Go rewrite of the Prolog-based **cleanmodels 3.x** that the community has been running on EE. Same defaults, same repair set, new repairs added, a few legacy ones not yet ported. Single static binary, no SWI-Prolog runtime, five platforms.
- **cleanmodels-qt v1.0** — Qt6 + CMake rewrite of the GUI (last shipped as `build0.8.0-HEAD` on Qt5 + qmake). Talks to v4 over its JSON-lines protocol; the legacy Prolog CLI is no longer supported.

**Get it:**

- cleanmodels: <https://github.com/plenarius/cleanmodels/releases>
- cleanmodels-qt: <https://github.com/plenarius/cleanmodels-qt/releases>

## cleanmodels: changes since 3.7

**Rewritten in Go.** Single static binary, no SWI-Prolog install. Linux x86_64/ARM64, macOS Intel/Apple Silicon, Windows x86_64.

**Binary compiler folded in.** v3 stopped at ASCII output and handed the binary compile to `nwnmdlcomp`. v4 does both. Output is checked against the engine's own compiler on the engine-compiled models in the test suite (`tests/fixtures/oracle`); see "Known divergences" below for where it isn't byte-identical. A larger run over 32,707 stock and 49,111 community models was done during development on another machine. It is not reproducible from the repository, so treat the test suite as the reference.

**Tangents baked at compile time.** Normal-mapped models get tangent vectors and a per-vertex handedness sign written into the binary via [mikktspace](http://mikktspace.com/) instead of being derived by the engine on load.

**Compiler correctness fixes** that 3.7 + `nwnmdlcomp` (and the engine's own compiler) get wrong:

- **Danglymesh constraints follow GPU vert expansion.** The MDX vertex section stores per-corner GPU verts (one per unique pos+UV+normal+colour), so an ASCII vert with N distinct UV uses becomes N GPU verts. Existing compilers wrote the un-expanded constraint count and left the trailing GPU verts with undefined behaviour in the engine's dangle simulation — visibly, this is the tail-tearing on `c_squirrel` and most stock danglymesh models. v4 expands constraints along the same mapping so verts and constraints stay 1:1.
- **Smart skin-bone truncation.** Stock files like `c_fox` and `c_dogzombie` carry verts with 5–6 bone influences in their ASCII; the binary format hard-caps at 4 per vertex. Existing compilers keep the first 4 listed and let the kept weights sum to less than 1.0, leaving those verts under-bound. v4 keeps the four heaviest weights and renormalizes the kept set to 1.0.

**Performance.** Same workload, same disk, same flags, on a 330-file binary tile corpus (~21 MB, 2,030 repairs applied):


| Tool      | Workers | Wall time | Per file | Memory |
| --------- | ------- | --------- | -------- | ------ |
| Prolog v3 | 1       | 129 s     | 391 ms   | 244 MB |
| Go v4     | 1       | 5.0 s     | 15 ms    | 16 MB  |
| Go v4     | 8       | **4.4 s** | 13 ms    | 16 MB  |


~26× single-threaded, ~29× with parallel workers, ~1/15th memory.

Compiling models that share a skeleton is faster too: each supermodel is parsed once per run instead of once per model. A batch of 5,015 human body-part models with `--game-dir` went from 1m48s to 18s with byte-identical output.

**Subcommand CLI.** `cleanmodels check | repair | compile | decompile | report`, plus `cleanmodels --version` (or `-v`) for the version, commit, and build date. The legacy flag form (`cleanmodels --check --fix-pivots in/ out/`) still works for existing scripts.

**New repairs.** Chamfer add/delete, dynamic water (flat/wavy/untouched), additional tilefade slicing/undo work, plus two EE-cleanup flags: `--standardize-texture0` (emit `texture0` instead of `bitmap`) and `--strip-ee-extras` (drop `wirecolor`/`specular`/`shininess` on output).

**New validation checks.** Resref-length warning (16-char engine cap on bitmap, texture, and material names — catches missing-texture bugs before they ship), PLT bitmap matching for character models, and a node-type field validation matrix (danglymesh, skin, emitter, light, aabb, misplaced-tile, and misplaced-dangly checks). Total registry is now 48 checks across structure, geometry, parameters, node types, animations, and tiles.

**`cleanmodels report`.** Uploads the file plus log and opens a tracked GitHub issue without requiring the user to have an account. Available in the GUI too.

**WASM build.** `cleanmodels-wasm.zip` runs the same pipeline in a browser page. Two static files, small JS API; useful if you maintain a model viewer or wiki tool that wants to accept binary `.mdl` drops.

**Bezier position keys.** `positionbezierkey` animations (as exported by NWmax) are preserved through ASCII and compiled to binary, tangents included. Other bezier controllers are skipped with a warning.

**Skinned models.** Skin meshes are compiled with the bind-pose data the engine reads when it draws them. Release candidates up to rc17 left it out, and a skinned model compiled by them could crash the client with an access violation. Recompile any skinned models built with an earlier candidate.

**Supermodels.** A model's node numbers now follow its supermodel, as the engine's compiler assigns them. Keep supermodels in the same folder as the models you compile, or point at them with `--supermodel-dir`. Most body-part models use a stock supermodel (`pmh0`, `pfh0`, `a_da`, ...) that lives inside the game's data files: `--game-dir <NWN install>` (or the `NWN_ROOT` environment variable) reads those straight from the install's BIF files, and `--resource-dir` accepts `.hak` files as well as folders. If a supermodel can't be found, the compile still succeeds and a warning says so.

**Tangents follow the material.** The engine bakes tangents for a mesh whose render hint is `NormalAndSpecMapped`, set on the mesh or in its `.mtr` material (named by `materialname`, else `texture0`). v4 now makes the same decision, so PBR tile content gets its tangents. Point `--resource-dir` at the folders or `.hak` files that hold your materials (patch haks such as `ts_bioforest_pbr.hak` included), or `--game-dir` for stock ones; without them only a mesh's own `renderhint` counts.

**Shadow default.** A trimesh, skin or danglymesh with no `shadow` line now casts shadows, as in the engine; walkmeshes still default to off.

**Dropped nodes are reported.** Nodes the binary leaves out (a parent that doesn't exist, a second root) now produce a warning naming the node and the reason, instead of vanishing silently.

**JSON-lines output.** `--json` for single results, NDJSON streaming for batches. Stable schema; this is what the Qt GUI consumes.

## cleanmodels-qt: changes since build0.8.0

**Qt6 + CMake.** Replaces Qt5 + qmake. C++17. Qt5 builds are gone.

**Talks to v4, not Prolog 3.x.** UI surface aligned to the new subcommand interface (Clean / Decompile / Compile modes). Findings come over JSON-lines from v4 instead of text-scraped from Prolog output.

**3D viewport** — new in this release. Drop a binary or ASCII MDL onto the preview pane:

- **BioWare DDS textures load directly.** The engine's proprietary DDS variant — used for almost every stock creature and tile texture — isn't readable by standard image libraries. The viewport decodes BC1 and BC3 blocks inline, so creatures show up textured instead of grey.
- **Skinned meshes render against their skeleton.** Animals, dragons, and any other skin-weighted geometry display correctly bound to the bone hierarchy.
- **Animations play back live.** The viewport picks an idle on load and exposes the model's full animation list for stepping through.
- Reference-model overlay (compare two models side by side, with a preferred-pose hint applied to the reference).

**Game Resources** section in the sidebar: a game install folder (passed as `--game-dir`) and extra folders or `.hak` files (`--resource-dir`), remembered between sessions. Compiled models then get engine node numbering against stock supermodels and tangents from PBR materials, with nothing extracted by hand.

**Report Issue** integrated — same upload/track flow as `cleanmodels report`, available under Help and on right-click for failed files.

**Five-platform packaging matching the CLI** (Linux x86_64/ARM64, macOS Intel/Apple Silicon, Windows x86_64). Linux ships as an AppImage (was: tarball that needed Qt5 installed). The matching cleanmodels CLI is bundled inside each Qt download next to the GUI binary, so a single download is enough to use either tool.

## Migration notes

The Qt GUI looks for `cleanmodels` (`cleanmodels.exe` on Windows) on `PATH` or alongside the GUI binary, and now ships with the matching v4 CLI bundled in that second slot, so no separate install is needed. If you keep an older `cleanmodels` higher on `PATH`, **it will not work** — the GUI requires v4.

Two behaviour changes worth knowing:

- **Per-axis scaling actually scales per axis.** The old GUI averaged X/Y/Z into a single number before passing it through. Presets that depended on that averaging will produce different output.
- **An empty input directory now exits non-zero.** Wrong-path bugs in CI used to silently succeed; they now fail loudly. Wrap with a guard if "directory might be empty" is intentional.

## Known divergences from the game compiler

Binary output is **not** byte-identical to the engine's own compiler in a few cases. All documented and tested:

- Vertex deduplication differs from the game's. On character body parts we produce fewer GPU verts. On skin meshes the count can be higher or lower: the engine's rule follows smoothing groups and is only partly reproduced. No visible difference has been seen in game.
- AABB tree split axis can differ. Pathing and collision are unaffected; the same faces end up in the same leaves.
- Tangent values match within tolerance (mean alignment ≥ 0.96, bad-corner ratio ≤ 5%), not bit-for-bit.
- Mesh bounds (centre, radius, bounding box) and surface area are computed from the expanded vertex list rather than the source one, and surface area is written as 0, so they can differ slightly from the engine's.
- Animation nodes with no matching geometry node get a different node number from the one the engine gives them.
- Animation nodes that are mesh nodes: the engine writes a full 512-byte mesh header for them, filled with uninitialised memory. We write none, so the binary is smaller. Controllers, keys, and tangents on those nodes match.

Two legacy steps not yet ported (UV-space TVert welding and tessellator midpoint-TVert dedup). Both are documented in `CLEAN.md` — same render output, marginally larger MDX block on heavily tessellated water meshes.

## Downloads

Per-platform builds and checksums live on the GitHub release pages:

- cleanmodels: [github.com/plenarius/cleanmodels/releases/latest](https://github.com/plenarius/cleanmodels/releases/latest) — Linux x86_64/ARM64, macOS Intel/Apple Silicon, Windows x86_64, and a browser WASM bundle.
- cleanmodels-qt: [github.com/plenarius/cleanmodels-qt/releases/latest](https://github.com/plenarius/cleanmodels-qt/releases/latest) — Linux x86_64/ARM64, macOS Intel/Apple Silicon, Windows x86_64.

The Qt download bundles the matching cleanmodels CLI next to the GUI binary. If you only want the CLI (CI scripts, headless servers, the WASM build), grab it from the cleanmodels release page directly.

The last Prolog releases stay published:

- cleanmodels Prolog: [build3.7.0-HEAD](https://github.com/plenarius/cleanmodels/releases/tag/build3.7.0-HEAD)
- cleanmodels-qt Qt5: [build0.8.0-HEAD](https://github.com/plenarius/cleanmodels-qt/releases/tag/build0.8.0-HEAD)

## Thanks

To OldMansBeard for designing the original [CleanModels3](https://neverwintervault.org/project/nwn1/other/cleanmodels3).

Format documentation and reference implementations: [nwn.wiki](https://nwn.wiki/display/NWN1/MDL), [xoreos-docs](https://github.com/xoreos/xoreos-docs), [neverwinter.nim](https://github.com/niv/neverwinter.nim), [nwnmdlcomp](https://github.com/nwneetools/nwnmdlcomp), [varenx/borealis_nwn_model_viewer](https://github.com/varenx/borealis_nwn_model_viewer) (Qt animation player + skinmesh rendering reference), and [dunahan/nwn_mdl_webviewer](https://github.com/dunahan/nwn_mdl_webviewer) (WebGL renderer cross-reference).

Morten S. Mikkelsen for [mikktspace](http://mikktspace.com/).

## License

MIT. See [LICENSE](https://github.com/plenarius/cleanmodels/blob/main/LICENSE).
