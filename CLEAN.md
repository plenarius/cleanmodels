# Clean pipeline

`cleanmodels check` and `cleanmodels repair` are the validation and auto-fix
half of the toolchain. `check` runs read-only diagnostics; `repair` runs the
same diagnostics with `fix=true` so the registered fixers can mutate the
model, plus a set of optional structural/tile/placeable transforms that live
outside the check registry. Together they're what we mean by "cleaning" a
model.

We try to be honest about what we cover, what we just warn about, and where
we differ from the legacy Prolog tool. Expect drift in edge cases — file an
issue with a minimal reproducing fixture if you hit one.

## CLI surface

```
cleanmodels check   [flags] <input>
cleanmodels repair  [flags] <input>
```

Both accept a single file or a directory (recursive with `--recursive`).
`check` runs every check in the registry with `fix=false`. `repair` runs the
same checks with `fix=true` and additionally applies whichever optional
transforms are enabled by flag. Pass `--dry-run` to repair to see what would
change without writing anything.

Checks can be selected explicitly:

```
--include-check geometry.foo,parameters.bar
--exclude-check tiles.x
```

These take comma-separated check names; the empty set means "all".

`cleanmodels-qt` (the Qt frontend) exposes the same checks as a tree of
selectable items — same registry on both sides, no separate wiring.

## What runs in the registry

48 checks live in [pkg/checks/](pkg/checks/), grouped by file:

- **structural.go** (9 checks) — root/dummy invariants, parent links, name
uniqueness, name length, supermodel reference, classification.
- **geometry.go** (9 checks) — vertex/face counts, degenerate faces,
multi-edges, normals, AABB tree shape, walkmesh material clamping.
- **parameters.go** (9 checks) — alpha/diffuse/specular ranges, tex slot
counts, materialname / texture resref length validation, render hint
validity.
- **node_types.go** (7 checks) — node-type-specific field validation
(rotatetexture only on tiles, danglymesh-required fields, etc.).
- **emitters.go** (7 checks) — emitter percent ranges, blend/render/update
string validity, particle limits, lifetime sanity.
- **animations.go** (4 checks) — controller key time ordering, animation
length, transtime, animroot reference.
- **tiles.go** (3 checks) — tile classification implications: walkmesh
presence, day2night/night2day length floor, tilefade Z constraints.

Each check returns a list of `CheckResult` values; running with `fix=true`
asks each check to mutate the model in place when it can. Some checks are
diagnostic-only (e.g. parameter range warnings); those are noted in the
result message.

## What `repair` does on top of the checks

Repair-only operations live in `pkg/mdl/repair_*.go` and are guarded by
explicit flags. They are not run by `check` and are not part of the registry:

- **Geometric repairs** — `--strip-degenerate`, `--split-multi-edge`,
`--snap-vertices`, `--scale`, `--scale-x/-y/-z`.
- **Pivot/structure** — `--fix-pivots`, `--fix-aabb`, `--reparent-children`,
`--wrap-root`.
- **Tile-specific transforms** — `--tilefade-z`, `--retile-water`,
`--retile-ground`, `--rotate-water`, `--rotate-ground`, `--raise`,
`--lower`, `--chamfer`, `--water` + `--dynamic-water`,
`--foliage`, `--splotch`.
- **Placeable transforms** — `--placeable-transparency`,
`--standardize-texture0`, `--strip-ee-extras`.
- **Material/render overrides** — `--render-override`, `--shadow-override`,
`--force-white`, `--merge-by-bitmap`, `--cull-invisible`.
- **Walkmesh** — `--remap-walkmesh-material FROM:TO`.
- **Tilefade undo** — `--tilefade-undo`.
- **Animation length floor** — `--fix-animations`.

Each of these returns a list of human-readable messages summarising what was
done; they go straight into the JSON `repairs` array per file.

### Chamfer

`--chamfer add` walks every mesh node, projects vertices into world space via
the parent chain, finds open boundary edges sitting on `X = ±5` or
`Y = ±5` whose face normal points roughly upward, and synthesises two
"Rosenkrantz Chamfer" triangles per edge. The new triangles are tagged with
smoothing group `1048576` and material `21` so they can be deleted again
later; vertex coordinates are offset 3 cm outward and 3 cm down. The world →
local back-projection means chamfering on a non-tile-aligned mesh inside a
nested transform chain still snaps to the tile grid.

`--chamfer delete` is the inverse: every face whose smoothing group is
`1048576` and whose world-space geometry matches one of the chamfer patterns
in `is_chamfer/3` (axis-aligned or corner-aligned, off-axis vertex within
1–5 cm of the boundary) is removed. Vertices and tverts that no remaining
face references are then dropped, and per-vertex normals/colors/tangents/
weights/dangly constraints are compacted in lockstep.

Implementation lives in `pkg/mdl/repair_chamfer.go` and
`pkg/mdl/repair_chamfer_add.go`; the world-space transform composer is
`pkg/mdl/transforms.go`, half-edge enumeration is `pkg/mdl/halfedge.go`.

### Dynamic water

`--water` flags a node as a water plane (matched against the bitmap key list
plus the eight stock `shiny_water` bitmaps). `--dynamic-water` then chooses
how the flagged node should behave:

- `**yes`** — leave the existing animmesh intact (no-op).
- `**no`** — drop the `AnimMeshData` (sampleperiod, animverts, animtverts,
clip rect) so the node serialises as a plain trimesh. Flat, static water.
- `**wavy`** — tessellate the plane to a 2 m pitch, weld the resulting
duplicate vertices, ensure a `default` 10 s animation exists, and emit
six per-vertex keyframes (`v, v', v'', v''', v'''', v`) into `animverts`,
with `Z`-perturbations driven by the legacy `perturb/12` polynomial. UVs
are static across frames. The per-tile wave amplitudes come from a
seeded RNG keyed on `model_name + node_name` so two runs over the same
input produce byte-identical output.

For `wavy`, the pre-pass also bakes the node's local transform chain into
the vertex positions and reparents the node directly under the model root,
matching the legacy raise-to-tile + zero-orientation + zero-position
sequence. This is needed because the perturb function operates in tile-local
coordinates.

Implementation: `pkg/mdl/repair_water.go`. Tessellator and welder are
shared utilities (`pkg/mdl/tessellate.go`, `pkg/mdl/weld.go`).

## Testing strategy

- Unit tests for every individual repair: see `pkg/mdl/repair_*_test.go`.
These build small synthetic meshes with hand-crafted edge cases and
assert post-conditions on vertex / face counts, indices, controllers, and
AABB structure.
- Integration tests for the check registry: `pkg/checks/*_test.go` walk the
fixture sets in `tests/fixtures/`.
  - `tests/fixtures/clean/` (5 files): models that pass every registered
  check. Adding new checks should not require changing these fixtures.
  - `tests/fixtures/broken/` (7 files): models that intentionally trip a
  specific check; each fixture is paired with the exact check it should
  fire.
- Determinism tests for the new repairs that involve any randomness:
`TestApplyWavyWater_Deterministic` runs the wavy-water generator twice
on the same input and asserts byte-identical animvert output.
- The `compile` / `decompile` oracle suites (see `COMPILE.md`,
`DECOMPILE.md`) indirectly exercise some of the repair paths because
several repairs run before compile in the end-to-end test harness.

## Known divergences from the legacy Prolog tool

- **Welder**: the legacy welder buckets vertices by a hash of position +
weight + constraint + color, and refuses to merge two vertices when
their averaged face normals are nearly opposite (dot < −0.9, the
back-of-thin-sheet case). We do bit-exact position welding by default and
expose `WeldOptions.PreserveOpposite` for callers that want the normal
divergence guard. The wavy-water repair turns it off because water is a
flat plane; for general re-welding callers should set it.
- `**weld_tverts`** (legacy) and the Prolog `recompute_normals` step are
not yet ported — the welder doesn't touch the UV index space, and we
rely on `compiler_normals.go` to regenerate per-corner normals during
compile when smoothing groups are present.
- `**make_midpoint_tvert`** in the legacy tessellator dedupes against
existing TVerts; we don't, so a heavily tessellated water plane produces
more TVerts than the Prolog version. Visually identical, slightly
larger MDX block.
- **Chamfer patterns**: legacy `is_chamfer/3` enumerates 24 explicit
rotations (3 vertex permutations × 8 boundary cases). We collapse those
into two predicates (`isChamferAxisAligned`, `isChamferCornerAligned`)
iterated over 3 vertex permutations × 8 boundary signs. Coverage should
be identical; if you hit a chamfer that survives `--chamfer delete`,
please file an issue with the offending face.

## Known gaps

- The Prolog tool has a small "MDL ASCII repair" step that fixes
whitespace and parameter ordering on the way in. We rely on the parser
being permissive and the writer being canonical instead, so there's no
separate ASCII-cleaning pass.
- `--chamfer add` is conservative about face normal: we require the cross
product of `(V2-V1, V3-V1)` to point upward (`Z > 0.866`) and have
small horizontal component perpendicular to the boundary. Faces wound
the other way (CCW vs CW) won't qualify. The legacy code also requires
this, but it's worth knowing if you have a tile that uses non-standard
face winding.
- Multi-channel UV welding: we don't currently weld TVerts1/2/3. If you
use those channels and want them re-welded after a repair, file an
issue with a sample.

