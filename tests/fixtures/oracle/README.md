# Oracle Test Fixtures

These files let you validate the cleanmodels binary compiler against NWN:EE's own internal model compiler — the gold standard.

## Test layout

| Directory | Pairs with | Used by |
|---|---|---|
| `ascii/` | `game_binary/` (1:1 by filename) | `TestOracleCompare`, `TestOracleVertexParity`, `TestOracleCompileAll` |
| `game_binary/` | `ascii/` (1:1 by filename) | `TestOracleCompare`, `TestOracleVertexParity` |
| `tangent_pairs/ascii/` | `tangent_pairs/game_binary/` (1:1) | `TestOracleTangents` (baked-tangent meshes only) |
| `tangent_pairs/game_binary/` | `tangent_pairs/ascii/` (1:1) | `TestOracleTangents` |

Tangent fixtures live in their own directory because the ASCII source and the game binary may come from different revisions of the same model — node names and face counts differ enough to break `TestOracleCompare`'s structural diff, but per-vertex tangent comparison only needs matching world positions, so it survives the drift.

## Model coverage

### General oracle (`ascii/` ↔ `game_binary/`)

| File | Coverage |
|---|---|
| `plc_dummy_only.mdl` | Minimal model — dummy nodes only, no geometry (compile-only) |
| `abp_weaprack_1.mdl` | Large static mesh (48 nodes), no animations |
| `plc_dd27.mdl` | Light + trimesh, single animation |
| `plc_crysblu.mdl` | Reference node + emitter + 8 anims |
| `zlc_o23.mdl` | Emitter + 10 animations |
| `plc_nc03.mdl` | Light + emitter + 10 anims |
| `plc_guillo2.mdl` | Danglymesh + emitter |
| `plc_statdwl.mdl` | Many animations (67) |
| `squid.mdl` | Danglymesh + 21 anims (compile-only — no game binary) |
| `a_dfa2_coat.mdl` | Skin node — complex (compile-only) |
| `tcei0_a01_01.mdl` | AABB node — tileset tile (compile-only) |
| `tin01_d11_01.mdl` | Hak-sourced tile — perfect vert parity baseline |
| `pmh0_head001.mdl` | Character head — body-part dedup-philosophy baseline |
| `pmh0_neck001.mdl` | Character neck — body-part skin (compile-only — ASCII drift vs binary) |
| `Red_M_Torso.mdl` | Character torso — body-part dedup-philosophy baseline |
| `INV_EQP_HELMET.mdl` | Inventory icon — perfect-parity simple case |
| `BUT_CHAT_BAR360.mdl` | UI button — perfect-parity simple case |
| `ctl_compass.mdl` | UI compass — divergent baseline (regression sentinel) |
| `c_marilithe.mdl` | **Danglymesh + 60 anims** creature — perfect parity (zero-baseline) |
| `c_marilith2.mdl` | **Skin + danglymesh + emitter** mega-fixture — documented baseline |
| `c_elemashm.mdl` | **Emitter** ash-elemental — perfect parity |
| `c_nightmare.mdl` | Emitter + 27 anims (compile-only — ASCII says `Tile` but binary classifies `Character`) |
| `PLC_D01.mdl` | **Animated emitter placeable** — perfect parity |
| `tdc01_a01_03.mdl` | **AABB walkmesh tile** — small documented baseline |
| `tdc01_g02_03.mdl` | AABB tile (compile-only — ASCII has 3 degenerate faces game prunes) |
| `wsf10_p01_01.mdl` | **AABB walkmesh tile** (Layonara hak) — `wok` walkmesh baseline |

### Tangent oracle (`tangent_pairs/`)

18 fixtures with baked tangents — the only place that exercises tangent generation end-to-end against an oracle.

| Group | Files | Mean alignment |
|---|---|---|
| `tin01_*` (12) | Hak-sourced tin01 tiles | 0.9974 – 1.0000 |
| `tdc01_g02_01`, `tdc01_g02_03` | Core game tdc01 tiles | **1.0000** (perfect) |
| `wsf10_p01_01`, `wsf10_s01_02`, `wsf10_s11_04` | Layonara hak wsf10 tiles | **1.0000** (perfect) |
| `TTR01_G02_02` | Core ttr01 tile | **1.0000** (perfect) |

## Skip list — files deliberately excluded

These filenames showed up in dump folders but are **not** in the fixture set because the comparison would be invalid. If you recover a matching ASCII for any of them, drop the file in and remove from the list.

### No ASCII source exists (hak ships pre-compiled binary only)

| File | Reason |
|---|---|
| `tin01_a03_01.mdl`, `tin01_b14_01.mdl`, `tin01_i01_01.mdl`, `tin01_z11_01.mdl`, `TIN01_Z10_01.mdl`, `tin01_f01_09.mdl` | Hak-only binary tiles |
| `a_ba.mdl`, `a_ba_casts.mdl`, `a_ba_custom.mdl`, `a_ba_med_weap.mdl`, `a_ba_non_combat.mdl` | Hak-only binary appearance models |
| `fx_flame01.mdl`, `gi_armor01.mdl` | Hak-only binary |

### ASCII drifts from binary (different revision shipped to game)

| File | Drift |
|---|---|
| `c_nightmare.mdl` | ASCII `classification Tile`, game binary `Character` |
| `pmh0_neck001.mdl` | ASCII has 16 faces; binary has 114 (ASCII is sparse subset) |
| `tdc01_g02_03.mdl` | ASCII has 19 faces on `object01`; game pruned 3 degenerates → 16 |

These three keep their ASCII for `TestOracleCompileAll` coverage but are **not** paired with a `game_binary/` so `TestOracleCompare` and `TestOracleVertexParity` skip them.

### Useless for tangent oracle

| File | Reason |
|---|---|
| `ashto_05[1-4].mdl` | Game binaries lack baked tangents |

## Workflow — adding a new oracle pair

### 1 — Get the game to compile a model

1. Copy your ASCII source into NWN:EE's development folder:
   ```
   Documents\Neverwinter Nights\development\
   ```

2. Launch NWN:EE and load any area (or create a test module).

3. Place an asset that loads the model:
   - **Placeables** — drop the placeable into the area
   - **Tilesets** — load an area using that tileset
   - **Creature parts** — place a creature that uses that super-model

4. Open the **debug console** (`~` key) and run:
   ```
   compileloadedmodels
   ```
   The game writes binaries to:
   ```
   Documents\Neverwinter Nights\compiled_models\
   ```

5. Alternatively, use the **Toolset Model Compiler** (Tools → Compile Model) on each file.

### 2 — Transfer files into this repo

- ASCII source → `tests/fixtures/oracle/ascii/`
- Game binary → `tests/fixtures/oracle/game_binary/`

Filenames must match (case-insensitive).

### 3 — Run the comparison

```bash
go test ./pkg/mdl/ -run 'TestOracleCompare|TestOracleVertexParity|TestOracleTangents' -v
```

What each test enforces:

- **`TestOracleCompare`** — structural diff (model name, supermodel, classification, node hierarchy, animation names, mesh face counts). Compares face counts, which now match the engine (degenerate faces are removed as the engine does).
- **`TestOracleVertexParity`** — per-mesh-node GPU vertex count. Defaults to **strict equality** with the game; documented divergences live in `vertParityBaseline` in `oracle_test.go`. Adding a new fixture forces parity unless you explicitly record an expected delta.
- **`TestOracleTangents`** — per-corner |dot(ourTangent, gameTangent)| ≥ alignment floor (currently 0.99 mean, ≤1% bad-corner ratio; the core-game fixtures are at 1.0000). Activates only on baked-tangent fixtures.
- **`TestOracleFaceAdjacency`** and **`TestCompiledFaceAdjacencyMatchesGame`** — the face adjacency numbers we write equal the engine's byte for byte, on the fixtures whose binary the EE compiler built from exactly this ASCII.

### 4 — Smoke test (no game binary needed)

```bash
go test ./pkg/mdl/ -run TestOracleCompileAll -v
```

## Known divergences (not bugs)

`COMPILE.md` has the full list and the reasoning. In short, they don't change render output:

- **Legacy-tool binaries.** `Red_M_Torso`, `pmh0_head001` and `ctl_compass` are shipped base-game assets whose binaries contain exact duplicate vertices (identical position, UV, normal and color) that the EE compiler's own merge cannot leave behind, so they were not built by the EE compile path. In every case the vertex-count difference equals the number of exact-duplicate vertices in the game binary (`torso_g` 62, `pmh0_head001g` 119, `ctl_compass` `box01` 104 and `cmp_pointer` 4). Locked in via `vertParityBaseline` in `oracle_test.go`; everything else matches the engine exactly.
- **Legacy adjacency.** In the same kind of binary the stored face adjacency can hold numbers at or above the face count, because the legacy tool computed it before faces were removed.
- **Drifted fixtures.** A few `tin01_*` hak tiles pair an ASCII with a different revision of the binary, so tangents on some corners differ in sign or handedness (0.65% of corners across the tangent pairs).

The engine keeps a walkmesh's AABB tree exactly as authored, so there is no split-order difference to document. Degenerate faces are removed as the engine removes them (`tdc01_g02_03` `object01` goes from 19 faces to 16).
