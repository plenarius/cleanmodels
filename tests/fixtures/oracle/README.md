# Oracle Test Fixtures

These files let you validate the cleanmodels binary compiler against NWN:EE's own internal model compiler — the gold standard.

## Model coverage

| File | Node types | Animations | Notes |
|---|---|---|---|
| `plc_dummy_only.mdl` | dummy | 0 | Minimal model — no geometry |
| `abp_weaprack_1.mdl` | dummy, trimesh | 0 | Large static mesh (48 nodes) |
| `plc_dd27.mdl` | dummy, light, trimesh | 1 | Light node |
| `plc_crysblu.mdl` | dummy, emitter, light, reference, trimesh | 8 | Reference node |
| `zlc_o23.mdl` | dummy, emitter, trimesh | 10 | Emitter with animation |
| `plc_nc03.mdl` | dummy, emitter, light, trimesh | 10 | Light + emitter |
| `plc_guillo2.mdl` | danglymesh, dummy, emitter, trimesh | 8 | Dangly + emitter |
| `plc_statdwl.mdl` | dummy, trimesh | 67 | Many animations |
| `squid.mdl` | danglymesh, dummy, trimesh | 21 | Dangly mesh, many animations |
| `a_dfa2_coat.mdl` | danglymesh, dummy, skin, trimesh | 43 | **Skin node** — complex |
| `tcei0_a01_01.mdl` | aabb, dummy, light, trimesh | 0 | **AABB node** — tileset tile |

## Workflow

### 1 — Get the game to compile each model

1. Copy **all files from `ascii/`** to your NWN:EE development folder:
   ```
   Documents\Neverwinter Nights\development\
   ```

2. Launch NWN:EE and load any area (or create a test module).

3. For **placeables** — open the Area Properties dialog, add the placeable to the area, and save/run the module so the placeable is loaded in memory.

4. For **tilesets** (`tcei0_a01_01.mdl`) — use an area that uses that tileset.

5. For **creature parts** (`a_dfa2_coat.mdl`) — place a creature that uses that super-model.

6. Open the **debug console** ( `~` key) and run:
   ```
   compileloadedmodels
   ```
   The game compiles every currently-loaded MDL to binary and writes the output to:
   ```
   Documents\Neverwinter Nights\compiled_models\
   ```

7. Alternatively, use the **Toolset Model Compiler** (Tools → Compile Model) on each file.

### 2 — Transfer binary files to this repo

Copy the game-compiled `.mdl` binary files into:
```
tests/fixtures/oracle/game_binary/
```
Filenames must match the source ASCII files (case-insensitive). The test will match them automatically.

### 3 — Run the comparison

```bash
go test ./pkg/mdl/ -run TestOracleCompare -v
```

The test:
- Parses each ASCII file
- Compiles it with **our** compiler
- Decompiles both our binary and the game binary
- Compares: model name, supermodel, classification, node count, node hierarchy, mesh vertex/face counts, animation names

Expected divergences (tolerated automatically):
- **Tangent / bitangent data** — the game generates these; we currently don't (IDs 132, 133 skipped)
- **AABB tree internal split order** — median-split may differ from game's algorithm
- **Vertex count** — game may merge identical verts; we expand to faces×3

Any other difference is reported as a test failure with both values shown.

### 4 — Smoke test (no game binary needed)

To confirm all ASCII oracle fixtures compile and decompile without error:

```bash
go test ./pkg/mdl/ -run TestOracleCompileAll -v
```
