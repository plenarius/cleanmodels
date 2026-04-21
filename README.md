# cleanmodels

Validate, repair, compile, and decompile Neverwinter Nights MDL model files.

A complete rewrite of [OldManBeard's CleanModels](https://neverwintervault.org/project/nwn1/other/tool/cleanmodels-0) in Go — zero dependencies, single static binary, cross-platform.

## Download

Grab the latest binary for your platform from the [Releases](https://github.com/plenarius/cleanmodels/releases) page.

| Platform | File |
|---|---|
| Linux (x86_64) | `cleanmodels-linux-amd64.zip` |
| Linux (ARM64) | `cleanmodels-linux-arm64.zip` |
| macOS (Intel) | `cleanmodels-darwin-amd64.zip` |
| macOS (Apple Silicon) | `cleanmodels-darwin-arm64.zip` |
| Windows (x86_64) | `cleanmodels-windows-amd64.zip` |

> **Looking for the legacy Prolog version?** The last Prolog-based release is
> [build3.7.0-HEAD](https://github.com/plenarius/cleanmodels/releases/tag/build3.7.0-HEAD).
> The GUI companion is at [cleanmodels-qt](https://github.com/plenarius/cleanmodels-qt).

## Building from source

Requires [Go 1.22+](https://go.dev/dl/). No external dependencies.

```
git clone https://github.com/plenarius/cleanmodels
cd cleanmodels
go build -o cleanmodels ./cmd/cleanmodels
```

## Usage

cleanmodels uses subcommands. Run `cleanmodels <command> --help` for full flag details.

### check — validate models

```
cleanmodels check models/
cleanmodels check --recursive --json haks/
cleanmodels check --fix --dry-run plc_torch.mdl
cleanmodels check --exclude-checks emitter_spread,missing_bitmap models/
```

| Flag | Description |
|---|---|
| `--fix` | Auto-fix safe issues (duplicate names, invalid parents) |
| `--dry-run` | Report what would be fixed without writing |
| `--include-checks` | Comma-separated check names to run exclusively |
| `--exclude-checks` | Comma-separated check names to skip |

### repair — fix and transform models

```
cleanmodels repair --all plc_torch.mdl repaired/
cleanmodels repair --fix-pivots --fix-aabb walkables/
cleanmodels repair --scale 2.0 plc_chair.mdl
cleanmodels repair --fix-tilefade --tilefade-z 5.0 tiles/
```

| Flag | Description |
|---|---|
| `--all` | Enable all repairs |
| `--fix-pivots` | Repair walkmesh pivot points |
| `--fix-aabb` | Rebuild AABB trees from walkmesh geometry |
| `--fix-tilefade` | Slice tile geometry for tilefade |
| `--tilefade-z` | Z height for tilefade slicing (default: 5.0) |
| `--strip-degenerate` | Remove zero-area faces |
| `--fix-animations` | Clamp negative/too-short animation lengths |
| `--reparent-children` | Reparent children of AABB/light nodes |
| `--wrap-root` | Wrap non-dummy root nodes in a dummy parent |
| `--split-multiedge` | Split faces sharing multiple edges |
| `--check` | Run validation checks after repair |
| `--scale` | Scale all vertex positions |
| `--scale-x/y/z` | Scale individual axes |
| `--classification` | Override classification (CHARACTER, DOOR, EFFECT, ITEM, TILE) |
| `--snap` | Vertex snapping mode: `binary`, `decimal`, `fine` |
| `--render` | Force render flag: `all`, `none` |
| `--shadow` | Force shadow flag: `all`, `none` |
| `--force-white` | Set ambient/diffuse to 1,1,1 |
| `--merge-by-bitmap` | Merge sibling trimeshes sharing a bitmap |
| `--cull-invisible` | Convert invisible meshes to dummy nodes |

### compile — ASCII to binary MDL

```
cleanmodels compile plc_torch.mdl plc_torch.mdl
cleanmodels compile --recursive models/ compiled/
```

### decompile — binary to ASCII MDL

```
cleanmodels decompile plc_torch.mdl plc_torch.mdl
cleanmodels decompile --force mystery_file.bin output.mdl
```

| Flag | Description |
|---|---|
| `--force` | Treat input as binary even if auto-detection fails |

### Common flags

These flags work with all commands:

| Flag | Description |
|---|---|
| `--json` | Output results as JSON |
| `--json-lines` | Stream NDJSON events (for GUI/tool integration) |
| `--verbose` | Show all warnings and info |
| `--quiet` | Suppress all output except errors |
| `--workers N` | Parallel workers for batch mode (default: CPU count) |
| `--recursive` | Process directories recursively |

### Legacy mode

For backwards compatibility with [cleanmodels-qt](https://github.com/plenarius/cleanmodels-qt) and existing scripts, the old flat-flag interface still works:

```
cleanmodels --check --fix-pivots input/ output/
cleanmodels --decompile-only model.mdl output.mdl
```

## What it does

cleanmodels understands the full MDL format for Neverwinter Nights: Enhanced Edition, including all nine geometry node types (dummy, trimesh, danglymesh, skin, animmesh, emitter, light, reference, aabb) and their complete parameter sets.

**Validation** — 100+ checks across structural integrity, geometry correctness, parameter bounds, animation consistency, emitter configuration, and tile-specific rules.

**Repair** — Walkmesh pivot point reconstruction, AABB tree rebuilding, tilefade slice computation, degenerate face removal, animation length clamping, and more.

**Compile/Decompile** — Convert between ASCII and binary MDL formats. The binary compiler has been validated against the NWN:EE game compiler across 32,707 stock models and 49,111 community models.

## Specifications

The MDL parser and compiler trace to these references:

- [nwn.wiki MDL Format](https://nwn.wiki/display/NWN1/MDL) — parameter-level documentation
- [xoreos-docs](https://github.com/xoreos/xoreos-docs) — Torlack's binary MDL specification and 010 Editor templates
- [neverwinter.nim](https://github.com/niv/neverwinter.nim) — Nim MDL implementation
- [nwnmdlcomp](https://github.com/nwneetools/nwnmdlcomp) — C++ MDL compiler reference

## License

MIT
