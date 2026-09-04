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

Most flags have short aliases for interactive use (e.g. `-r` for `--recursive`, `-v` for `--verbose`). The tables below show both forms.

### Quick start

```bash
# Check a single model
cleanmodels check plc_torch.mdl

# Check an entire directory recursively
cleanmodels check -r models/

# Repair all common issues in a tileset, output to a separate folder
cleanmodels repair -a -r tiles/ repaired/

# Decompile a binary MDL to ASCII
cleanmodels decompile plc_torch.mdl plc_torch.mdl
```

### Batch processing

Point any subcommand at a directory to process all `.mdl` files inside it. No shell loops or scripts needed.

```bash
# All MDLs in a flat directory
cleanmodels repair -a models/

# Recursive (includes subdirectories)
cleanmodels repair -a -r haks/

# Output to a separate folder (originals untouched)
cleanmodels repair -a -r haks/ cleaned/

# Control parallelism (defaults to CPU count)
cleanmodels repair -a -r -w 4 haks/
```

A live progress bar is shown automatically when output is a terminal.

### Output and logging

Warnings and progress are written to **stderr**. Structured results (JSON) go to **stdout**. This matters when redirecting output:

```bash
# Save warnings/progress to a file
cleanmodels check -r -v models/ 2> log.txt

# Save everything (stdout + stderr)
cleanmodels check -r -v models/ &> log.txt

# See output on screen AND save to file
cleanmodels check -r -v models/ 2>&1 | tee log.txt

# Machine-readable JSON output
cleanmodels check -r -j models/ > results.json
```

### check — validate models

```bash
cleanmodels check models/
cleanmodels check -r -j haks/
cleanmodels check -f -n plc_torch.mdl
cleanmodels check --exclude-checks emitter_spread,missing_bitmap models/
```

| Flag | Short | Description |
|---|---|---|
| `--fix` | `-f` | Auto-fix safe issues (duplicate names, invalid parents) |
| `--dry-run` | `-n` | Report what would be fixed without writing output |
| `--include-checks` | | Comma-separated check names to run exclusively |
| `--exclude-checks` | | Comma-separated check names to skip |

### repair — fix and transform models

```bash
cleanmodels repair -a plc_torch.mdl repaired/
cleanmodels repair --fix-pivots --fix-aabb walkables/
cleanmodels repair --scale 2.0 plc_chair.mdl
cleanmodels repair --fix-tilefade --tilefade-z 5.0 tiles/
```

| Flag | Short | Description |
|---|---|---|
| `--all` | `-a` | Enable all structural repairs |
| `--dry-run` | `-n` | Report what would be fixed without writing output |
| `--fix-pivots` | | Repair walkmesh pivot points |
| `--fix-aabb` | | Rebuild AABB trees from walkmesh geometry |
| `--fix-tilefade` | | Slice tile geometry for tilefade |
| `--tilefade-z` | | Z height for tilefade slicing (default: 5.0) |
| `--strip-degenerate` | | Remove zero-area faces |
| `--fix-animations` | | Clamp negative/too-short animation lengths |
| `--reparent-children` | | Reparent children of AABB/light nodes |
| `--wrap-root` | | Wrap non-dummy root nodes in a dummy parent |
| `--split-multiedge` | | Split faces sharing multiple edges |
| `--check` | | Run validation checks after repair |
| `--scale` | | Scale all vertex positions |
| `--scale-x/y/z` | | Scale individual axes |
| `--classification` | | Override classification (CHARACTER, DOOR, EFFECT, ITEM, TILE) |
| `--snap` | | Vertex snapping mode: `binary`, `decimal`, `fine` |
| `--render` | | Force render flag: `all`, `none` |
| `--shadow` | | Force shadow flag: `all`, `none` |
| `--force-white` | | Set ambient/diffuse to 1,1,1 |
| `--merge-by-bitmap` | | Merge sibling trimeshes sharing a bitmap |
| `--cull-invisible` | | Convert invisible meshes to dummy nodes |

### compile — ASCII to binary MDL

```bash
cleanmodels compile plc_torch.mdl plc_torch.mdl
cleanmodels compile -r models/ compiled/
```

### decompile — binary to ASCII MDL

```bash
cleanmodels decompile plc_torch.mdl plc_torch.mdl
cleanmodels decompile -f mystery_file.bin output.mdl
```

| Flag | Short | Description |
|---|---|---|
| `--force` | `-f` | Treat input as binary even if auto-detection fails |

### Common flags

These flags work with all subcommands:

| Flag | Short | Description |
|---|---|---|
| `--json` | `-j` | Output results as JSON to stdout |
| `--json-lines` | | Stream NDJSON events (for GUI/tool integration) |
| `--verbose` | `-v` | Show all warnings and info |
| `--quiet` | `-q` | Suppress all output except errors |
| `--workers N` | `-w N` | Parallel workers for batch mode (default: CPU count) |
| `--recursive` | `-r` | Process directories recursively |

### report — submit a bug report

If you encounter a model that crashes or produces incorrect output, you can submit a report directly from the CLI. Reports are sent to a relay that creates a GitHub issue — no GitHub account required.

```bash
# Report a problematic model
cleanmodels report model.mdl

# Include details about what went wrong
cleanmodels report --error "missing faces after decompile" model.mdl

# Report multiple files with context
cleanmodels report --command "repair --fix-pivots" --notes "visual artifacts" tile1.mdl tile2.mdl
```

| Flag | Description |
|---|---|
| `--command` | The cleanmodels command that was run |
| `--error` | Error output or description of the problem (optional) |
| `--notes` | Additional notes or context |

### Legacy mode

For backwards compatibility with [cleanmodels-qt](https://github.com/plenarius/cleanmodels-qt) and existing scripts, the old flat-flag interface still works:

```bash
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
