# Product

## Register

product

## Users

NWN community modders who build, fix, and maintain 3D model files (`.mdl`) for Neverwinter Nights: Enhanced Edition. The audience spans experienced 3D artists comfortable in NWMax/Blender, module builders with limited technical background, and HAK packagers who run the tool over hundreds of files at a time. The shared context is a content pipeline: 3D editor → ASCII MDL → cleanmodels (validate, repair, compile) → game-loadable binary.

The CLI is invoked three ways: directly at a terminal, from shell scripts and CI pipelines, and as the backend of the Qt GUI (`cleanmodels-qt`). All three paths must work without surprises.

## Product Purpose

The quality gate for NWN MDL files. cleanmodels validates ASCII model sources against the format the game compiler expects, repairs known issues (pivots, tilefade, tangents, dangly constraints, walkmesh materials, water, chamfers), and compiles to game-binary MDLs that are bit-faithful to what the game itself would produce.

It exists because the legacy Prolog tool was slow, opaque, and occasionally destructive, and because users running large HAK pipelines need to bulk-compile thousands of models confidently and ship the binaries with HAKs so the game doesn't compile on first load. Success looks like: HAK builders trust the binaries, individual modders use it as a save-step linter, and CI pipelines treat its exit codes as authoritative.

## Brand Personality

**Fast, precise, flexible.** A sharp instrument: quick to produce correct results, trustworthy in its output, and accommodating of the wide variety of models found in the wild (old, malformed, hand-edited, tool-exported, decompiled).

Emotional goal: confident and informed. The user trusts the tool and understands exactly what it changed. Output reads like a well-written compiler error, not a chatbot.

## Anti-references

- ASCII art banners, splash logos, emoji in normal output. Reserved for `--help` at most, never used in machine-consumable streams.
- Chatty or apologetic language ("Oh no, something went wrong!"). Errors state what, where, and what to do.
- Progress bars on operations that finish in milliseconds. Streaming per-file lines for batches; nothing for single files.
- Enterprise-verbose framing ("Initializing pipeline orchestration..."). The tool just does the thing.
- Hidden side effects, silent reformatting, or destructive defaults. Every change is announced in the log; `--dry-run` is real.
- Flat 60-flag CLI surfaces. Subcommands carry the cognitive load (`check`, `compile`, `decompile`, `clean`).

## Design Principles

1. **Speed is a feature.** The legacy Prolog implementation was the friction. Every operation should feel instant on single models and saturate I/O on batches. Slow paths are bugs.
2. **Correctness over convenience.** Binary output must match the game's compiler when given the same ASCII source. Checks should catch real issues, not generate noise. When in doubt, preserve the original data and explain.
3. **Approachable complexity.** Deep options exist (pivot repair, tilefade slicing, walkmesh remap, dangly constraint smoothing) but the default path produces a good result without configuration. Subcommands group complexity so the common path stays simple.
4. **Explain what changed.** Every fix, warning, and repair produces a human-readable message. The JSON-lines protocol is the contract between CLI and GUI. The debug log and per-file fix counts are the user's audit trail.
5. **Stay focused.** NWN MDL files only. No scope creep into other formats, game engines, or unrelated tooling.

## Accessibility & Inclusion

- **TTY-aware output.** ANSI color and styling auto-detected when stdout is a terminal; disabled when piped or redirected. `NO_COLOR` env honored. `--color=always|never|auto` overrides.
- **Machine-readable mode.** `--json` for single structured results, NDJSON streaming for batches. The schema is stable and complete enough that the GUI never needs to scrape the human-readable log.
- **Plain-language errors.** No jargon-only failures. Every error includes file, location (offset / line / node name when applicable), expected vs found, and a one-line suggestion.
- **Cross-platform parity.** Linux, macOS, Windows. Path handling is correct on all three; line endings respected; output identical wherever the user invokes it.
- **Locale-safe.** ASCII-only diagnostic strings; no smart quotes or em-dashes in machine output. Numbers and floats use `.` as decimal separator regardless of locale.
