# cleanmodels

Go library and CLI for parsing, validating, repairing, and compiling NWN:EE MDL model files. Paired with [cleanmodels-qt](https://github.com/plenarius/cleanmodels-qt) as its desktop GUI.

## Design Context

### Users
NWN community modders who build, fix, and maintain 3D model files (.mdl) for Neverwinter Nights: Enhanced Edition. These users range from experienced 3D artists to module builders with limited technical background. They work in a pipeline that includes 3D editors (NWMax/Blender), the game's toolset, and cleanmodels as the quality gate before assets ship.

### Brand Personality
**Fast, precise, flexible.** The tool should feel like a sharp instrument — quick to produce correct results, trustworthy in its output, and accommodating of the wide variety of models found in the NWN community (old, malformed, hand-edited, tool-exported). It never corrupts data and always explains what it changed.

### Aesthetic Direction
- **Go library API**: Follow Go stdlib conventions. Simple public API surface. Zero external dependencies is a hard requirement.
- **Anti-references**: Not a framework, not a plugin system, not a kitchen sink. NWN MDL only.

### CLI UX Direction

**Audience**: All paths — modders running one-offs from a terminal, shell scripts automating batch jobs, CI pipelines validating models, and as the Qt GUI backend via JSON-lines.

**Emotional goal**: Confident + informed. The user trusts the tool completely and understands exactly what it did and why.

**Output personality**: Speaks when it has something to say. Never chatty, never cute. Color-coded severity when outputting to a TTY (auto-detect); plain text when piped. Errors include context (file, offset, what was expected vs found) and suggestions when actionable ("this looks like a binary MDL — did you mean --decompile?").

**Batch output**: Per-file streaming progress — the user watches it work through the list. Summary at the end.

**Help structure**: Subcommands (`cleanmodels check`, `cleanmodels compile`, `cleanmodels repair`) instead of 60+ flat flags. Each subcommand has its own focused flag set. Common flags (--json, --json-lines, --quiet, --verbose, --workers) shared across subcommands.

**Anti-references**: No ASCII art banners, no emoji, no "oopsie" language, no walls of text, no progress bars for sub-second operations. Not enterprise-verbose either — no XML, no Java stack traces, no config file ceremony.

**Machine output**: JSON-lines (NDJSON) is the contract with the Qt GUI. `--json` for single structured results. Both must be stable, parseable, and complete (every fix, warning, check result included).

### Design Principles
1. **Speed is a feature** — The legacy Prolog tool was too slow. Every operation should feel instant on single models, and batch processing should saturate I/O, not CPU.
2. **Correctness over convenience** — Binary output must match the game's own compiler. Checks should catch real issues, not generate noise. When in doubt, preserve the original data.
3. **Approachable complexity** — Deep options exist (pivot repair, tilefade slicing, walkmesh material remapping) but the default "all fixes" path should produce a good result without configuration. Subcommands group complexity so the common path is simple.
4. **Explain what changed** — Every fix, warning, and repair should produce a human-readable message. The JSON-lines protocol is the contract between CLI and GUI.
5. **Stay focused** — NWN MDL files only. No scope creep into other formats, game engines, or unrelated tooling.
