---

name: cleanmodels
description: CLI output design for the NWN MDL validator, repair tool, and compiler
colors:
  ansi-red: '#cc0000'
  ansi-yellow: '#c4a000'
  ansi-green: '#4e9a06'
  ansi-dim: '#888888'
  ansi-default: '#d3d7cf'
  json-key: '#5fafff'
typography:
  body:
    fontFamily: 'ui-monospace, SF Mono, Menlo, Consolas, Liberation Mono, monospace'
    fontSize: '14px'
    fontWeight: 400
    lineHeight: '1.4'
  label:
    fontFamily: 'ui-monospace, SF Mono, Menlo, Consolas, Liberation Mono, monospace'
    fontSize: '14px'
    fontWeight: 700
spacing:
  indent: '2 spaces'
  separator: ', '
components:
  severity-error:
    textColor: '{colors.ansi-red}'
  severity-warning:
    textColor: '{colors.ansi-yellow}'
  severity-info:
    textColor: '{colors.ansi-dim}'
  severity-success:
    textColor: '{colors.ansi-green}'
  filename:
    textColor: '{colors.ansi-default}'
typography: '{typography.label}'

---

# Design System: cleanmodels

## 1. Overview

**Creative North Star: "The Compiler That Talks Back"**

cleanmodels' interface is its terminal output and its JSON-lines stream. Both are designed to feel like a well-engineered compiler: terse, predictable, color-coded by severity, and safe to pipe. The reference points are `rustc`, `clang`, and `eslint --quiet`. The anti-reference is anything chatty, ASCII-decorated, or fond of itself.

The CLI runs in three contexts: a developer's interactive terminal, a shell script or CI pipeline, and the Qt GUI's backend process. The same binary serves all three with no mode flag — TTY detection picks the right surface automatically. Color and progress are gifts to the human reader; pipes get plain ASCII; `--json` and NDJSON streams get a stable schema the GUI can rely on without scraping prose.

This system explicitly rejects ASCII-art banners, emoji decoration, chatty error messages, and progress bars on operations that finish in milliseconds. It rejects "enterprise" framing language and rejects hidden side effects: every change is announced.

**Key Characteristics:**

- Severity is the color axis. Red = error, yellow = warning, green = success/repair, dim = info.
- Filename is bold, severity tags are colored, body text is plain.
- One line per diagnostic. Indentation conveys nesting, not decoration.
- Output is identical across Linux, macOS, and Windows.
- Machine streams (`--json`, NDJSON) are first-class, stable, and complete.

## 2. Colors

A minimal ANSI palette that maps directly to severity. Concrete hex values are listed for documentation, but the CLI emits standard ANSI escape sequences so the user's terminal theme controls the actual rendering.

### Primary

- **ANSI Red** (`\033[31m`, ~`#cc0000`): Errors and fatal diagnostics. The only color that signals "you must read this." Used on severity tags, error counts, and the` ERROR:` prefix.
- **ANSI Yellow** (`\033[33m`, ~`#c4a000`): Warnings. Things the user should know about but that didn't stop the operation. Used on severity tags, warning counts, parse warnings, and decompile warnings.
- **ANSI Green** (`\033[32m`, ~`#4e9a06`): Success and repair. Used on` [REPAIR]`tags, the`[FIXED]`suffix, repair counts, the`clean`verdict, and the`ok` summary.
- **ANSI Dim** (`\033[2m`, ~`#888888`): Informational severity (`SevInfo`), the` [ACTION]`tag, and helper text. Hidden from default output; surfaced under`--verbose`.

### Neutral

- **Default Foreground** (terminal default): All body text — diagnostic messages, node names, file paths, JSON output. The user's terminal owns this color; the tool never sets it.
- **Bold** (`\033[1m`): Filenames at the head of a diagnostic block. Reserved for that role.

### Named Rules

**The Severity-Only Rule.** Color is reserved for severity. Don't tint information, decoration, or "branding" — the moment a non-severity element gets a color, the severity signal degrades. Filenames may be bold but are never colored.

**The TTY Rule.** Color and styling are emitted only when stdout is a TTY. When piped, redirected, or run with `--color=never` or `NO_COLOR`, output is plain ASCII. When run with `--color=always`, color is forced regardless. The detection is per-stream — stderr can be styled while stdout JSON stays plain.

## 3. Typography

**Body Font:** Whatever the user's terminal uses. cleanmodels does not ship typography; it produces ASCII text and trusts the terminal to render it. The schema field `body` describes the assumed monospace stack for documentation purposes only.

**Character:** Monospace, single-weight, single-size, no fallback fonts. Output renders identically in a 24x80 terminal, a tiling window manager pane, and a CI log viewer.

### Hierarchy

- **Filename** (bold, default color): The first token of every per-file output block. Bold is the only weight contrast in use.
- **Severity Tag** (`[REPAIR]`, `[WARN]`, `[ACTION]`, severity name in `[severity]`): Bracketed, colored, fixed-width-ish. Always at the start of a detail line.
- **Body** (default weight, default color): Diagnostic messages, node names, suggestions. No emphasis, no italics, no underlines.
- **Helper text** (`{severity-info.textColor}` / dim): Stats blocks under `--verbose`, batch summary footers.

### Named Rules

**The ASCII-Only Rule.** Output uses ASCII characters exclusively. No smart quotes, no em-dashes, no Unicode arrows or box-drawing characters in default output. `--help` may use Unicode in section dividers; everything else stays portable.

**The One-Line Rule.** Every diagnostic fits on one logical line. Multi-line context (e.g. a code excerpt) is reserved for parse errors that genuinely need it.

## 4. Elevation

Not applicable. The CLI is a flat surface — there are no layers, no hover states, and no z-axis. Hierarchy is conveyed by indentation, severity color, and bracketed tags only.

The closest analog is the in-place batch progress view (`batch.go`'s `livePainter`), which uses ANSI cursor-up and clear-line escapes to repaint a fixed window of recent lines. This is mechanical, not aesthetic — the rendering targets a pinned tail buffer, not a stacking model.

### Named Rules

**The Flat-Surface Rule.** Indentation is the only depth cue. Two-space indent under a filename header for diagnostic lines; no further nesting. If something needs three levels of indentation, it should be its own diagnostic instead.

## 5. Components

### Single-File Diagnostic Block

The basic unit of human-readable output. One filename header, zero or more detail lines.

```
<filename-bold>  <count-summary-colored>
  [<severity-tag-colored>] <check> (<node>): <message>[ <[FIXED]-green>]
  [REPAIR-green] <repair-message>
  [WARN-yellow] decompile [<node>] off=<offset>: <message>
  [ACTION-dim] <action-message>
```

The header summary lists fix/warning/error counts in that order, separated by `,` , each colored to match its severity.

### Batch Progress (Streaming)

When processing many files, each file completion emits one line in the form:

```
[<index>/<total>] <filename> ............................... <fix-count> repair[s]
```

When `--json` or `--ndjson` is set, the same per-file completion emits a JSON event instead (see schema below).

### Batch Progress (Live, TTY only)

`batch.go` paints a rolling window of the last N completed files using ANSI cursor-up + clear-line escapes. Disabled when stdout is piped, and below a four-file threshold — for one-, two-, or three-file batches the streaming line-per-file form is calmer and avoids cursor flashes for an operation that completes in <1s. The cursor is hidden during the live phase and restored on exit.

Batch lines are width-aware on TTY: when `termWriter.cols > 0`, the dot leader between the basename and the status shrinks so the assembled line fits within `cols-1` columns, preserving a minimum of three dots. When piped or width is unknown, the historical 50-char filename region is preserved verbatim.

### JSON-Lines Event Schema

The contract between CLI and GUI. Stable across releases.

```json
{"type": "batch_start", "total": 1234}
{"type": "file_start", "file": "foo.mdl", "index": 1, "total": 1234}
{"type": "file_done",  "file": "foo.mdl", "index": 1, "total": 1234, "fixes": 3,
 "result": { "file": "foo.mdl", "checks": [...], "repairs": [...], "actions": [...], "warnings": [...] }}
{"type": "batch_done", "message": "1234 files, 421 repaired, 12 errors"}
```

Field rules:

- `type` is always present and stable.
- `result` matches `Result` in `process.go` exactly. Only present on `file_done`.
- Empty arrays are omitted (`omitempty`); consumers must treat missing as empty.
- `error` on `Result` is set if the file failed to process; other arrays may still be populated.

### Exit Codes

- `0` — success, no errors
- `1` — at least one file produced a `SevError` or `SevFatal` check
- `2` — usage error: invalid argument, unknown flag, invalid `--color` value, or no `.mdl` files found at the input path. Printed to stderr; no JSON event.
- `64..78` — reserved for `sysexits.h` semantics if a future subcommand needs them

A scripted batch over a directory that resolves to zero files exits `2`, not `0`. This makes "wrong path" errors visible in CI; if a clean directory is expected, wrap the call accordingly.

### Stream Routing (stdout vs stderr)

Single rule, applied uniformly to single-file and batch invocations:

| Mode | stdout | stderr |
|---|---|---|
| Plain (default) | per-file detail + final summary | nothing (errors only on failure) |
| `--json` | JSON object or array | final human summary, single-line |
| `--json-lines` | NDJSON event stream (incl. summary event) | nothing |
| `--quiet` | nothing | nothing (errors only on failure) |

Errors, the empty-batch hint, and `cleanmodels: <message>` diagnostics always go to stderr regardless of mode. The stdout stream is reserved for output a downstream consumer would care about: human prose in plain mode, machine data in `--json` modes, nothing in `--quiet`.

This means a user running `cleanmodels check ./tiles > log.txt` captures the entire human report (per-file lines and summary) in `log.txt` in plain mode, and gets a machine-clean JSON file in `--json` mode with the human-readable summary still visible on the terminal.

### `--help` Surface

Subcommand-grouped help, one block per subcommand. Long-form flags use `--kebab-case`; short-form aliases are explicit (`-v`, `-q`, `-j`). Examples appear in a final `Examples:` section. The `--help` text is the only place where Unicode dividers and slightly verbose copy are permitted.

## 6. Do's and Don'ts

### Do

- **Do** color severity tags only. Filenames bold; everything else default.
- **Do** detect TTY per stream. Honour `NO_COLOR`, `--color=never`, and `--color=always`.
- **Do** emit one line per diagnostic. Indent two spaces under the filename header.
- **Do** keep the JSON event schema additive. New fields are okay; renaming or removing existing fields is a breaking change.
- **Do** announce every change in `[REPAIR]` / `[FIXED]` form. Silent fixes erode trust.
- **Do** preserve the original file when the operation fails. The user's source is sacred.

### Don't

- **Don't** print ASCII art, banners, splash logos, or emoji in normal output. `--help` may be slightly more decorative; nothing else.
- **Don't** apologise. "Oh no, something went wrong!" is not how a compiler talks. State what, where, what to do.
- **Don't** show progress bars on operations that finish in milliseconds. Stream per-file lines for batches; nothing for single files.
- **Don't** use chatty enterprise framing ("Initializing pipeline orchestration..."). The tool just runs.
- **Don't** colour anything that isn't severity. Tinting filenames, paths, or counts blue/cyan/magenta corrupts the severity signal.
- **Don't** emit smart quotes, em-dashes, or non-ASCII characters in machine output. JSON consumers will choke; locale-different terminals will render mojibake.
- **Don't** scrape the human-readable log to detect outcomes. The JSON event schema is the contract; if it doesn't expose what the GUI needs, extend it.
- **Don't** mix stdout and stderr. Stdout carries diagnostics and JSON; stderr carries usage errors and panics. A consumer should be able to redirect either independently.
- **Don't** depend on terminal width. Wrap nothing; let the terminal handle long lines. Wrapping breaks copy-paste and grepping.

