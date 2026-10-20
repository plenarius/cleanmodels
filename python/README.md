# cleanmodels for Python

A Python port of cleanmodels' **decompiler**, **ASCII parser** and **compiler**:
binary or ASCII `.mdl` in, byte for byte what `cleanmodels decompile` and
`cleanmodels compile` write out, warnings included. It exists so tools that
can't ship the Go binary, such as [Neverblender](https://github.com/plenarius/neverblender)
in Blender, read and write models exactly as cleanmodels does.

Standard library only; Python 3.10 or later (Blender 3.3 and up).

```python
from cleanmodels import compile, decompile_file, lookup_dirs, parse, write

path = "pmh0_neck001.mdl"
model = decompile_file(path, skeleton_dirs=lookup_dirs(path))  # skeletons: model folder, then the game
text = write(model)                                             # what `cleanmodels decompile` prints
for w in model.warnings:
    print(w.node, w.message)
for w in model.notes:                                           # Go's notes: info, not warnings
    print(w.node, w.message)

res = parse(open("c_wolf.mdl", "rb").read())                    # an ASCII model
for e in res.errors:                                            # Go's parse_warnings
    print(e)
dirs = lookup_dirs("c_wolf.mdl")                                # supermodels, skeletons, materials
data = compile(res.model, dirs, dirs, warn=print)               # what `cleanmodels compile` writes
```

or `python -m cleanmodels decompile|compile [--game-dir DIR] [--warnings] in.mdl [out.mdl]`.

## One library, two languages

The Go code in `pkg/mdl` is the source of truth; this package mirrors it file
for file:

| Python | Go |
|---|---|
| `binary.py` | `binary.go` (decompiler) |
| `writer.py` | `writer.go` (ASCII writer) |
| `parser.py` | `parser.go`, `declared.go` (ASCII parser) |
| `nearmatch.py` | `nearmatch.go` |
| `compiler.py` | `compiler.go`, `compiler_dropped.go` (compiler) |
| `compiler_node.py`, `compiler_mesh.py` | `compiler_node.go`, `compiler_mesh.go` |
| `compiler_normals.py`, `compiler_tangents.py` | `compiler_normals.go`, `compiler_tangents.go` |
| `compiler_controllers.py`, `compiler_tree.py`, `compiler_order.py`, `compiler_skinbind.py` | `compiler_controllers.go`, `compiler_tree.go`, `compiler_order.go`, `compiler_skinbind.go` |
| `supermodel.py`, `materials.py`, `compiler_skeleton.py` | `supermodel.go`, `materials.go`, `skeleton.go` and `twoda.go` (compile side) |
| `vec.py`, `compiler_buf.py` | `vec.go`, `transforms.go` (modelBounds); `patchBuf` |
| `types.py` | `types.go` |
| `controllers.py` | `controllers.go`, `declared.go` |
| `skeleton.py` | `skeleton.go`, `compiler_tree.go` (skin bones of body parts, decompile side) |
| `resources.py` | `resources.go`, `cmd/cleanmodels/gameinstall.go` |
| `f32.py` | `format.go` (float32 `%g`) |
| `gostr.py` | Go's `strings` / `strconv` / `unicode` behaviour: Unicode spaces, case mapping and folding (`EqualFold`), `%q`, `ParseFloat` / `ParseInt` |
| `gomath.py` | Go's `math.Sin` / `Cos`, `sort.SliceStable`, amd64 float-to-int conversion |

float32 arithmetic is rounded after every operation, as Go does on amd64.
`PORTED_FROM` in `__init__.py` names the release it matches.

## Parity

**A change to the decompiler, writer, parser or compiler needs the same change
here.** CI runs three tests, each against the Go build of the same commit, so a
Go change that isn't ported fails CI in the same pull request:

- `python/parity_test.py`, the decompiler: float formatting on 300,000+ float32
  values; every binary model in `tests/fixtures`, and 40 corruptions of each
  (truncation, bit flips, junk pointers and counts, NaN floats).
- `python/parity_parse.py`, the parser (`parse` then `write`, against
  `python/internal/parsefmt`, which does what `cleanmodels decompile` does with
  ASCII): number and string parsing on 160,000+ adversarial inputs, and
  case-insensitive name matching (`EqualFold`) on 1.1 million string pairs; every
  ASCII model in `tests/fixtures`, and 40 textual corruptions of each (dropped
  and duplicated lines, junk tokens, wrong counts, NaN/1.#INF/huge numbers,
  missing endlist, unknown keywords, odd whitespace and bytes).
- `python/parity_compile.py`, the compiler (a model decompiled or parsed, then
  compiled, against `python/internal/compileref`, the library calls
  `cleanmodels compile` makes): every model in `tests/fixtures`, and 20
  mutations of each (numbers nudged, lines dropped or duplicated, names
  repeated, bytes corrupted). Compiled bytes and compile warnings.

Each requires identical output and warnings, or both refusing the file.
`python/internal/foldorbits` prints the case-folding table in `gostr.py`.

Before a release, also run them on the game's models:

```sh
nwn_resman_extract -p .mdl -d /tmp/stock     # binary and ASCII models
python python/parity_test.py --corpus /tmp/stock
python python/parity_parse.py --corpus /tmp/stock --mutate-corpus 20
python python/parity_compile.py --corpus /tmp/stock --cli ./cleanmodels   # --cli: also the CLI's own output
```

At v4.10.0, on the game's 32,832 models (25,597 binary, 7,235 ASCII), against
the Go build of the release commit:
- decompile: every binary model identical;
- parse (output and parse warnings): every ASCII model identical, and 20
  mutations of every 50th one identical or refused by both;
- compile (bytes and warnings): every model identical, the binary ones
  decompiled first and the ASCII ones parsed, and the same as
  `cleanmodels compile` itself (`--cli`).

(v4.8.0 was also checked on the 32,832 ASCII files `cleanmodels decompile`
writes for these models.)

## Not ported

Repairs, checks and variants.
