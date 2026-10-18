# cleanmodels for Python

A Python port of cleanmodels' **decompiler**: binary `.mdl` in, the same ASCII
`cleanmodels decompile` writes out, byte for byte. It exists so tools that can't
ship the Go binary, such as [Neverblender](https://github.com/plenarius/neverblender)
in Blender, read binary models exactly as cleanmodels does.

Standard library only; Python 3.10 or later (Blender 3.3 and up).

```python
from cleanmodels import decompile_file, lookup_dirs, write

path = "pmh0_neck001.mdl"
model = decompile_file(path, skeleton_dirs=lookup_dirs(path))  # skeletons: model folder, then the game
text = write(model)                                             # what `cleanmodels decompile` prints
for w in model.warnings:
    print(w.node, w.message)
```

or `python -m cleanmodels decompile [--game-dir DIR] in.mdl [out.mdl]`.

## One decompiler, two languages

The Go code in `pkg/mdl` is the source of truth; this package mirrors it file
for file:

| Python | Go |
|---|---|
| `binary.py` | `binary.go` (decompiler) |
| `writer.py` | `writer.go` (ASCII writer) |
| `types.py` | `types.go` |
| `controllers.py` | `controllers.go`, `declared.go` |
| `f32.py` | `format.go` (float32 `%g`) |
| `skeleton.py` | `skeleton.go`, `compiler_tree.go` (skin bones of body parts) |
| `resources.py` | `resources.go`, `cmd/cleanmodels/gameinstall.go` |

`PORTED_FROM` in `__init__.py` names the release it matches.

**A change to the decompiler or the writer needs the same change here.** CI runs
`python/parity_test.py`, which compares this package with the Go build of the
same commit:

- float formatting on 300,000+ float32 values;
- every binary model in `tests/fixtures`, identical output;
- 40 deterministic corruptions of each (truncation, bit flips, junk pointers and
  counts, NaN floats): identical output, or both refuse the file.

So a Go change that isn't ported fails CI in the same pull request.

Before a release, also run it on the game's models:

```sh
nwn_resman_extract -p .mdl -d /tmp/stock     # or any folder of binary models
python python/parity_test.py --corpus /tmp/stock
```

At v4.8.0: all 25,597 binary models of the game decompile identically.

## Not ported

The ASCII parser, repairs, checks, the compiler and variants. ASCII input to
`python -m cleanmodels decompile` is copied through rather than re-serialised.
