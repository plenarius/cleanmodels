"""python -m cleanmodels decompile|compile [--game-dir DIR] [--supermodel-dir DIR]... [--resource-dir DIR]... <in.mdl> [out.mdl]

The decompile and compile subcommands of the Go CLI, for one file:

- decompile: a binary model is decompiled, an ASCII one parsed, and the ASCII
  written to out.mdl or stdout;
- compile: the model (binary decompiled, ASCII parsed) is compiled to out.mdl,
  which is required (give the input twice to compile in place).

Supermodels, skeletons and materials are looked up as the CLI does: the
model's folder, the --supermodel-dir and --resource-dir folders, then the game.
"""

import argparse
import os
import sys

from . import binary, compiler, parser, resources, writer


def _dirs(a):
    """process.go lookupDirs: (supermodel dirs, resource dirs)."""
    folder = os.path.dirname(a.input) or "."
    sup = [folder] + a.supermodel_dir + a.resource_dir
    res = [folder] + a.resource_dir
    g = resources.find_game_install(a.game_dir)
    if g:
        sup.append(g)
        res.append(g)
    return sup, res


def _read(a, data, sup):
    """The model and its warnings as text lines; raises ValueError."""
    if binary.is_binary_mdl(data):
        try:
            model = binary.decompile(data, sup)
        except binary.DecompileError as e:
            raise ValueError(str(e))
        return model, ["%s%s" % ((w.node + ": ") if w.node else "", w.message) for w in model.warnings]
    try:
        res = parser.parse(data)
    except parser.NotModelError as e:
        raise ValueError(str(e))
    res.model.file_type = "ascii"
    return res.model, [str(e) for e in res.errors]


def main(argv=None):
    p = argparse.ArgumentParser(prog="python -m cleanmodels")
    sub = p.add_subparsers(dest="cmd", required=True)
    for name in ("decompile", "compile"):
        d = sub.add_parser(name)
        d.add_argument("--game-dir", default="")
        d.add_argument("--supermodel-dir", action="append", default=[])
        d.add_argument("--resource-dir", action="append", default=[])
        d.add_argument("--warnings", action="store_true", help="print warnings to stderr")
        d.add_argument("input")
        d.add_argument("output", nargs="?" if name == "decompile" else None)
    a = p.parse_args(argv)

    with open(a.input, "rb") as f:
        data = f.read()
    sup, res = _dirs(a)
    try:
        model, warns = _read(a, data, sup)
    except ValueError as e:
        print("cleanmodels: %s: %s" % (a.input, e), file=sys.stderr)
        return 1
    if a.cmd == "compile":
        try:
            out = compiler.compile(model, sup, res, warns.append)
        except compiler.CompileError as e:
            print("cleanmodels: %s: %s" % (a.input, e), file=sys.stderr)
            return 1
    else:
        out = writer.write(model).encode("latin-1")
    if a.warnings:
        for w in warns:
            print("warning: %s" % w, file=sys.stderr)
    if a.output:
        with open(a.output, "wb") as f:
            f.write(out)
    else:
        sys.stdout.buffer.write(out)
    return 0


if __name__ == "__main__":
    sys.exit(main())
