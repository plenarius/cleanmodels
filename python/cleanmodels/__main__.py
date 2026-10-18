"""python -m cleanmodels decompile [--game-dir DIR] [--supermodel-dir DIR]... [--resource-dir DIR]... <in.mdl> [out.mdl]

The decompile subcommand of the Go CLI, for checking parity. ASCII input is
not re-serialised here (that needs the parser); it is copied through.
"""

import argparse
import sys

from . import binary, resources, writer


def main(argv=None):
    p = argparse.ArgumentParser(prog="python -m cleanmodels")
    sub = p.add_subparsers(dest="cmd", required=True)
    d = sub.add_parser("decompile")
    d.add_argument("--game-dir", default="")
    d.add_argument("--supermodel-dir", action="append", default=[])
    d.add_argument("--resource-dir", action="append", default=[])
    d.add_argument("--warnings", action="store_true", help="print decompile warnings to stderr")
    d.add_argument("input")
    d.add_argument("output", nargs="?")
    a = p.parse_args(argv)

    with open(a.input, "rb") as f:
        data = f.read()
    if not binary.is_binary_mdl(data):
        out = data
    else:
        dirs = resources.lookup_dirs(a.input, a.supermodel_dir, a.resource_dir, a.game_dir)
        try:
            model = binary.decompile(data, dirs)
        except binary.DecompileError as e:
            print("cleanmodels: %s: %s" % (a.input, e), file=sys.stderr)
            return 1
        if a.warnings:
            for w in model.warnings:
                print("warning: %s%s" % ((w.node + ": ") if w.node else "", w.message), file=sys.stderr)
        out = writer.write(model).encode("latin-1")
    if a.output:
        with open(a.output, "wb") as f:
            f.write(out)
    else:
        sys.stdout.buffer.write(out)
    return 0


if __name__ == "__main__":
    sys.exit(main())
