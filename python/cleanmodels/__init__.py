"""cleanmodels for Python: the decompiler, ASCII parser and compiler of the
cleanmodels Go library (pkg/mdl), with the ASCII writer.

Byte for byte what `cleanmodels decompile` and `cleanmodels compile` write
(checked against the Go build of the same commit by python/parity_test.py,
parity_parse.py and parity_compile.py); PORTED_FROM is the cleanmodels release
the port matches. Standard library only, so it can be bundled with Neverblender.

    from cleanmodels import compile, decompile_file, lookup_dirs, parse, write
    model = decompile_file("c_wolf.mdl", skeleton_dirs=lookup_dirs("c_wolf.mdl"))
    text = write(model)                      # ASCII
    model = parse(text.encode("latin-1")).model
    data = compile(model, lookup_dirs("c_wolf.mdl"), lookup_dirs("c_wolf.mdl"))  # binary
"""

from .binary import DecompileError, decompile, decompile_file, is_binary_mdl
from .parser import NotModelError, ParseError, ParseResult, parse, parse_file
from .compiler import CompileError, compile, compile_file  # noqa: A004 - mirrors Go's mdl.Compile
from .resources import find_game_install, lookup_dirs
from .writer import write, write_file

PORTED_FROM = "v4.12.0"

__all__ = ["DecompileError", "decompile", "decompile_file", "is_binary_mdl", "NotModelError", "ParseError",
           "ParseResult", "parse", "parse_file", "CompileError", "compile", "compile_file", "find_game_install",
           "lookup_dirs", "write", "write_file", "PORTED_FROM"]
