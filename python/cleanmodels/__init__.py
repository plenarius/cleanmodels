"""cleanmodels for Python: the decompiler of the cleanmodels Go library.

A port of pkg/mdl's binary decompiler and ASCII writer. Its output is the same
text `cleanmodels decompile` writes (checked against the Go build by
python/parity_test.py); PORTED_FROM is the cleanmodels release the port
matches. Standard library only, so it can be bundled with Neverblender.

    from cleanmodels import decompile_file, write
    model = decompile_file("c_wolf.mdl", skeleton_dirs=lookup_dirs("c_wolf.mdl"))
    text = write(model)
"""

from .binary import DecompileError, decompile, decompile_file, is_binary_mdl
from .resources import find_game_install, lookup_dirs
from .writer import write, write_file

PORTED_FROM = "v4.8.0"

__all__ = ["DecompileError", "decompile", "decompile_file", "is_binary_mdl", "find_game_install",
           "lookup_dirs", "write", "write_file", "PORTED_FROM"]
