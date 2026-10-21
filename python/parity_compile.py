"""Parity test: the Python compiler must write exactly what `cleanmodels compile` writes.

    python python/parity_compile.py                  # build the Go reference, check the fixtures
    python python/parity_compile.py --corpus DIR     # also every model in DIR (e.g. the game's)

Each model is read the way `cleanmodels compile` reads it (a binary one
decompiled, an ASCII one parsed) and compiled with the model's folder and the
game install as supermodel and resource locations. The Go side is
python/internal/compileref, built from this checkout (the same library calls
the CLI makes). Compared: the compiled bytes and the compile warnings, or both
refusing the model. Checks:
  1. every model under tests/fixtures;
  2. deterministic mutations of each (numbers nudged, lines dropped or
     duplicated, names repeated; for binaries, corrupted bytes);
  3. with --corpus, every model in DIR;
  4. with --cli, also the bytes `cleanmodels compile` itself writes.
Exits 1 on any difference.
"""

import argparse
import hashlib
import json
import multiprocessing as mp
import os
import random
import re
import shutil
import struct
import subprocess
import sys
import tempfile
import time
import zlib

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(HERE)
sys.path.insert(0, HERE)

from cleanmodels import binary, compiler, parser, resources  # noqa: E402

GAME = resources.find_game_install()


def go_build(out_dir):
    exe = os.path.join(out_dir, "compileref" + (".exe" if os.name == "nt" else ""))
    subprocess.run(["go", "build", "-o", exe, "./python/internal/compileref"], cwd=REPO, check=True)
    return exe


def go_compile(exe, paths):
    """{path: (error or None, warnings, sha256)} from the Go reference."""
    r = subprocess.run([exe, "-game", GAME], input="\n".join(paths) + "\n", capture_output=True, text=True,
                       check=True)
    out = {}
    for line in r.stdout.splitlines():
        d = json.loads(line)
        out[d["path"]] = (d.get("error") or None, d["warnings"], d.get("sha256"))
    return out



def python_compile(path):
    """(error or None, warnings, sha256, seconds)."""
    t = time.time()
    with open(path, "rb") as f:
        data = f.read()
    dirs = [os.path.dirname(path)] + ([GAME] if GAME else [])
    try:
        if data[:4] == b"\0\0\0\0":
            model = binary.decompile(data, dirs)
        else:
            model = parser.parse(data).model
            model.file_type = "ascii"
    except Exception as e:  # noqa: BLE001 - a refusal, compared with Go's
        return "read: %s" % e, [], None, time.time() - t
    warns = []
    try:
        out = compiler.compile(model, dirs, dirs, warns.append)
    except compiler.CompileError as e:
        return "compile: %s" % e, warns, None, time.time() - t
    return None, warns, hashlib.sha256(out).hexdigest(), time.time() - t


CLI_BATCH = 2000  # files per `cleanmodels compile` call


def cli_compile(cli, paths):
    """{path: sha256 of what `cleanmodels compile` writes for it, or None when it
    refuses}. One call compiles a directory's files in batches into an output
    directory, in parallel (--workers), rather than one process per file: that
    took two thirds of a corpus run."""
    by_dir = {}
    for p in paths:
        by_dir.setdefault(os.path.dirname(p), []).append(p)
    out = {}
    # Since 4.13 the CLI refuses to write a model whose binary lost triangles
    # (a damaged render list, as the mutations make); --force writes it, as
    # the library does. Older CLIs have no such flag.
    help_text = subprocess.run([cli, "compile", "--help"], capture_output=True, text=True)
    force = ["--force"] if "-force" in help_text.stdout + help_text.stderr else []
    d = tempfile.mkdtemp(prefix="cmparity_cli")
    try:
        n = 0
        for files in by_dir.values():
            for i in range(0, len(files), CLI_BATCH):
                chunk = files[i:i + CLI_BATCH]
                o = os.path.join(d, str(n))
                n += 1
                os.makedirs(o)
                subprocess.run([cli, "compile", "-q", "--workers", str(os.cpu_count() or 4)] + force + chunk + [o],
                               capture_output=True)
                for p in chunk:
                    q = os.path.join(o, os.path.basename(p))
                    if os.path.exists(q):
                        with open(q, "rb") as f:
                            out[p] = hashlib.sha256(f.read()).hexdigest()
                    else:
                        out[p] = None
    finally:
        shutil.rmtree(d, ignore_errors=True)
    return out


def _py_job(path):
    try:
        res = python_compile(path)
    except Exception as e:  # noqa: BLE001 - a crash is a failure, reported
        res = ("python crash: %s: %s" % (type(e).__name__, e), [], "crash", 0.0)
    return path, res


def models(root):
    out = []
    for dirpath, _, files in os.walk(root):
        for f in files:
            if f.lower().endswith(".mdl"):
                out.append(os.path.join(dirpath, f))
    return sorted(out)


_NUM = re.compile(rb"-?\d+\.\d+(?:e-?\d+)?|-?\d+")


def mutate(data, rng):
    """A deterministic damage of a model: ASCII edits, or byte corruption of a binary."""
    if data[:4] == b"\0\0\0\0":
        b = bytearray(data)
        for _ in range(rng.randrange(1, 6)):
            i = rng.randrange(12, max(13, len(b) - 4))
            if rng.random() < 0.5:
                b[i] ^= 1 << rng.randrange(8)
            else:
                b[i:i + 4] = struct.pack("<I", rng.choice([0, 1, 0xFFFFFFFF, rng.getrandbits(32)]))
        return bytes(b), "bytes"
    lines = data.split(b"\n")
    kind = rng.choice(["nudge", "drop", "dup", "rename", "multi"])
    for _ in range(1 if kind != "multi" else rng.randrange(2, 12)):
        k = kind if kind != "multi" else rng.choice(["nudge", "drop", "dup", "rename"])
        i = rng.randrange(len(lines))
        if k == "nudge":
            nums = list(_NUM.finditer(lines[i]))
            if nums:
                m = rng.choice(nums)
                v = rng.choice([b"0", b"-0", b"1", b"-1", b"0.5", b"1e-7", b"65536", b"nan", b"inf", b"2.5",
                                m.group() + b"1", b"-" + m.group()])
                lines[i] = lines[i][:m.start()] + v + lines[i][m.end():]
        elif k == "drop":
            del lines[i]
        elif k == "dup":
            lines.insert(i, lines[i])
        else:
            w = lines[i].split()
            if len(w) >= 2 and w[0].lower() in (b"node", b"parent", b"bitmap", b"setsupermodel"):
                lines[i] = lines[i].replace(w[-1], rng.choice([b"rootdummy", b"NULL", w[-1].upper(), b"x"]))
    return b"\n".join(lines), kind


def compare(go_exe, paths, label, pool, timing=None, cli_exe=None):
    t0 = time.time()
    g = {}
    for i in range(0, len(paths), 2000):
        g.update(go_compile(go_exe, paths[i:i + 2000]))
    tg = time.time() - t0
    t0 = time.time()
    clis = cli_compile(cli_exe, paths) if cli_exe else {}
    tc = time.time() - t0
    t0 = time.time()
    res = pool.imap_unordered(_py_job, paths, chunksize=4)
    same = both = 0
    bad = []
    for path, p in res:
        cli = clis.get(path) if cli_exe else False
        if timing is not None:
            timing.append((p[3], path))
        ge, gw, gs = g.get(path, ("missing from Go output", [], None))
        pe, pw, ps = p[0], p[1], p[2]
        if ge is not None and pe is not None:
            both += 1
            continue
        if cli is not False and cli != ps and ge is None and pe is None:
            bad.append((path, "bytes differ from the CLI's"))
            continue
        if ge is None and pe is None and gs == ps and gw == pw:
            same += 1
        else:
            why = ("python: %s" % pe if pe else "go: %s" % ge if ge else
                   "bytes" if gs != ps else "warnings: go %r, python %r" % (gw, pw))
            bad.append((path, why))
    print("%s: %d identical, %d refused by both, %d different (go %.0fs,%s python %.0fs)"
          % (label, same, both, len(bad), tg, " cli %.0fs," % tc if cli_exe else "", time.time() - t0))
    for path, why in sorted(bad)[:20]:
        print("  %s: %s" % (path, why[:300]))
    return not bad


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--go", help="compileref binary (default: build python/internal/compileref)")
    ap.add_argument("--mutations", type=int, default=20, help="mutations per fixture (default 20)")
    ap.add_argument("--corpus", action="append", default=[], help="also compare every model in DIR")
    ap.add_argument("--jobs", type=int, default=2, help="Python processes (default 2)")
    ap.add_argument("--cli", help="also compare with `CLI compile in out` (e.g. a cleanmodels release build)")
    ap.add_argument("--slowest", type=int, default=0, help="print the N slowest Python compiles")
    a = ap.parse_args()
    tmp = tempfile.mkdtemp(prefix="cmparity_compile")
    ok = True
    timing = []
    try:
        go = a.go or go_build(tmp)
        with mp.Pool(max(1, a.jobs)) as pool:
            fixtures = models(os.path.join(REPO, "tests", "fixtures"))
            ok &= compare(go, fixtures, "fixtures", pool, timing, cli_exe=a.cli)
            mdir = os.path.join(tmp, "mut")
            muts = []
            for p in fixtures:
                with open(p, "rb") as f:
                    data = f.read()
                for i in range(a.mutations):
                    rng = random.Random(zlib.crc32(("%s|%d" % (os.path.basename(p), i)).encode()))
                    d = os.path.join(mdir, "%s_%d" % (os.path.basename(os.path.dirname(p)), i))
                    os.makedirs(d, exist_ok=True)
                    q = os.path.join(d, os.path.basename(p))
                    with open(q, "wb") as f:
                        f.write(mutate(data, rng)[0])
                    muts.append(q)
            if muts:
                ok &= compare(go, sorted(muts), "mutations", pool, cli_exe=a.cli)
            for c in a.corpus:
                ok &= compare(go, models(c), "corpus %s" % c, pool, timing, cli_exe=a.cli)
    finally:
        shutil.rmtree(tmp, ignore_errors=True)
    if a.slowest:
        for t, p in sorted(timing, reverse=True)[:a.slowest]:
            print("  %.2fs %s" % (t, p))
    print("parity: %s" % ("passed" if ok else "FAILED"))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
