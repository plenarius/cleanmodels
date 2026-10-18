"""Parity test: the Python decompiler must write exactly what `cleanmodels decompile` writes.

    python python/parity_test.py                 # build the Go CLI, check the fixtures
    python python/parity_test.py --go ./cleanmodels
    python python/parity_test.py --corpus DIR    # also every binary model in DIR (e.g. the game's)

Checks, each against the Go build of this checkout:
  1. float formatting on 300,000+ float32 values (every exponent, powers of two, random);
  2. every binary model under tests/fixtures: identical ASCII;
  3. deterministic corruptions of each fixture (truncation, bit flips, junk pointers and
     counts, NaN floats): identical ASCII, or both refuse the file;
  4. with --corpus, every binary model in DIR.
Exits 1 on any difference.
"""

import argparse
import multiprocessing as mp
import os
import random
import shutil
import struct
import subprocess
import sys
import tempfile
import zlib

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(HERE)
sys.path.insert(0, HERE)

from cleanmodels import binary, resources, writer  # noqa: E402
from cleanmodels.f32 import fmt_float  # noqa: E402


def go_build(out_dir):
    exe = os.path.join(out_dir, "cleanmodels" + (".exe" if os.name == "nt" else ""))
    subprocess.run(["go", "build", "-o", exe, "./cmd/cleanmodels"], cwd=REPO, check=True)
    return exe


def check_floats(tmp):
    exe = os.path.join(tmp, "f32fmt" + (".exe" if os.name == "nt" else ""))
    subprocess.run(["go", "build", "-o", exe, "./python/internal/f32fmt"], cwd=REPO, check=True)
    rng = random.Random(7)
    bits = set()
    while len(bits) < 300000:
        b = rng.getrandbits(32)
        if (b >> 23) & 0xFF != 0xFF:
            bits.add(b)
    for e in range(255):
        bits.update({e << 23, (e << 23) | 0x80000000, (e << 23) | 1, (e << 23) | 0x7FFFFF})
    bits = sorted(bits)
    r = subprocess.run([exe], input="\n".join(map(str, bits)) + "\n", capture_output=True, text=True, check=True)
    go = r.stdout.split()
    bad = 0
    for b, g in zip(bits, go):
        p = fmt_float(struct.unpack("<f", struct.pack("<I", b))[0])
        if p != g:
            bad += 1
            if bad <= 5:
                print("  float 0x%08x: go %s, python %s" % (b, g, p))
    print("floats: %d of %d identical" % (len(bits) - bad, len(bits)))
    return bad == 0


def python_decompile(path, data=None):
    if data is None:
        with open(path, "rb") as f:
            data = f.read()
    try:
        return writer.write(binary.decompile(data, resources.lookup_dirs(path))).encode("latin-1")
    except binary.DecompileError:
        return None


def go_decompile(go, path):
    r = subprocess.run([go, "decompile", path], capture_output=True)
    return r.stdout if r.returncode == 0 else None


def binary_models(root):
    out = []
    for dirpath, _, files in os.walk(root):
        for f in sorted(files):
            if f.lower().endswith(".mdl"):
                p = os.path.join(dirpath, f)
                with open(p, "rb") as fh:
                    if fh.read(4) == b"\0\0\0\0":
                        out.append(p)
    return sorted(out)


def mutate(data, rng):
    b = bytearray(data)
    kind = rng.choice(["truncate", "flip", "word", "nan", "multi"])
    if kind == "truncate":
        b = b[:rng.randrange(12, len(b))]
    else:
        for _ in range(1 if kind != "multi" else rng.randrange(2, 30)):
            k = kind if kind != "multi" else rng.choice(["flip", "word", "nan"])
            i = rng.randrange(0, len(b) - 4)
            if k == "flip":
                b[i] ^= 1 << rng.randrange(8)
            elif k == "word":
                b[i:i + 4] = struct.pack("<I", rng.choice([0, 1, 0xFFFFFFFF, 0x7FFFFFFF, rng.getrandbits(32),
                                                           rng.randrange(0, len(b))]))
            else:
                b[i:i + 4] = struct.pack("<I", rng.choice([0x7FC00000, 0x7F800000, 0xFF800000]))
    b[0:4] = b"\0\0\0\0"
    return bytes(b), kind


def _compare(args):
    go, path, mutation = args
    if mutation is None:
        g, p = go_decompile(go, path), python_decompile(path)
        return path, "", ("same" if g == p else "both failed" if g is None and p is None else "DIFF")
    with open(path, "rb") as f:
        data = f.read()
    rng = random.Random(zlib.crc32(("%s|%d" % (os.path.basename(path), mutation)).encode()))
    data, kind = mutate(data, rng)
    d = tempfile.mkdtemp(prefix="cmparity")
    try:
        p = os.path.join(d, os.path.basename(path))
        with open(p, "wb") as f:
            f.write(data)
        try:
            py = python_decompile(p, data)
        except Exception as e:  # noqa: BLE001 - a crash is a failure, reported
            return path, kind, "python crash: %s: %s" % (type(e).__name__, e)
        g = go_decompile(go, p)
    finally:
        shutil.rmtree(d, ignore_errors=True)
    if g is None and py is None:
        return path, kind, "both failed"
    return path, kind, ("same" if g == py else "DIFF" if g is not None and py is not None else "one failed")


def run(go, paths, mutations, label):
    jobs = [(go, p, None) for p in paths] + [(go, p, i) for p in paths for i in range(mutations)]
    with mp.Pool(max(1, (os.cpu_count() or 2))) as pool:
        res = pool.map(_compare, jobs, chunksize=4)
    bad = [r for r in res if r[2] not in ("same", "both failed")]
    same = sum(1 for r in res if r[2] == "same")
    both = sum(1 for r in res if r[2] == "both failed")
    print("%s: %d identical, %d refused by both, %d different" % (label, same, both, len(bad)))
    for path, kind, what in bad[:10]:
        print("  %s %s %s" % (what, kind or "unchanged", os.path.relpath(path, REPO)))
    return not bad


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--go", help="cleanmodels binary to compare with (default: build this checkout)")
    ap.add_argument("--mutations", type=int, default=40, help="corruptions per fixture (default 40)")
    ap.add_argument("--corpus", action="append", default=[], help="also compare every binary model in DIR")
    a = ap.parse_args()
    tmp = tempfile.mkdtemp(prefix="cmparity_build")
    try:
        go = a.go or go_build(tmp)
        ok = check_floats(tmp)
        ok &= run(go, binary_models(os.path.join(REPO, "tests", "fixtures")), a.mutations, "fixtures")
        for c in a.corpus:
            ok &= run(go, binary_models(c), 0, "corpus %s" % c)
    finally:
        shutil.rmtree(tmp, ignore_errors=True)
    print("parity: %s" % ("passed" if ok else "FAILED"))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
