"""Parity test: the Python ASCII parser must read models exactly as Go's does.

    python python/parity_parse.py                 # build the Go helper, check the fixtures
    python python/parity_parse.py --go ./parsefmt
    python python/parity_parse.py --corpus DIR    # also every ASCII model in DIR

Checks, each against the Go build of this checkout (python/internal/parsefmt,
which does what `cleanmodels decompile in.mdl` does with ASCII input: ParseFile,
then Write):
  1. number parsing (parseFloat / parseInt) on 180,000+ strings: float32
     halfway cases, long and hex mantissas, inf/nan/1.#INF, underscores, junk;
  2. the string functions the parser uses (Fields, TrimSpace, ToLower, ToUpper,
     EqualFold, Quote) on byte strings with Unicode spaces and invalid UTF-8;
  3. every ASCII model under tests/fixtures: identical ASCII out, identical
     parse warnings, or both refuse the file;
  4. deterministic text mutations of each (dropped, duplicated and truncated
     lines, junk tokens, wrong counts, NaN/1.#INF/huge numbers, missing
     endlist, unknown keywords, odd whitespace and bytes): the same;
  5. with --corpus, every ASCII model in DIR (and --mutate-corpus N mutations
     of a sample of them).
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
from decimal import Context, Decimal
from fractions import Fraction

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(HERE)
sys.path.insert(0, HERE)

from cleanmodels import gostr, parser, writer  # noqa: E402

_GO = None


def go_build(out_dir):
    exe = os.path.join(out_dir, "parsefmt" + (".exe" if os.name == "nt" else ""))
    subprocess.run(["go", "build", "-o", exe, "./python/internal/parsefmt"], cwd=REPO, check=True)
    return exe


def run_go(go, mode, lines):
    r = subprocess.run([go, mode], input=("\n".join(lines) + "\n").encode(), capture_output=True, check=True)
    return r.stdout.decode("latin-1").split("\n")[:-1]


# --- 1. numbers ---

def number_strings(rng):
    out = set()
    for _ in range(15000):
        b = rng.getrandbits(31)
        if (b + 1) >> 23 >= 0xFF:
            continue
        f, g = (struct.unpack("<f", struct.pack("<I", x))[0] for x in (b, b + 1))
        m = (Fraction(f) + Fraction(g)) / 2  # a float32 halfway point: exact decimal
        k = m.denominator.bit_length() - 1
        s = format(Decimal(m.numerator * 5 ** k).scaleb(-k, Context(prec=2000)), "f")  # exact: m is dyadic
        out.update({s, s + "0000000001", "-" + s, repr(f), "%.9g" % f, "%.8e" % f, float(m).hex()})
        if "." in s:
            ip, fp = s.split(".")
            out.update(ip + "." + fp[:n] for n in (9, 12, 17, 18, 20, 25, 40))
    for _ in range(20000):
        n = rng.randrange(1, 30)
        s = "".join(rng.choice("0123456789") for _ in range(n))
        if rng.random() < 0.6:
            p = rng.randrange(0, n + 1)
            s = s[:p] + "." + s[p:]
        if rng.random() < 0.5:
            s += rng.choice("eE") + rng.choice(["", "+", "-"]) + str(rng.randrange(0, 60))
        if rng.random() < 0.3:
            s = "-" + s
        if rng.random() < 0.05:
            p = rng.randrange(0, len(s) + 1)
            s = s[:p] + rng.choice("_xXpP.#+-eaf") + s[p:]
        out.add(s)
    out.update([
        "0", "-0", "+0", ".5", "5.", "1e", "1e+", "e5", ".", "-", "+", "inf", "-inf", "+Inf", "INFINITY", "infin",
        "infx", "nan", "NaN", "-nan", "+nan", "1.#INF", "-1.#IND", "1.#QNAN", "0x1p3", "0x1.8p1", "0X1P-2", "0x10",
        "0x", "0x.8p1", "0xp1", "1_000", "1__0", "_1", "1_", "1_.5", "1._5", "1e1_0", "0x_1p0", "1e_5", "0x1p_1",
        "3.4028235e38", "3.4028236e38", "3.40282356779733661637539395458142568448e38",
        "3.40282356779733661637539395458142568447e38", "1e39", "1e-46", "7.006492321624085e-46",
        "7.006492321624086e-46", "2147483647", "2147483648", "-2147483648", "-2147483649", "2147483647.5",
        "-2147483648.5", "0.49999999999999994", "2.5", "-2.5", "1e10", "1e400", "-1e400", "1e-400", "0x1p128",
        "0x1.fffffep127", "0x1.ffffffp127", "0x1p-149", "0x1p-150", "0x1.000001p-150", "0x1p99999999",
        "1e99999999", "0.0000e99999", "1,5", "1.5.5", "--1", "0x0p0", "-0x0p0", "0x123456789abcdef123p0",
        "123456789012345678901234567890e-30", "0." + "0" * 1000 + "1e1001", "0." + "0" * 10010 + "1e10011",
        "1.7976931348623159e308", "+.5e-3", "-.5E+3", "1\xa0", "\xb9",
    ])
    return sorted(out)


def check_numbers(go):
    strs = number_strings(random.Random(11))
    got = run_go(go, "numbers", [s.encode("latin-1").hex() for s in strs])
    bad = 0
    for s, g in zip(strs, got):
        f, i = gostr.parse_float32(s), gostr.parse_int(s)
        p = "%s %s" % ("ERR" if f is None else struct.unpack("<I", struct.pack("<f", f))[0],
                       "ERR" if i is None else i)
        if p != g:
            bad += 1
            if bad <= 5:
                print("  number %r: go %s, python %s" % (s[:60], g, p))
    print("numbers: %d of %d identical" % (len(strs) - bad, len(strs)))
    return bad == 0


# --- 2. strings ---

_PIECES = [" ", "\t", "\r", "\n", "\x0b", "\x0c", "\x1c", "\x1f", "\x85", "\xa0", "\xc2\x85", "\xc2\xa0",
           "\xe1\x9a\x80", "\xe2\x80\x80", "\xe2\x80\x8a", "\xe2\x80\x8b", "\xe2\x80\xa8", "\xe2\x80\xaf",
           "\xe2\x81\x9f", "\xe3\x80\x80", "\xe2\x80", "\xe2", "\xc2", "\xff", "\xc3\xa9", "\xc3\x89", "\xe9",
           "\xc4\xb0", "\xe2\x84\xaa", "\xc5\xbf", "\xe1\xbe\x80", "\xef\xbf\xbd", "\xf0\x9f\x98\x80",
           "\xed\xa0\x80", "\xc0\x80", "\xf4\x90\x80\x80", "a", "B", "K", "k", "S", '"', "\\", "\x00", "\x7f",
           "#", "endlist", "ENDLIST", "animLength", "\xc2\x80", "\xcd\xb8", "\xc3\x9f", "\xc7\x85", "\xc3"]


def check_strings(go):
    rng = random.Random(12)
    strs = sorted({"".join(rng.choice(_PIECES) for _ in range(rng.randrange(0, 8))) for _ in range(40000)}
                  | {"endli\xc5\xbft", "anim\xe2\x84\xaalength", "Endlist"})
    got = run_go(go, "strings", [s.encode("latin-1").hex() for s in strs])
    bad = 0
    for s, g in zip(strs, got):
        f = gostr.fields(s)
        p = [gostr.trim_space(s), "\x00".join(f), str(len(f)), gostr.go_lower(s), gostr.go_upper(s), gostr.quote(s),
             str(gostr.equal_fold_ascii(s, "endlist")).lower(), str(gostr.equal_fold_ascii(s, "animlength")).lower(),
             s.rstrip("\r")]
        p = " ".join(x if i in (2, 6, 7) else x.encode("latin-1").hex() for i, x in enumerate(p))
        if gostr.simple_spaces(s) and (s.strip() != gostr.trim_space(s) or s.split() != f):
            p += " (fast path differs)"
        if p != g:
            bad += 1
            if bad <= 5:
                print("  string %r: go %s, python %s" % (s, g, p))
    print("strings: %d of %d identical" % (len(strs) - bad, len(strs)))
    return bad == 0


# --- 3-5. models ---

def python_parse(data):
    """(status, warnings, output) as parsefmt files reports them."""
    try:
        res = parser.parse(data)
    except parser.NotModelError as e:
        return "refused: %s" % e, None, b""
    return "ok", [str(e) for e in res.errors], writer.write(res.model).encode("latin-1")


def go_parse(go, paths):
    r = subprocess.run([go, "files"], input=("\n".join(paths) + "\n").encode("utf-8", "surrogateescape"),
                       capture_output=True, check=True)
    b, i, out = r.stdout, 0, []
    for _ in paths:
        j = b.index(b"\n", i)
        slen, nw = map(int, b[i:j].split())
        i = j + 1
        status = b[i:i + slen].decode("latin-1")
        i += slen
        warns = []
        for _ in range(nw + 1):
            j = b.index(b"\n", i)
            n = int(b[i:j])
            warns.append(b[j + 1:j + 1 + n])
            i = j + 1 + n
        text = warns.pop()
        out.append((status, [w.decode("latin-1") for w in warns], text))
    return out


def ascii_models(root):
    out = []
    for dirpath, _, files in os.walk(root):
        for f in sorted(files):
            if f.lower().endswith(".mdl"):
                p = os.path.join(dirpath, f)
                with open(p, "rb") as fh:
                    if fh.read(4) != b"\0\0\0\0":
                        out.append(p)
    return sorted(out)


_JUNK = ["NaN", "nan", "1.#INF", "-1.#IND", "inf", "1e39", "-1e39", "3.4028236e38", "1e-50", "99999999999",
         "2147483648", "-2147483649", "0x1p4", "1_0", "1,5", "abc", "-", ".", "1e", "\xe9", "\xc3\xa9", "\xff",
         "1.5abc", "-0", "+1", "\x00", "4294967295", "1e10", "#", "0.5", "-2.5"]
_KEYWORDS = ["verts", "faces", "tverts", "weights", "constraints", "aabb", "positionkey", "orientationkey",
             "alphakey", "colorkey", "endlist", "endnode", "node", "node trimesh", "node dummy x", "newanim a b",
             "doneanim", "parent", "position", "orientation", "scale", "bitmap", "shadow", "positionbezierkey",
             "birthratebezierkey", "detonatekey", "animlength", "length", "transtime", "event", "classification",
             "setsupermodel", "newmodel", "donemodel", "texindices0", "mirrorlist", "multimaterial", "animverts",
             "sampleperiod", "colors", "tangents", "texturenames", "flaresizes", "flarecolorshifts", "selfilumcolor",
             "positon", "diffuze", "renderr", "xyzzy", "affectedbywind true", "setfillumcolor", "isdynamic",
             "p2p_type", "opacity", "ENDNODE", "Verts", "endli\xc5\xbft", "position\xe2\x84\xaaey"]
_SPACES = [" ", "\t", "\r", "\x0b", "\x0c", "\x1c", "\xa0", "\xc2\xa0", "\xe2\x80\x80", "\xe3\x80\x80", "  "]


def mutate(data, rng):
    """A deterministic textual corruption of an ASCII model (latin-1 str in and out)."""
    lines = data.split("\n")
    kind = rng.choice(["drop", "dup", "truncate", "junk", "count", "number", "endlist", "keyword", "space",
                       "case", "crlf", "multi"])
    n = 1 if kind != "multi" else rng.randrange(2, 25)
    for _ in range(n):
        k = kind if kind != "multi" else rng.choice(["drop", "dup", "junk", "count", "number", "keyword", "space",
                                                     "case", "endlist"])
        if not lines:
            break
        i = rng.randrange(len(lines))
        toks = lines[i].split()
        if k == "drop":
            del lines[i:i + rng.choice([1, 1, 1, 2, 5])]
        elif k == "dup":
            lines[i:i] = lines[i:i + rng.choice([1, 1, 3])]
        elif k == "truncate":
            if rng.random() < 0.5:
                del lines[i:]
            else:
                lines[i] = lines[i][:rng.randrange(len(lines[i]) + 1)]
                del lines[i + 1:]
        elif k == "junk":
            toks.insert(rng.randrange(len(toks) + 1), rng.choice(_JUNK + _KEYWORDS))
            lines[i] = " ".join(toks)
        elif k == "count":
            idx = [j for j, l in enumerate(lines) if len(l.split()) == 2 and l.split()[1].isdigit()]
            if idx:
                j = rng.choice(idx)
                t = lines[j].split()
                c = int(t[1])
                t[1] = str(rng.choice([0, 1, c - 1, c + 1, c * 2, -1, -c, 2147483647, 50000000, 4000000000,
                                       c + rng.randrange(1, 20)])) if rng.random() < 0.9 else rng.choice(_JUNK)
                lines[j] = "  " + " ".join(t)
        elif k == "number":
            idx = [j for j, t in enumerate(toks) if t[:1].isdigit() or t[:1] in "-."]
            if idx:
                toks[rng.choice(idx)] = rng.choice(_JUNK)
                lines[i] = " ".join(toks)
        elif k == "endlist":
            lines = [l for l in lines if l.strip().lower() != "endlist" or rng.random() < 0.5]
        elif k == "keyword":
            lines.insert(i, rng.choice(_KEYWORDS) + rng.choice(["", " 3", " 1 2 3", " " + rng.choice(_JUNK)]))
        elif k == "space":
            if toks:
                sp = [rng.choice(_SPACES) for _ in toks]
                lines[i] = rng.choice(["", rng.choice(_SPACES)]) + "".join(t + s for t, s in zip(toks, sp))
        elif k == "case":
            lines[i] = lines[i].upper() if rng.random() < 0.5 else lines[i].swapcase()
        elif k == "crlf":
            lines = [l + "\r" for l in lines]
    return "\n".join(lines), kind


def _compare(job):
    go, items = job
    d = tempfile.mkdtemp(prefix="cmparse")
    try:
        paths, results = [], []
        for path, m in items:
            with open(path, "rb") as f:
                data = f.read()
            kind = ""
            if m is not None:
                rng = random.Random(zlib.crc32(("%s|%d" % (os.path.basename(path), m)).encode()))
                text, kind = mutate(data.decode("latin-1"), rng)
                data = text.encode("latin-1")
                p = os.path.join(d, "%d.mdl" % len(paths))
                with open(p, "wb") as f:
                    f.write(data)
            else:
                p = path
            paths.append(p)
            try:
                py = python_parse(data)
            except Exception as e:  # noqa: BLE001 - a crash is a failure, reported
                py = ("python crash: %s: %s" % (type(e).__name__, e), None, b"")
            results.append((path, kind, py))
        gos = go_parse(go, paths)
    finally:
        shutil.rmtree(d, ignore_errors=True)
    out = []
    for (path, kind, py), g in zip(results, gos):
        if py[0] != "ok" and py[0] == g[0]:
            what = "both refused"
        elif py == g:
            what = "same"
        elif py[0] != g[0]:
            what = "status: go %r, python %r" % (g[0][:100], py[0][:100])
        elif py[1] != g[1]:
            diff = next((i for i, (a, b) in enumerate(zip(py[1], g[1])) if a != b), min(len(py[1]), len(g[1])))
            what = "warnings: go %r, python %r" % (g[1][diff:diff + 1], py[1][diff:diff + 1])
        else:
            what = "output differs"
        out.append((path, kind, what))
    return out


def run(go, paths, mutations, label, procs):
    items = [(p, None) for p in paths] + [(p, i) for p in paths for i in range(mutations)]
    jobs = [(go, items[i:i + 50]) for i in range(0, len(items), 50)]
    with mp.Pool(procs) as pool:
        res = [r for chunk in pool.imap(_compare, jobs) for r in chunk]
    bad = [r for r in res if r[2] not in ("same", "both refused")]
    same = sum(1 for r in res if r[2] == "same")
    both = sum(1 for r in res if r[2] == "both refused")
    print("%s: %d identical, %d refused by both, %d different" % (label, same, both, len(bad)))
    for path, kind, what in bad[:10]:
        print("  %s [%s] %s" % (path, kind or "unchanged", what))
    return not bad


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--go", help="parsefmt binary to compare with (default: build this checkout's)")
    ap.add_argument("--mutations", type=int, default=40, help="mutations per fixture (default 40)")
    ap.add_argument("--corpus", action="append", default=[], help="also compare every ASCII model in DIR")
    ap.add_argument("--mutate-corpus", type=int, default=0, metavar="N",
                    help="also N mutations each of every 50th corpus model")
    ap.add_argument("--procs", type=int, default=2, help="worker processes (default 2)")
    a = ap.parse_args()
    tmp = tempfile.mkdtemp(prefix="cmparse_build")
    try:
        go = a.go or go_build(tmp)
        ok = check_numbers(go)
        ok &= check_strings(go)
        ok &= run(go, ascii_models(os.path.join(REPO, "tests", "fixtures")), a.mutations, "fixtures", a.procs)
        for c in a.corpus:
            models = ascii_models(c)
            ok &= run(go, models, 0, "corpus %s" % c, a.procs)
            if a.mutate_corpus:
                ok &= run(go, models[::50], a.mutate_corpus, "mutated corpus %s" % c, a.procs)
    finally:
        shutil.rmtree(tmp, ignore_errors=True)
    print("parity: %s" % ("passed" if ok else "FAILED"))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
