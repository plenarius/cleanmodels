"""Resource lookup: a port of pkg/mdl/resources.go and cmd/cleanmodels/gameinstall.go.

A location is a directory (models matched in the directory itself), an
ERF-family archive (.hak, .erf, .mod), or the game install (recognised by
data/nwn_base.key), whose resources are read out of the BIF files its key
files list.
"""

import glob
import os
import struct
import sys
import threading

RES_TYPE_TGA = 3
RES_TYPE_PLT = 6
RES_TYPE_MDL = 2002
RES_TYPE_2DA = 2017
RES_TYPE_MTR = 2072
_WANTED = {RES_TYPE_MDL, RES_TYPE_MTR, RES_TYPE_2DA, RES_TYPE_PLT, RES_TYPE_TGA}
_OVR_TYPES = {".mdl": RES_TYPE_MDL, ".mtr": RES_TYPE_MTR, ".2da": RES_TYPE_2DA, ".plt": RES_TYPE_PLT,
              ".tga": RES_TYPE_TGA}


class ResourceRef:
    __slots__ = ("id", "stamp", "read")

    def __init__(self, rid, stamp, read):
        self.id = rid
        self.stamp = stamp
        self.read = read


_archives = {}
_archives_lock = threading.Lock()


def _open_archive(loc):
    with _archives_lock:
        if loc in _archives:
            return _archives[loc]
    a = None
    try:
        if os.path.isdir(loc):
            if os.path.exists(os.path.join(loc, "data", "nwn_base.key")):
                try:
                    a = _read_game_install(loc)
                except (OSError, ValueError):
                    a = None
        elif os.path.exists(loc):
            try:
                a = _read_erf(loc)
            except (OSError, ValueError):
                a = None
    except OSError:
        a = None
    with _archives_lock:
        _archives[loc] = a
    return a


def _read_at(path, off, size):
    with open(path, "rb") as f:
        f.seek(off)
        return f.read(size)


def _read_erf(path):
    with open(path, "rb") as f:
        hdr = f.read(160)
        if len(hdr) < 160:
            raise ValueError("short header")
        if hdr[0:8] not in (b"HAK V1.0", b"ERF V1.0", b"MOD V1.0"):
            raise ValueError("%s: not an ERF archive" % path)
        count = struct.unpack_from("<I", hdr, 16)[0]
        key_off = struct.unpack_from("<I", hdr, 24)[0]
        res_off = struct.unpack_from("<I", hdr, 28)[0]
        f.seek(key_off)
        keys = f.read(count * 24)
        f.seek(res_off)
        table = f.read(count * 8)
    entries = {}
    for i in range(min(count, len(keys) // 24, len(table) // 8)):
        k = keys[i * 24:i * 24 + 24]
        typ = struct.unpack_from("<H", k, 20)[0]
        if typ not in _WANTED:
            continue
        name = k[:16].rstrip(b"\0").decode("latin-1").lower()
        off, size = struct.unpack_from("<II", table, i * 8)
        entries[(name, typ)] = (lambda o=off, s=size: _read_at(path, o, s))
    return entries


def _read_game_install(root):
    data_dir = os.path.join(root, "data")
    key_files = sorted(glob.glob(os.path.join(glob.escape(data_dir), "*.key")))
    entries = {}
    ovr = os.path.join(root, "ovr")
    if os.path.isdir(ovr):
        for name in sorted(os.listdir(ovr)):
            p = os.path.join(ovr, name)
            base, ext = os.path.splitext(name)
            typ = _OVR_TYPES.get(ext.lower())
            if typ is None or os.path.isdir(p):
                continue
            entries[(base.lower(), typ)] = (lambda p=p: open(p, "rb").read())
    for kf in key_files:
        _index_key_file(entries, root, kf)
    return entries


def _index_key_file(entries, root, path):
    with open(path, "rb") as f:
        data = f.read()
    if len(data) < 64 or data[0:8] != b"KEY V1  ":
        raise ValueError("%s: not a KEY V1 file" % path)
    bif_count, key_count, file_off, key_off = struct.unpack_from("<IIII", data, 8)
    if file_off + bif_count * 12 > len(data) or key_off + key_count * 22 > len(data):
        raise ValueError("%s: truncated" % path)
    bifs = []
    for i in range(bif_count):
        name_off, name_len = struct.unpack_from("<IH", data, file_off + i * 12 + 4)
        if name_off + name_len > len(data):
            raise ValueError("%s: bad BIF name" % path)
        rel = data[name_off:name_off + name_len].rstrip(b"\0").decode("latin-1").replace("\\", "/")
        bifs.append(_resolve_case(root, rel))
    for i in range(key_count):
        e = key_off + i * 22
        typ = struct.unpack_from("<H", data, e + 16)[0]
        if typ not in _WANTED:
            continue
        name = data[e:e + 16].rstrip(b"\0").decode("latin-1").lower()
        rid = struct.unpack_from("<I", data, e + 18)[0]
        bif, idx = rid >> 20, rid & 0xFFFFF
        if bif >= len(bifs):
            continue
        k = (name, typ)
        if k in entries:
            continue
        entries[k] = (lambda b=bifs[bif], ix=idx: _read_bif(b, ix))


def _read_bif(path, idx):
    hdr = _read_at(path, 0, 20)
    if hdr[0:8] != b"BIFFV1  ":
        raise ValueError("%s: not a BIFF V1 file" % path)
    if idx >= struct.unpack_from("<I", hdr, 8)[0]:
        raise ValueError("%s: resource %d out of range" % (path, idx))
    ent = _read_at(path, struct.unpack_from("<I", hdr, 16)[0] + idx * 16, 16)
    off, size = struct.unpack_from("<II", ent, 4)
    return _read_at(path, off, size)


def _resolve_case(root, rel):
    cur = root
    for part in rel.split("/"):
        nxt = os.path.join(cur, part)
        if not os.path.exists(nxt):
            try:
                for e in sorted(os.listdir(cur)):
                    if e.lower() == part.lower():
                        nxt = os.path.join(cur, e)
                        break
            except OSError:
                pass
        cur = nxt
    return cur


def find_model_resource(name, locs):
    return find_resource(name, RES_TYPE_MDL, ".mdl", locs)


def find_resource(name, typ, ext, locs):
    """First match in locs, in order (resources.go findResource)."""
    name = name.lower()
    for loc in locs:
        a = _open_archive(loc)
        if a is not None:
            read = a.get((name, typ))
            if read is not None:
                return ResourceRef(loc + "#" + name + ext, "", read)
            continue
        try:
            listing = sorted(os.listdir(loc))
        except OSError:
            continue
        for e in listing:
            p = os.path.join(loc, e)
            if e.lower() == name + ext and not os.path.isdir(p):
                try:
                    st = os.stat(p)
                    stamp = "%d:%d" % (st.st_size, st.st_mtime_ns)
                except OSError:
                    stamp = ""
                return ResourceRef(p, stamp, (lambda p=p: open(p, "rb").read()))
    return None


# --- game install (cmd/cleanmodels/gameinstall.go) ---

def has_game_data(d):
    return os.path.isfile(os.path.join(d, "data", "nwn_base.key"))


def game_install_candidates():
    home = os.path.expanduser("~")
    steam = os.path.join("steamapps", "common", "Neverwinter Nights")
    c = []
    if sys.platform.startswith("win"):
        for pf in (os.environ.get("ProgramFiles(x86)"), os.environ.get("ProgramFiles"), r"C:\Program Files (x86)"):
            if pf:
                c += [os.path.join(pf, "Steam", steam), os.path.join(pf, "GOG Galaxy", "Games", "NWN Enhanced Edition")]
        c.append(r"C:\GOG Games\Neverwinter Nights Enhanced Edition")
    elif sys.platform == "darwin":
        c.append(os.path.join(home, "Library", "Application Support", "Steam", steam))
    else:
        c += [
            os.path.join(home, ".local", "share", "Steam", steam),
            os.path.join(home, ".steam", "steam", steam),
            os.path.join(home, ".steam", "root", steam),
            os.path.join(home, ".var", "app", "com.valvesoftware.Steam", ".local", "share", "Steam", steam),
            os.path.join(home, "GOG Games", "Neverwinter Nights Enhanced Edition"),
        ]
    return c


def find_game_install(flag_dir=""):
    """--game-dir, then $NWN_ROOT, then the usual install locations; "" if none."""
    if flag_dir:
        return flag_dir
    root = os.environ.get("NWN_ROOT", "")
    if root:
        return root
    for cand in game_install_candidates():
        if has_game_data(cand):
            return cand
    return ""


def lookup_dirs(path, supermodel_dirs=(), resource_dirs=(), game_dir=""):
    """The skeleton/supermodel search list for a model file (process.go lookupDirs)."""
    dirs = [os.path.dirname(path) or "."] + list(supermodel_dirs) + list(resource_dirs)
    g = find_game_install(game_dir)
    if g:
        dirs.append(g)
    return dirs
