"""patchBuf: a growable little-endian byte buffer with back-patching (compiler.go)."""

import struct
from array import array
from itertools import chain
from sys import byteorder

_U32 = struct.Struct("<I")
_LE = byteorder == "little"


def _f32_array(values):
    """array('f') of the values: rounds to float32 (overflow to infinity) like
    a Go float32 conversion; the compiler's values are float32 already."""
    a = array("f", values)
    if not _LE:
        a.byteswap()
    return a


def encode_str(s):
    """A model string's bytes (strings hold latin-1 code points)."""
    try:
        return s.encode("latin-1")
    except UnicodeEncodeError:
        return s.encode("utf-8")


class PatchBuf:
    __slots__ = ("b",)

    def __init__(self):
        self.b = bytearray()

    def len(self):
        return len(self.b)

    def u8(self, v):
        self.b.append(v & 0xFF)

    def u16(self, v):
        v &= 0xFFFF
        self.b += bytes((v & 0xFF, v >> 8))

    def u32(self, v):
        self.b += _U32.pack(v & 0xFFFFFFFF)

    i32 = u32

    def f32(self, v):
        self.b += _f32_array((v,)).tobytes()

    def vec3(self, v):
        self.b += _f32_array(v[:3]).tobytes()

    def zeros(self, n):
        self.b += bytes(n)

    def raw(self, data):
        self.b += data

    def fixed_str(self, s, width):
        """s null-terminated in a zero-padded field of width bytes (truncated to width-1)."""
        d = encode_str(s)
        if len(d) >= width:
            d = d[:width - 1]
        self.b += d
        self.b += bytes(width - len(d))

    def placeholder(self):
        off = len(self.b)
        self.b += b"\0\0\0\0"
        return off

    def patch_u32(self, off, v):
        _U32.pack_into(self.b, off, v & 0xFFFFFFFF)

    def proxy_list(self, offset, count, alloc):
        self.u32(offset)
        self.u32(count)
        self.u32(alloc)

    def proxy_list_empty(self):
        self.b += bytes(12)

    # bulk writers

    def f32s(self, values):
        self.b += _f32_array(values).tobytes()

    def vec3s(self, vs):
        self.b += _f32_array(chain.from_iterable((v[0], v[1], v[2]) for v in vs)).tobytes()

    def vec2s(self, vs):
        self.b += _f32_array(chain.from_iterable((v[0], v[1]) for v in vs)).tobytes()

    def u16s(self, values):
        a = array("H", [v & 0xFFFF for v in values])
        if not _LE:
            a.byteswap()
        self.b += a.tobytes()

    def i16s(self, values):
        self.u16s(values)
