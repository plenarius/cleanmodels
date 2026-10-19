"""Go semantics the compiler depends on, bit for bit.

- math.Sin / math.Cos: Go computes these in pure Go (Cephes polynomials, no
  libm), so Python's math.sin can differ in the last bit. Ported from Go 1.26
  src/math/sin.go and trig_reduce.go.
- float -> int conversions as amd64 does them (CVTTSS2SQ: NaN and anything out
  of range become the minimum int64).
- strings.ToLower and strconv.Quote come from gostr (the parser's port).
- sort.SliceStable (insertion sort in blocks of 20, then SymMerge), for sorts
  whose comparator can be inconsistent (NaN weights).
"""

import struct

from .gostr import go_lower, quote as go_quote  # noqa: F401 - one implementation, the parser's

_U64 = struct.Struct("<Q")
_F64 = struct.Struct("<d")
_M64 = (1 << 64) - 1

_SIN = (1.58962301576546568060e-10, -2.50507477628578072866e-8, 2.75573136213857245213e-6,
        -1.98412698295895385996e-4, 8.33333333332211858878e-3, -1.66666666666666307295e-1)
_COS = (-1.13585365213876817300e-11, 2.08757008419747316778e-9, -2.75573141792967388112e-7,
        2.48015872888517045348e-5, -1.38888888888730564116e-3, 4.16666666666665929218e-2)
_PI4A = 7.85398125648498535156e-1
_PI4B = 3.77489470793079817668e-8
_PI4C = 2.69515142907905952645e-15
_FOUR_OVER_PI = _F64.unpack(bytes.fromhex("3ff45f306dc9c883")[::-1])[0]
_PI4 = _F64.unpack(bytes.fromhex("3fe921fb54442d18")[::-1])[0]
_REDUCE_THRESHOLD = float(1 << 29)

_M_PI4 = (
    0x0000000000000001, 0x45f306dc9c882a53, 0xf84eafa3ea69bb81, 0xb6c52b3278872083,
    0xfca2c757bd778ac3, 0x6e48dc74849ba5c0, 0x0c925dd413a32439, 0xfc3bd63962534e7d,
    0xd1046bea5d768909, 0xd338e04d68befc82, 0x7323ac7306a673e9, 0x3908bf177bf25076,
    0x3ff12fffbc0b301f, 0xde5e2316b414da3e, 0xda6cfd9e4f96136e, 0x9e8c7ecd3cbfd45a,
    0xea4f758fd7cbe2f6, 0x7a0e73ef14a525d4, 0xd7f6bf623f1aba10, 0xac06608df8f6d757,
)


def _trig_reduce(x):
    """trig_reduce.go trigReduce: (j, z) for x >= reduceThreshold."""
    if x < _PI4:
        return 0, x
    ix = _U64.unpack(_F64.pack(x))[0]
    exp = ((ix >> 52) & 0x7FF) - 1023 - 52
    ix &= ~(0x7FF << 52) & _M64
    ix |= 1 << 52
    digit, bitshift = (exp + 61) // 64, (exp + 61) % 64
    m = _M_PI4

    def hi_lo(a, b):
        return ((m[a] << bitshift) & _M64) | (m[b] >> (64 - bitshift) if bitshift else 0)

    z0 = hi_lo(digit, digit + 1)
    z1 = hi_lo(digit + 1, digit + 2)
    z2 = hi_lo(digit + 2, digit + 3)
    z2hi = (z2 * ix) >> 64
    p1 = z1 * ix
    z1hi, z1lo = p1 >> 64, p1 & _M64
    z0lo = (z0 * ix) & _M64
    lo = z1lo + z2hi
    c = lo >> 64
    lo &= _M64
    hi = (z0lo + z1hi + c) & _M64
    j = hi >> 61
    hi = ((hi << 3) & _M64) | (lo >> 61)
    lz = 64 - hi.bit_length()
    e = (1023 - (lz + 1)) & _M64
    hi = ((hi << (lz + 1)) & _M64) | (lo >> (64 - (lz + 1)))
    hi >>= 64 - 52
    hi |= (e << 52) & _M64
    z = _F64.unpack(_U64.pack(hi & _M64))[0]
    if j & 1 == 1:
        j = (j + 1) & 7
        z -= 1
    return j, z * _PI4


def _reduce(x):
    if x >= _REDUCE_THRESHOLD:
        return _trig_reduce(x)
    j = int(x * _FOUR_OVER_PI)
    y = float(j)
    if j & 1 == 1:
        j += 1
        y += 1
    j &= 7
    return j, ((x - y * _PI4A) - y * _PI4B) - y * _PI4C


def _poly_sin(z, zz):
    s = _SIN
    return z + z * zz * ((((((s[0] * zz) + s[1]) * zz + s[2]) * zz + s[3]) * zz + s[4]) * zz + s[5])


def _poly_cos(zz):
    c = _COS
    return 1.0 - 0.5 * zz + zz * zz * ((((((c[0] * zz) + c[1]) * zz + c[2]) * zz + c[3]) * zz + c[4]) * zz + c[5])


def go_sin(x):
    """Go's math.Sin."""
    if x == 0 or x != x:
        return x
    if x in (float("inf"), float("-inf")):
        return float("nan")
    sign = False
    if x < 0:
        x = -x
        sign = True
    j, z = _reduce(x)
    if j > 3:
        sign = not sign
        j -= 4
    zz = z * z
    y = _poly_cos(zz) if j in (1, 2) else _poly_sin(z, zz)
    return -y if sign else y


def go_cos(x):
    """Go's math.Cos."""
    if x != x or x in (float("inf"), float("-inf")):
        return float("nan")
    sign = False
    x = abs(x)
    j, z = _reduce(x)
    if j > 3:
        j -= 4
        sign = not sign
    if j > 1:
        sign = not sign
    zz = z * z
    y = _poly_sin(z, zz) if j in (1, 2) else _poly_cos(zz)
    return -y if sign else y


_MIN_I64 = -(1 << 63)


def go_int(x):
    """int(x) for a float on amd64: truncation; NaN and out of range give min int64."""
    if x != x or not -9.223372036854775808e18 <= x < 9.223372036854775808e18:
        return _MIN_I64
    return int(x)


def slice_stable(data, less):
    """sort.SliceStable(data, less) in place (Go's insertion sort + SymMerge)."""
    n = len(data)
    block = 20
    a, b = 0, block
    while b <= n:
        _insertion_sort(data, less, a, b)
        a, b = b, b + block
    _insertion_sort(data, less, a, n)
    while block < n:
        a, b = 0, 2 * block
        while b <= n:
            _sym_merge(data, less, a, a + block, b)
            a, b = b, b + 2 * block
        m = a + block
        if m < n:
            _sym_merge(data, less, a, m, n)
        block *= 2


def _insertion_sort(data, less, a, b):
    for i in range(a + 1, b):
        j = i
        while j > a and less(data[j], data[j - 1]):
            data[j], data[j - 1] = data[j - 1], data[j]
            j -= 1


def _sym_merge(data, less, a, m, b):
    if m - a == 1:
        i, j = m, b
        while i < j:
            h = (i + j) // 2
            if less(data[h], data[a]):
                i = h + 1
            else:
                j = h
        for k in range(a, i - 1):
            data[k], data[k + 1] = data[k + 1], data[k]
        return
    if b - m == 1:
        i, j = a, m
        while i < j:
            h = (i + j) // 2
            if not less(data[m], data[h]):
                i = h + 1
            else:
                j = h
        for k in range(m, i, -1):
            data[k], data[k - 1] = data[k - 1], data[k]
        return
    mid = (a + b) // 2
    n = mid + m
    if m > mid:
        start, r = n - b, mid
    else:
        start, r = a, m
    p = n - 1
    while start < r:
        c = (start + r) // 2
        if not less(data[p - c], data[c]):
            start = c + 1
        else:
            r = c
    end = n - start
    if start < m and m < end:
        _rotate(data, start, m, end)
    if a < start and start < mid:
        _sym_merge(data, less, a, start, mid)
    if mid < end and end < b:
        _sym_merge(data, less, mid, end, b)


def _swap_range(data, a, b, n):
    for i in range(n):
        data[a + i], data[b + i] = data[b + i], data[a + i]


def _rotate(data, a, m, b):
    i = m - a
    j = b - m
    while i != j:
        if i > j:
            _swap_range(data, m - i, m, j)
            i -= j
        else:
            _swap_range(data, m - i, m + j - i, i)
            j -= i
    _swap_range(data, m - i, m, i)

