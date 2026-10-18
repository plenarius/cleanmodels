"""float32 helpers: rounding to float32 and Go's %g formatting of a float32.

Go formats float32 values with strconv.AppendFloat(v, 'g', -1, 32): the
shortest decimal that reads back as the same float32, laid out as %g with an
exponent threshold of 6 (format.go appendFloat). Python only has that for
float64, so Go's float32 algorithm is ported here (Dragonbox, as in Go 1.26).
"""

import struct

_PACK_F = struct.Struct("<f")
_PACK_I = struct.Struct("<I")


def f32(x):
    """Round a float to float32 (round half to even, overflow to infinity)."""
    try:
        return _PACK_F.unpack(_PACK_F.pack(x))[0]
    except OverflowError:
        return float("inf") if x > 0 else float("-inf")


def f32_bits(x):
    return _PACK_I.unpack(_PACK_F.pack(x))[0]


# --- Go's shortest float32 digits (Go 1.26 internal/strconv/ftoadbox.go) ---
#
# Ported rather than approximated, from the Go release cleanmodels is built
# with (go.mod): Go 1.26 formats shortest floats with Dragonbox, which at an
# exponent boundary (a power of two) can pick a different shortest decimal
# than the Ryu of Go 1.21-1.25 (2^-12: 0.00024414063, not ...062). _POW10_HI
# holds the high 64 bits of Go's pow10Tab rows (pow10tab.go) a float32 can need.

_POW10_HI = {
    -60: 0xCDB02555653131B6,
    -59: 0x808E17555F3EBF11,
    -58: 0xA0B19D2AB70E6ED6,
    -57: 0xC8DE047564D20A8B,
    -56: 0xFB158592BE068D2E,
    -55: 0x9CED737BB6C4183D,
    -54: 0xC428D05AA4751E4C,
    -53: 0xF53304714D9265DF,
    -52: 0x993FE2C6D07B7FAB,
    -51: 0xBF8FDB78849A5F96,
    -50: 0xEF73D256A5C0F77C,
    -49: 0x95A8637627989AAD,
    -48: 0xBB127C53B17EC159,
    -47: 0xE9D71B689DDE71AF,
    -46: 0x9226712162AB070D,
    -45: 0xB6B00D69BB55C8D1,
    -44: 0xE45C10C42A2B3B05,
    -43: 0x8EB98A7A9A5B04E3,
    -42: 0xB267ED1940F1C61C,
    -41: 0xDF01E85F912E37A3,
    -40: 0x8B61313BBABCE2C6,
    -39: 0xAE397D8AA96C1B77,
    -38: 0xD9C7DCED53C72255,
    -37: 0x881CEA14545C7575,
    -36: 0xAA242499697392D2,
    -35: 0xD4AD2DBFC3D07787,
    -34: 0x84EC3C97DA624AB4,
    -33: 0xA6274BBDD0FADD61,
    -32: 0xCFB11EAD453994BA,
    -31: 0x81CEB32C4B43FCF4,
    -30: 0xA2425FF75E14FC31,
    -29: 0xCAD2F7F5359A3B3E,
    -28: 0xFD87B5F28300CA0D,
    -27: 0x9E74D1B791E07E48,
    -26: 0xC612062576589DDA,
    -25: 0xF79687AED3EEC551,
    -24: 0x9ABE14CD44753B52,
    -23: 0xC16D9A0095928A27,
    -22: 0xF1C90080BAF72CB1,
    -21: 0x971DA05074DA7BEE,
    -20: 0xBCE5086492111AEA,
    -19: 0xEC1E4A7DB69561A5,
    -18: 0x9392EE8E921D5D07,
    -17: 0xB877AA3236A4B449,
    -16: 0xE69594BEC44DE15B,
    -15: 0x901D7CF73AB0ACD9,
    -14: 0xB424DC35095CD80F,
    -13: 0xE12E13424BB40E13,
    -12: 0x8CBCCC096F5088CB,
    -11: 0xAFEBFF0BCB24AAFE,
    -10: 0xDBE6FECEBDEDD5BE,
    -9: 0x89705F4136B4A597,
    -8: 0xABCC77118461CEFC,
    -7: 0xD6BF94D5E57A42BC,
    -6: 0x8637BD05AF6C69B5,
    -5: 0xA7C5AC471B478423,
    -4: 0xD1B71758E219652B,
    -3: 0x83126E978D4FDF3B,
    -2: 0xA3D70A3D70A3D70A,
    -1: 0xCCCCCCCCCCCCCCCC,
    0: 0x8000000000000000,
    1: 0xA000000000000000,
    2: 0xC800000000000000,
    3: 0xFA00000000000000,
    4: 0x9C40000000000000,
    5: 0xC350000000000000,
    6: 0xF424000000000000,
    7: 0x9896800000000000,
    8: 0xBEBC200000000000,
    9: 0xEE6B280000000000,
    10: 0x9502F90000000000,
    11: 0xBA43B74000000000,
    12: 0xE8D4A51000000000,
    13: 0x9184E72A00000000,
    14: 0xB5E620F480000000,
    15: 0xE35FA931A0000000,
    16: 0x8E1BC9BF04000000,
    17: 0xB1A2BC2EC5000000,
    18: 0xDE0B6B3A76400000,
    19: 0x8AC7230489E80000,
    20: 0xAD78EBC5AC620000,
    21: 0xD8D726B7177A8000,
    22: 0x878678326EAC9000,
    23: 0xA968163F0A57B400,
    24: 0xD3C21BCECCEDA100,
    25: 0x84595161401484A0,
    26: 0xA56FA5B99019A5C8,
    27: 0xCECB8F27F4200F3A,
    28: 0x813F3978F8940984,
    29: 0xA18F07D736B90BE5,
    30: 0xC9F2C9CD04674EDE,
    31: 0xFC6F7C4045812296,
    32: 0x9DC5ADA82B70B59D,
    33: 0xC5371912364CE305,
    34: 0xF684DF56C3E01BC6,
    35: 0x9A130B963A6C115C,
    36: 0xC097CE7BC90715B3,
    37: 0xF0BDC21ABB48DB20,
    38: 0x96769950B50D88F4,
    39: 0xBC143FA4E250EB31,
    40: 0xEB194F8E1AE525FD,
    41: 0x92EFD1B8D0CF37BE,
    42: 0xB7ABC627050305AD,
    43: 0xE596B7B0C643C719,
    44: 0x8F7E32CE7BEA5C6F,
    45: 0xB35DBF821AE4F38B,
    46: 0xE0352F62A19E306E,
    47: 0x8C213D9DA502DE45,
    48: 0xAF298D050E4395D6,
    49: 0xDAF3F04651D47B4C,
    50: 0x88D8762BF324CD0F,
    51: 0xAB0E93B6EFEE0053,
    52: 0xD5D238A4ABE98068,
    53: 0x85A36366EB71F041,
    54: 0xA70C3C40A64E6C51,
    55: 0xD0CF4B50CFE20765,
    56: 0x82818F1281ED449F,
    57: 0xA321F2D7226895C7,
    58: 0xCBEA6F8CEB02BB39,
    59: 0xFEE50B7025C36A08,
    60: 0x9F4F2726179A2245,
}

_M64 = (1 << 64) - 1
_M32 = (1 << 32) - 1


def _trim_zeros(x):
    n = 0
    while x and x % 10 == 0:
        x //= 10
        n += 1
    return x, n


def _pow32(k, e):
    """dboxPow32: (phi, beta)."""
    phi = _POW10_HI[k]
    if k < 0 or k > 27:
        phi = (phi + 1) & _M64
    return phi, e + (1 + ((k * 108853) >> 15)) - 1


def _shortest_digits(x):
    """Return (digits, dp) for the float32 x (> 0): Go's dboxFtoa32."""
    b = f32_bits(x) & 0x7FFFFFFF
    exp = (b >> 23) & 0xFF
    mant = b & 0x7FFFFF
    denorm = False
    if exp == 0:
        exp += 1
        denorm = True
    else:
        mant |= 1 << 23
    exp += -127
    exp -= 23
    if mant == 1 << 23 and not denorm:
        k0 = -((exp * 631305 - 261663) >> 21)
        phi, beta = _pow32(k0, exp)
        xi = (((phi - (phi >> 25)) & _M64) >> (40 - beta)) & _M32
        zi = (((phi + (phi >> 24)) & _M64) >> (40 - beta)) & _M32
        if exp != 2 and exp != 3:
            xi = (xi + 1) & _M32
        q = zi // 10
        if xi <= q * 10:
            q, zeros = _trim_zeros(q)
            return _digits(q, -k0 + 1 + zeros)
        yru = (((phi >> (39 - beta)) + 1) & _M32) // 2
        if exp == -77 and yru % 2 != 0:
            yru -= 1
        elif yru < xi:
            yru += 1
        return _digits(yru, -k0)

    k0 = -((exp * 78913) >> 18)
    phi, beta = _pow32(1 + k0, exp)
    u = (((mant * 2 + 1) & _M32) << beta) & _M32
    r96 = (u * phi) >> 32
    zi, exact = (r96 >> 32) & _M32, (r96 & _M32) == 0
    s, r = zi // 100, zi % 100
    delta = (phi >> (63 - beta)) & _M32

    def parity(mant2):
        r64 = ((mant2 & _M32) * phi) & _M64
        return ((r64 >> (64 - beta)) & 1) != 0, ((r64 >> (32 - beta)) & _M32) == 0

    if r < delta:
        if r != 0 or not exact or mant % 2 == 0:
            s, zeros = _trim_zeros(s)
            return _digits(s, -k0 + 1 + zeros)
        s -= 1
        r = 100
    elif r == delta:
        par, ex = parity(mant * 2 - 1)
        if par or (ex and mant % 2 == 0):
            s, zeros = _trim_zeros(s)
            return _digits(s, -k0 + 1 + zeros)
    D = (r + 5 - delta // 2) & _M32
    t, rho = D // 10, D % 10
    yru = (10 * s + t) & _M32
    if rho == 0:
        par, ex = parity(mant * 2)
        if par != (((D - 5) & _M32) % 2 != 0) or (ex and yru % 2 != 0):
            yru -= 1
    return _digits(yru, -k0)


def _digits(mant, exp):
    d = str(mant)
    return d, len(d) + exp


_cache = {}


def fmt_float(x):
    """Format a float32 value like Go's strconv.FormatFloat(v, 'g', -1, 32)."""
    key = _PACK_F.pack(x)
    s = _cache.get(key)
    if s is None:
        s = _cache[key] = _format(x)
    return s


def _format(x):
    if x != x:
        return "NaN"
    if x in (float("inf"), float("-inf")):
        return "+Inf" if x > 0 else "-Inf"
    neg = x < 0 or (x == 0.0 and str(x)[0] == "-")
    if x == 0.0:
        return "-0" if neg else "0"
    digits, dp = _shortest_digits(x)
    nd = len(digits)
    exp = dp - 1
    if exp < -4 or exp >= 6:
        out = digits[0]
        if nd > 1:
            out += "." + digits[1:]
        out += "e" + ("-" if exp < 0 else "+") + ("%02d" % abs(exp))
    elif dp <= 0:
        out = "0." + "0" * (-dp) + digits
    elif dp >= nd:
        out = digits + "0" * (dp - nd)
    else:
        out = digits[:dp] + "." + digits[dp:]
    return "-" + out if neg else out


def fmt_vec3(v):
    return "%s %s %s" % (fmt_float(v[0]), fmt_float(v[1]), fmt_float(v[2]))
