"""Fuzzy parameter name matching for the ASCII parser: a port of pkg/mdl/nearmatch.go."""

from .gostr import go_lower

KNOWN_ALIASES = {
    "setfillumcolor": "selfillumcolor",
    "n_dynamic_type": "ndynamictype",
    "filedependancy": "filedependency",
    "texture0": "bitmap",
    "isdynamic": "ndynamictype",
}


def near_match_param(text, valid_params):
    """NearMatchParam: the matched name, or None."""
    lower = go_lower(text)
    for p in valid_params:
        if lower == p:
            return p
    canonical = KNOWN_ALIASES.get(lower)
    if canonical is not None and canonical in valid_params:
        return canonical
    # Levenshtein distance <= 2, ties to the lexicographically first name
    best_dist, best = 3, ""
    for p in valid_params:
        d = levenshtein(lower, p)
        if d < best_dist or (d == best_dist and (best == "" or p < best)):
            best_dist, best = d, p
    if best_dist <= 2 and best != "":
        return best
    return None


def levenshtein(a, b):
    """Edit distance in runes; 3 when the rune counts differ by more than 2."""
    if not a:
        return len(b)
    if not b:
        return len(a)
    # a is ASCII or go_lower's valid UTF-8 (as latin-1); b is ASCII
    ra = a if a.isascii() else a.encode("latin-1").decode("utf-8")
    rb = b
    la, lb = len(ra), len(rb)
    if la - lb > 2 or lb - la > 2:
        return 3
    prev = list(range(lb + 1))
    for i in range(1, la + 1):
        curr = [i] + [0] * lb
        ca = ra[i - 1]
        for j in range(1, lb + 1):
            curr[j] = min(prev[j] + 1, curr[j - 1] + 1, prev[j - 1] + (ca != rb[j - 1]))
        prev = curr
    return prev[lb]
