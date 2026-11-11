"""share_neighbours.go: a mesh's stored face neighbours in binary face numbering,
after a compile dropped degenerate faces and left them in source-face numbering
(nwn-mdl-spec MDL-MESH-020/021/022). See the Go file for the reasoning."""

import math

MAX_DROPPED_FACES = 64


def _edge_at(verts, f, k):
    """edgeAt: the edge's two end positions, the lesser first, -0 as +0; None for
    an edge of zero length, with a NaN coordinate, or with an index out of range."""
    nv = len(verts)
    i, j = f.verts[k], f.verts[(k + 1) % 3]
    if i < 0 or i >= nv or j < 0 or j >= nv:
        return None
    a, b = verts[i], verts[j]
    for v in (a[0], a[1], a[2], b[0], b[1], b[2]):
        if math.isnan(v):
            return None
    if a[0] == b[0] and a[1] == b[1] and a[2] == b[2]:
        return None
    if _less(b, a):
        a, b = b, a
    return (a[0] + 0.0, a[1] + 0.0, a[2] + 0.0, b[0] + 0.0, b[1] + 0.0, b[2] + 0.0)


def _less(a, b):
    if a[0] != b[0]:
        return a[0] < b[0]
    if a[1] != b[1]:
        return a[1] < b[1]
    return a[2] < b[2]


def stored_neighbours(m):
    """storedNeighbours: each face's three neighbours (-1 for none)."""
    nf = len(m.faces)
    adj = [[a[0], a[1], a[2]] for a in m.stored_adjacency]
    edges = []
    by_edge = {}
    for i, f in enumerate(m.faces):
        e3 = []
        for k in range(3):
            e = _edge_at(m.verts, f, k)
            e3.append(e)
            if e is not None:
                by_edge.setdefault(e, []).append(i)
        edges.append(e3)

    def shares(o, k, n):
        if n < 0 or n >= nf or n == o:
            return False
        e = edges[o][k]
        return e is not None and e in edges[n]

    def count(lst):
        valid = invalid = 0
        for o, row in enumerate(lst):
            for k in range(3):
                j = row[k]
                if j < 0 or j == o or edges[o][k] is None:
                    continue
                if shares(o, k, j):
                    valid += 1
                else:
                    invalid += 1
        return valid, invalid

    valid, invalid = count(adj)
    if invalid == 0:
        return adj

    max_c = min(MAX_DROPPED_FACES, nf)

    def linked(r, c, k):
        j = adj[r][k]
        o = r - c
        e = edges[o][k]
        if j < 0 or j == r or j >= nf or e is None:
            return 0
        for n in by_edge[e]:
            s = j - n
            if n == o or s < 0 or s > max_c or (j > r and s < c) or (j < r and s > c):
                continue
            return 2
        return -2

    none = -(1 << 30)
    score = [none] * (max_c + 1)
    score[0] = 0
    dropped = []
    for r in range(nf):
        nxt = [none] * (max_c + 1)
        drop = [False] * (max_c + 1)
        for c in range(max_c + 1):
            if c <= r and score[c] != none:
                nxt[c] = score[c] + linked(r, c, 0) + linked(r, c, 1) + linked(r, c, 2)
            if c > 0 and score[c - 1] != none and score[c - 1] - 1 > nxt[c]:
                nxt[c] = score[c - 1] - 1
                drop[c] = True
        dropped.append(drop)
        score = nxt
    end = 0
    for c in range(1, max_c + 1):
        if score[c] > score[end]:
            end = c
    if end == 0:
        return adj
    shift = [0] * nf
    gone = [False] * nf
    c = end
    for r in range(nf - 1, -1, -1):
        if dropped[r][c]:
            gone[r] = True
            c -= 1
        shift[r] = c

    out = [[-1, -1, -1] for _ in range(nf)]
    for r in range(nf):
        if gone[r]:
            continue
        o = r - shift[r]
        for k in range(3):
            j = adj[r][k]
            if 0 <= j < nf and not gone[j]:
                out[o][k] = j - shift[j]
    v, inv = count(out)
    if inv > 0 or v <= valid:
        return adj
    return out
