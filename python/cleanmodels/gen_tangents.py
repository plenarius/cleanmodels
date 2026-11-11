"""binary_gentangents.go: a normal-mapped mesh whose tangents the game
generated with clone vertices is written without them, when the compile
generates the same again (nwn-mdl-spec MDL-MESH-043). See the Go file for the
reasoning."""

from .compiler_mesh import build_expanded_mesh, skin_merge_keys, CompileError
from .compiler_tangents import generate_tangents
from .f32 import f32_bits
from .gostr import quote
from .materials import is_normal_mapped
from .types import WARN_GENERAL, DanglyData, Face, SkinData


def omit_generated_tangents(d, material_dirs):
    """omitGeneratedTangents."""
    filled = set(id(f.node) for f in d.tangent_fills)
    for n in d.model.nodes:
        if n.mesh is None or not n.mesh.tangents or id(n) in filled:
            continue
        rl = d.render_lists.get(n) if d.render_lists else None
        ok, clones = _regenerable_tangents(n, rl or [], material_dirs)
        if ok:
            n.mesh.tangents_omitted = True
        if ok and clones == 0:
            d.note(WARN_GENERAL, 0, n.name,
                   "mesh %s: tangents left out: a compile generates the same ones (the mesh is normal-mapped)"
                   % quote(n.name))
        elif ok:
            d.note(WARN_GENERAL, 0, n.name,
                   "mesh %s: tangents left out: a compile generates the same ones (the mesh is normal-mapped), and only "
                   "a generating compile keeps their %d cloned vertices at the end of the vertex list"
                   % (quote(n.name), clones))


def _nan3(lst):
    for v in lst:
        for x in v:
            if x != x:
                return True
    return False


def _regenerable_tangents(n, rl, material_dirs):
    """regenerableTangents: (True, clones folded) when it rewrote the mesh."""
    m = n.mesh
    if n.aabb is not None or n.anim_mesh is not None:
        return False, 0
    nv, nf = len(m.verts), len(m.faces)
    if nv == 0 or nf == 0 or len(rl) != 3 * nf or len(m.tangents) != nv or len(m.normals) != nv or len(m.tverts) != nv:
        return False, 0
    weights = n.skin.weights if n.skin is not None else []
    constraints = n.dangly.constraints if n.dangly is not None else []

    def full(lst):
        return len(lst) == 0 or len(lst) == nv

    if (not full(m.colors) or not full(m.tverts1) or not full(m.tverts2) or not full(m.tverts3)
            or not full(weights) or not full(constraints)
            or len(m.tex_indices0) + len(m.tex_indices1) + len(m.tex_indices2) + len(m.tex_indices3)
            + len(m.mirror_list) + len(m.corner_normals) > 0):
        return False, 0
    # Even a list in first-use vertex order is taken (see the Go file).
    if any(v >= nv for v in rl):
        return False, 0
    if (_nan3(m.verts) or _nan3(m.tverts) or _nan3(m.tverts1) or _nan3(m.tverts2) or _nan3(m.tverts3)
            or _nan3(m.normals) or _nan3(m.colors) or _nan3(m.tangents)
            or any(c != c for c in constraints) or any(f != f for w in weights for f in w.weights)):
        return False, 0
    if not is_normal_mapped(m, material_dirs):
        return False, 0

    skin_keys = skin_merge_keys(n.skin) if weights else []
    first = {}
    base = [0] * nv
    for i in range(nv):
        k = [f32_bits(x) for x in m.verts[i]] + [f32_bits(x) for x in m.tverts[i]] + [f32_bits(x) for x in m.normals[i]]
        for lst in (m.tverts1, m.tverts2, m.tverts3, m.colors):
            if lst:
                k += [f32_bits(x) for x in lst[i]]
        if constraints:
            k.append(f32_bits(constraints[i]))
        k = (tuple(k), skin_keys[i] if skin_keys else "")
        j = first.get(k)
        if j is None:
            first[k] = j = i
        base[i] = j

    used = [False] * nv
    for i, f in enumerate(m.faces):
        for k in range(3):
            a = f.verts[k]
            if a < 0 or a >= nv or base[a] != base[rl[3 * i + k]]:
                return False, 0
            used[base[a]] = True
    keep = [i for i in range(nv) if used[i]]
    new_idx = [0] * nv
    for ni, oi in enumerate(keep):
        new_idx[oi] = ni
    faces = []
    for f in m.faces:
        v = [new_idx[base[f.verts[k]]] for k in range(3)]
        faces.append(Face(v, f.smooth_group, v, f.material, f.normal, f.plane_d))

    def pick(lst):
        return [lst[oi] for oi in keep] if lst else []

    c = _copy_mesh(m)
    c.verts, c.tverts, c.normals, c.colors = pick(m.verts), pick(m.tverts), pick(m.normals), pick(m.colors)
    c.tverts1, c.tverts2, c.tverts3 = pick(m.tverts1), pick(m.tverts2), pick(m.tverts3)
    c.tangents = []
    c.faces = faces
    skin = None
    new_weights = None
    if weights:
        new_weights = pick(weights)
        skin = SkinData()
        skin.weights = new_weights
    dangly = None
    new_constraints = None
    if n.dangly is not None:
        dangly = DanglyData()
        dangly.__dict__.update(n.dangly.__dict__)
        if constraints:
            new_constraints = pick(constraints)
            dangly.constraints = new_constraints

    try:
        exp = build_expanded_mesh(c, strip_degenerate=True, skin_keys=skin_merge_keys(skin), dangly=dangly)
    except CompileError:
        return False, 0
    tan, hand = generate_tangents(exp, False)
    if len(exp.positions) != nv or tan is None or len(tan) != nv or len(exp.face_verts) != nf:
        return False, 0
    for k, fv in enumerate(exp.face_verts):
        if exp.face_src[k] != k or list(fv) != [rl[3 * k], rl[3 * k + 1], rl[3 * k + 2]]:
            return False, 0

    def same(a, b):
        return f32_bits(a[0]) == f32_bits(b[0]) and f32_bits(a[1]) == f32_bits(b[1]) and f32_bits(a[2]) == f32_bits(b[2])

    def same_uv(a, b):
        return f32_bits(a[0]) == f32_bits(b[0]) and f32_bits(a[1]) == f32_bits(b[1])

    new_skin_keys = skin_merge_keys(skin) if skin is not None else []
    for i in range(nv):
        o = exp.orig_vert[i]
        t = m.tangents[i]
        w = -1.0 if t[3] < 0 else 1.0
        if (not same(exp.positions[i], m.verts[i]) or not same_uv(exp.uvs[i], m.tverts[i])
                or not same(exp.normals[i], m.normals[i]) or not same(tan[i], t) or hand[i] != w):
            return False, 0
        if m.colors and not same(exp.colors[i], m.colors[i]):
            return False, 0
        for lst, el in ((m.tverts1, exp.uvs1), (m.tverts2, exp.uvs2), (m.tverts3, exp.uvs3)):
            if lst and not same_uv(el[i], lst[i]):
                return False, 0
        if new_skin_keys and new_skin_keys[o] != skin_keys[i]:
            return False, 0
        if new_constraints and f32_bits(new_constraints[o]) != f32_bits(constraints[i]):
            return False, 0

    m.verts, m.tverts, m.normals, m.colors = c.verts, c.tverts, c.normals, c.colors
    m.tverts1, m.tverts2, m.tverts3 = c.tverts1, c.tverts2, c.tverts3
    m.tangents = []
    m.faces = faces
    if n.skin is not None and new_weights is not None:
        n.skin.weights = new_weights
    if n.dangly is not None and new_constraints is not None:
        n.dangly.constraints = new_constraints
    return True, nv - len(keep)


def _copy_mesh(m):
    c = type(m).__new__(type(m))
    c.__dict__.update(m.__dict__)
    return c
