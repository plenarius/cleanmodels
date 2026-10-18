"""Controller type IDs: a port of pkg/mdl/controllers.go and declared.go."""

COMPILER_UNKNOWN = 0
COMPILER_BIOWARE = 1
COMPILER_NWNMDLCOMP = 2

# (name, number of float columns per key row; -1 for time-only)
UNIVERSAL = {8: ("position", 3), 20: ("orientation", 4), 36: ("scale", 1)}

MESH = {100: ("selfillumcolor", 3), 128: ("alpha", 1)}

NODE = {
    # Light controllers (node type flag 3)
    (76, 3): ("color", 3),
    (88, 3): ("radius", 1),
    (140, 3): ("multiplier", 1),
    (96, 3): ("shadowradius", 1),
    (100, 3): ("verticaldisplacement", 1),
    # Emitter controllers (node type flag 5), compiler-independent
    (84, 5): ("alphastart", 1),
    (80, 5): ("alphaend", 1),
    (88, 5): ("birthrate", 1),
    (204, 5): ("blurlength", 1),
    (92, 5): ("bounce_co", 1),
    (108, 5): ("colorstart", 3),
    (96, 5): ("colorend", 3),
    (120, 5): ("combinetime", 1),
    (228, 5): ("detonate", -1),
    (124, 5): ("drag", 1),
    (128, 5): ("fps", 1),
    (136, 5): ("framestart", 1),
    (132, 5): ("frameend", 1),
    (140, 5): ("grav", 1),
    (144, 5): ("lifeexp", 1),
    (208, 5): ("lightningdelay", 1),
    (212, 5): ("lightningradius", 1),
    (216, 5): ("lightningscale", 1),
    (220, 5): ("lightningsubdiv", 1),
    (148, 5): ("mass", 1),
    (152, 5): ("p2p_bezier2", 1),
    (156, 5): ("p2p_bezier3", 1),
    (160, 5): ("particlerot", 1),
    (164, 5): ("randvel", 1),
    (168, 5): ("sizestart", 1),
    (172, 5): ("sizeend", 1),
    (176, 5): ("sizestart_y", 1),
    (180, 5): ("sizeend_y", 1),
    (184, 5): ("spread", 1),
    (188, 5): ("threshold", 1),
    (192, 5): ("velocity", 1),
    (196, 5): ("xsize", 1),
    (200, 5): ("ysize", 1),
}

BIOWARE_EMITTER = {
    448: ("alphamid", 1), 452: ("colormid", 3), 464: ("percentstart", 1), 465: ("percentmid", 1),
    466: ("percentend", 1), 468: ("sizemid", 1), 472: ("sizemid_y", 1),
}

NWNMDLCOMP_EMITTER = {
    464: ("alphamid", 1), 468: ("colormid", 3), 480: ("percentstart", 1), 481: ("percentmid", 1),
    482: ("percentend", 1), 484: ("sizemid", 1), 488: ("sizemid_y", 1),
}


def controller_id(type_id, node_flag, compiler):
    """Return (name, num_cols) for a controller type on a node type, or None."""
    if node_flag == 5:
        if compiler in (COMPILER_BIOWARE, COMPILER_UNKNOWN) and type_id in BIOWARE_EMITTER:
            return BIOWARE_EMITTER[type_id]
        if compiler in (COMPILER_NWNMDLCOMP, COMPILER_UNKNOWN) and type_id in NWNMDLCOMP_EMITTER:
            return NWNMDLCOMP_EMITTER[type_id]
    d = NODE.get((type_id, node_flag))
    if d is not None:
        return d
    if node_flag & 32 == 32 and type_id in MESH:
        return MESH[type_id]
    return UNIVERSAL.get(type_id)


# declared.go: properties written even at their default value when the source
# declared them (they compile to controllers).
EMITTER_DECLARED_NAMES = frozenset(
    name for (tid, flag), (name, _) in NODE.items()
    if flag == 5 and name not in ("detonate", "lightningsubdiv"))
