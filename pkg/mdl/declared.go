package mdl

// Declared tracks which controller-backed properties of a node were present in
// the source — an ASCII line, or a controller in a binary — as opposed to
// merely holding their default value.
//
// The in-game compiler writes a controller for a property exactly when the
// ASCII has a line for it, whatever its value: across the 26 oracle models,
// 248 trimesh nodes agreed on scale/alpha/selfillumcolor with no exceptions,
// as did every light property and 1023 of 1025 emitter properties. NWmax
// exports every emitter property, zeros included, so skipping default values
// (as we used to) left the compiled node 20 controllers short of the engine's.
// The engine appears to rely on such controllers to initialise node state (see
// the position/orientation note in compiler_controllers.go and issue #12), so
// the compiler emits a controller for every declared property.

// lightDeclaredNames are the light properties compiled to controllers.
var lightDeclaredNames = map[string]bool{
	"color": true, "radius": true, "multiplier": true,
	"shadowradius": true, "verticaldisplacement": true,
}

// emitterDeclaredNames are the emitter properties the engine compiles to
// controllers whenever the ASCII names them. Left out:
//   - mid/percent values: BioWare-only controller IDs the engine's compile of
//     NWmax models never wrote;
//   - lightningsubdiv: the engine did not write it for an explicit
//     "lightningSubDiv 0" (c_marilith2, both eyeglow nodes), so it keeps the
//     emit-only-if-non-zero rule.
var emitterDeclaredNames = map[string]bool{}

func init() {
	for k, def := range nodeControllers {
		if k.NodeFlag == 5 && def.Name != "detonate" && def.Name != "lightningsubdiv" {
			emitterDeclaredNames[def.Name] = true
		}
	}
}

// declaredName maps an ASCII keyword on node n to the canonical name used in
// Declared, or reports false if the keyword is not a controller-backed
// property of this node.
func (n *Node) declaredName(keyword string) (string, bool) {
	switch {
	case keyword == "scale":
		return keyword, true
	case keyword == "setfillumcolor" && n.Mesh != nil:
		return "selfillumcolor", true
	case (keyword == "alpha" || keyword == "selfillumcolor") && n.Mesh != nil:
		return keyword, true
	case n.Light != nil && lightDeclaredNames[keyword]:
		return keyword, true
	case n.Emitter != nil && emitterDeclaredNames[keyword]:
		return keyword, true
	}
	return "", false
}

func (n *Node) declare(name string) {
	if n.Declared == nil {
		n.Declared = make(map[string]bool)
	}
	n.Declared[name] = true
}

func (n *Node) isDeclared(name string) bool { return n.Declared[name] }
