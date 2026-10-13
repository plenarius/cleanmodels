package mdl

// Compiler identifies which compiler produced a binary MDL.
// BioWare and nwnmdlcomp use different controller type IDs for some
// emitter properties.
// Ref: load_binary.pl compiler/1 (line 17), controller_type/3 (lines 1196-1257)
type Compiler int

const (
	CompilerUnknown    Compiler = iota
	CompilerBioWare             // Original BioWare compiler, ships with the game
	CompilerNwnmdlcomp          // Community compiler (nwnmdlcomp)
)

// controllerKey uniquely identifies a controller mapping entry.
type controllerKey struct {
	TypeID   uint32
	NodeFlag uint32 // binary node type flag: 0 = all nodes
}

// ControllerDef defines a controller's parameter name and data width.
type ControllerDef struct {
	Name    string
	NumCols int // number of float columns per key row (1=float, 3=vec3, 4=quat/vec4)
}

// ControllerID returns the parameter name for a given controller type ID and
// binary node type flag. When the compiler affects the mapping (emitter mid/percent
// controllers), both bioware and nwnmdlcomp variants are checked.
// Returns the definition and true if found, or zero value and false.
//
// Ref: load_binary.pl controller_type/3 (lines 1196-1257)
func ControllerID(typeID uint32, nodeFlag uint32, compiler Compiler) (ControllerDef, bool) {
	// Emitters: check compiler-specific mappings first
	if nodeFlag == 5 {
		if compiler == CompilerBioWare || compiler == CompilerUnknown {
			if def, ok := biowareEmitterControllers[typeID]; ok {
				return def, true
			}
		}
		if compiler == CompilerNwnmdlcomp || compiler == CompilerUnknown {
			if def, ok := nwnmdlcompEmitterControllers[typeID]; ok {
				return def, true
			}
		}
	}

	// Check node-type-specific controllers
	if def, ok := nodeControllers[controllerKey{typeID, nodeFlag}]; ok {
		return def, true
	}

	// Check mesh controllers (any node with mesh flag bit 5 set = 32)
	if nodeFlag&32 == 32 {
		if def, ok := meshControllers[typeID]; ok {
			return def, true
		}
	}

	// Check universal controllers (position, orientation, scale)
	if def, ok := universalControllers[typeID]; ok {
		return def, true
	}

	return ControllerDef{}, false
}

// universalControllers apply to all node types.
// Ref: load_binary.pl controller_type(ID,_,Name) -- lines 1196-1198
var universalControllers = map[uint32]ControllerDef{
	8:  {Name: "position", NumCols: 3},
	20: {Name: "orientation", NumCols: 4},
	36: {Name: "scale", NumCols: 1},
}

// meshControllers apply to any node with the mesh bit set (flag & 32 == 32).
// Ref: load_binary.pl controller_type(ID,NodeType,...) -- lines 1254-1255
var meshControllers = map[uint32]ControllerDef{
	100: {Name: "selfillumcolor", NumCols: 3},
	128: {Name: "alpha", NumCols: 1},
}

// nodeControllers are specific to a single node type flag.
// Light controllers: Ref load_binary.pl controller_type(ID,3,...) lines 1200-1204
// Emitter controllers: Ref load_binary.pl controller_type(ID,5,...) lines 1206-1251
var nodeControllers = map[controllerKey]ControllerDef{
	// Light controllers (node type flag 3)
	{76, 3}:  {Name: "color", NumCols: 3},
	{88, 3}:  {Name: "radius", NumCols: 1},
	{140, 3}: {Name: "multiplier", NumCols: 1},
	{96, 3}:  {Name: "shadowradius", NumCols: 1},
	{100, 3}: {Name: "verticaldisplacement", NumCols: 1},

	// Emitter controllers (node type flag 5), compiler-independent
	{84, 5}:  {Name: "alphastart", NumCols: 1},
	{80, 5}:  {Name: "alphaend", NumCols: 1},
	{88, 5}:  {Name: "birthrate", NumCols: 1},
	{204, 5}: {Name: "blurlength", NumCols: 1},
	{92, 5}:  {Name: "bounce_co", NumCols: 1},
	{108, 5}: {Name: "colorstart", NumCols: 3},
	{96, 5}:  {Name: "colorend", NumCols: 3},
	{120, 5}: {Name: "combinetime", NumCols: 1},
	{228, 5}: {Name: "detonate", NumCols: -1},
	{124, 5}: {Name: "drag", NumCols: 1},
	{128, 5}: {Name: "fps", NumCols: 1},
	{136, 5}: {Name: "framestart", NumCols: 1},
	{132, 5}: {Name: "frameend", NumCols: 1},
	{140, 5}: {Name: "grav", NumCols: 1},
	{144, 5}: {Name: "lifeexp", NumCols: 1},
	{208, 5}: {Name: "lightningdelay", NumCols: 1},
	{212, 5}: {Name: "lightningradius", NumCols: 1},
	{216, 5}: {Name: "lightningscale", NumCols: 1},
	{220, 5}: {Name: "lightningsubdiv", NumCols: 1},
	{148, 5}: {Name: "mass", NumCols: 1},
	{152, 5}: {Name: "p2p_bezier2", NumCols: 1},
	{156, 5}: {Name: "p2p_bezier3", NumCols: 1},
	{160, 5}: {Name: "particlerot", NumCols: 1},
	{164, 5}: {Name: "randvel", NumCols: 1},
	{168, 5}: {Name: "sizestart", NumCols: 1},
	{172, 5}: {Name: "sizeend", NumCols: 1},
	{176, 5}: {Name: "sizestart_y", NumCols: 1},
	{180, 5}: {Name: "sizeend_y", NumCols: 1},
	{184, 5}: {Name: "spread", NumCols: 1},
	{188, 5}: {Name: "threshold", NumCols: 1},
	{192, 5}: {Name: "velocity", NumCols: 1},
	{196, 5}: {Name: "xsize", NumCols: 1},
	{200, 5}: {Name: "ysize", NumCols: 1},
}

// biowareEmitterControllers are emitter controller IDs specific to BioWare's compiler.
// These differ from nwnmdlcomp for mid/percent controllers.
// Ref: load_binary.pl controller_type(ID,5,Name) :- compiler(bioware) -- lines 1207-1246
var biowareEmitterControllers = map[uint32]ControllerDef{
	448: {Name: "alphamid", NumCols: 1},
	452: {Name: "colormid", NumCols: 3},
	464: {Name: "percentstart", NumCols: 1},
	465: {Name: "percentmid", NumCols: 1},
	466: {Name: "percentend", NumCols: 1},
	468: {Name: "sizemid", NumCols: 1},
	472: {Name: "sizemid_y", NumCols: 1},
}

// nwnmdlcompEmitterControllers are emitter controller IDs specific to nwnmdlcomp.
// Ref: load_binary.pl controller_type(ID,5,Name) :- compiler(nwnmdlcomp) -- lines 1208-1245
var nwnmdlcompEmitterControllers = map[uint32]ControllerDef{
	464: {Name: "alphamid", NumCols: 1},
	468: {Name: "colormid", NumCols: 3},
	480: {Name: "percentstart", NumCols: 1},
	481: {Name: "percentmid", NumCols: 1},
	482: {Name: "percentend", NumCols: 1},
	484: {Name: "sizemid", NumCols: 1},
	488: {Name: "sizemid_y", NumCols: 1},
}