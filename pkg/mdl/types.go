// Package mdl provides types and operations for Neverwinter Nights MDL model files.
//
// The data model is designed from the binary MDL spec outward. Node capabilities
// use composition: a base Node struct holds optional capability struct pointers
// (nil = absent), mirroring the binary content_node bitfield.
//
// Key references:
//   - Binary format: xoreos-docs/templates/NWN1MDL.bt
//   - EE extensions: plenarius/cleanmodels/load_binary.pl
//   - ASCII parameters: plenarius/cleanmodels/load_models.pl paramtype/3 (lines 648-898)
//   - ASCII output: plenarius/cleanmodels/output_models.pl
package mdl

import (
	"encoding/json"
	"fmt"
	"strings"
)

const TileAnimMinLength float32 = 0.03333333

// Vec3 is a 3-component float vector used for positions, colors, normals.
type Vec3 struct {
	X, Y, Z float32
}

func (v Vec3) Index(axis int) float32 {
	switch axis {
	case 0: return v.X
	case 1: return v.Y
	default: return v.Z
	}
}

func (v *Vec3) SetIndex(axis int, val float32) {
	switch axis {
	case 0: v.X = val
	case 1: v.Y = val
	default: v.Z = val
	}
}

// Vec4 is a 4-component float vector used for orientations and tangents.
type Vec4 struct {
	X, Y, Z, W float32
}

// Face represents a single triangle in a mesh.
// Ref: NWN1MDL.bt mesh_face (line ~405)
type Face struct {
	Verts       [3]int32
	SmoothGroup int32
	UVs         [3]int32
	Material    int32
	Normal      Vec3
	PlaneD      float32
}

// VertexWeight holds bone weight data for a single vertex in a skinmesh.
// Each vertex can reference up to 4 bones.
// Ref: nwn.wiki "Model Table of Parameters" -- weights
type VertexWeight struct {
	Bones   []string
	Weights []float32
}

// MeshData holds geometry and material properties for mesh-capable nodes.
// Present on: trimesh, animmesh, danglymesh, skin, aabb.
// Ref: NWN1MDL.bt header_mesh (line 370)
// EE extensions: load_binary.pl get_common_mesh_data (line 467)
// NewMeshData returns a MeshData with sensible defaults (Render=1, Alpha=1.0).
func NewMeshData() *MeshData {
	return &MeshData{Render: 1, Alpha: 1.0}
}

type MeshData struct {
	Verts         []Vec3
	Faces         []Face
	Normals       []Vec3
	CornerNormals [][3]Vec3 // per-face-corner normals from smoothing groups (compiler-generated)
	Tangents      []Vec4
	Colors        []Vec3
	TVerts   []Vec3
	TVerts1  []Vec3
	TVerts2  []Vec3
	TVerts3  []Vec3

	TexIndices0 [][3]int32
	TexIndices1 [][3]int32
	TexIndices2 [][3]int32
	TexIndices3 [][3]int32

	Diffuse          Vec3
	Ambient          Vec3
	Specular         Vec3
	Shininess        float32
	Bitmap           string
	Texture1         string
	Texture2         string
	MaterialName     string // EE: occupies original texture3[64] slot
	RenderHint       string // EE: 0=none, 2=NormalAndSpecMapped
	Shadow           int32
	Beaming          int32
	Render           int32
	TransparencyHint int32
	Alpha            float32
	SelfIllumColor   Vec3
	TileFade         int32
	RotateTexture    int32
	LightMapped      int32 // EE: offset after RotateTexture

	Multimaterial []string
}

// SkinData holds bone weight data for skinmesh nodes.
// Ref: NWN1MDL.bt header_skin (line 501)
type SkinData struct {
	Weights []VertexWeight
}

// DanglyData holds danglymesh physics properties.
// Ref: NWN1MDL.bt header_dangly (line 586)
type DanglyData struct {
	Displacement float32
	Tightness    float32
	Period       float32
	DisplType    int32
	Constraints  []float32
}

// EmitterData holds particle emitter properties (~60 params).
// Ref: NWN1MDL.bt header_emitter (line 321)
// Ref: nwn.wiki "MDL ASCII Emitter Nodes"
// Ref: load_binary.pl get_node_specific_data(5) (line 378)
type EmitterData struct {
	DeadSpace   float32
	BlastRadius float32
	BlastLength float32
	XGrid       int32
	YGrid       int32
	SpawnType   int32
	Update      string
	Render      string
	Blend       string
	Texture     string
	ChunkName   string
	TwoSidedTex int32
	Loop        int32
	RenderOrder int32

	// Flags from EmitterFlags bitfield (load_binary.pl line 396)
	P2P          int32
	P2PSel       int32
	AffectedByWind int32
	IsTinted     int32 // m_isTinted in Prolog
	Bounce       int32
	Random       int32
	Inherit      int32
	InheritVel   int32
	InheritLocal int32
	Splat        int32
	InheritPart  int32

	// NWMax-only properties (not exported by NWMax but accepted on import)
	P2PType     string
	RenderSel   int32
	BlendSel    int32
	UpdateSel   int32
	SpawnTypeSel int32
	Opacity     int32
	IconSize    int32
	LockAxes    int32
	Chunky      int32

	// Controller-animatable values
	AlphaStart float32
	AlphaMid   float32
	AlphaEnd   float32
	ColorStart Vec3
	ColorMid   Vec3
	ColorEnd   Vec3
	SizeStart  float32
	SizeMid    float32
	SizeEnd    float32
	SizeStartY float32
	SizeMidY   float32
	SizeEndY   float32
	BirthRate  float32
	LifeExp    float32
	Mass       float32
	Spread     float32
	ParticleRot float32
	Velocity   float32
	RandVel    float32
	BounceCo   float32
	BlurLength float32
	FPS        float32
	FrameStart float32
	FrameEnd   float32
	Grav       float32
	Drag       float32
	Threshold  float32
	CombineTime float32
	PercentStart float32
	PercentMid   float32
	PercentEnd   float32
	LightningDelay  float32
	LightningRadius float32
	LightningScale  float32
	LightningSubDiv float32
	P2PBezier2 float32
	P2PBezier3 float32
	XSize      float32
	YSize      float32
}

// LightData holds light node properties.
// Ref: NWN1MDL.bt header_light (line 259)
// Ref: load_binary.pl get_node_specific_data(3) (line 343)
type LightData struct {
	FlareRadius      float32
	LensFlares       int32
	FlareSizes       []float32
	FlarePositions   []float32
	FlareColorShifts []Vec3
	TextureNames     []string
	LightPriority    int32
	AmbientOnly      int32
	NDynamicType     int32
	AffectDynamic    int32
	Shadow           int32
	GenerateFlare    int32
	FadingLight      int32
	NegativeLight    int32 // NWMax only, not compiled

	// Controller values
	Color                 Vec3
	Radius                float32
	Multiplier            float32
	ShadowRadius          float32
	VerticalDisplacement  float32

}

// ReferenceData holds model reference properties.
// Ref: NWN1MDL.bt header_reference (line 343)
// Ref: load_binary.pl get_node_specific_data(17) (line 409)
type ReferenceData struct {
	RefModel     string
	Reattachable int32
}

// AabbEntry represents one node in the AABB binary tree.
// Ref: NWN1MDL.bt entry_aabb (line 605)
type AabbEntry struct {
	BoundMin Vec3
	BoundMax Vec3
	LeafFace int32
	Plane    uint32
}

// AabbData holds walkmesh AABB tree data for aabb nodes.
type AabbData struct {
	Entries []AabbEntry
}

// AnimMeshData holds animmesh-specific properties.
// Ref: NWN1MDL.bt header_anim (line 561)
// Ref: load_models.pl paramtype(animmesh,...) (line 696)
type AnimMeshData struct {
	SamplePeriod float32
	ClipU        float32
	ClipV        float32
	ClipW        float32
	ClipH        float32
	AnimVerts    []Vec3
	AnimTVerts   []Vec3
}

// Node represents a single node in the model hierarchy.
// Capability struct pointers are nil when absent, mirroring the
// binary content_node bitfield (NWN1MDL.bt line 202).
// Ref: load_binary.pl node_type/2 (line 325)
type Node struct {
	Name         string
	Parent       string
	PartNumber   int32
	Position     Vec3
	Orientation  Vec4
	Scale        float32
	InheritColor int32
	WireColor    Vec3

	Mesh      *MeshData
	Skin      *SkinData
	Dangly    *DanglyData
	Emitter   *EmitterData
	Light     *LightData
	Reference *ReferenceData
	Aabb      *AabbData
	AnimMesh  *AnimMeshData
	Camera    bool
}

// NodeType returns the MDL ASCII node type keyword for this node.
// Mirrors load_binary.pl node_type/2 (line 325).
func (n *Node) NodeType() string {
	switch {
	case n.Aabb != nil:
		return "aabb"
	case n.Dangly != nil:
		return "danglymesh"
	case n.Skin != nil:
		return "skin"
	case n.AnimMesh != nil:
		return "animmesh"
	case n.Emitter != nil:
		return "emitter"
	case n.Light != nil:
		return "light"
	case n.Reference != nil:
		return "reference"
	case n.Camera:
		return "camera"
	case n.Mesh != nil:
		return "trimesh"
	default:
		return "dummy"
	}
}

// IsShadowCaster returns true if this node is a mesh type that casts shadows.
func (n *Node) IsShadowCaster() bool {
	if n.Mesh == nil || n.Mesh.Shadow != 1 {
		return false
	}
	switch n.NodeType() {
	case "trimesh", "skin", "danglymesh", "animmesh":
		return true
	}
	return false
}

// NodeTypeFlag returns the binary node type flag.
// Ref: load_binary.pl node_type/2 (line 325)
func (n *Node) NodeTypeFlag() uint32 {
	switch {
	case n.Aabb != nil:
		return 545
	case n.Dangly != nil:
		return 289
	case n.AnimMesh != nil:
		return 161
	case n.Skin != nil:
		return 97
	case n.Mesh != nil:
		return 33
	case n.Reference != nil:
		return 17
	case n.Camera:
		return 9
	case n.Emitter != nil:
		return 5
	case n.Light != nil:
		return 3
	default:
		return 1
	}
}

// PositionKey is a time-keyed position controller value.
type PositionKey struct {
	Time  float32
	Value Vec3
}

// OrientationKey is a time-keyed orientation controller value.
type OrientationKey struct {
	Time  float32
	Value Vec4
}

// FloatKey is a time-keyed float controller value (alpha, radius, etc.)
type FloatKey struct {
	Time  float32
	Value float32
}

// ColorKey is a time-keyed color controller value.
type ColorKey struct {
	Time  float32
	Value Vec3
}

// AnimNode represents a node's animation data within a newanim block.
// Each field holds time-keyed controller data for animatable properties.
// Ref: load_binary.pl controller_type/3 (lines 1196-1257)
type AnimNode struct {
	Name   string
	Parent string

	// Base node controllers
	PositionKeys    []PositionKey
	OrientationKeys []OrientationKey
	ScaleKeys       []FloatKey

	// Mesh controllers
	AlphaKeys          []FloatKey
	SelfIllumColorKeys []ColorKey

	// Light controllers
	ColorKeys              []ColorKey
	RadiusKeys             []FloatKey
	MultiplierKeys         []FloatKey
	ShadowRadiusKeys       []FloatKey
	VerticalDisplacementKeys []FloatKey

	// Emitter controllers
	AlphaStartKeys      []FloatKey
	AlphaMidKeys        []FloatKey
	AlphaEndKeys        []FloatKey
	BirthRateKeys       []FloatKey
	BlurLengthKeys      []FloatKey
	BounceCoKeys        []FloatKey
	ColorStartKeys      []ColorKey
	ColorMidKeys        []ColorKey
	ColorEndKeys        []ColorKey
	CombineTimeKeys     []FloatKey
	DragKeys            []FloatKey
	FPSKeys             []FloatKey
	FrameStartKeys      []FloatKey
	FrameEndKeys        []FloatKey
	GravKeys            []FloatKey
	LifeExpKeys         []FloatKey
	LightningDelayKeys  []FloatKey
	LightningRadiusKeys []FloatKey
	LightningScaleKeys  []FloatKey
	LightningSubDivKeys []FloatKey
	MassKeys            []FloatKey
	P2PBezier2Keys      []FloatKey
	P2PBezier3Keys      []FloatKey
	ParticleRotKeys     []FloatKey
	PercentStartKeys    []FloatKey
	PercentMidKeys      []FloatKey
	PercentEndKeys      []FloatKey
	RandVelKeys         []FloatKey
	SizeStartKeys       []FloatKey
	SizeMidKeys         []FloatKey
	SizeEndKeys         []FloatKey
	SizeStartYKeys      []FloatKey
	SizeMidYKeys        []FloatKey
	SizeEndYKeys        []FloatKey
	SpreadKeys          []FloatKey
	ThresholdKeys       []FloatKey
	VelocityKeys        []FloatKey
	XSizeKeys           []FloatKey
	YSizeKeys           []FloatKey
	DetonateKeys        []FloatKey

	// AnimMesh data (populated for animmesh animation nodes from binary or ASCII)
	AnimMesh *AnimMeshData
	Mesh     *MeshData
}

// AnimEvent is a timed event within an animation.
type AnimEvent struct {
	Time float32
	Name string
}

// Animation represents a single named animation on a model.
// Ref: load_binary.pl get_animation_header (line 144)
type Animation struct {
	Name      string
	Length    float32
	TransTime float32
	Root      string
	Events    []AnimEvent
	Nodes     []AnimNode
}

// Model represents a complete parsed MDL model.
// Ref: load_binary.pl get_model_header (line 113)
type Model struct {
	Name            string
	SuperModel      string
	Classification  string
	AnimationScale  float32
	IgnoreFog       int32
	FileDependancy  string
	FileType        string // "binary" or "ascii"
	UseTexture0     bool   // emit "texture0" instead of "bitmap" in ASCII output

	Nodes      []*Node
	Animations []Animation
	Warnings   []DecompileWarning
}

// FindNode returns the first node with the given name, or nil.
func (m *Model) FindNode(name string) *Node {
	for _, n := range m.Nodes {
		if strings.EqualFold(n.Name, name) {
			return n
		}
	}
	return nil
}

// RootNode returns the first node whose parent is "NULL", or nil.
func (m *Model) RootNode() *Node {
	for _, n := range m.Nodes {
		if strings.EqualFold(n.Parent, "NULL") {
			return n
		}
	}
	return nil
}

// Severity represents the severity level of a check result.
type Severity int

const (
	SevInfo Severity = iota
	SevWarning
	SevError
	SevFatal
)

var severityNames = [...]string{
	SevInfo:    "info",
	SevWarning: "warning",
	SevError:   "error",
	SevFatal:   "fatal",
}

func (s Severity) String() string {
	if int(s) < len(severityNames) {
		return severityNames[s]
	}
	return fmt.Sprintf("severity(%d)", s)
}

func (s Severity) MarshalJSON() ([]byte, error) {
	return json.Marshal(s.String())
}

func (s *Severity) UnmarshalJSON(data []byte) error {
	var str string
	if err := json.Unmarshal(data, &str); err != nil {
		var num int
		if numErr := json.Unmarshal(data, &num); numErr != nil {
			return err
		}
		*s = Severity(num)
		return nil
	}
	for i, name := range severityNames {
		if name == str {
			*s = Severity(i)
			return nil
		}
	}
	return fmt.Errorf("unknown severity: %q", str)
}

// CheckResult represents a single issue found by a check.
type CheckResult struct {
	Check    string   `json:"check"`
	Node     string   `json:"node,omitempty"`
	Severity Severity `json:"severity"`
	Message  string   `json:"message"`
	Fixed    bool     `json:"fixed"`
}

// DecompileWarnKind categorizes decompilation warnings.
type DecompileWarnKind int

const (
	WarnUnknownController  DecompileWarnKind = 0
	WarnPointerOutOfBounds DecompileWarnKind = 2
	WarnGeneral            DecompileWarnKind = 4
	WarnTruncatedData      DecompileWarnKind = 6
)

// DecompileWarning represents an issue found during binary decompilation.
// Accumulated on the Model, never causing an abort.
type DecompileWarning struct {
	Kind    DecompileWarnKind `json:"kind"`
	Offset  int64             `json:"offset"`
	Node    string            `json:"node,omitempty"`
	Message string            `json:"message"`
}

// CheckFunc is the signature for all model checks.
// Each check examines a model and returns zero or more results.
// When fix is true, the check may mutate the model to repair issues.
type CheckFunc func(model *Model, file string, fix bool) []CheckResult

// ClassificationFromCode converts a binary classification code to its string name.
// Ref: load_binary.pl classification_code/2 (line 176)
func ClassificationFromCode(code int) string {
	switch code {
	case 1:
		return "EFFECT"
	case 2:
		return "TILE"
	case 4:
		return "CHARACTER"
	case 8:
		return "DOOR"
	default:
		return "OTHER"
	}
}

// ClassificationToCode converts a classification string to its binary code.
//
// Accepts the plural forms ("Effects", "Tiles", ...) that NWMax and other
// common exporters write, not just the singular canonical names — otherwise
// they silently fall through to 0 ("OTHER") with no warning. That's exactly
// what happened to vdr_magearmor.mdl (issue #12): its ASCII source declares
// "classification Effects", which the parser uppercases to "EFFECTS" — one
// letter away from the "EFFECT" this switch used to require — so the
// compiled binary's m_nTypeMask silently ended up 0 instead of 1. Verified
// against the retail vdr_magearmor2.mdl, whose compiled binary has
// m_nTypeMask=1.
func ClassificationToCode(class string) int {
	class = strings.TrimSuffix(class, "S")
	switch class {
	case "EFFECT":
		return 1
	case "TILE":
		return 2
	case "CHARACTER":
		return 4
	case "DOOR":
		return 8
	default:
		return 0
	}
}
