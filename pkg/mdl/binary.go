// Binary MDL decompiler.
//
// Reads NWN binary (compiled) .mdl files and produces Model structs.
// All struct layouts are derived from NWN1MDL.bt (xoreos-docs).
// EE extensions from load_binary.pl.
//
// Pointer conventions (from NWN1MDL.bt):
//   - GoToPointer(p)    = FSeek(sizeof(header_file) + p) = seek to 12 + p
//   - GoToMDXPointer(p) = FSeek(sizeof(header_file) + hf.p_start_mdx + p) = seek to 12 + mdxOffset + p
//
// Ref: xoreos-docs/templates/NWN1MDL.bt
// Ref: load_binary.pl
package mdl

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"io"
	"math"
	"os"
	"strings"
)

// header_mesh layout constants (offsets from start of header_mesh).
// Ref: NWN1MDL.bt header_mesh (line 370)
const (
	meshHeaderSize       = 512
	meshHeaderPMdxVertex = 444 // int32 p_mdx_vertex
	meshHeaderCountVerts = 448 // uint16 count_vertexes
	meshHeaderPMdxTex0   = 452 // int32 p_mdx_texture0
)

// DecompileFile reads a binary MDL from disk.
//
// The full file is slurped into memory and parsed from a bytes.Reader.
// Binary MDLs top out in the low single-digit megabytes and the parser
// does many small (4-byte) random-access reads via binary.Read, so reading
// once and parsing in-memory avoids ~one syscall per field.
func DecompileFile(path string) (*Model, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Decompile(bytes.NewReader(data), int64(len(data)))
}

// Decompile reads a binary MDL from a ReadSeeker.
func Decompile(r io.ReadSeeker, fileSize int64) (*Model, error) {
	d := &decompiler{
		r:           r,
		fileSize:    fileSize,
		model:       &Model{AnimationScale: 1.0, FileType: "binary"},
		compiler:    CompilerUnknown,
		visitedPtrs: make(map[uint32]bool),
		maxAlloc:    fileSize * 16,
	}

	if err := d.readFileHeader(); err != nil {
		return nil, fmt.Errorf("file header: %w", err)
	}
	if err := d.readModelHeader(); err != nil {
		return nil, fmt.Errorf("model header: %w", err)
	}
	if d.rootNodePtr != 0 {
		if err := d.readNode(d.rootNodePtr, "NULL"); err != nil {
			d.warn(WarnTruncatedData, 0, "", "geometry read error: %v", err)
		}
	}
	seenAnims := make(map[uint32]bool)
	for _, ptr := range d.animPtrs {
		if ptr == 0 || seenAnims[ptr] {
			continue
		}
		seenAnims[ptr] = true
		d.visitedPtrs = make(map[uint32]bool)
		if err := d.readAnimation(ptr); err != nil {
			d.warn(WarnTruncatedData, int64(ptr), "", "animation read error: %v", err)
		}
	}

	for _, ps := range d.pendingSkins {
		d.resolveSkinWeights(ps)
	}

	return d.model, nil
}

type decompiler struct {
	r        io.ReadSeeker
	fileSize int64
	model    *Model
	compiler Compiler

	// header_file fields
	mdxOffset uint32 // hf.p_start_mdx
	mdxSize   uint32 // hf.size_mdx

	rootNodePtr uint32
	animPtrs    []uint32

	nodeOrder    []nodeInfo
	pendingSkins []pendingSkin
	visitedPtrs  map[uint32]bool

	totalAlloc int64
	maxAlloc   int64

	readErr error // first read error encountered, for truncation detection
}

type nodeInfo struct {
	name       string
	partNumber int32
}

type pendingSkin struct {
	node         *Node
	bonePartNums []int16
	skinWeights  [][4]float32
	skinBoneRefs [][4]int16
}

func (d *decompiler) trackAlloc(bytes int64) bool {
	d.totalAlloc += bytes
	return d.totalAlloc <= d.maxAlloc
}

func (d *decompiler) warn(kind DecompileWarnKind, offset int64, node string, format string, args ...interface{}) {
	d.model.Warnings = append(d.model.Warnings, DecompileWarning{
		Kind:    kind,
		Offset:  offset,
		Node:    node,
		Message: fmt.Sprintf(format, args...),
	})
}

// goToPointer converts a model-relative pointer to absolute file offset.
// Ref: NWN1MDL.bt GoToPointer: FSeek(sizeof(header_file) + p)
func goToPointer(p uint32) int64 {
	return 12 + int64(p)
}

// goToMDXPointer converts an MDX-relative pointer to absolute file offset.
// Ref: NWN1MDL.bt GoToMDXPointer: FSeek(sizeof(header_file) + hf.p_start_mdx + p)
func (d *decompiler) goToMDXPointer(p int32) int64 {
	if p < 0 {
		return -1
	}
	return 12 + int64(d.mdxOffset) + int64(p)
}

func (d *decompiler) seek(offset int64) error {
	if offset < 0 || offset > d.fileSize {
		return fmt.Errorf("seek out of bounds: %d (file size %d)", offset, d.fileSize)
	}
	_, err := d.r.Seek(offset, io.SeekStart)
	return err
}

func (d *decompiler) pos() int64 {
	pos, _ := d.r.Seek(0, io.SeekCurrent)
	return pos
}

func (d *decompiler) readU32() (uint32, error) {
	var v uint32
	err := binary.Read(d.r, binary.LittleEndian, &v)
	if err != nil && d.readErr == nil {
		d.readErr = err
	}
	return v, err
}

func (d *decompiler) readI32() (int32, error) {
	var v int32
	err := binary.Read(d.r, binary.LittleEndian, &v)
	if err != nil && d.readErr == nil {
		d.readErr = err
	}
	return v, err
}

func (d *decompiler) readU32AsI32() int32 {
	v, _ := d.readU32()
	return int32(v)
}

func (d *decompiler) readU16() (uint16, error) {
	var v uint16
	err := binary.Read(d.r, binary.LittleEndian, &v)
	if err != nil && d.readErr == nil {
		d.readErr = err
	}
	return v, err
}

func (d *decompiler) readI16() (int16, error) {
	var v int16
	err := binary.Read(d.r, binary.LittleEndian, &v)
	if err != nil && d.readErr == nil {
		d.readErr = err
	}
	return v, err
}

func (d *decompiler) readF32() (float32, error) {
	var bits uint32
	if err := binary.Read(d.r, binary.LittleEndian, &bits); err != nil {
		if d.readErr == nil {
			d.readErr = err
		}
		return 0, err
	}
	v := math.Float32frombits(bits)
	if math.IsNaN(float64(v)) || math.IsInf(float64(v), 0) {
		d.warn(WarnGeneral, d.pos()-4, "", "NaN/Inf float replaced with 0 (raw bits: 0x%08X)", bits)
		return 0, nil
	}
	return v, nil
}

// readVec3 reads three consecutive float32 values into a Vec3.
func (d *decompiler) readVec3() Vec3 {
	x, _ := d.readF32()
	y, _ := d.readF32()
	z, _ := d.readF32()
	return Vec3{X: x, Y: y, Z: z}
}

func (d *decompiler) readByte() (byte, error) {
	var v byte
	err := binary.Read(d.r, binary.LittleEndian, &v)
	if err != nil && d.readErr == nil {
		d.readErr = err
	}
	return v, err
}

func (d *decompiler) skip(n int) error {
	_, err := d.r.Seek(int64(n), io.SeekCurrent)
	return err
}

func (d *decompiler) readFixedString(n int) (string, error) {
	buf := make([]byte, n)
	if _, err := io.ReadFull(d.r, buf); err != nil {
		return "", err
	}
	end := 0
	for end < n && buf[end] != 0 {
		end++
	}
	clean := make([]byte, 0, end)
	for i := 0; i < end; i++ {
		if buf[i] >= 0x20 && buf[i] < 0x7F {
			clean = append(clean, buf[i])
		}
	}
	return strings.ToLower(string(clean)), nil
}

// readArrayDef reads a 12-byte array_definition {p_array_start, nr_used, nr_alloc}.
// Ref: NWN1MDL.bt array_definition (line 76)
type binArrayDef struct {
	Ptr   uint32
	Count uint32
	Alloc uint32
}

func (d *decompiler) readArrayDef() (binArrayDef, error) {
	var a binArrayDef
	if err := binary.Read(d.r, binary.LittleEndian, &a); err != nil {
		return a, err
	}
	return a, nil
}

// readPointerArray reads an array of uint32 pointers from an array_definition.
func (d *decompiler) readPointerArray(a binArrayDef) ([]uint32, error) {
	if a.Count == 0 || a.Ptr == 0 {
		return nil, nil
	}
	pos := d.pos()
	absPtr := goToPointer(a.Ptr)
	if absPtr+int64(a.Count)*4 > d.fileSize {
		return nil, fmt.Errorf("array pointer out of bounds")
	}
	if !d.trackAlloc(int64(a.Count) * 4) {
		return nil, fmt.Errorf("pointer array allocation exceeds limit")
	}
	if err := d.seek(absPtr); err != nil {
		return nil, err
	}
	ptrs := make([]uint32, a.Count)
	if err := binary.Read(d.r, binary.LittleEndian, &ptrs); err != nil {
		return nil, err
	}
	if err := d.seek(pos); err != nil {
		return ptrs, err
	}
	return ptrs, nil
}

// readFileHeader reads the 12-byte header_file.
// Ref: NWN1MDL.bt header_file (line 97)
func (d *decompiler) readFileHeader() error {
	if err := d.seek(0); err != nil {
		return err
	}
	var binMdlID uint32
	if err := binary.Read(d.r, binary.LittleEndian, &binMdlID); err != nil {
		return err
	}
	if err := binary.Read(d.r, binary.LittleEndian, &d.mdxOffset); err != nil {
		return err
	}
	if err := binary.Read(d.r, binary.LittleEndian, &d.mdxSize); err != nil {
		return err
	}
	return nil
}

// readModelHeader reads header_model (which contains header_geometry).
// Ref: NWN1MDL.bt header_geometry (line 103) + header_model (line 116)
//
// Some community models have a corrupted or non-standard header (e.g. shifted
// by 4 bytes, invalid pointers). When detected, the decompiler recovers the
// model name via heuristic and returns a partial (empty-geometry) model with
// a warning instead of aborting.
func (d *decompiler) readModelHeader() error {
	if err := d.seek(12); err != nil {
		return err
	}

	// -- header_geometry --
	d.skip(8) // p_func1, p_func2

	modelName, err := d.readFixedString(64)
	if err != nil {
		return err
	}

	rootPtr, err := d.readU32()
	if err != nil {
		return err
	}

	countNodes, _ := d.readU32()

	// Sanity check: if rootPtr is 0 and countNodes is 0 but the file is large
	// enough to contain a model, the header may be misaligned. Try to recover
	// the model name from a nearby offset.
	if rootPtr == 0 && countNodes == 0 && d.fileSize > 256 {
		recovered := d.tryRecoverName(modelName)
		if recovered != "" {
			d.model.Name = recovered
			d.warn(WarnTruncatedData, 12, "", "non-standard header layout, recovered name '%s'; geometry unavailable", recovered)
			d.model.SuperModel = "NULL"
			d.model.AnimationScale = 1.0
			d.rootNodePtr = 0
			return nil
		}
	}

	d.model.Name = modelName
	d.rootNodePtr = rootPtr

	d.skip(12) // unknown1
	d.skip(12) // unknown2
	d.skip(4)  // ref_count
	d.skip(4)  // type + padding

	// -- header_model --
	d.skip(1) // unknown0
	d.skip(1) // unknown1

	classCode, err := d.readByte()
	if err != nil {
		return err
	}
	d.model.Classification = ClassificationFromCode(int(classCode))

	fog, err := d.readByte()
	if err != nil {
		return err
	}
	d.model.IgnoreFog = int32(fog)

	d.skip(4) // count_child_model

	animArrayDef, err := d.readArrayDef()
	if err != nil {
		return err
	}
	animPtrs, err := d.readPointerArray(animArrayDef)
	if err != nil {
		d.warn(WarnPointerOutOfBounds, int64(animArrayDef.Ptr), modelName, "animation array out of bounds, skipping animations")
		animPtrs = nil
	}
	d.animPtrs = animPtrs

	d.skip(4)  // p_supermodel
	d.skip(24) // bounds
	d.skip(4)  // radius

	animScale, err := d.readF32()
	if err != nil {
		return err
	}
	d.model.AnimationScale = animScale

	superModel, err := d.readFixedString(64)
	if err != nil {
		return err
	}
	if superModel == "" {
		superModel = "NULL"
	}
	d.model.SuperModel = superModel

	return nil
}

// tryRecoverName attempts to find a valid model name when the standard header
// parse yields garbage. Checks alternate offsets where names appear in known
// non-standard community binary formats (e.g. 4-byte shifted headers).
func (d *decompiler) tryRecoverName(standardName string) string {
	// If the standard name already looks valid, no recovery needed
	if len(standardName) > 3 && !strings.ContainsAny(standardName, "\x00") {
		return ""
	}

	// Try reading name at offset 16 (4 bytes before standard position)
	// This handles models with a missing p_func2 or different header_file size
	if d.fileSize > 80 {
		if err := d.seek(16); err == nil {
			if name, err := d.readFixedString(64); err == nil && len(name) > 3 {
				allPrintable := true
				for _, c := range name {
					if c < 32 || c > 126 {
						allPrintable = false
						break
					}
				}
				if allPrintable {
					return name
				}
			}
		}
	}
	return ""
}

// readNode reads a complete node (header + type-specific data + children).
// Ref: NWN1MDL.bt node struct (line 634)
const maxReadNodeDepth = 256

func (d *decompiler) readNode(nodePtr uint32, parentName string) error {
	return d.readNodeDepth(nodePtr, parentName, 0)
}

func (d *decompiler) readNodeDepth(nodePtr uint32, parentName string, depth int) error {
	if nodePtr == 0 {
		return nil
	}
	if depth > maxReadNodeDepth {
		d.warn(WarnGeneral, 0, parentName, "node tree exceeds max depth %d; truncating", maxReadNodeDepth)
		return nil
	}
	if d.visitedPtrs[nodePtr] {
		d.warn(WarnPointerOutOfBounds, int64(nodePtr), "", "cycle detected at node pointer")
		return nil
	}
	d.visitedPtrs[nodePtr] = true

	absPtr := goToPointer(nodePtr)
	if absPtr >= d.fileSize {
		return fmt.Errorf("node pointer %d out of bounds", nodePtr)
	}
	if err := d.seek(absPtr); err != nil {
		return err
	}

	// -- header_node (line 215) --
	// uint32 p_func1..p_func6 (6 * 4 = 24)
	d.skip(24)

	// uint32 color_inherit
	inheritColor, _ := d.readI32()

	// uint32 node_number
	nodeNumber, _ := d.readI32()

	// char node_name[32]
	nodeName, _ := d.readFixedString(32)

	// uint32 p_geometry, p_parent_node (2 * 4 = 8)
	d.skip(8)

	// array_definition children (12 bytes)
	childrenDef, _ := d.readArrayDef()

	// array_definition controller_keys (12 bytes)
	ctrlKeysDef, _ := d.readArrayDef()

	// array_definition controller_data (12 bytes)
	ctrlDataDef, _ := d.readArrayDef()

	// content_node (uint32 bitfield)
	contentBits, _ := d.readU32()

	node := &Node{
		Name:         nodeName,
		Parent:       parentName,
		PartNumber:   nodeNumber,
		InheritColor: inheritColor,
		Scale:        1.0,
		Orientation:  Vec4{W: 1.0},
	}
	d.nodeOrder = append(d.nodeOrder, nodeInfo{name: nodeName, partNumber: nodeNumber})

	// Parse content_node bitfield (line 202):
	// bit 0: has_header (always 1)
	// bit 1: has_light
	// bit 2: has_emitter
	// bit 3: has_camera (unused)
	// bit 4: has_reference
	// bit 5: has_mesh
	// bit 6: has_skin
	// bit 7: has_anim
	// bit 8: has_dangly
	// bit 9: has_aabb
	hasLight := contentBits&0x02 != 0
	hasEmitter := contentBits&0x04 != 0
	hasCamera := contentBits&0x08 != 0
	hasReference := contentBits&0x10 != 0
	hasMesh := contentBits&0x20 != 0
	hasSkin := contentBits&0x40 != 0
	hasAnim := contentBits&0x80 != 0
	hasDangly := contentBits&0x100 != 0
	hasAABB := contentBits&0x200 != 0
	_ = contentBits & 0x400 // trigger bit (binary-only, decompiled as trimesh)

	// Type-specific headers follow the node header in this exact order per the spec.
	// Ref: NWN1MDL.bt node struct (line 634-728)

	if hasLight {
		node.Light = &LightData{}
		d.readLightHeader(node)
	}
	if hasEmitter {
		node.Emitter = &EmitterData{}
		d.readEmitterHeader(node)
	}
	if hasCamera {
		node.Camera = true
	}
	if hasReference {
		node.Reference = &ReferenceData{}
		d.readReferenceHeader(node)
	}
	if hasMesh {
		node.Mesh = NewMeshData()
		d.readMeshHeader(node)
	}
	ensureMesh := func() {
		if node.Mesh == nil {
			node.Mesh = NewMeshData()
		}
	}
	if hasSkin {
		ensureMesh()
		node.Skin = &SkinData{}
		d.readSkinHeader(node)
	}
	if hasAnim {
		ensureMesh()
		node.AnimMesh = &AnimMeshData{}
		d.readAnimMeshHeader(node)
	}
	if hasDangly {
		ensureMesh()
		node.Dangly = &DanglyData{}
		d.readDanglyHeader(node)
	}
	if hasAABB {
		ensureMesh()
		node.Aabb = &AabbData{}
		d.readAABBHeader(node)
	}

	if d.readErr != nil {
		d.warn(WarnTruncatedData, d.pos(), nodeName, "truncated binary: some fields may be zeroed")
		d.readErr = nil
	}

	d.model.Nodes = append(d.model.Nodes, node)

	// Read controllers
	if ctrlKeysDef.Count > 0 {
		d.readControllers(node, contentBits, ctrlKeysDef, ctrlDataDef)
	}

	// Read children
	childPtrs, _ := d.readPointerArray(childrenDef)
	for _, cp := range childPtrs {
		if cp != 0 {
			if err := d.readNodeDepth(cp, nodeName, depth+1); err != nil {
				d.warn(WarnTruncatedData, int64(cp), nodeName, "child read: %v", err)
			}
		}
	}

	return nil
}

// readLightHeader reads header_light (line 259).
func (d *decompiler) readLightHeader(node *Node) {
	l := node.Light

	l.FlareRadius, _ = d.readF32()

	// array_definition unknown (12)
	d.skip(12)

	// array_definition flare_sizes (12)
	flareSizesDef, _ := d.readArrayDef()
	// array_definition flare_positions (12)
	flarePosDef, _ := d.readArrayDef()
	// array_definition flare_color_shifts (12)
	flareColorDef, _ := d.readArrayDef()
	// array_definition flare_textures (12)
	flareTexDef, _ := d.readArrayDef()

	l.LightPriority, _ = d.readI32()
	l.AmbientOnly, _ = d.readI32()
	l.NDynamicType, _ = d.readI32()
	l.AffectDynamic, _ = d.readI32()
	l.Shadow, _ = d.readI32()
	l.GenerateFlare, _ = d.readI32()
	l.FadingLight, _ = d.readI32()

	// Read flare arrays from their pointers
	pos := d.pos()

	if flareSizesDef.Count > 0 && flareSizesDef.Ptr != 0 {
		d.readFloatArrayAt(&l.FlareSizes, flareSizesDef)
	}
	if flarePosDef.Count > 0 && flarePosDef.Ptr != 0 {
		d.readFloatArrayAt(&l.FlarePositions, flarePosDef)
	}
	if flareColorDef.Count > 0 && flareColorDef.Ptr != 0 {
		absPtr := goToPointer(flareColorDef.Ptr)
		if absPtr+int64(flareColorDef.Count)*12 <= d.fileSize && d.trackAlloc(int64(flareColorDef.Count)*12) && d.seek(absPtr) == nil {
			l.FlareColorShifts = make([]Vec3, flareColorDef.Count)
			for i := uint32(0); i < flareColorDef.Count; i++ {
				l.FlareColorShifts[i] = d.readVec3()
			}
		}
	}
	if flareTexDef.Count > 0 && flareTexDef.Ptr != 0 {
		texPtrs, _ := d.readPointerArray(flareTexDef)
		for _, tp := range texPtrs {
			if tp == 0 {
				continue
			}
			if d.seek(goToPointer(tp)) == nil {
				// texture_name is a null-terminated string in the spec
				name, _ := d.readFixedString(64)
				l.TextureNames = append(l.TextureNames, name)
			}
		}
	}

	if len(l.TextureNames) > 0 {
		l.LensFlares = int32(len(l.TextureNames))
	}

	d.seek(pos)
}

// readEmitterHeader reads header_emitter (line 321).
func (d *decompiler) readEmitterHeader(node *Node) {
	em := node.Emitter
	em.DeadSpace, _ = d.readF32()
	em.BlastRadius, _ = d.readF32()
	em.BlastLength, _ = d.readF32()

	em.XGrid = d.readU32AsI32()
	em.YGrid = d.readU32AsI32()
	em.SpawnType = d.readU32AsI32()

	em.Update, _ = d.readFixedString(32)
	em.Render, _ = d.readFixedString(32)
	em.Blend, _ = d.readFixedString(32)
	em.Texture, _ = d.readFixedString(64)

	chunk, _ := d.readFixedString(16)
	if chunk != "" {
		em.ChunkName = chunk
	}

	em.TwoSidedTex = d.readU32AsI32()
	em.Loop = d.readU32AsI32()

	ro, _ := d.readU16()
	em.RenderOrder = int32(ro)
	d.skip(2) // padding

	flags, _ := d.readU32()
	em.P2P = int32(flags & 1)
	em.P2PSel = int32((flags >> 1) & 1)
	em.AffectedByWind = int32((flags >> 2) & 1)
	em.IsTinted = int32((flags >> 3) & 1)
	em.Bounce = int32((flags >> 4) & 1)
	em.Random = int32((flags >> 5) & 1)
	em.Inherit = int32((flags >> 6) & 1)
	em.InheritVel = int32((flags >> 7) & 1)
	em.InheritLocal = int32((flags >> 8) & 1)
	em.Splat = int32((flags >> 9) & 1)
	em.InheritPart = int32((flags >> 10) & 1)
}

// readReferenceHeader reads header_reference (line 343).
func (d *decompiler) readReferenceHeader(node *Node) {
	node.Reference.RefModel, _ = d.readFixedString(64)
	r, _ := d.readI32()
	node.Reference.Reattachable = r
}

// readMeshHeader reads header_mesh (line 369).
// This is the most complex header -- every field is specified in the .bt template.
func (d *decompiler) readMeshHeader(node *Node) {
	mesh := node.Mesh

	// uint32 p_func1, p_func2
	d.skip(8)

	// array_definition faces
	facesDef, _ := d.readArrayDef()

	// vertex bound_min (12), vertex bound_max (12)
	d.skip(24)

	// float radius
	d.skip(4)

	// vertex average (12)
	d.skip(12)

	mesh.Diffuse = d.readVec3()
	mesh.Ambient = d.readVec3()
	mesh.Specular = d.readVec3()

	mesh.Shininess, _ = d.readF32()

	mesh.Shadow = d.readU32AsI32()
	mesh.Beaming = d.readU32AsI32()
	mesh.Render = d.readU32AsI32()
	mesh.TransparencyHint = d.readU32AsI32()

	// uint32 unknown1 -- EE repurposed as RenderHint
	renderHintVal, _ := d.readU32()
	switch renderHintVal {
	case 2:
		mesh.RenderHint = "NormalAndSpecMapped"
	case 0:
		// none
	default:
		mesh.RenderHint = "None"
	}

	// char texture0[64] (bitmap)
	bitmap, _ := d.readFixedString(64)
	if bitmap != "" {
		mesh.Bitmap = bitmap
	}
	// char texture1[64]
	tex1, _ := d.readFixedString(64)
	if tex1 != "" {
		mesh.Texture1 = tex1
	}
	// char texture2[64]
	tex2, _ := d.readFixedString(64)
	if tex2 != "" {
		mesh.Texture2 = tex2
	}
	// char texture3[64] -- EE repurposed as MaterialName
	matName, _ := d.readFixedString(64)
	if matName != "" {
		mesh.MaterialName = matName
	}

	// uint32 tile_fade
	mesh.TileFade = d.readU32AsI32()

	// array_definition vertex_indices (12)
	d.skip(12)
	// array_definition face_leftover (12)
	d.skip(12)
	// array_definition vertex_indices_count (12)
	d.skip(12)
	// array_definition vertex_indices_offset (12)
	d.skip(12)

	// int32 p_mdx_unknown1
	d.skip(4)
	// uint32 unknown2
	d.skip(4)
	// mesh_type type (uint32)
	d.skip(4)

	// int32 p_start_mdx -- per-node MDX start (unused, we use the per-vertex pointers)
	d.skip(4)

	// int32 p_mdx_vertex
	pMdxVertex, _ := d.readI32()
	// uint16 count_vertexes, count_textures
	countVerts, _ := d.readU16()
	countTextures, _ := d.readU16()

	// int32 p_mdx_texture0..3
	pMdxTex0, _ := d.readI32()
	pMdxTex1, _ := d.readI32()
	pMdxTex2, _ := d.readI32()
	pMdxTex3, _ := d.readI32()

	// int32 p_mdx_vertex_normals
	pMdxNormals, _ := d.readI32()
	// int32 p_mdx_vertex_colors
	pMdxColors, _ := d.readI32()

	// int32 p_mdx_tex_anim0..5 (6 * 4 = 24)
	// EE repurposes tex_anim3 as tangent, tex_anim5 as bitangent
	d.skip(12) // anim0, anim1, anim2
	pMdxTangent, _ := d.readI32()  // tex_anim3 -> tangent
	d.skip(4)                       // anim4
	pMdxBitangent, _ := d.readI32() // tex_anim5 -> bitangent

	// byte light_mapped, rotate_texture, uint16 padding
	lightMapped, _ := d.readByte()
	mesh.LightMapped = int32(lightMapped)
	rotateTex, _ := d.readByte()
	mesh.RotateTexture = int32(rotateTex)
	d.skip(2) // padding

	// float vertex_normal_sum
	d.skip(4)
	// uint32 unknown3
	d.skip(4)

	// Save position at end of header_mesh (512 bytes from start).
	// Data reads below seek to MDX/model pointers; we must restore so
	// subsequent type-specific headers (e.g. header_dangly) are read
	// from the correct offset.
	headerEnd := d.pos()
	defer func() { d.seek(headerEnd) }()

	nVerts := int(countVerts)

	// Vertices (MDX pointer)
	if nVerts > 0 && pMdxVertex >= 0 {
		d.readMDXVec3Array(&mesh.Verts, pMdxVertex, nVerts)
	}

	// TVerts (MDX pointers, only 2 floats each = texcoord not vec3)
	if nVerts > 0 && pMdxTex0 >= 0 {
		d.readMDXTexCoords(&mesh.TVerts, pMdxTex0, nVerts)
	}
	if countTextures > 1 && pMdxTex1 >= 0 {
		d.readMDXTexCoords(&mesh.TVerts1, pMdxTex1, nVerts)
	}
	if countTextures > 2 && pMdxTex2 >= 0 {
		d.readMDXTexCoords(&mesh.TVerts2, pMdxTex2, nVerts)
	}
	if countTextures > 3 && pMdxTex3 >= 0 {
		d.readMDXTexCoords(&mesh.TVerts3, pMdxTex3, nVerts)
	}

	// Normals (MDX pointer)
	if nVerts > 0 && pMdxNormals >= 0 {
		d.readMDXVec3Array(&mesh.Normals, pMdxNormals, nVerts)
	}

	// Colors (MDX pointer, RGBA bytes per spec, but Prolog reads as 3 floats)
	if nVerts > 0 && pMdxColors >= 0 {
		d.readMDXColors(&mesh.Colors, pMdxColors, nVerts)
	}

	// Tangents / Bitangents (EE, MDX pointers)
	if nVerts > 0 && pMdxTangent >= 0 {
		d.readMDXTangents(mesh, pMdxTangent, pMdxBitangent, nVerts)
	}

	// Faces (model pointer, not MDX)
	if facesDef.Count > 0 && facesDef.Ptr != 0 {
		d.readFaces(mesh, facesDef)
	}
}

// readSkinHeader reads header_skin (line 501).
func (d *decompiler) readSkinHeader(node *Node) {
	// array_definition weights (12)
	d.skip(12)

	// int32 p_weight_vertex, p_bone_ref_index
	pWeightVertex, _ := d.readI32()
	pBoneRefIndex, _ := d.readI32()

	// int32 p_bone_mapping, count_bone_mapping
	d.skip(8)

	// array_definition bone_quats (12), bone_vertex (12), bone_constants (12)
	d.skip(36)

	bonePartNums := make([]int16, 64)
	for i := range bonePartNums {
		bonePartNums[i], _ = d.readI16()
	}

	// Save position at end of header_skin so subsequent headers are read correctly.
	headerEnd := d.pos()
	defer func() { d.seek(headerEnd) }()

	nVerts := len(node.Mesh.Verts)
	if nVerts == 0 {
		return
	}

	// [4]float32 = 16 bytes, [4]int16 = 8 bytes per vertex
	if !d.trackAlloc(int64(nVerts) * 24) {
		return
	}
	skinWeights := make([][4]float32, nVerts)
	skinBoneRefs := make([][4]int16, nVerts)

	if pWeightVertex >= 0 {
		absPtr := d.goToMDXPointer(pWeightVertex)
		if absPtr >= 0 && absPtr+int64(nVerts)*16 <= d.fileSize {
			if d.seek(absPtr) == nil {
				for i := 0; i < nVerts; i++ {
					skinWeights[i][0], _ = d.readF32()
					skinWeights[i][1], _ = d.readF32()
					skinWeights[i][2], _ = d.readF32()
					skinWeights[i][3], _ = d.readF32()
				}
			}
		}
	}
	if pBoneRefIndex >= 0 {
		absPtr := d.goToMDXPointer(pBoneRefIndex)
		if absPtr >= 0 && absPtr+int64(nVerts)*8 <= d.fileSize {
			if d.seek(absPtr) == nil {
				for i := 0; i < nVerts; i++ {
					skinBoneRefs[i][0], _ = d.readI16()
					skinBoneRefs[i][1], _ = d.readI16()
					skinBoneRefs[i][2], _ = d.readI16()
					skinBoneRefs[i][3], _ = d.readI16()
				}
			}
		}
	}

	d.pendingSkins = append(d.pendingSkins, pendingSkin{
		node:         node,
		bonePartNums: bonePartNums,
		skinWeights:  skinWeights,
		skinBoneRefs: skinBoneRefs,
	})
}

// readAnimMeshHeader reads header_anim (line 561).
func (d *decompiler) readAnimMeshHeader(node *Node) {
	node.AnimMesh.SamplePeriod, _ = d.readF32()
	d.skip(12) // array_definition animation_vertices (legacy)
	d.skip(12) // array_definition animation_texcoords (legacy)
	d.skip(12) // array_definition animation_normals (legacy)
	pAnimVerts, _ := d.readI32()
	pAnimTVerts, _ := d.readI32()
	nVertexSets, _ := d.readI32()
	nTVertSets, _ := d.readI32()

	headerEnd := d.pos()

	nVerts := len(node.Mesh.Verts)
	totalAnimVerts := int64(nVertexSets) * int64(nVerts)
	if totalAnimVerts > 0 && totalAnimVerts <= int64(d.fileSize) && pAnimVerts > 0 && nVerts > 0 {
		d.readCoreVec3Array(&node.AnimMesh.AnimVerts, pAnimVerts, int(totalAnimVerts))
	}
	totalAnimTVerts := int64(nTVertSets) * int64(nVerts)
	if totalAnimTVerts > 0 && totalAnimTVerts <= int64(d.fileSize) && pAnimTVerts > 0 && nVerts > 0 {
		d.readCoreVec2AsVec3Array(&node.AnimMesh.AnimTVerts, pAnimTVerts, int(totalAnimTVerts))
	}

	d.seek(headerEnd)
}

// readDanglyHeader reads header_dangly (line 586).
func (d *decompiler) readDanglyHeader(node *Node) {
	constraintsDef, _ := d.readArrayDef()
	node.Dangly.Displacement, _ = d.readF32()
	node.Dangly.Tightness, _ = d.readF32()
	node.Dangly.Period, _ = d.readF32()

	// Save position at end of header_dangly so subsequent headers are read correctly.
	headerEnd := d.pos()

	if constraintsDef.Count > 0 && constraintsDef.Ptr != 0 {
		d.readFloatArrayAt(&node.Dangly.Constraints, constraintsDef)
	}

	d.seek(headerEnd)
}

// readAABBHeader reads header_aabb (line 622).
func (d *decompiler) readAABBHeader(node *Node) {
	pAABB, _ := d.readU32()
	if pAABB != 0 {
		d.readAABBTree(node.Aabb, pAABB)
	}
}

// readAABBTree reads the AABB binary tree iteratively to avoid stack overflow.
// Uses an explicit work stack with visited-pointer tracking for cycle detection.
// Ref: NWN1MDL.bt entry_aabb (line 605)
func (d *decompiler) readAABBTree(aabb *AabbData, rootPtr uint32) {
	visited := make(map[uint32]bool)
	stack := []uint32{rootPtr}

	for len(stack) > 0 {
		ptr := stack[len(stack)-1]
		stack = stack[:len(stack)-1]

		if ptr == 0 || visited[ptr] {
			continue
		}
		visited[ptr] = true

		absPtr := goToPointer(ptr)
		if absPtr+40 > d.fileSize {
			continue
		}
		if d.seek(absPtr) != nil {
			continue
		}

		var entry AabbEntry
		entry.BoundMin = d.readVec3()
		entry.BoundMax = d.readVec3()

		pLeft, _ := d.readU32()
		pRight, _ := d.readU32()

		leafFace, _ := d.readI32()
		entry.LeafFace = leafFace

		plane, _ := d.readU32()
		entry.Plane = plane

		aabb.Entries = append(aabb.Entries, entry)

		// Push right first so left is processed first (pre-order traversal)
		if pRight > 0 {
			stack = append(stack, pRight)
		}
		if pLeft > 0 {
			stack = append(stack, pLeft)
		}
	}
}

// readFaces reads mesh faces from a model-data pointer.
// Ref: NWN1MDL.bt face struct (line 361)
func (d *decompiler) readFaces(mesh *MeshData, facesDef binArrayDef) {
	absPtr := goToPointer(facesDef.Ptr)
	if absPtr >= d.fileSize {
		return
	}
	if d.seek(absPtr) != nil {
		return
	}
	count := int(facesDef.Count)
	if absPtr+int64(count)*32 > d.fileSize {
		return
	}
	if !d.trackAlloc(int64(count) * 32) {
		return
	}
	mesh.Faces = make([]Face, count)
	for i := 0; i < count; i++ {
		normal := d.readVec3()
		d.skip(4) // float distance
		surfaceID, _ := d.readI32()
		d.skip(6) // ushort adj_face_ids[3]
		v0, _ := d.readU16()
		v1, _ := d.readU16()
		v2, _ := d.readU16()

		mesh.Faces[i] = Face{
			Verts:       [3]int32{int32(v0), int32(v1), int32(v2)},
			SmoothGroup: 0,
			UVs:         [3]int32{int32(v0), int32(v1), int32(v2)},
			Normal:      normal,
			Material:    surfaceID,
		}
	}

	// Clamp out-of-range vertex indices to prevent downstream panics.
	nVerts := int32(len(mesh.Verts))
	if nVerts > 0 {
		for i := range mesh.Faces {
			for vi := 0; vi < 3; vi++ {
				if mesh.Faces[i].Verts[vi] < 0 || mesh.Faces[i].Verts[vi] >= nVerts {
					mesh.Faces[i].Verts[vi] = 0
				}
				if mesh.Faces[i].UVs[vi] < 0 || mesh.Faces[i].UVs[vi] >= nVerts {
					mesh.Faces[i].UVs[vi] = 0
				}
			}
		}
	}
}

// Core block data readers -- use goToPointer convention

func (d *decompiler) readCoreVec3Array(out *[]Vec3, ptr int32, count int) {
	absPtr := goToPointer(uint32(ptr))
	d.readVec3ArrayAt(out, absPtr, count)
}

func (d *decompiler) readCoreVec2AsVec3Array(out *[]Vec3, ptr int32, count int) {
	absPtr := goToPointer(uint32(ptr))
	d.readVec2ArrayAt(out, absPtr, count)
}

// MDX data readers -- all use GoToMDXPointer convention

func (d *decompiler) readMDXVec3Array(out *[]Vec3, mdxPtr int32, count int) {
	absPtr := d.goToMDXPointer(mdxPtr)
	d.readVec3ArrayAt(out, absPtr, count)
}

func (d *decompiler) readMDXTexCoords(out *[]Vec3, mdxPtr int32, count int) {
	absPtr := d.goToMDXPointer(mdxPtr)
	d.readVec2ArrayAt(out, absPtr, count)
}

// readVec3ArrayAt reads count Vec3 values from an absolute file offset.
func (d *decompiler) readVec3ArrayAt(out *[]Vec3, absPtr int64, count int) {
	if absPtr < 0 || absPtr+int64(count)*12 > d.fileSize {
		return
	}
	if !d.trackAlloc(int64(count) * 12) {
		return
	}
	if d.seek(absPtr) != nil {
		return
	}
	*out = make([]Vec3, count)
	for i := 0; i < count; i++ {
		(*out)[i] = d.readVec3()
	}
}

// readVec2ArrayAt reads count Vec2 values (as Vec3 with Z=0) from an absolute file offset.
func (d *decompiler) readVec2ArrayAt(out *[]Vec3, absPtr int64, count int) {
	if absPtr < 0 || absPtr+int64(count)*8 > d.fileSize {
		return
	}
	if !d.trackAlloc(int64(count) * 12) {
		return
	}
	if d.seek(absPtr) != nil {
		return
	}
	*out = make([]Vec3, count)
	for i := 0; i < count; i++ {
		(*out)[i].X, _ = d.readF32()
		(*out)[i].Y, _ = d.readF32()
	}
}

func (d *decompiler) readMDXColors(out *[]Vec3, mdxPtr int32, count int) {
	absPtr := d.goToMDXPointer(mdxPtr)
	if absPtr < 0 || absPtr+int64(count)*4 > d.fileSize {
		return
	}
	if !d.trackAlloc(int64(count) * 12) {
		return
	}
	if d.seek(absPtr) != nil {
		return
	}
	*out = make([]Vec3, count)
	for i := 0; i < count; i++ {
		r, _ := d.readByte()
		g, _ := d.readByte()
		b, _ := d.readByte()
		d.skip(1) // alpha
		(*out)[i] = Vec3{X: float32(r) / 255.0, Y: float32(g) / 255.0, Z: float32(b) / 255.0}
	}
}

func (d *decompiler) readMDXTangents(mesh *MeshData, tangentPtr, bitangentPtr int32, count int) {
	tangentAbs := d.goToMDXPointer(tangentPtr)
	if tangentAbs < 0 || tangentAbs+int64(count)*12 > d.fileSize {
		return
	}
	if !d.trackAlloc(int64(count) * 16) {
		return
	}

	// Read bitangents for handedness
	var bitangents []Vec3
	if bitangentPtr >= 0 {
		btAbs := d.goToMDXPointer(bitangentPtr)
		if btAbs >= 0 && btAbs+int64(count)*12 <= d.fileSize && d.trackAlloc(int64(count)*12) {
			if d.seek(btAbs) == nil {
				bitangents = make([]Vec3, count)
				for i := 0; i < count; i++ {
					bitangents[i] = d.readVec3()
				}
			}
		}
	}

	if d.seek(tangentAbs) != nil {
		return
	}
	mesh.Tangents = make([]Vec4, count)
	for i := 0; i < count; i++ {
		t := d.readVec3()
		w := float32(1.0)
		if i < len(bitangents) && i < len(mesh.Normals) {
			cross := vecCross(mesh.Normals[i], t)
			dot := vecDot(cross, bitangents[i])
			if dot < 0 {
				w = -1.0
			}
		}
		mesh.Tangents[i] = Vec4{X: t.X, Y: t.Y, Z: t.Z, W: w}
	}
}

func (d *decompiler) readFloatArrayAt(out *[]float32, a binArrayDef) {
	absPtr := goToPointer(a.Ptr)
	if absPtr+int64(a.Count)*4 > d.fileSize {
		return
	}
	if d.seek(absPtr) != nil {
		return
	}
	if !d.trackAlloc(int64(a.Count) * 4) {
		return
	}
	*out = make([]float32, a.Count)
	for i := uint32(0); i < a.Count; i++ {
		(*out)[i], _ = d.readF32()
	}
}

// resolveSkinWeights maps bone indices to node names after all nodes are read.
// Ref: load_binary.pl do_skinbones (line 596)
func (d *decompiler) resolveSkinWeights(ps pendingSkin) {
	nVerts := len(ps.skinWeights)
	if !d.trackAlloc(int64(nVerts) * 64) {
		return
	}
	ps.node.Skin.Weights = make([]VertexWeight, nVerts)

	for i := 0; i < nVerts; i++ {
		w := VertexWeight{}
		for j := 0; j < 4; j++ {
			weight := ps.skinWeights[i][j]
			boneRef := ps.skinBoneRefs[i][j]
			if weight == 0 || boneRef < 0 || int(boneRef) >= len(ps.bonePartNums) {
				continue
			}
			partNum := ps.bonePartNums[boneRef]
			if partNum < 0 {
				continue
			}
			idx := int(partNum)
			if idx >= 0 && idx < len(d.nodeOrder) {
				w.Bones = append(w.Bones, d.nodeOrder[idx].name)
				w.Weights = append(w.Weights, weight)
			}
		}
		ps.node.Skin.Weights[i] = w
	}
}

// binControllerKey holds one parsed controller struct entry from the binary file.
// Ref: NWN1MDL.bt controller struct (line 233), 12 bytes each.
type binControllerKey struct {
	Type        uint32
	ValueCount  uint16
	TimeStart   uint16
	DataStart   uint16
	ColumnCount byte
}

// readControllerKeys reads the binary controller key structs from the file.
func (d *decompiler) readControllerKeys(keysDef binArrayDef) []binControllerKey {
	absPtr := goToPointer(keysDef.Ptr)
	if absPtr >= d.fileSize || absPtr+int64(keysDef.Count)*12 > d.fileSize {
		return nil
	}
	if d.seek(absPtr) != nil {
		return nil
	}
	if !d.trackAlloc(int64(keysDef.Count) * 12) {
		return nil
	}
	keys := make([]binControllerKey, keysDef.Count)
	for i := uint32(0); i < keysDef.Count; i++ {
		keys[i].Type, _ = d.readU32()
		keys[i].ValueCount, _ = d.readU16()
		keys[i].TimeStart, _ = d.readU16()
		keys[i].DataStart, _ = d.readU16()
		keys[i].ColumnCount, _ = d.readByte()
		d.skip(1)
	}
	return keys
}

type controllerRow struct {
	Time   float32
	Values []float32
}

// readControllerRows reads time+value rows from the data array for a single
// controller key. Handles seeking, bounds checks, and quaternion conversion.
func (d *decompiler) readControllerRows(def ControllerDef, dataPtr uint32, timeKeyStart, dataStart, numRows, numCols int) []controllerRow {
	if numRows <= 0 {
		return nil
	}
	// Each row: controllerRow struct (~32 bytes) + []float32 slice (numCols * 4 bytes)
	if !d.trackAlloc(int64(numRows) * (32 + int64(numCols)*4)) {
		return nil
	}
	dataAbsPtr := goToPointer(dataPtr)
	rows := make([]controllerRow, 0, numRows)

	for row := 0; row < numRows; row++ {
		timePtr := dataAbsPtr + int64(timeKeyStart+row)*4
		if timePtr < 0 || timePtr+4 > d.fileSize {
			break
		}
		if d.seek(timePtr) != nil {
			break
		}
		timeVal, _ := d.readF32()

		valPtr := dataAbsPtr + int64(dataStart+row*numCols)*4
		if valPtr < 0 || valPtr+int64(numCols)*4 > d.fileSize {
			break
		}
		if d.seek(valPtr) != nil {
			break
		}
		floats := make([]float32, numCols)
		for c := 0; c < numCols; c++ {
			floats[c], _ = d.readF32()
		}

		if numCols == 4 && def.Name == "orientation" {
			floats = quaternionToAngleAxis(floats)
		}

		rows = append(rows, controllerRow{Time: timeVal, Values: floats})
	}
	return rows
}

// resolveControllerDefs resolves key structs into (ControllerDef, numCols, key)
// tuples, handling compiler detection and column override.
func (d *decompiler) resolveControllerDefs(keys []binControllerKey, nodeFlag uint32, nodeName string) []struct {
	def     ControllerDef
	numCols int
	key     binControllerKey
} {
	var out []struct {
		def     ControllerDef
		numCols int
		key     binControllerKey
	}
	for _, k := range keys {
		if nodeFlag == 5 {
			d.detectCompiler(k.Type)
		}
		def, ok := ControllerID(k.Type, nodeFlag, d.compiler)
		if !ok {
			d.warn(WarnUnknownController, int64(k.Type), nodeName,
				"unknown controller type %d for node flag %d", k.Type, nodeFlag)
			continue
		}
		numCols := int(k.ColumnCount) & 0x0F
		if def.NumCols > 0 {
			numCols = def.NumCols
		}
		out = append(out, struct {
			def     ControllerDef
			numCols int
			key     binControllerKey
		}{def, numCols, k})
	}
	return out
}

// readControllers reads all controller key structs first, then processes them.
// NOTE (intentional behavior change): geometry nodes always use the first
// keyframe value even when N>1 keyframes are present. Previously multi-frame
// controllers were silently ignored; now we apply rows[0] and log a warning.
// This matches how the game engine evaluates static geometry controllers.
func (d *decompiler) readControllers(node *Node, contentBits uint32, keysDef, dataDef binArrayDef) {
	keys := d.readControllerKeys(keysDef)
	if len(keys) == 0 {
		return
	}
	nodeFlag := node.NodeTypeFlag()
	for _, entry := range d.resolveControllerDefs(keys, nodeFlag, node.Name) {
		rows := d.readControllerRows(entry.def, dataDef.Ptr, int(entry.key.TimeStart), int(entry.key.DataStart), int(entry.key.ValueCount), entry.numCols)
		if len(rows) == 0 {
			continue
		}
		d.setStaticController(node, entry.def.Name, rows[0].Values)
		if len(rows) > 1 {
			d.warn(WarnGeneral, 0, node.Name, "controller %s has %d keyframes on geometry node; using first value only",
				entry.def.Name, len(rows))
		}
	}
}

func (d *decompiler) detectCompiler(typeID uint32) {
	if d.compiler != CompilerUnknown {
		return
	}
	switch typeID {
	case 448, 452:
		d.compiler = CompilerBioWare
	case 480, 481, 482, 484, 488:
		d.compiler = CompilerNwnmdlcomp
	}
}


func quaternionToAngleAxis(q []float32) []float32 {
	if len(q) < 4 {
		return q
	}
	x, y, z, w := q[0], q[1], q[2], q[3]
	sinHalf := float32(math.Sqrt(float64(x*x + y*y + z*z)))
	if sinHalf < 1e-6 {
		return []float32{0, 0, 1, 0}
	}
	angle := float32(2.0 * math.Atan2(float64(sinHalf), float64(w)))
	return []float32{x / sinHalf, y / sinHalf, z / sinHalf, angle}
}

func (d *decompiler) setStaticController(node *Node, name string, vals []float32) {
	switch name {
	case "position":
		if len(vals) >= 3 {
			node.Position = Vec3{X: vals[0], Y: vals[1], Z: vals[2]}
		}
	case "orientation":
		if len(vals) >= 4 {
			node.Orientation = Vec4{X: vals[0], Y: vals[1], Z: vals[2], W: vals[3]}
		}
	case "scale":
		if len(vals) >= 1 {
			node.Scale = vals[0]
		}
	case "selfillumcolor":
		if len(vals) >= 3 && node.Mesh != nil {
			node.Mesh.SelfIllumColor = Vec3{X: vals[0], Y: vals[1], Z: vals[2]}
		}
	case "alpha":
		if len(vals) >= 1 && node.Mesh != nil {
			node.Mesh.Alpha = vals[0]
		}
	case "color":
		if len(vals) >= 3 && node.Light != nil {
			node.Light.Color = Vec3{X: vals[0], Y: vals[1], Z: vals[2]}
		}
	case "radius":
		if len(vals) >= 1 && node.Light != nil {
			node.Light.Radius = vals[0]
		}
	case "multiplier":
		if len(vals) >= 1 && node.Light != nil {
			node.Light.Multiplier = vals[0]
		}
	case "shadowradius":
		if len(vals) >= 1 && node.Light != nil {
			node.Light.ShadowRadius = vals[0]
		}
	case "verticaldisplacement":
		if len(vals) >= 1 && node.Light != nil {
			node.Light.VerticalDisplacement = vals[0]
		}
	default:
		if node.Emitter != nil {
			d.setEmitterController(node.Emitter, name, vals)
		}
	}
}

func (d *decompiler) setEmitterController(em *EmitterData, name string, vals []float32) {
	if len(vals) == 0 {
		return
	}
	if getter, ok := emitterFloatFields[name]; ok {
		*getter(em) = vals[0]
		return
	}
	if getter, ok := emitterColorFields[name]; ok {
		if len(vals) >= 3 {
			*getter(em) = Vec3{X: vals[0], Y: vals[1], Z: vals[2]}
		}
		return
	}
}

// readAnimation reads an animation block.
// Ref: NWN1MDL.bt header_animation (line 745) + animation struct (line 753)
func (d *decompiler) readAnimation(ptr uint32) error {
	absPtr := goToPointer(ptr)
	if absPtr >= d.fileSize {
		return fmt.Errorf("animation pointer out of bounds: %d", ptr)
	}
	if err := d.seek(absPtr); err != nil {
		return err
	}

	// header_geometry (same struct as model)
	d.skip(8) // p_func1, p_func2
	animName, _ := d.readFixedString(64)
	rootNodePtr, _ := d.readU32()
	d.skip(4)  // count_nodes
	d.skip(12) // unknown1
	d.skip(12) // unknown2
	d.skip(4)  // ref_count
	d.skip(4)  // type + padding

	// header_animation fields after geometry
	animLength, _ := d.readF32()
	transTime, _ := d.readF32()
	animRoot, _ := d.readFixedString(64)

	// array_definition events
	eventsDef, _ := d.readArrayDef()

	anim := Animation{
		Name:      animName,
		Length:    animLength,
		TransTime: transTime,
		Root:      animRoot,
	}

	// Read events
	if eventsDef.Count > 0 && eventsDef.Ptr != 0 {
		evtAbs := goToPointer(eventsDef.Ptr)
		if evtAbs+int64(eventsDef.Count)*36 <= d.fileSize && d.trackAlloc(int64(eventsDef.Count)*36) {
			if d.seek(evtAbs) == nil {
				for i := uint32(0); i < eventsDef.Count; i++ {
					t, _ := d.readF32()
					name, _ := d.readFixedString(32)
					anim.Events = append(anim.Events, AnimEvent{Time: t, Name: name})
				}
			}
		}
	}

	// Read animation nodes from root
	if rootNodePtr != 0 {
		d.readAnimNode(&anim, rootNodePtr, "NULL")
	}

	d.model.Animations = append(d.model.Animations, anim)
	return nil
}

func (d *decompiler) readAnimNode(anim *Animation, nodePtr uint32, parentName string) {
	d.readAnimNodeDepth(anim, nodePtr, parentName, 0)
}

func (d *decompiler) readAnimNodeDepth(anim *Animation, nodePtr uint32, parentName string, depth int) {
	if nodePtr == 0 {
		return
	}
	if depth > maxReadNodeDepth {
		d.warn(WarnGeneral, 0, parentName, "anim node tree exceeds max depth %d; truncating", maxReadNodeDepth)
		return
	}
	if d.visitedPtrs[nodePtr] {
		d.warn(WarnPointerOutOfBounds, int64(nodePtr), "", "cycle detected at anim node pointer")
		return
	}
	d.visitedPtrs[nodePtr] = true
	absPtr := goToPointer(nodePtr)
	if absPtr >= d.fileSize {
		return
	}
	if d.seek(absPtr) != nil {
		return
	}

	// header_node
	d.skip(24) // p_func1..6
	d.skip(4)  // inheritColor
	d.skip(4)  // nodeNumber
	nodeName, _ := d.readFixedString(32)
	d.skip(8)  // p_geometry, p_parent_node

	childrenDef, _ := d.readArrayDef()
	ctrlKeysDef, _ := d.readArrayDef()
	ctrlDataDef, _ := d.readArrayDef()
	contentBits, _ := d.readU32()

	nodeFlag := uint32(1) // dummy default
	hasLight := contentBits&0x02 != 0
	hasEmitter := contentBits&0x04 != 0
	hasMesh := contentBits&0x20 != 0

	if hasLight {
		nodeFlag = 3
	} else if hasEmitter {
		nodeFlag = 5
	} else if hasMesh {
		nodeFlag = 33
	}

	hasAnim := contentBits&0x80 != 0

	// Skip type-specific headers (animation nodes have them too).
	// For animmesh nodes, we read the mesh+animmesh headers; for others, skip.
	if hasLight {
		d.skip(4 + 5*12 + 7*4) // header_light: 92 bytes
	}
	if hasEmitter {
		d.skip(4*3 + 4*3 + 32*3 + 64 + 16 + 4*2 + 2 + 2 + 4) // header_emitter: 216 bytes
	}
	if contentBits&0x10 != 0 { // reference
		d.skip(64 + 4) // header_reference: 68 bytes
	}

	var animMeshVerts int
	var animFacesDef binArrayDef
	var animPMdxVertex, animPMdxTex0 int32
	if hasMesh && hasAnim {
		meshStart := d.pos()
		d.skip(8)                               // p_func1, p_func2
		animFacesDef, _ = d.readArrayDef()       // offset 8, 12 bytes
		d.skip(meshHeaderPMdxVertex - 20)        // skip to pMdxVertex field
		animPMdxVertex, _ = d.readI32()          // offset meshHeaderPMdxVertex
		countVerts, _ := d.readU16()             // offset meshHeaderCountVerts
		d.skip(2)                                // countTextures
		animPMdxTex0, _ = d.readI32()            // offset meshHeaderPMdxTex0
		animMeshVerts = int(countVerts)
		d.seek(meshStart + meshHeaderSize)
	} else if hasMesh {
		d.skip(meshHeaderSize)
	}
	if contentBits&0x40 != 0 { // skin
		d.skip(12 + 4*2 + 4*2 + 12*3 + 64*2) // header_skin: 192 bytes
	}

	var animMeshData *AnimMeshData
	if hasAnim {
		animMeshData = &AnimMeshData{}
		animMeshData.SamplePeriod, _ = d.readF32()
		d.skip(12) // array_definition animation_vertices (legacy)
		d.skip(12) // array_definition animation_texcoords (legacy)
		d.skip(12) // array_definition animation_normals (legacy)
		pAnimVerts, _ := d.readI32()
		pAnimTVerts, _ := d.readI32()
		nVertexSets, _ := d.readI32()
		nTVertSets, _ := d.readI32()

		if animMeshVerts > 0 {
			totalAV := int64(nVertexSets) * int64(animMeshVerts)
			if totalAV > 0 && totalAV <= int64(d.fileSize) && pAnimVerts > 0 {
				d.readCoreVec3Array(&animMeshData.AnimVerts, pAnimVerts, int(totalAV))
			}
			totalAT := int64(nTVertSets) * int64(animMeshVerts)
			if totalAT > 0 && totalAT <= int64(d.fileSize) && pAnimTVerts > 0 {
				d.readCoreVec2AsVec3Array(&animMeshData.AnimTVerts, pAnimTVerts, int(totalAT))
			}
		}
	}
	if contentBits&0x100 != 0 { // dangly
		d.skip(12 + 4*3) // header_dangly: 24 bytes
	}
	if contentBits&0x200 != 0 { // aabb
		d.skip(4) // header_aabb: 4 bytes
	}

	animNode := AnimNode{
		Name:   nodeName,
		Parent: parentName,
	}

	// Read mesh data for animmesh animation nodes
	if hasMesh && hasAnim && animMeshVerts > 0 {
		headerEnd := d.pos()
		mesh := NewMeshData()
		if animPMdxVertex >= 0 {
			d.readMDXVec3Array(&mesh.Verts, animPMdxVertex, animMeshVerts)
		}
		if animPMdxTex0 >= 0 {
			d.readMDXTexCoords(&mesh.TVerts, animPMdxTex0, animMeshVerts)
		}
		if animFacesDef.Count > 0 && animFacesDef.Ptr != 0 {
			d.readFaces(mesh, animFacesDef)
		}
		animNode.Mesh = mesh
		d.seek(headerEnd)
	}

	if animMeshData != nil && (animMeshData.SamplePeriod != 0 || len(animMeshData.AnimVerts) > 0 || len(animMeshData.AnimTVerts) > 0) {
		animNode.AnimMesh = animMeshData
	}

	// Read controllers for anim node
	if ctrlKeysDef.Count > 0 {
		d.readAnimControllers(&animNode, nodeFlag, ctrlKeysDef, ctrlDataDef)
	}

	anim.Nodes = append(anim.Nodes, animNode)

	// Read children
	childPtrs, _ := d.readPointerArray(childrenDef)
	for _, cp := range childPtrs {
		if cp != 0 {
			d.readAnimNodeDepth(anim, cp, nodeName, depth+1)
		}
	}
}

func (d *decompiler) readAnimControllers(animNode *AnimNode, nodeFlag uint32, keysDef, dataDef binArrayDef) {
	keys := d.readControllerKeys(keysDef)
	if len(keys) == 0 {
		return
	}
	for _, entry := range d.resolveControllerDefs(keys, nodeFlag, animNode.Name) {
		rows := d.readControllerRows(entry.def, dataDef.Ptr, int(entry.key.TimeStart), int(entry.key.DataStart), int(entry.key.ValueCount), entry.numCols)
		for _, r := range rows {
			d.addAnimKey(animNode, entry.def.Name, r.Time, r.Values)
		}
	}
}

func (d *decompiler) addAnimKey(an *AnimNode, name string, time float32, vals []float32) {
	if name == "detonate" {
		an.DetonateKeys = append(an.DetonateKeys, FloatKey{Time: time, Value: 1})
		return
	}
	if getter, ok := animControllerFloatTable[name]; ok {
		if len(vals) >= 1 {
			slice := getter(an)
			*slice = append(*slice, FloatKey{Time: time, Value: vals[0]})
		}
		return
	}
	if getter, ok := animControllerColorTable[name]; ok {
		if len(vals) >= 3 {
			slice := getter(an)
			*slice = append(*slice, ColorKey{Time: time, Value: Vec3{X: vals[0], Y: vals[1], Z: vals[2]}})
		}
		return
	}
	switch name {
	case "position":
		if len(vals) >= 3 { an.PositionKeys = append(an.PositionKeys, PositionKey{Time: time, Value: Vec3{X: vals[0], Y: vals[1], Z: vals[2]}}) }
	case "orientation":
		if len(vals) >= 4 { an.OrientationKeys = append(an.OrientationKeys, OrientationKey{Time: time, Value: Vec4{X: vals[0], Y: vals[1], Z: vals[2], W: vals[3]}}) }
	}
}
