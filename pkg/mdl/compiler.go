// Binary MDL compiler.
//
// Converts Model structs to NWN binary (compiled) .mdl files.
// Every write mirrors the corresponding read in binary.go exactly.
//
// Pointer convention (matches binary.go):
//   Core-relative: p such that file_offset = 12 + p
//   MDX-relative:  p such that file_offset = 12 + core_len + p
//
// Layout of the output file:
//   [0-11]   header_file  {0, core_len, mdx_len}
//   [12 ...]  core block   (model header + node tree + anim headers)
//   [12+core_len ...] MDX block (vertex/normal/uv data)
package mdl

import (
	"encoding/binary"
	"fmt"
	"io"
	"math"
	"strings"
)

// patchBuf is a growable byte slice with random-access patching.
type patchBuf struct {
	b []byte
}

func (p *patchBuf) len() int { return len(p.b) }

func (p *patchBuf) u8(v byte) { p.b = append(p.b, v) }

func (p *patchBuf) u16le(v uint16) {
	p.b = append(p.b, byte(v), byte(v>>8))
}

func (p *patchBuf) u32le(v uint32) {
	p.b = append(p.b, byte(v), byte(v>>8), byte(v>>16), byte(v>>24))
}

func (p *patchBuf) i32le(v int32)  { p.u32le(uint32(v)) }
func (p *patchBuf) f32le(v float32) { p.u32le(math.Float32bits(v)) }

func (p *patchBuf) vec3(v Vec3) {
	p.f32le(v.X)
	p.f32le(v.Y)
	p.f32le(v.Z)
}

func (p *patchBuf) zeros(n int) {
	for i := 0; i < n; i++ {
		p.b = append(p.b, 0)
	}
}

func (p *patchBuf) raw(v []byte) { p.b = append(p.b, v...) }

// fixedStr writes s as null-terminated into a fixed-width field (zero-padded).
func (p *patchBuf) fixedStr(s string, width int) {
	if len(s) >= width {
		s = s[:width-1]
	}
	p.b = append(p.b, []byte(s)...)
	p.zeros(width - len(s))
}

// placeholder writes 4 zero bytes and returns the byte offset for later patching.
func (p *patchBuf) placeholder() int {
	off := len(p.b)
	p.u32le(0)
	return off
}

// patchU32 writes a uint32 at the given byte offset (must already exist).
func (p *patchBuf) patchU32(off int, v uint32) {
	binary.LittleEndian.PutUint32(p.b[off:], v)
}

// proxyList writes a 12-byte array_definition {offset, count, alloc}.
func (p *patchBuf) proxyList(offset, count, alloc uint32) {
	p.u32le(offset)
	p.u32le(count)
	p.u32le(alloc)
}

func (p *patchBuf) proxyListEmpty() { p.proxyList(0, 0, 0) }

// writeCtrlBlock writes binCtrlKey records + time/data arrays and patches the
// corresponding ProxyList placeholders for controller_keys and controller_data.
func (c *compiler) writeCtrlBlock(keys []binCtrlKey, timeArr, dataArr []float32,
	keysPtrPos, keysNumPos, keysAlcPos, dataPtrPos, dataNumPos, dataAlcPos int) {
	if len(keys) == 0 {
		return
	}
	keysOff := c.core.len()
	for _, k := range keys {
		c.core.u32le(k.Type)
		c.core.u16le(k.ValueCount)
		c.core.u16le(k.TimeStart)
		c.core.u16le(k.DataStart)
		c.core.u8(k.ColumnCount)
		c.core.u8(0)
	}
	dataOff := c.core.len()
	for _, t := range timeArr {
		c.core.f32le(t)
	}
	for _, v := range dataArr {
		c.core.f32le(v)
	}
	totalData := uint32(len(timeArr) + len(dataArr))
	c.core.patchU32(keysPtrPos, uint32(keysOff))
	c.core.patchU32(keysNumPos, uint32(len(keys)))
	c.core.patchU32(keysAlcPos, uint32(len(keys)))
	c.core.patchU32(dataPtrPos, uint32(dataOff))
	c.core.patchU32(dataNumPos, totalData)
	c.core.patchU32(dataAlcPos, totalData)
}

// compiler holds all state for one binary MDL compilation.
type compiler struct {
	core *patchBuf // model data ("core") block
	vol  *patchBuf // vertex data ("volatile" / MDX) block

	model *Model

	// nodeIDs maps a node instance → sequential part number. Keyed by identity,
	// not name, because names are not unique (see compiler_tree.go).
	nodeIDs map[*Node]int32
	nextID  int32

	// nodeOffsets maps a node instance → core offset (for parent pointer patching)
	nodeOffsets map[*Node]int32

	// childrenByNode maps a node instance → its children, resolved by tree
	// position rather than by name (built once in newCompiler).
	childrenByNode map[*Node][]*Node

	// geomNodeIndex maps lowercased node name → first *Node with that name.
	// Only for lookups that are inherently name-based (skin bone references).
	geomNodeIndex map[string]*Node

	// geomOccur groups geometry nodes by lowercased name in declaration order,
	// used to pair animation nodes with the right duplicate.
	geomOccur map[string][]*Node

	// animGeomPair maps the animation nodes of the animation currently being
	// written to their geometry counterparts. Reset per animation.
	animGeomPair map[*AnimNode]*Node

	// lastExpanded holds the expanded mesh from the most recent writeMeshHeaderInner call.
	lastExpanded *expandedMesh

	// danglyConstraintsPtrPos stores the core buffer position of the constraints
	// array pointer, used by writeDanglyConstraints to patch after all headers.
	danglyConstraintsPtrPos int

	// err captures the first non-fatal compilation error
	err error
}

func newCompiler(m *Model) *compiler {
	// First name wins, so a duplicate cannot shadow the node that name-based
	// references (skin bones) have always resolved to.
	gi := make(map[string]*Node, len(m.Nodes))
	for _, n := range m.Nodes {
		if n == nil {
			continue
		}
		key := strings.ToLower(n.Name)
		if _, exists := gi[key]; !exists {
			gi[key] = n
		}
	}
	_, children := resolveGeomTree(m.Nodes)
	return &compiler{
		core:           &patchBuf{},
		vol:            &patchBuf{},
		model:          m,
		nodeIDs:        make(map[*Node]int32),
		nodeOffsets:    make(map[*Node]int32),
		childrenByNode: children,
		geomNodeIndex:  gi,
		geomOccur:      nodeOccurrences(m.Nodes),
	}
}

// CompileFile writes a binary MDL to the given path.
func CompileFile(model *Model, path string) error {
	return atomicWriteFile(path, func(w io.Writer) error { return Compile(model, w) })
}

// Compile writes a binary MDL to w.
func Compile(model *Model, w io.Writer) error {
	if model == nil {
		return fmt.Errorf("compile: nil model")
	}
	c := newCompiler(model)

	// Pre-pass: assign sequential part numbers to every geometry node.
	root := model.RootNode()
	if root != nil {
		c.assignNodeIDs(root)
	}

	if err := c.writeModel(); err != nil {
		return err
	}
	if c.err != nil {
		return c.err
	}

	// File header: {0, core_len, mdx_len}
	hdr := make([]byte, 12)
	binary.LittleEndian.PutUint32(hdr[0:], 0)
	binary.LittleEndian.PutUint32(hdr[4:], uint32(c.core.len()))
	binary.LittleEndian.PutUint32(hdr[8:], uint32(c.vol.len()))

	if _, err := w.Write(hdr); err != nil {
		return err
	}
	if _, err := w.Write(c.core.b); err != nil {
		return err
	}
	_, err := w.Write(c.vol.b)
	return err
}

// assignNodeIDs walks the geometry node tree in iterative DFS order and assigns
// sequential IDs. Uses a visited set to guard against cyclic parent references.
func (c *compiler) assignNodeIDs(root *Node) {
	visited := make(map[*Node]bool, len(c.model.Nodes))
	stack := []*Node{root}
	for len(stack) > 0 {
		n := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		if n == nil || visited[n] {
			continue
		}
		visited[n] = true
		c.nodeIDs[n] = c.nextID
		c.nextID++
		children := c.childrenOf(n)
		// push in reverse so left-to-right DFS order is preserved
		for i := len(children) - 1; i >= 0; i-- {
			if !visited[children[i]] {
				stack = append(stack, children[i])
			}
		}
	}
}

// childrenOf returns all direct children of n in the geometry tree (O(1) lookup).
// Resolved by tree position, so duplicate-named siblings each keep their own
// children instead of both claiming every child of that name.
func (c *compiler) childrenOf(n *Node) []*Node {
	return c.childrenByNode[n]
}

// geomNodeByName returns the geometry node matching name (case-insensitive).
func (c *compiler) geomNodeByName(name string) *Node {
	return c.geomNodeIndex[strings.ToLower(name)]
}

// writeModel writes the ProxyModel header + animation pointers + geometry tree + animations.
func (c *compiler) writeModel() error {
	m := c.model

	// ---- header_geometry (112 bytes) ----
	// Ref: binary.go readModelHeader → d.skip(8) + readFixedString(64) + readU32×2 + skip×4
	c.core.zeros(8)                       // p_func1, p_func2
	c.core.fixedStr(m.Name, 64)           // model name
	rootPtrOff := c.core.placeholder()    // root node ptr (patched after node tree)
	countNodesOff := c.core.placeholder() // count_nodes (patched after node tree)
	c.core.zeros(12)                      // unknown1
	c.core.zeros(12)                      // unknown2
	c.core.zeros(4)                       // ref_count
	c.core.u32le(2)                       // type = 2 (geometry / model)

	// ---- header_model (120 bytes, offsets 112-231) ----
	c.core.zeros(2)                        // unknown0, unknown1
	c.core.u8(byte(ClassificationToCode(strings.ToUpper(m.Classification)))) // class code
	c.core.u8(byte(m.IgnoreFog))           // fog
	c.core.zeros(4)                        // count_child_model

	// animations ProxyList placeholder (offset 120)
	animListOff := c.core.placeholder()
	animListNumOff := c.core.placeholder()
	animListAllocOff := c.core.placeholder()

	c.core.zeros(4)  // p_supermodel
	c.core.zeros(24) // bounds (bmin + bmax)
	c.core.zeros(4)  // radius
	c.core.f32le(m.AnimationScale)
	super := m.SuperModel
	if super == "" || strings.EqualFold(super, "NULL") {
		super = "NULL"
	}
	c.core.fixedStr(super, 64) // supermodel name (64 bytes)
	// Total so far: 232 bytes ✓

	// ---- animation pointer array ----
	// Collect non-empty animations
	anims := m.Animations
	animArrayOff := c.core.len()
	animPtrOffs := make([]int, len(anims))
	for i := range anims {
		animPtrOffs[i] = c.core.placeholder()
	}

	// Patch animation list ProxyList
	if len(anims) > 0 {
		c.core.patchU32(animListOff, uint32(animArrayOff))
		c.core.patchU32(animListNumOff, uint32(len(anims)))
		c.core.patchU32(animListAllocOff, uint32(len(anims)))
	}
	// (if no anims, leave as zero ProxyList)

	// ---- geometry node tree ----
	var nodeCount int32
	var rootOff int32
	root := m.RootNode()
	if root != nil {
		rootOff = c.writeNode(root, 0, &nodeCount)
		c.core.patchU32(rootPtrOff, uint32(rootOff))
	}
	c.core.patchU32(countNodesOff, uint32(nodeCount))

	// ---- animation blocks ----
	for i, anim := range anims {
		animOff := c.writeAnimation(&anim)
		c.core.patchU32(animPtrOffs[i], uint32(animOff))
	}

	return nil
}


// writeAnimation writes a single Animation to core and returns its core offset.
func (c *compiler) writeAnimation(anim *Animation) int32 {
	animOff := int32(c.core.len())

	// ---- header_geometry (112 bytes) ----
	c.core.zeros(8)              // p_func1, p_func2
	c.core.fixedStr(anim.Name, 64) // animation name
	rootPtrOff := c.core.placeholder()    // root anim node ptr
	countNodesOff := c.core.placeholder() // count_nodes
	c.core.zeros(12)             // unknown1
	c.core.zeros(12)             // unknown2
	c.core.zeros(4)              // ref_count
	c.core.u32le(1)              // type = 1 (animation)

	// ---- header_animation (84 bytes after geometry) ----
	c.core.f32le(anim.Length)
	c.core.f32le(anim.TransTime)
	animRoot := anim.Root
	if animRoot == "" {
		animRoot = c.model.Name
	}
	c.core.fixedStr(animRoot, 64)

	// events ProxyList (placeholder)
	evtListOff := c.core.placeholder()
	evtListNumOff := c.core.placeholder()
	evtListAllocOff := c.core.placeholder()

	// ---- events array ----
	if len(anim.Events) > 0 {
		evtOff := int32(c.core.len())
		c.core.patchU32(evtListOff, uint32(evtOff))
		c.core.patchU32(evtListNumOff, uint32(len(anim.Events)))
		c.core.patchU32(evtListAllocOff, uint32(len(anim.Events)))
		for _, ev := range anim.Events {
			c.core.f32le(ev.Time)
			c.core.fixedStr(ev.Name, 32)
		}
	}

	// ---- animation node tree ----
	// Resolve parentage by tree position, not name, so duplicate-named
	// animation nodes each keep their own subtree and controllers.
	_, animChildIdx := resolveAnimTree(anim.Nodes)
	c.animGeomPair = pairAnimNodesToGeom(anim.Nodes, c.geomOccur)

	var animNodeCount int32
	var rootAnimOff int32
	var rootAnimNode *AnimNode
	for i := range anim.Nodes {
		if isRootParent(anim.Nodes[i].Parent) {
			rootAnimNode = &anim.Nodes[i]
			break
		}
	}
	if rootAnimNode == nil && len(anim.Nodes) > 0 {
		rootAnimNode = &anim.Nodes[0]
	}
	if rootAnimNode != nil {
		visited := make(map[*AnimNode]bool, len(anim.Nodes))
		rootAnimOff = c.writeAnimNode(rootAnimNode, animChildIdx, 0, &animNodeCount, visited)
	}

	c.core.patchU32(rootPtrOff, uint32(rootAnimOff))
	c.core.patchU32(countNodesOff, uint32(animNodeCount))

	return animOff
}

// writeAnimNode writes a single AnimNode and its children.
// Mirrors the read order in readAnimNodeDepth.
// Cycle safety: visited tracks already-written anim nodes to break cycles.
// animChildren maps lowercased parent name → child AnimNodes (O(1) lookup).
func (c *compiler) writeAnimNode(an *AnimNode, animChildren map[*AnimNode][]*AnimNode, parentOff int32, count *int32, visited map[*AnimNode]bool) int32 {
	if visited[an] {
		return 0
	}
	visited[an] = true
	// Animation nodes use contentBits derived from the geometry node, but
	// trimesh/skin/dangly/aabb bits are only set when the animation node
	// carries actual mesh data (animmesh). The game writes contentBits=1
	// (dummy) for trimesh animation nodes that only carry keyframes.
	// Light and emitter bits ARE preserved because the decompiler needs
	// them for controller ID dispatch.
	geomNode := c.animGeomPair[an]
	if geomNode == nil {
		geomNode = c.geomNodeByName(an.Name)
	}
	contentBits := uint32(1)
	hasLight := false
	hasEmitter := false
	hasRef := false
	hasMesh := false
	hasSkin := false
	hasAnimMesh := false
	hasDangly := false
	hasAABB := false
	meshCtrlOnly := false
	if geomNode != nil {
		gf := geomNode.NodeTypeFlag()
		if gf&0x02 != 0 { hasLight = true; contentBits |= 0x02 }
		if gf&0x04 != 0 { hasEmitter = true; contentBits |= 0x04 }
		if gf&0x10 != 0 { hasRef = true; contentBits |= 0x10 }
		// Mesh/skin/dangly/aabb only for animmesh animation nodes
		if gf&0x80 != 0 || (an.AnimMesh != nil) {
			if gf&0x20 != 0 { hasMesh = true; contentBits |= 0x20 }
			if gf&0x40 != 0 { hasSkin = true; contentBits |= 0x40 }
			if gf&0x80 != 0 { hasAnimMesh = true; contentBits |= 0x80 }
			if gf&0x100 != 0 { hasDangly = true; contentBits |= 0x100 }
			if gf&0x200 != 0 { hasAABB = true; contentBits |= 0x200 }
		} else if gf&0x20 != 0 && (len(an.AlphaKeys) > 0 || len(an.SelfIllumColorKeys) > 0) {
			// A plain (non-animmesh) trimesh animation node that carries
			// mesh controllers — alpha or self-illumination keys — must
			// still set the mesh content bit. Controller type IDs 128
			// (alpha) and 100 (selfillumcolor) are only resolved as mesh
			// controllers when the node's mesh bit is set; without it the
			// keys are silently dropped on compile (and unreadable on
			// decompile). BioWare does exactly this: e.g. vdr_magearmor2's
			// "shield" trimesh anim nodes are mesh-flagged and carry
			// alphakey while storing no geometry. We match that — set the
			// bit and emit an empty mesh header, but write no vert/face
			// data. See issue #12.
			contentBits |= 0x20
			meshCtrlOnly = true
		}
	}

	nodeOff := int32(c.core.len())
	*count++

	// ---- header_node (112 bytes) ----
	c.core.zeros(24)              // p_func1..p_func6
	c.core.i32le(0)               // inheritColor
	partNum := int32(0)
	if geomNode != nil {
		if id, ok := c.nodeIDs[geomNode]; ok {
			partNum = id
		}
	}
	c.core.i32le(partNum) // node_number / m_ID
	c.core.fixedStr(an.Name, 32) // node_name
	c.core.i32le(0)               // p_geometry (tree ptr) — engine fills at load
	c.core.i32le(parentOff)       // p_parent_node

	// children ProxyList — will be patched
	childListOffPos := c.core.placeholder()
	childListNumPos := c.core.placeholder()
	childListAllocPos := c.core.placeholder()

	// controller_keys ProxyList
	ctrlKeysPtrPos := c.core.placeholder()
	ctrlKeysNumPos := c.core.placeholder()
	ctrlKeysAllocPos := c.core.placeholder()

	// controller_data ProxyList
	ctrlDataPtrPos := c.core.placeholder()
	ctrlDataNumPos := c.core.placeholder()
	ctrlDataAllocPos := c.core.placeholder()

	c.core.u32le(contentBits) // content_node

	// Type-specific headers (same order as readAnimNodeDepth).
	if hasLight && geomNode != nil {
		c.writeLightHeader(geomNode)
	} else if hasLight {
		c.core.zeros(92)
	}
	if hasEmitter && geomNode != nil {
		c.writeEmitterHeader(geomNode)
	} else if hasEmitter {
		c.core.zeros(216)
	}
	if hasRef && geomNode != nil {
		c.writeReferenceHeader(geomNode)
	} else if hasRef {
		c.core.zeros(68)
	}

	// For mesh nodes in animation: write a minimal mesh header + skin/anim/dangly/aabb.
	var expanded *expandedMesh
	if hasMesh && geomNode != nil {
		c.writeMeshHeaderForAnimNode(geomNode, an)
		expanded = c.lastExpanded
	} else if hasMesh || meshCtrlOnly {
		// meshCtrlOnly: an empty 512-byte mesh header, no geometry. Just
		// enough for the mesh content bit to be structurally valid so the
		// alpha/selfillum controllers resolve; the actual geometry lives
		// on the matching geometry node. Matches BioWare's layout.
		c.core.zeros(meshHeaderSize)
	}

	if hasSkin && geomNode != nil {
		c.writeSkinHeader(geomNode, expanded)
	} else if hasSkin {
		c.core.zeros(100)
	}

	// AnimMesh sub-header
	var animMeshVertPtrPos, animMeshTVertPtrPos int
	var animMeshNVertSets, animMeshNTVertSets int
	if hasAnimMesh {
		animMeshVertPtrPos, animMeshTVertPtrPos, animMeshNVertSets, animMeshNTVertSets = c.writeAnimMeshHeader(geomNode, an)
	}

	if hasDangly && geomNode != nil {
		c.writeDanglyHeader(geomNode)
	} else if hasDangly {
		c.core.zeros(24)
	}
	if hasAABB {
		c.core.zeros(4) // pAABB placeholder (no AABB tree in anim nodes)
	}

	// Deferred variable-length data (must come after all fixed-size headers).
	if hasMesh && expanded != nil && geomNode != nil {
		animMesh := geomNode.Mesh
		if an != nil && an.Mesh != nil {
			animMesh = an.Mesh
		}
		c.writeMeshFaceData(animMesh, expanded)
	}
	if hasDangly && geomNode != nil {
		c.writeDanglyConstraints(geomNode)
	}

	// Write anim vert/tvert data into core (animMesh).
	// Resolve data source: prefer an.AnimMesh, fall back to geomNode.AnimMesh.
	if hasAnimMesh && animMeshNVertSets > 0 {
		var srcVerts []Vec3
		if an.AnimMesh != nil {
			srcVerts = an.AnimMesh.AnimVerts
		} else if geomNode != nil && geomNode.AnimMesh != nil {
			srcVerts = geomNode.AnimMesh.AnimVerts
		}
		animVertsOff := int32(c.core.len())
		c.core.patchU32(animMeshVertPtrPos, uint32(animVertsOff))
		for _, v := range srcVerts {
			c.core.vec3(v)
		}
	}
	if hasAnimMesh && animMeshNTVertSets > 0 {
		var srcTVerts []Vec3
		if an.AnimMesh != nil {
			srcTVerts = an.AnimMesh.AnimTVerts
		} else if geomNode != nil && geomNode.AnimMesh != nil {
			srcTVerts = geomNode.AnimMesh.AnimTVerts
		}
		animTVertsOff := int32(c.core.len())
		c.core.patchU32(animMeshTVertPtrPos, uint32(animTVertsOff))
		for _, v := range srcTVerts {
			c.core.f32le(v.X)
			c.core.f32le(v.Y)
		}
	}

	// ---- Controllers ----
	nodeFlag := contentBits
	if hasLight {
		nodeFlag = 3
	} else if hasEmitter {
		nodeFlag = 5
	} else if hasMesh || meshCtrlOnly {
		nodeFlag = 33
	}
	ctrlKeys, timeArr, dataArr := c.encodeAnimNodeControllers(an, nodeFlag)
	c.writeCtrlBlock(ctrlKeys, timeArr, dataArr,
		ctrlKeysPtrPos, ctrlKeysNumPos, ctrlKeysAllocPos,
		ctrlDataPtrPos, ctrlDataNumPos, ctrlDataAllocPos)

	// ---- Children ----
	children := animChildren[an]

	childArrayOff := int32(c.core.len())
	childOffsets := make([]int, len(children))
	for i := range children {
		childOffsets[i] = c.core.placeholder()
	}

	if len(children) > 0 {
		c.core.patchU32(childListOffPos, uint32(childArrayOff))
	}
	c.core.patchU32(childListNumPos, uint32(len(children)))
	c.core.patchU32(childListAllocPos, uint32(len(children)))

	for i, child := range children {
		childOff := c.writeAnimNode(child, animChildren, nodeOff, count, visited)
		c.core.patchU32(childOffsets[i], uint32(childOff))
	}

	return nodeOff
}

// writeMeshHeaderForAnimNode writes the 512-byte mesh header for an animation node.
//
// Regular trimesh anim nodes do NOT duplicate vertex or face data — the game
// writes a zeroed header with all MDX pointers = -1. Only animmesh nodes (which
// carry per-animation vertex positions) write real MDX data.
//
// Returns facesDef ptr, pMdxVertex, countVerts, pMdxTex0 for use by caller.
func (c *compiler) writeMeshHeaderForAnimNode(geomNode *Node, an *AnimNode) (int32, int32, int32, int32) {
	// AnimmMesh anim nodes carry their own per-animation vertex sets — full write.
	isAnimMesh := (an != nil && an.AnimMesh != nil) || (geomNode != nil && geomNode.AnimMesh != nil)
	if isAnimMesh {
		mesh := geomNode.Mesh
		if an != nil && an.Mesh != nil {
			mesh = an.Mesh
		}
		if mesh == nil {
			c.core.zeros(meshHeaderSize)
			return 0, -1, 0, -1
		}
		fp, mv, nv, mt := c.writeMeshHeaderInner(mesh, geomNode)
		return fp, mv, int32(nv), mt
	}

	// Regular trimesh/skin/dangly/aabb anim nodes: write a zeroed header.
	// All MDX pointers remain -1 so the decompiler skips vertex reads.
	// The geometry section already has the authoritative mesh data.
	c.core.zeros(meshHeaderSize)
	return 0, -1, 0, -1
}

// writeAnimMeshHeader writes a 56-byte header_anim sub-header.
// Returns placeholders to patch pAnimVerts and pAnimTVerts after writing data.
func (c *compiler) writeAnimMeshHeader(geomNode *Node, an *AnimNode) (pAnimVertPos, pAnimTVertPos int, nVertexSets, nTVertSets int) {
	var sp float32
	var animVerts []Vec3
	var animTVerts []Vec3
	if an != nil && an.AnimMesh != nil {
		sp = an.AnimMesh.SamplePeriod
		animVerts = an.AnimMesh.AnimVerts
		animTVerts = an.AnimMesh.AnimTVerts
	} else if geomNode != nil && geomNode.AnimMesh != nil {
		sp = geomNode.AnimMesh.SamplePeriod
		animVerts = geomNode.AnimMesh.AnimVerts
		animTVerts = geomNode.AnimMesh.AnimTVerts
	}

	nVerts := 0
	if geomNode != nil && geomNode.Mesh != nil {
		nVerts = len(geomNode.Mesh.Verts)
	}
	if nVerts > 0 {
		nVertexSets = len(animVerts) / nVerts
		nTVertSets = len(animTVerts) / nVerts
	}

	c.core.f32le(sp)
	c.core.proxyListEmpty() // animation_vertices (legacy)
	c.core.proxyListEmpty() // animation_texcoords (legacy)
	c.core.proxyListEmpty() // animation_normals (legacy)
	pAnimVertPos = c.core.placeholder()  // pAnimVerts
	pAnimTVertPos = c.core.placeholder() // pAnimTVerts
	c.core.i32le(int32(nVertexSets))
	c.core.i32le(int32(nTVertSets))

	return
}
