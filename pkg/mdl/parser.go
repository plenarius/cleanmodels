// ASCII MDL parser.
//
// Parses NWN ASCII .mdl files into Model structs. Covers all node types and
// parameters from load_models.pl paramtype/3 declarations (lines 648-898).
//
// Features:
//   - Accumulated error handling (never aborts on a single bad line)
//   - near_match typo tolerance (Levenshtein <= 2)
//   - Unknown node types fall back to dummy
//
// Ref: load_models.pl (full file)
// Ref: output_models.pl (for parameter ordering on write)
package mdl

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"math"
	"os"
	"runtime"
	"strconv"
	"strings"
)

// ParseError records a non-fatal parse issue.
type ParseError struct {
	Line    int
	Message string
}

func (e ParseError) Error() string {
	return fmt.Sprintf("line %d: %s", e.Line, e.Message)
}

func parserPanicMessage(r interface{}) string {
	if os.Getenv("CLEANMODELS_DEBUG") != "" {
		buf := make([]byte, 4096)
		n := runtime.Stack(buf, false)
		return fmt.Sprintf("parser panic: %v\n%s", r, buf[:n])
	}
	return fmt.Sprintf("parser panic: %v", r)
}

// ParseResult holds the parsed model and any accumulated errors.
type ParseResult struct {
	Model  *Model
	Errors []ParseError
}

// ParseFile reads an ASCII MDL file from disk.
func ParseFile(path string) (result *ParseResult, err error) {
	defer func() {
		if r := recover(); r != nil {
			result = &ParseResult{
				Model:  &Model{AnimationScale: 1.0},
				Errors: []ParseError{{Message: parserPanicMessage(r)}},
			}
			err = fmt.Errorf("parser panic: %v", r)
		}
	}()

	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	return Parse(f)
}

// Parse reads an ASCII MDL from a reader.
func Parse(r io.Reader) (result *ParseResult, err error) {
	defer func() {
		if r := recover(); r != nil {
			result = &ParseResult{
				Model:  &Model{AnimationScale: 1.0},
				Errors: []ParseError{{Message: parserPanicMessage(r)}},
			}
			err = fmt.Errorf("parser panic: %v", r)
		}
	}()

	p := &parser{
		scanner:  bufio.NewScanner(r),
		model:    &Model{AnimationScale: 1.0},
		maxAlloc: 512 * 1024 * 1024,
	}
	p.scanner.Buffer(make([]byte, 0, 1024*1024), 1024*1024)
	p.parse()
	if err := p.scanner.Err(); err != nil {
		p.errorf("read error: %v", err)
	}
	result = &ParseResult{
		Model:  p.model,
		Errors: p.errors,
	}
	if !p.sawModel {
		if !p.sawContent {
			return result, fmt.Errorf("%w: the file is empty (or holds only comments)", ErrNotModel)
		}
		return result, fmt.Errorf("%w: no \"newmodel\" line (it should start with \"newmodel <name>\")", ErrNotModel)
	}
	if p.model.Name == "" {
		p.errorf("no \"newmodel\" line, so the model has no name")
		result.Errors = p.errors
	}
	return result, nil
}

// ErrNotModel is returned (wrapped, with detail) by Parse when the input shows no
// sign of being an ASCII MDL at all: no model, geometry, node or animation line.
// Such a file used to parse "successfully" into an empty model, so check passed
// it and compile wrote a bogus binary from it.
var ErrNotModel = errors.New("not an ASCII MDL model")

type parser struct {
	scanner *bufio.Scanner
	// sawContent is set once any non-blank, non-comment line is read; sawModel once
	// a line that structures an MDL (newmodel, node, ...) is. They tell an empty file
	// from a file of something else.
	sawContent, sawModel bool
	line                 int
	model                *Model
	errors               []ParseError

	currentNode     *Node
	currentAnim     *Animation
	currentAnimNode *AnimNode
	nodeStack       []*Node

	allocBytes int64
	maxAlloc   int64
}

func (p *parser) errorf(format string, args ...interface{}) {
	p.errors = append(p.errors, ParseError{
		Line:    p.line,
		Message: fmt.Sprintf(format, args...),
	})
}

// floatParam stores the value of a one-value parameter line ("shininess 1.0")
// in dst. A missing or unusable value leaves dst as it was and is reported with
// the line number, rather than silently keeping the default.
func (p *parser) floatParam(tokens []string, dst *float32) {
	v, err := parseFloat(safeIndex(tokens, 1))
	if err != nil {
		p.badParam(tokens, "number")
		return
	}
	*dst = v
}

// intParam is floatParam for integer parameters.
func (p *parser) intParam(tokens []string, dst *int32) {
	v, err := parseInt(safeIndex(tokens, 1))
	if err != nil {
		p.badParam(tokens, "integer")
		return
	}
	*dst = v
}

// boolParam is intParam for a flag that some tools write as true/false
// instead of 1/0 (NWNCQ's emitters do). Reading "true" as unusable would have
// dropped the flag; any other non-number is still reported.
func (p *parser) boolParam(tokens []string, dst *int32) {
	switch strings.ToLower(safeIndex(tokens, 1)) {
	case "true":
		*dst = 1
	case "false":
		*dst = 0
	default:
		p.intParam(tokens, dst)
	}
}

// floatAt reads tokens[i] as a float for a line whose values are all needed
// ("position x y z"). An unusable one is set to 0 and reported, as the list
// readers do, instead of silently becoming 0.
func (p *parser) floatAt(tokens []string, i int) float32 {
	v, err := parseFloat(safeIndex(tokens, i))
	if err != nil {
		p.errorf("%q is not a usable number for value %d of %q, set to 0", safeIndex(tokens, i), i, tokens[0])
		return 0
	}
	return v
}

// shortParam reports a line with fewer values than its keyword needs.
func (p *parser) shortParam(tokens []string, want int) {
	p.errorf("%q has %d value(s), expected %d; the line is ignored", tokens[0], len(tokens)-1, want)
}

// vec3Param stores a "keyword x y z" line in dst. A short line or an unusable
// number leaves dst as it was and is reported with the line number.
func (p *parser) vec3Param(tokens []string, dst *Vec3) {
	if len(tokens) < 4 {
		p.shortParam(tokens, 3)
		return
	}
	v, ok := parseVec3(tokens[1:])
	if !ok {
		p.errorf("%q has an unusable number (%s), keeping the default", tokens[0], strings.Join(tokens[1:4], " "))
		return
	}
	*dst = v
}

// vec4Param is vec3Param for "keyword x y z w" lines.
func (p *parser) vec4Param(tokens []string, dst *Vec4) {
	if len(tokens) < 5 {
		p.shortParam(tokens, 4)
		return
	}
	v, ok := parseVec4(tokens[1:])
	if !ok {
		p.errorf("%q has an unusable number (%s), keeping the default", tokens[0], strings.Join(tokens[1:5], " "))
		return
	}
	*dst = v
}

// countParam reads the entry count that opens a list. When it is unusable the
// list cannot be read, so say so rather than letting its lines fall through.
func (p *parser) countParam(tokens []string) (int32, bool) {
	v, err := parseInt(safeIndex(tokens, 1))
	if err != nil {
		p.badParam(tokens, "count")
		return 0, false
	}
	return v, true
}

func (p *parser) badParam(tokens []string, kind string) {
	if len(tokens) < 2 {
		p.errorf("%q has no value, keeping the default", tokens[0])
		return
	}
	p.errorf("%q is not a usable %s for %q, keeping the default", tokens[1], kind, tokens[0])
}

func (p *parser) trackAlloc(bytes int64) bool {
	p.allocBytes += bytes
	return p.allocBytes <= p.maxAlloc
}

func (p *parser) nextLine() (string, bool) {
	for p.scanner.Scan() {
		p.line++
		line := strings.TrimSpace(p.scanner.Text())
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		return line, true
	}
	return "", false
}

func (p *parser) parse() {
	for {
		line, ok := p.nextLine()
		if !ok {
			break
		}

		tokens := tokenize(line)
		if len(tokens) == 0 {
			continue
		}
		p.sawContent = true

		keyword := strings.ToLower(tokens[0])
		switch keyword {
		case "newmodel", "donemodel", "beginmodelgeom", "endmodelgeom", "node", "newanim", "setsupermodel":
			p.sawModel = true
		}

		switch {
		case keyword == "newmodel":
			p.parseNewModel(tokens)
		case keyword == "donemodel":
			// End of model
		case keyword == "setsupermodel":
			p.parseSuperModel(tokens)
		case keyword == "setanimationscale":
			p.parseAnimationScale(tokens)
		case keyword == "classification":
			p.parseClassification(tokens)
		case keyword == "ignorefog":
			p.parseIgnoreFog(tokens)
		case keyword == "filedependancy" || keyword == "filedependency":
			if len(tokens) >= 2 {
				p.model.FileDependancy = tokens[1]
			}
		case keyword == "beginmodelgeom":
			// Start of geometry section
		case keyword == "endmodelgeom":
			// End of geometry section
		case keyword == "node":
			p.parseNodeStart(tokens)
		case keyword == "endnode":
			p.parseNodeEnd()
		case keyword == "newanim":
			p.parseNewAnim(tokens)
		case keyword == "doneanim":
			p.parseDoneAnim()
		case (keyword == "length" || keyword == "animlength") && p.currentAnim != nil && p.currentAnimNode == nil:
			p.parseAnimLength(tokens)
		case keyword == "transtime" && p.currentAnim != nil && p.currentAnimNode == nil:
			p.parseAnimTransTime(tokens)
		case keyword == "animroot":
			p.parseAnimRoot(tokens)
		case keyword == "event" && p.currentAnim != nil:
			p.parseAnimEvent(tokens)
		default:
			p.parseParameter(tokens)
		}
	}
}

func (p *parser) parseNewModel(tokens []string) {
	if len(tokens) >= 2 {
		p.model.Name = tokens[1]
	}
	p.model.FileType = "ascii"
}

// newShadowedMeshData is a mesh as the game compiler defaults it when the source
// has no "shadow" line: it casts shadows. Compiled against the engine's output
// for 180 such meshes (159 trimesh, 8 skin, 13 danglymesh), it wrote shadow 1
// every time; we wrote 0, so a model compiled from a source without the line
// cast no shadow in game. Walkmeshes (aabb) default to 0 and animmesh is
// unverified, so both keep the zero default.
func newShadowedMeshData() *MeshData {
	m := NewMeshData()
	m.Shadow = 1
	return m
}

func (p *parser) parseSuperModel(tokens []string) {
	if len(tokens) >= 3 {
		p.model.SuperModel = tokens[2]
	} else if len(tokens) >= 2 {
		p.model.SuperModel = tokens[1]
	}
}

func (p *parser) parseAnimationScale(tokens []string) {
	if len(tokens) >= 2 {
		p.floatParam(tokens, &p.model.AnimationScale)
	}
}

func (p *parser) parseClassification(tokens []string) {
	if len(tokens) >= 2 {
		p.model.Classification = strings.ToUpper(tokens[1])
	}
}

func (p *parser) parseIgnoreFog(tokens []string) {
	if len(tokens) >= 2 {
		p.intParam(tokens, &p.model.IgnoreFog)
	}
}

func (p *parser) parseNodeStart(tokens []string) {
	if len(tokens) < 3 {
		p.errorf("node requires type and name")
		return
	}

	nodeType := strings.ToLower(tokens[1])
	nodeName := tokens[2]

	if !p.trackAlloc(512) {
		p.errorf("allocation limit exceeded creating node %q", nodeName)
		return
	}

	node := &Node{
		Name:        nodeName,
		Scale:       1.0,
		Orientation: Vec4{W: 1.0},
	}

	switch nodeType {
	case "trimesh":
		node.Mesh = newShadowedMeshData()
	case "skin":
		node.Mesh = newShadowedMeshData()
		node.Skin = &SkinData{}
	case "animmesh":
		node.Mesh = NewMeshData()
		node.AnimMesh = &AnimMeshData{}
	case "danglymesh":
		node.Mesh = newShadowedMeshData()
		node.Dangly = &DanglyData{}
	case "aabb":
		node.Mesh = NewMeshData()
		node.Aabb = &AabbData{}
	case "emitter":
		node.Emitter = &EmitterData{}
	case "light":
		node.Light = &LightData{}
	case "reference":
		node.Reference = &ReferenceData{}
	case "camera":
		node.Camera = true
	case "dummy":
		// No extra data
	case "patch":
		// Old BioWare node type, fall back to dummy
		p.errorf("unknown node type %q, treating as dummy", nodeType)
	default:
		p.errorf("unknown node type %q, treating as dummy", nodeType)
	}

	if p.currentAnim != nil {
		// Animation node
		animNode := &AnimNode{
			Name: nodeName,
		}
		p.currentAnimNode = animNode
	} else {
		p.currentNode = node
	}

	if p.currentAnim == nil {
		if len(p.nodeStack) > 0 {
			parent := p.nodeStack[len(p.nodeStack)-1]
			node.Parent = parent.Name
		}
		p.nodeStack = append(p.nodeStack, node)
		p.model.Nodes = append(p.model.Nodes, node)
	}
}

func (p *parser) parseNodeEnd() {
	if p.currentAnim != nil && p.currentAnimNode != nil {
		p.currentAnim.Nodes = append(p.currentAnim.Nodes, *p.currentAnimNode)
		p.currentAnimNode = nil
		return
	}

	if len(p.nodeStack) > 0 {
		p.nodeStack = p.nodeStack[:len(p.nodeStack)-1]
	}
	if len(p.nodeStack) > 0 {
		p.currentNode = p.nodeStack[len(p.nodeStack)-1]
	} else {
		p.currentNode = nil
	}
}

func (p *parser) parseNewAnim(tokens []string) {
	if len(tokens) < 3 {
		p.errorf("newanim requires name and model name")
		return
	}
	p.currentAnim = &Animation{
		Name: tokens[1],
	}
}

func (p *parser) parseDoneAnim() {
	if p.currentAnim != nil {
		p.model.Animations = append(p.model.Animations, *p.currentAnim)
		p.currentAnim = nil
	}
}

func (p *parser) parseAnimLength(tokens []string) {
	if len(tokens) < 2 {
		return
	}
	idx := 1
	if strings.EqualFold(tokens[0], "animlength") && len(tokens) >= 3 {
		idx = 2
	}
	if idx > 1 {
		tokens = tokens[1:]
	}
	p.floatParam(tokens, &p.currentAnim.Length)
}

func (p *parser) parseAnimTransTime(tokens []string) {
	if len(tokens) >= 2 {
		p.floatParam(tokens, &p.currentAnim.TransTime)
	}
}

func (p *parser) parseAnimRoot(tokens []string) {
	if p.currentAnim != nil && len(tokens) >= 2 {
		p.currentAnim.Root = tokens[1]
	}
}

func (p *parser) parseAnimEvent(tokens []string) {
	if p.currentAnim != nil && len(tokens) >= 3 {
		if t, err := parseFloat(tokens[1]); err == nil {
			p.currentAnim.Events = append(p.currentAnim.Events, AnimEvent{
				Time: t,
				Name: tokens[2],
			})
		}
	}
}

func (p *parser) parseParameter(tokens []string) {
	if len(tokens) == 0 {
		return
	}

	keyword := strings.ToLower(tokens[0])

	// Handle animation node parameters
	if p.currentAnim != nil && p.currentAnimNode != nil {
		p.parseAnimNodeParam(keyword, tokens)
		return
	}

	if p.currentNode == nil {
		return
	}

	// Route to appropriate handler based on node capabilities
	node := p.currentNode
	if name, ok := node.declaredName(keyword); ok {
		node.declare(name)
	}

	switch keyword {
	case "parent":
		if len(tokens) >= 2 {
			node.Parent = tokens[1]
		}
	case "#part-number":
		if len(tokens) >= 2 {
			p.intParam(tokens, &node.PartNumber)
		}
	case "position":
		p.vec3Param(tokens, &node.Position)
	case "orientation":
		p.vec4Param(tokens, &node.Orientation)
	case "scale":
		if len(tokens) >= 2 {
			p.floatParam(tokens, &node.Scale)
		}
	case "inheritcolor":
		if len(tokens) >= 2 {
			p.intParam(tokens, &node.InheritColor)
		}
	case "wirecolor":
		p.vec3Param(tokens, &node.WireColor)
	default:
		p.parseNodeSpecificParam(keyword, tokens)
	}
}

func (p *parser) parseNodeSpecificParam(keyword string, tokens []string) {
	node := p.currentNode
	if node == nil {
		return
	}

	// Mesh parameters
	if node.Mesh != nil {
		if p.parseMeshParam(node.Mesh, keyword, tokens) {
			return
		}
	}

	// Type-specific parameters
	switch {
	case node.Skin != nil:
		if p.parseSkinParam(node, keyword, tokens) {
			return
		}
	case node.Dangly != nil:
		if p.parseDanglyParam(node.Dangly, keyword, tokens) {
			return
		}
	case node.AnimMesh != nil:
		if p.parseAnimMeshParam(node.AnimMesh, keyword, tokens) {
			return
		}
	case node.Aabb != nil:
		if p.parseAabbParam(node.Aabb, keyword, tokens) {
			return
		}
	case node.Emitter != nil:
		if p.parseEmitterParam(node.Emitter, keyword, tokens) {
			return
		}
	case node.Light != nil:
		if p.parseLightParam(node.Light, keyword, tokens) {
			return
		}
	case node.Reference != nil:
		if p.parseReferenceParam(node.Reference, keyword, tokens) {
			return
		}
	}

	// Try fuzzy matching for unrecognized parameters
	nodeType := node.NodeType()
	validParams := validParamsForType(nodeType)
	if matched, ok := NearMatchParam(keyword, validParams); ok && matched != keyword {
		p.errorf("fuzzy matched %q to %q", keyword, matched)
		newTokens := make([]string, len(tokens))
		copy(newTokens, tokens)
		newTokens[0] = matched
		p.parseParameter(newTokens)
		return
	}
}

func (p *parser) parseMeshParam(mesh *MeshData, keyword string, tokens []string) bool {
	switch keyword {
	case "diffuse":
		p.vec3Param(tokens, &mesh.Diffuse)
	case "ambient":
		p.vec3Param(tokens, &mesh.Ambient)
	case "specular":
		p.vec3Param(tokens, &mesh.Specular)
	case "shininess":
		p.floatParam(tokens, &mesh.Shininess)
	case "bitmap", "texture0":
		if len(tokens) >= 2 {
			mesh.Bitmap = tokens[1]
		}
	case "texture1":
		if len(tokens) >= 2 {
			mesh.Texture1 = tokens[1]
		}
	case "texture2":
		if len(tokens) >= 2 {
			mesh.Texture2 = tokens[1]
		}
	case "materialname":
		if len(tokens) >= 2 {
			mesh.MaterialName = tokens[1]
		}
	case "renderhint":
		if len(tokens) >= 2 {
			mesh.RenderHint = tokens[1]
		}
	case "shadow":
		p.intParam(tokens, &mesh.Shadow)
	case "beaming":
		p.intParam(tokens, &mesh.Beaming)
	case "render":
		p.intParam(tokens, &mesh.Render)
	case "transparencyhint":
		p.intParam(tokens, &mesh.TransparencyHint)
	case "alpha":
		p.floatParam(tokens, &mesh.Alpha)
	case "selfillumcolor", "setfillumcolor":
		p.vec3Param(tokens, &mesh.SelfIllumColor)
	case "tilefade":
		p.intParam(tokens, &mesh.TileFade)
	case "rotatetexture":
		p.intParam(tokens, &mesh.RotateTexture)
	case "lightmapped":
		p.intParam(tokens, &mesh.LightMapped)
	case "center":
		// center can be mn3 or man -- accept both forms
		// As mn3: center x y z
		// As man: center <string>
		if len(tokens) >= 4 {
			// vec3 form -- ignore, not stored as mesh property
		}
	case "verts":
		p.parseCountedVec3(tokens, &mesh.Verts)
	case "faces":
		p.parseCountedFaces(tokens, &mesh.Faces)
	case "tverts":
		p.parseCountedVec3(tokens, &mesh.TVerts)
	case "tverts1":
		p.parseCountedVec3(tokens, &mesh.TVerts1)
	case "tverts2":
		p.parseCountedVec3(tokens, &mesh.TVerts2)
	case "tverts3":
		p.parseCountedVec3(tokens, &mesh.TVerts3)
	case "colors":
		p.parseCountedVec3(tokens, &mesh.Colors)
	case "normals":
		p.parseCountedVec3(tokens, &mesh.Normals)
	case "tangents":
		p.parseCountedVec4(tokens, &mesh.Tangents)
	case "texindices0":
		p.parseCountedInt3(tokens, &mesh.TexIndices0)
	case "texindices1":
		p.parseCountedInt3(tokens, &mesh.TexIndices1)
	case "texindices2":
		p.parseCountedInt3(tokens, &mesh.TexIndices2)
	case "texindices3":
		p.parseCountedInt3(tokens, &mesh.TexIndices3)
	case "multimaterial":
		p.parseCountedStrings(tokens, &mesh.Multimaterial)
	case "mirrorlist":
		p.parseCountedInts(tokens, &mesh.MirrorList)
	default:
		return false
	}
	return true
}

func (p *parser) parseSkinParam(node *Node, keyword string, tokens []string) bool {
	switch keyword {
	case "weights":
		if len(tokens) >= 2 {
			count, ok := p.countParam(tokens)
			if ok && count > 0 {
				p.readWeightList(node.Skin, int(count))
			}
		}
	default:
		return false
	}
	return true
}

func (p *parser) parseDanglyParam(dangly *DanglyData, keyword string, tokens []string) bool {
	switch keyword {
	case "displacement":
		p.floatParam(tokens, &dangly.Displacement)
	case "tightness":
		p.floatParam(tokens, &dangly.Tightness)
	case "period":
		p.floatParam(tokens, &dangly.Period)
	case "constraints":
		if len(tokens) >= 2 {
			count, ok := p.countParam(tokens)
			if ok && count > 0 {
				p.readFloatList(&dangly.Constraints, int(count))
			}
		}
	case "displtype":
		p.intParam(tokens, &dangly.DisplType)
	case "danglymesh", "showdispl", "gizmo":
		// Accepted but not stored (display-only parameters)
	default:
		return false
	}
	return true
}

func (p *parser) parseAnimMeshParam(am *AnimMeshData, keyword string, tokens []string) bool {
	switch keyword {
	case "sampleperiod":
		p.floatParam(tokens, &am.SamplePeriod)
	case "clipu":
		p.floatParam(tokens, &am.ClipU)
	case "clipv":
		p.floatParam(tokens, &am.ClipV)
	case "clipw":
		p.floatParam(tokens, &am.ClipW)
	case "cliph":
		p.floatParam(tokens, &am.ClipH)
	case "animverts":
		if len(tokens) >= 2 {
			count, ok := p.countParam(tokens)
			if ok && count > 0 {
				p.readVec3List(&am.AnimVerts, int(count))
			}
		}
	case "animtverts":
		if len(tokens) >= 2 {
			count, ok := p.countParam(tokens)
			if ok && count > 0 {
				p.readVec3List(&am.AnimTVerts, int(count))
			}
		}
	default:
		return false
	}
	return true
}

func (p *parser) parseAabbParam(aabb *AabbData, keyword string, tokens []string) bool {
	switch keyword {
	case "aabb":
		if len(tokens) >= 7 {
			// The form the game and NWMax write: the root entry shares the
			// keyword's line and the preorder tree follows, with no count.
			p.readInlineAabb(&aabb.Entries, tokens[1:])
		} else if len(tokens) >= 2 {
			count, ok := p.countParam(tokens)
			if ok && count > 0 {
				p.readAabbList(&aabb.Entries, int(count))
			}
		}
	default:
		return false
	}
	return true
}

// readInlineAabb reads a tree written without a count. Entries are in preorder
// and an entry with a negative face has two children, so the tree ends when
// every branch has reached a leaf.
func (p *parser) readInlineAabb(out *[]AabbEntry, first []string) {
	pending := 1
	tokens := first
	for pending > 0 {
		if len(tokens) < 7 {
			p.errorf("aabb entry needs 7 values, found %d", len(tokens))
			return
		}
		if !p.trackAlloc(32) {
			p.errorf("allocation limit exceeded reading aabb entries")
			return
		}
		w := p.newListWarn()
		var f [6]float32
		for k := range f {
			f[k] = w.float(tokens[k])
		}
		face := w.int(tokens[6])
		w.done()
		*out = append(*out, AabbEntry{
			BoundMin: Vec3{X: f[0], Y: f[1], Z: f[2]},
			BoundMax: Vec3{X: f[3], Y: f[4], Z: f[5]},
			LeafFace: face,
		})
		pending--
		if face < 0 {
			pending += 2
		}
		if pending == 0 {
			return
		}
		line, ok := p.nextLine()
		if !ok {
			p.errorf("aabb tree ends early: %d entries still expected", pending)
			return
		}
		tokens = tokenize(line)
	}
}

func (p *parser) parseEmitterParam(em *EmitterData, keyword string, tokens []string) bool {
	switch keyword {
	case "deadspace":
		p.floatParam(tokens, &em.DeadSpace)
	case "blastradius":
		p.floatParam(tokens, &em.BlastRadius)
	case "blastlength":
		p.floatParam(tokens, &em.BlastLength)
	case "xgrid":
		p.intParam(tokens, &em.XGrid)
	case "ygrid":
		p.intParam(tokens, &em.YGrid)
	case "spawntype":
		p.intParam(tokens, &em.SpawnType)
	case "update":
		if len(tokens) >= 2 {
			em.Update = tokens[1]
		}
	case "render":
		if len(tokens) >= 2 {
			em.Render = tokens[1]
		}
	case "blend":
		if len(tokens) >= 2 {
			em.Blend = tokens[1]
		}
	case "texture":
		if len(tokens) >= 2 {
			em.Texture = tokens[1]
		}
	case "chunkname":
		if len(tokens) >= 2 {
			em.ChunkName = tokens[1]
		}
	case "twosidedtex":
		p.intParam(tokens, &em.TwoSidedTex)
	case "loop":
		p.intParam(tokens, &em.Loop)
	case "renderorder":
		p.intParam(tokens, &em.RenderOrder)
	case "p2p":
		p.intParam(tokens, &em.P2P)
	case "p2p_sel":
		p.intParam(tokens, &em.P2PSel)
	case "affectedbywind":
		p.boolParam(tokens, &em.AffectedByWind)
	case "m_istinted":
		p.intParam(tokens, &em.IsTinted)
	case "bounce":
		p.intParam(tokens, &em.Bounce)
	case "random":
		p.intParam(tokens, &em.Random)
	case "inherit":
		p.intParam(tokens, &em.Inherit)
	case "inheritvel":
		p.intParam(tokens, &em.InheritVel)
	case "inherit_local":
		p.intParam(tokens, &em.InheritLocal)
	case "splat":
		p.intParam(tokens, &em.Splat)
	case "inherit_part":
		p.intParam(tokens, &em.InheritPart)
	case "p2p_type":
		if len(tokens) >= 2 {
			em.P2PType = tokens[1]
		}
	case "render_sel":
		p.intParam(tokens, &em.RenderSel)
	case "blend_sel":
		p.intParam(tokens, &em.BlendSel)
	case "update_sel":
		p.intParam(tokens, &em.UpdateSel)
	case "spawntype_sel":
		p.intParam(tokens, &em.SpawnTypeSel)
	case "opacity":
		p.intParam(tokens, &em.Opacity)
	case "iconsize":
		p.intParam(tokens, &em.IconSize)
	case "lockaxes":
		p.intParam(tokens, &em.LockAxes)
	case "chunky":
		p.intParam(tokens, &em.Chunky)
	default:
		if getter, ok := emitterFloatFields[keyword]; ok {
			p.floatParam(tokens, getter(em))
			return true
		}
		if getter, ok := emitterColorFields[keyword]; ok {
			p.vec3Param(tokens, getter(em))
			return true
		}
		return false
	}
	return true
}

func (p *parser) parseLightParam(light *LightData, keyword string, tokens []string) bool {
	switch keyword {
	case "flareradius":
		p.floatParam(tokens, &light.FlareRadius)
	case "lensflares":
		p.intParam(tokens, &light.LensFlares)
	case "lightpriority":
		p.intParam(tokens, &light.LightPriority)
	case "ambientonly":
		p.intParam(tokens, &light.AmbientOnly)
	case "isdynamic":
		p.intParam(tokens, &light.NDynamicType)
	case "ndynamictype", "n_dynamic_type":
		p.intParam(tokens, &light.NDynamicType)
	case "affectdynamic":
		p.intParam(tokens, &light.AffectDynamic)
	case "shadow":
		p.intParam(tokens, &light.Shadow)
	case "generateflare":
		p.intParam(tokens, &light.GenerateFlare)
	case "fadinglight":
		p.intParam(tokens, &light.FadingLight)
	case "negativelight":
		p.intParam(tokens, &light.NegativeLight)
	case "color":
		p.vec3Param(tokens, &light.Color)
	case "radius":
		p.floatParam(tokens, &light.Radius)
	case "multiplier":
		p.floatParam(tokens, &light.Multiplier)
	case "shadowradius":
		p.floatParam(tokens, &light.ShadowRadius)
	case "verticaldisplacement":
		p.floatParam(tokens, &light.VerticalDisplacement)
	case "flaresizes":
		if len(tokens) >= 2 {
			count, ok := p.countParam(tokens)
			if ok && count > 0 {
				p.readFloatList(&light.FlareSizes, int(count))
			}
		}
	case "flarepositions":
		if len(tokens) >= 2 {
			count, ok := p.countParam(tokens)
			if ok && count > 0 {
				p.readFloatList(&light.FlarePositions, int(count))
			}
		}
	case "flarecolorshifts":
		if len(tokens) >= 2 {
			count, ok := p.countParam(tokens)
			if ok && count > 0 {
				p.readVec3List(&light.FlareColorShifts, int(count))
			}
		}
	case "texturenames":
		if len(tokens) >= 2 {
			count, ok := p.countParam(tokens)
			if ok && count > 0 {
				p.readStringList(&light.TextureNames, int(count))
			}
		}
	default:
		return false
	}
	return true
}

func (p *parser) parseReferenceParam(ref *ReferenceData, keyword string, tokens []string) bool {
	switch keyword {
	case "refmodel":
		if len(tokens) >= 2 {
			ref.RefModel = tokens[1]
		}
	case "reattachable":
		p.intParam(tokens, &ref.Reattachable)
	default:
		return false
	}
	return true
}

// emitterFloatFields maps controller/keyword names to float32 field pointers on EmitterData.
// Shared by binary decompiler's setEmitterController and the ASCII parser's parseEmitterParam.
var emitterFloatFields = map[string]func(*EmitterData) *float32{
	"alphastart":      func(e *EmitterData) *float32 { return &e.AlphaStart },
	"alphamid":        func(e *EmitterData) *float32 { return &e.AlphaMid },
	"alphaend":        func(e *EmitterData) *float32 { return &e.AlphaEnd },
	"birthrate":       func(e *EmitterData) *float32 { return &e.BirthRate },
	"blurlength":      func(e *EmitterData) *float32 { return &e.BlurLength },
	"bounce_co":       func(e *EmitterData) *float32 { return &e.BounceCo },
	"combinetime":     func(e *EmitterData) *float32 { return &e.CombineTime },
	"drag":            func(e *EmitterData) *float32 { return &e.Drag },
	"fps":             func(e *EmitterData) *float32 { return &e.FPS },
	"framestart":      func(e *EmitterData) *float32 { return &e.FrameStart },
	"frameend":        func(e *EmitterData) *float32 { return &e.FrameEnd },
	"grav":            func(e *EmitterData) *float32 { return &e.Grav },
	"lifeexp":         func(e *EmitterData) *float32 { return &e.LifeExp },
	"lightningdelay":  func(e *EmitterData) *float32 { return &e.LightningDelay },
	"lightningradius": func(e *EmitterData) *float32 { return &e.LightningRadius },
	"lightningscale":  func(e *EmitterData) *float32 { return &e.LightningScale },
	"lightningsubdiv": func(e *EmitterData) *float32 { return &e.LightningSubDiv },
	"mass":            func(e *EmitterData) *float32 { return &e.Mass },
	"p2p_bezier2":     func(e *EmitterData) *float32 { return &e.P2PBezier2 },
	"p2p_bezier3":     func(e *EmitterData) *float32 { return &e.P2PBezier3 },
	"particlerot":     func(e *EmitterData) *float32 { return &e.ParticleRot },
	"percentstart":    func(e *EmitterData) *float32 { return &e.PercentStart },
	"percentmid":      func(e *EmitterData) *float32 { return &e.PercentMid },
	"percentend":      func(e *EmitterData) *float32 { return &e.PercentEnd },
	"randvel":         func(e *EmitterData) *float32 { return &e.RandVel },
	"sizestart":       func(e *EmitterData) *float32 { return &e.SizeStart },
	"sizemid":         func(e *EmitterData) *float32 { return &e.SizeMid },
	"sizeend":         func(e *EmitterData) *float32 { return &e.SizeEnd },
	"sizestart_y":     func(e *EmitterData) *float32 { return &e.SizeStartY },
	"sizemid_y":       func(e *EmitterData) *float32 { return &e.SizeMidY },
	"sizeend_y":       func(e *EmitterData) *float32 { return &e.SizeEndY },
	"spread":          func(e *EmitterData) *float32 { return &e.Spread },
	"threshold":       func(e *EmitterData) *float32 { return &e.Threshold },
	"velocity":        func(e *EmitterData) *float32 { return &e.Velocity },
	"xsize":           func(e *EmitterData) *float32 { return &e.XSize },
	"ysize":           func(e *EmitterData) *float32 { return &e.YSize },
}

var emitterColorFields = map[string]func(*EmitterData) *Vec3{
	"colorstart": func(e *EmitterData) *Vec3 { return &e.ColorStart },
	"colormid":   func(e *EmitterData) *Vec3 { return &e.ColorMid },
	"colorend":   func(e *EmitterData) *Vec3 { return &e.ColorEnd },
}

type animFloatEntry struct {
	Name   string
	Getter func(*AnimNode) *[]FloatKey
}

type animColorEntry struct {
	Name   string
	Getter func(*AnimNode) *[]ColorKey
}

// animControllerFloatList is the ordered source-of-truth for float animation controllers.
// Used by the binary decompiler (addAnimKey), the ASCII parser (key table), and the writer.
var animControllerFloatList = []animFloatEntry{
	{"scale", func(an *AnimNode) *[]FloatKey { return &an.ScaleKeys }},
	{"alpha", func(an *AnimNode) *[]FloatKey { return &an.AlphaKeys }},
	{"radius", func(an *AnimNode) *[]FloatKey { return &an.RadiusKeys }},
	{"multiplier", func(an *AnimNode) *[]FloatKey { return &an.MultiplierKeys }},
	{"shadowradius", func(an *AnimNode) *[]FloatKey { return &an.ShadowRadiusKeys }},
	{"verticaldisplacement", func(an *AnimNode) *[]FloatKey { return &an.VerticalDisplacementKeys }},
	{"alphastart", func(an *AnimNode) *[]FloatKey { return &an.AlphaStartKeys }},
	{"alphamid", func(an *AnimNode) *[]FloatKey { return &an.AlphaMidKeys }},
	{"alphaend", func(an *AnimNode) *[]FloatKey { return &an.AlphaEndKeys }},
	{"birthrate", func(an *AnimNode) *[]FloatKey { return &an.BirthRateKeys }},
	{"blurlength", func(an *AnimNode) *[]FloatKey { return &an.BlurLengthKeys }},
	{"bounce_co", func(an *AnimNode) *[]FloatKey { return &an.BounceCoKeys }},
	{"combinetime", func(an *AnimNode) *[]FloatKey { return &an.CombineTimeKeys }},
	{"drag", func(an *AnimNode) *[]FloatKey { return &an.DragKeys }},
	{"fps", func(an *AnimNode) *[]FloatKey { return &an.FPSKeys }},
	{"framestart", func(an *AnimNode) *[]FloatKey { return &an.FrameStartKeys }},
	{"frameend", func(an *AnimNode) *[]FloatKey { return &an.FrameEndKeys }},
	{"grav", func(an *AnimNode) *[]FloatKey { return &an.GravKeys }},
	{"lifeexp", func(an *AnimNode) *[]FloatKey { return &an.LifeExpKeys }},
	{"lightningdelay", func(an *AnimNode) *[]FloatKey { return &an.LightningDelayKeys }},
	{"lightningradius", func(an *AnimNode) *[]FloatKey { return &an.LightningRadiusKeys }},
	{"lightningscale", func(an *AnimNode) *[]FloatKey { return &an.LightningScaleKeys }},
	{"lightningsubdiv", func(an *AnimNode) *[]FloatKey { return &an.LightningSubDivKeys }},
	{"mass", func(an *AnimNode) *[]FloatKey { return &an.MassKeys }},
	{"p2p_bezier2", func(an *AnimNode) *[]FloatKey { return &an.P2PBezier2Keys }},
	{"p2p_bezier3", func(an *AnimNode) *[]FloatKey { return &an.P2PBezier3Keys }},
	{"particlerot", func(an *AnimNode) *[]FloatKey { return &an.ParticleRotKeys }},
	{"percentstart", func(an *AnimNode) *[]FloatKey { return &an.PercentStartKeys }},
	{"percentmid", func(an *AnimNode) *[]FloatKey { return &an.PercentMidKeys }},
	{"percentend", func(an *AnimNode) *[]FloatKey { return &an.PercentEndKeys }},
	{"randvel", func(an *AnimNode) *[]FloatKey { return &an.RandVelKeys }},
	{"sizestart", func(an *AnimNode) *[]FloatKey { return &an.SizeStartKeys }},
	{"sizemid", func(an *AnimNode) *[]FloatKey { return &an.SizeMidKeys }},
	{"sizeend", func(an *AnimNode) *[]FloatKey { return &an.SizeEndKeys }},
	{"sizestart_y", func(an *AnimNode) *[]FloatKey { return &an.SizeStartYKeys }},
	{"sizemid_y", func(an *AnimNode) *[]FloatKey { return &an.SizeMidYKeys }},
	{"sizeend_y", func(an *AnimNode) *[]FloatKey { return &an.SizeEndYKeys }},
	{"spread", func(an *AnimNode) *[]FloatKey { return &an.SpreadKeys }},
	{"threshold", func(an *AnimNode) *[]FloatKey { return &an.ThresholdKeys }},
	{"velocity", func(an *AnimNode) *[]FloatKey { return &an.VelocityKeys }},
	{"xsize", func(an *AnimNode) *[]FloatKey { return &an.XSizeKeys }},
	{"ysize", func(an *AnimNode) *[]FloatKey { return &an.YSizeKeys }},
	{"detonate", func(an *AnimNode) *[]FloatKey { return &an.DetonateKeys }},
}

var animControllerColorList = []animColorEntry{
	{"selfillumcolor", func(an *AnimNode) *[]ColorKey { return &an.SelfIllumColorKeys }},
	{"color", func(an *AnimNode) *[]ColorKey { return &an.ColorKeys }},
	{"colorstart", func(an *AnimNode) *[]ColorKey { return &an.ColorStartKeys }},
	{"colormid", func(an *AnimNode) *[]ColorKey { return &an.ColorMidKeys }},
	{"colorend", func(an *AnimNode) *[]ColorKey { return &an.ColorEndKeys }},
}

// Map versions for O(1) lookup in the binary decompiler and ASCII parser.
var animControllerFloatTable map[string]func(*AnimNode) *[]FloatKey
var animControllerColorTable map[string]func(*AnimNode) *[]ColorKey
var animFloatKeyTable map[string]func(*AnimNode) *[]FloatKey
var animColorKeyTable map[string]func(*AnimNode) *[]ColorKey

func init() {
	animControllerFloatTable = make(map[string]func(*AnimNode) *[]FloatKey, len(animControllerFloatList))
	animFloatKeyTable = make(map[string]func(*AnimNode) *[]FloatKey, len(animControllerFloatList))
	for _, e := range animControllerFloatList {
		animControllerFloatTable[e.Name] = e.Getter
		animFloatKeyTable[e.Name+"key"] = e.Getter
	}
	animControllerColorTable = make(map[string]func(*AnimNode) *[]ColorKey, len(animControllerColorList))
	animColorKeyTable = make(map[string]func(*AnimNode) *[]ColorKey, len(animControllerColorList))
	for _, e := range animControllerColorList {
		animControllerColorTable[e.Name] = e.Getter
		animColorKeyTable[e.Name+"key"] = e.Getter
	}
}

// tryAnimKeyTable handles table-driven float/color key parsing.
// Returns true if the keyword was handled.
func (p *parser) tryAnimKeyTable(keyword string, tokens []string, an *AnimNode) bool {
	if keyword == "detonatekey" {
		if len(tokens) >= 2 {
			count, ok := p.countParam(tokens)
			if ok {
				p.readDetonateKeys(&an.DetonateKeys, int(count))
			}
		}
		return true
	}
	if getter, ok := animFloatKeyTable[keyword]; ok {
		if len(tokens) >= 2 {
			count, ok := p.countParam(tokens)
			if ok {
				p.readFloatKeys(getter(an), int(count))
			}
		} else {
			p.readFloatKeysUntilEnd(getter(an))
		}
		return true
	}
	if getter, ok := animColorKeyTable[keyword]; ok {
		if len(tokens) >= 2 {
			count, ok := p.countParam(tokens)
			if ok {
				p.readColorKeys(getter(an), int(count))
			}
		} else {
			p.readColorKeysUntilEnd(getter(an))
		}
		return true
	}
	return false
}

// parseAnimNodeParam handles parameters inside animation node blocks.
// Ref: load_models.pl paramtype(anim,...) (lines 816-888)
func (p *parser) parseAnimNodeParam(keyword string, tokens []string) {
	an := p.currentAnimNode
	if an == nil {
		return
	}

	switch keyword {
	case "parent":
		if len(tokens) >= 2 {
			an.Parent = tokens[1]
		}
	case "#part-number":
		// Ignored for anim nodes
	case "position":
		if len(tokens) >= 4 {
			x, y, z := p.floatAt(tokens, 1), p.floatAt(tokens, 2), p.floatAt(tokens, 3)
			if len(an.PositionKeys) == 0 {
				an.PositionKeys = append(an.PositionKeys, PositionKey{Time: 0, Value: Vec3{X: x, Y: y, Z: z}})
			}
		} else {
			p.shortParam(tokens, 3)
		}
	case "orientation":
		if len(tokens) >= 5 {
			x, y, z, w := p.floatAt(tokens, 1), p.floatAt(tokens, 2), p.floatAt(tokens, 3), p.floatAt(tokens, 4)
			if len(an.OrientationKeys) == 0 {
				an.OrientationKeys = append(an.OrientationKeys, OrientationKey{Time: 0, Value: Vec4{X: x, Y: y, Z: z, W: w}})
			}
		} else {
			p.shortParam(tokens, 4)
		}

	// Controller key lists — handle both counted ("positionkey 2") and
	// uncounted ("positionkey" + lines + "endlist") formats.
	case "positionkey":
		if len(tokens) >= 2 {
			count, ok := p.countParam(tokens)
			if ok {
				p.readPositionKeys(&an.PositionKeys, int(count))
			}
		} else {
			p.readPositionKeysUntilEnd(&an.PositionKeys)
		}
	case "positionbezierkey":
		// Rows are "time x y z tanIn.xyz tanOut.xyz" (10 columns). Rows with
		// fewer columns are kept as zero-tangent keys rather than dropped.
		an.PositionBezier = true
		if len(tokens) >= 2 {
			count, ok := p.countParam(tokens)
			if ok {
				p.readPositionBezierKeys(&an.PositionKeys, int(count))
			}
		} else {
			p.readPositionBezierKeysUntilEnd(&an.PositionKeys)
		}
	case "orientationkey":
		if len(tokens) >= 2 {
			count, ok := p.countParam(tokens)
			if ok {
				p.readOrientationKeys(&an.OrientationKeys, int(count))
			}
		} else {
			p.readOrientationKeysUntilEnd(&an.OrientationKeys)
		}
	// AnimMesh data in anim blocks (verts, faces, tverts, sampleperiod, clip*, animverts, animtverts)
	case "verts":
		if len(tokens) >= 2 {
			if an.Mesh == nil {
				an.Mesh = NewMeshData()
			}
			count, ok := p.countParam(tokens)
			if ok {
				p.readVec3List(&an.Mesh.Verts, int(count))
			}
		}
	case "faces":
		if len(tokens) >= 2 {
			if an.Mesh == nil {
				an.Mesh = NewMeshData()
			}
			count, ok := p.countParam(tokens)
			if ok {
				p.readFaceList(&an.Mesh.Faces, int(count))
			}
		}
	case "tverts":
		if len(tokens) >= 2 {
			if an.Mesh == nil {
				an.Mesh = NewMeshData()
			}
			count, ok := p.countParam(tokens)
			if ok {
				p.readVec3List(&an.Mesh.TVerts, int(count))
			}
		}
	case "sampleperiod":
		if len(tokens) >= 2 {
			if an.AnimMesh == nil {
				an.AnimMesh = &AnimMeshData{}
			}
			p.floatParam(tokens, &an.AnimMesh.SamplePeriod)
		}
	case "clipu":
		if len(tokens) >= 2 {
			if an.AnimMesh == nil {
				an.AnimMesh = &AnimMeshData{}
			}
			p.floatParam(tokens, &an.AnimMesh.ClipU)
		}
	case "clipv":
		if len(tokens) >= 2 {
			if an.AnimMesh == nil {
				an.AnimMesh = &AnimMeshData{}
			}
			p.floatParam(tokens, &an.AnimMesh.ClipV)
		}
	case "clipw":
		if len(tokens) >= 2 {
			if an.AnimMesh == nil {
				an.AnimMesh = &AnimMeshData{}
			}
			p.floatParam(tokens, &an.AnimMesh.ClipW)
		}
	case "cliph":
		if len(tokens) >= 2 {
			if an.AnimMesh == nil {
				an.AnimMesh = &AnimMeshData{}
			}
			p.floatParam(tokens, &an.AnimMesh.ClipH)
		}
	case "animverts":
		if len(tokens) >= 2 {
			if an.AnimMesh == nil {
				an.AnimMesh = &AnimMeshData{}
			}
			count, ok := p.countParam(tokens)
			if ok {
				p.readVec3List(&an.AnimMesh.AnimVerts, int(count))
			}
		}
	case "animtverts":
		if len(tokens) >= 2 {
			if an.AnimMesh == nil {
				an.AnimMesh = &AnimMeshData{}
			}
			count, ok := p.countParam(tokens)
			if ok {
				p.readVec3List(&an.AnimMesh.AnimTVerts, int(count))
			}
		}

	case "endlist":

	default:
		if p.tryAnimKeyTable(keyword, tokens, an) {
			return
		}
		// Other bezier lists (scalar/colour) are not modelled. The key readers
		// are line-based and take only the leading time and value, so routing
		// them through the linear reader degrades them to linear keyframes —
		// keeping the sampled values and dropping the tangents — rather than
		// misparsing them. positionbezierkey is handled explicitly above.
		if strings.HasSuffix(keyword, "bezierkey") {
			base := strings.TrimSuffix(keyword, "bezierkey")
			p.parseAnimNodeParam(base+"key", tokens)
			return
		}
		// Static emitter/light/mesh values on animation nodes become
		// single-keyframe controllers at t=0, matching nwnmdlcomp behavior.
		if getter, ok := animControllerFloatTable[keyword]; ok {
			if len(tokens) >= 2 {
				if v, err := parseFloat(safeIndex(tokens, 1)); err != nil {
					p.badParam(tokens, "number")
				} else if keys := getter(an); len(*keys) == 0 {
					*keys = append(*keys, FloatKey{Time: 0, Value: v})
				}
			}
			return
		}
		if getter, ok := animControllerColorTable[keyword]; ok {
			if len(tokens) >= 4 {
				r, g, b := p.floatAt(tokens, 1), p.floatAt(tokens, 2), p.floatAt(tokens, 3)
				keys := getter(an)
				if len(*keys) == 0 {
					*keys = append(*keys, ColorKey{Time: 0, Value: Vec3{X: r, Y: g, Z: b}})
				}
			} else {
				p.shortParam(tokens, 3)
			}
			return
		}
	}
}

// List reading helpers

// parseCountedVec3 extracts a count from tokens[1] and reads that many Vec3 lines.
func (p *parser) parseCountedVec3(tokens []string, out *[]Vec3) {
	if len(tokens) >= 2 {
		if count, ok := p.countParam(tokens); ok && count > 0 {
			p.readVec3List(out, int(count))
		}
	}
}

func (p *parser) parseCountedVec4(tokens []string, out *[]Vec4) {
	if len(tokens) >= 2 {
		if count, ok := p.countParam(tokens); ok && count > 0 {
			p.readVec4List(out, int(count))
		}
	}
}

func (p *parser) parseCountedFaces(tokens []string, out *[]Face) {
	if len(tokens) >= 2 {
		if count, ok := p.countParam(tokens); ok && count > 0 {
			p.readFaceList(out, int(count))
		}
	}
}

func (p *parser) parseCountedInt3(tokens []string, out *[][3]int32) {
	if len(tokens) >= 2 {
		if count, ok := p.countParam(tokens); ok && count > 0 {
			p.readInt3List(out, int(count))
		}
	}
}

func (p *parser) parseCountedInts(tokens []string, out *[]int32) {
	if len(tokens) >= 2 {
		if count, ok := p.countParam(tokens); ok && count > 0 {
			p.readIntList(out, int(count))
		}
	}
}

func (p *parser) parseCountedStrings(tokens []string, out *[]string) {
	if len(tokens) >= 2 {
		if count, ok := p.countParam(tokens); ok && count > 0 {
			p.readStringList(out, int(count))
		}
	}
}

// maxListWarnings caps the warnings one list can produce, so a file with
// thousands of damaged lines does not bury everything else.
const maxListWarnings = 5

// listWarn reports damaged entries while a counted list is read.
//
// A list declares how many entries it has, and other data refers to them by
// index (faces into verts and tverts, controllers into keys). So a damaged
// entry is kept, with the unusable values set to 0 and a warning naming the
// line. Dropping it instead would shift every later entry down one slot and
// silently point those indices at the wrong data, with no warning at all.
type listWarn struct {
	p     *parser
	shown int
	total int
}

func (p *parser) newListWarn() *listWarn { return &listWarn{p: p} }

func (w *listWarn) add(format string, args ...interface{}) {
	w.total++
	if w.shown < maxListWarnings {
		w.shown++
		w.p.errorf(format, args...)
	}
}

func (w *listWarn) done() {
	if w.total > w.shown {
		w.p.errorf("%d more damaged value(s) in this list were set to 0", w.total-w.shown)
	}
}

// float parses one float component, setting it to 0 (and warning) if it is
// not a usable number. That includes values outside the float32 range and
// nan/inf, which parseFloat rejects.
func (w *listWarn) float(tok string) float32 {
	v, err := parseFloat(tok)
	if err != nil {
		w.add("%q is not a usable number, set to 0", tok)
		return 0
	}
	return v
}

// int parses one integer component, setting it to 0 (and warning) if it is
// not a usable number.
func (w *listWarn) int(tok string) int32 {
	v, err := parseInt(tok)
	if err != nil {
		w.add("%q is not a usable integer, set to 0", tok)
		return 0
	}
	return v
}

// short reports an entry with fewer values than the format needs.
func (w *listWarn) short(have, want int) {
	w.add("entry has %d value(s), expected %d; the missing ones are set to 0", have, want)
}

// tok returns tokens[i], or "" when the entry is short (already reported).
func tok(tokens []string, i int) string {
	if i < len(tokens) {
		return tokens[i]
	}
	return ""
}

func (p *parser) readVec3List(out *[]Vec3, count int) {
	if !p.trackAlloc(int64(count) * 12) {
		p.errorf("allocation limit exceeded reading %d vec3 entries", count)
		return
	}
	w := p.newListWarn()
	for i := 0; i < count; i++ {
		line, ok := p.nextLine()
		if !ok {
			break
		}
		tokens := tokenize(line)
		if len(tokens) < 3 {
			w.short(len(tokens), 3)
		}
		var v Vec3
		if len(tokens) > 0 {
			v.X = w.float(tokens[0])
		}
		if len(tokens) > 1 {
			v.Y = w.float(tokens[1])
		}
		if len(tokens) > 2 {
			v.Z = w.float(tokens[2])
		}
		*out = append(*out, v)
	}
	w.done()
}

func (p *parser) readVec4List(out *[]Vec4, count int) {
	if !p.trackAlloc(int64(count) * 16) {
		p.errorf("allocation limit exceeded reading %d vec4 entries", count)
		return
	}
	w := p.newListWarn()
	for i := 0; i < count; i++ {
		line, ok := p.nextLine()
		if !ok {
			break
		}
		tokens := tokenize(line)
		if len(tokens) < 4 {
			w.short(len(tokens), 4)
		}
		var v Vec4
		if len(tokens) > 0 {
			v.X = w.float(tokens[0])
		}
		if len(tokens) > 1 {
			v.Y = w.float(tokens[1])
		}
		if len(tokens) > 2 {
			v.Z = w.float(tokens[2])
		}
		if len(tokens) > 3 {
			v.W = w.float(tokens[3])
		}
		*out = append(*out, v)
	}
	w.done()
}

func (p *parser) readFaceList(out *[]Face, count int) {
	if !p.trackAlloc(int64(count) * 48) {
		p.errorf("allocation limit exceeded reading %d face entries", count)
		return
	}
	w := p.newListWarn()
	for i := 0; i < count; i++ {
		line, ok := p.nextLine()
		if !ok {
			break
		}
		tokens := tokenize(line)
		if len(tokens) < 8 {
			w.short(len(tokens), 8)
		}
		var n [8]int32
		for k := 0; k < 8 && k < len(tokens); k++ {
			n[k] = w.int(tokens[k])
		}
		*out = append(*out, Face{
			Verts:       [3]int32{n[0], n[1], n[2]},
			SmoothGroup: n[3],
			UVs:         [3]int32{n[4], n[5], n[6]},
			Material:    n[7],
		})
	}
	w.done()
}

func (p *parser) readIntList(out *[]int32, count int) {
	if !p.trackAlloc(int64(count) * 4) {
		p.errorf("allocation limit exceeded reading %d int entries", count)
		return
	}
	w := p.newListWarn()
	for i := 0; i < count; i++ {
		line, ok := p.nextLine()
		if !ok {
			break
		}
		tokens := tokenize(line)
		*out = append(*out, w.int(tok(tokens, 0)))
	}
	w.done()
}

func (p *parser) readFloatList(out *[]float32, count int) {
	if !p.trackAlloc(int64(count) * 4) {
		p.errorf("allocation limit exceeded reading %d float entries", count)
		return
	}
	w := p.newListWarn()
	for i := 0; i < count; i++ {
		line, ok := p.nextLine()
		if !ok {
			break
		}
		tokens := tokenize(line)
		*out = append(*out, w.float(tok(tokens, 0)))
	}
	w.done()
}

func (p *parser) readInt3List(out *[][3]int32, count int) {
	if !p.trackAlloc(int64(count) * 12) {
		p.errorf("allocation limit exceeded reading %d int3 entries", count)
		return
	}
	w := p.newListWarn()
	for i := 0; i < count; i++ {
		line, ok := p.nextLine()
		if !ok {
			break
		}
		tokens := tokenize(line)
		if len(tokens) < 3 {
			w.short(len(tokens), 3)
		}
		var e [3]int32
		for k := 0; k < 3 && k < len(tokens); k++ {
			e[k] = w.int(tokens[k])
		}
		*out = append(*out, e)
	}
	w.done()
}

func (p *parser) readStringList(out *[]string, count int) {
	if !p.trackAlloc(int64(count) * 32) {
		p.errorf("allocation limit exceeded reading %d string entries", count)
		return
	}
	for i := 0; i < count; i++ {
		line, ok := p.nextLine()
		if !ok {
			break
		}
		*out = append(*out, strings.TrimSpace(line))
	}
}

func (p *parser) readAabbList(out *[]AabbEntry, count int) {
	if !p.trackAlloc(int64(count) * 32) {
		p.errorf("allocation limit exceeded reading %d aabb entries", count)
		return
	}
	w := p.newListWarn()
	for i := 0; i < count; i++ {
		line, ok := p.nextLine()
		if !ok {
			break
		}
		tokens := tokenize(line)
		if len(tokens) < 7 {
			w.short(len(tokens), 7)
		}
		var f [6]float32
		for k := 0; k < 6 && k < len(tokens); k++ {
			f[k] = w.float(tokens[k])
		}
		var face int32
		if len(tokens) > 6 {
			face = w.int(tokens[6])
		}
		*out = append(*out, AabbEntry{
			BoundMin: Vec3{X: f[0], Y: f[1], Z: f[2]},
			BoundMax: Vec3{X: f[3], Y: f[4], Z: f[5]},
			LeafFace: face,
		})
	}
	w.done()
}

func (p *parser) readWeightList(skin *SkinData, count int) {
	// Per line: VertexWeight plus bone name strings (often long in community models).
	if !p.trackAlloc(int64(count) * 512) {
		p.errorf("allocation limit exceeded reading %d skin weights", count)
		return
	}
	ww := p.newListWarn()
	defer ww.done()
	for i := 0; i < count; i++ {
		line, ok := p.nextLine()
		if !ok {
			break
		}
		tokens := tokenize(line)
		w := VertexWeight{}
		// Weight format: bonename1 weight1 bonename2 weight2 ...
		for j := 0; j+1 < len(tokens); j += 2 {
			wt, err := parseFloat(tokens[j+1])
			if err != nil {
				ww.add("weight %q for bone %q is not a usable number, that influence is ignored", tokens[j+1], tokens[j])
				continue
			}
			if wt > 0 {
				w.Bones = append(w.Bones, tokens[j])
				w.Weights = append(w.Weights, wt)
			}
		}
		skin.Weights = append(skin.Weights, w)
	}
}

// keyValues parses the first n tokens of an animation key line (time first).
//
// Unlike a list entry, a key cannot be kept with an invented time or pose: a
// time of 0 in the middle of a track breaks the ordering the animation relies
// on. So a key with too few values, or no usable time, is skipped and reported.
// A bad value after the time is set to 0, as in a list, and reported.
func (w *listWarn) keyValues(tokens []string, n int) ([]float32, bool) {
	if len(tokens) < n {
		w.add("key has %d value(s), expected %d; the key is skipped", len(tokens), n)
		return nil, false
	}
	t, err := parseFloat(tokens[0])
	if err != nil {
		w.add("key time %q is not a usable number; the key is skipped", tokens[0])
		return nil, false
	}
	v := make([]float32, n)
	v[0] = t
	for i := 1; i < n; i++ {
		v[i] = w.float(tokens[i])
	}
	return v, true
}

// keyParser parses one key line, appends it, and reports whether the key was
// kept. Any problem is reported through w.
type keyParser func(w *listWarn, tokens []string) bool

// readKeys reads a counted run of key lines.
func (p *parser) readKeys(count int, bytesPerKey int64, label string, parse keyParser) {
	if !p.trackAlloc(int64(count) * bytesPerKey) {
		p.errorf("allocation limit exceeded reading %d %s", count, label)
		return
	}
	w := p.newListWarn()
	defer w.done()
	for i := 0; i < count; i++ {
		line, ok := p.nextLine()
		if !ok {
			break
		}
		parse(w, tokenize(line))
	}
}

// readKeysUntilEnd reads key lines until "endlist". allocBytes is tracked per
// key kept.
func (p *parser) readKeysUntilEnd(allocBytes int64, label string, parse keyParser) {
	w := p.newListWarn()
	defer w.done()
	for {
		line, ok := p.nextLine()
		if !ok {
			break
		}
		tokens := tokenize(line)
		if len(tokens) == 0 {
			continue
		}
		if strings.EqualFold(tokens[0], "endlist") {
			break
		}
		if !p.trackAlloc(allocBytes) {
			p.errorf("allocation limit exceeded reading %s", label)
			break
		}
		parse(w, tokens)
	}
}

func positionKeyParser(out *[]PositionKey) keyParser {
	return func(w *listWarn, tokens []string) bool {
		v, ok := w.keyValues(tokens, 4)
		if ok {
			*out = append(*out, PositionKey{Time: v[0], Value: Vec3{X: v[1], Y: v[2], Z: v[3]}})
		}
		return ok
	}
}

// positionBezierKeyParser parses "time x y z [tanIn.xyz tanOut.xyz]"; the
// tangents are optional.
func positionBezierKeyParser(out *[]PositionKey) keyParser {
	return func(w *listWarn, tokens []string) bool {
		v, ok := w.keyValues(tokens, 4)
		if !ok {
			return false
		}
		var f [10]float32
		copy(f[:4], v)
		for i := 4; i < len(f) && i < len(tokens); i++ {
			f[i] = w.float(tokens[i])
		}
		*out = append(*out, PositionKey{
			Time:   f[0],
			Value:  Vec3{X: f[1], Y: f[2], Z: f[3]},
			TanIn:  Vec3{X: f[4], Y: f[5], Z: f[6]},
			TanOut: Vec3{X: f[7], Y: f[8], Z: f[9]},
		})
		return true
	}
}

func orientationKeyParser(out *[]OrientationKey) keyParser {
	return func(w *listWarn, tokens []string) bool {
		v, ok := w.keyValues(tokens, 5)
		if ok {
			*out = append(*out, OrientationKey{Time: v[0], Value: Vec4{X: v[1], Y: v[2], Z: v[3], W: v[4]}})
		}
		return ok
	}
}

func floatKeyParser(out *[]FloatKey) keyParser {
	return func(w *listWarn, tokens []string) bool {
		v, ok := w.keyValues(tokens, 2)
		if ok {
			*out = append(*out, FloatKey{Time: v[0], Value: v[1]})
		}
		return ok
	}
}

func colorKeyParser(out *[]ColorKey) keyParser {
	return func(w *listWarn, tokens []string) bool {
		v, ok := w.keyValues(tokens, 4)
		if ok {
			*out = append(*out, ColorKey{Time: v[0], Value: Vec3{X: v[1], Y: v[2], Z: v[3]}})
		}
		return ok
	}
}

// detonateKeyParser accepts detonate key entries that are time-only (NWN spec:
// NumCols=-1, value 1) or time plus value. Both formats are accepted for
// interoperability.
func detonateKeyParser(out *[]FloatKey) keyParser {
	return func(w *listWarn, tokens []string) bool {
		if len(tokens) == 1 {
			v, ok := w.keyValues(tokens, 1)
			if ok {
				*out = append(*out, FloatKey{Time: v[0], Value: 1})
			}
			return ok
		}
		return floatKeyParser(out)(w, tokens)
	}
}

func (p *parser) readPositionKeys(out *[]PositionKey, count int) {
	p.readKeys(count, 16, "position keys", positionKeyParser(out))
}

func (p *parser) readPositionBezierKeys(out *[]PositionKey, count int) {
	p.readKeys(count, 40, "position bezier keys", positionBezierKeyParser(out))
}

func (p *parser) readPositionBezierKeysUntilEnd(out *[]PositionKey) {
	p.readKeysUntilEnd(40, "position bezier keys", positionBezierKeyParser(out))
}

func (p *parser) readOrientationKeys(out *[]OrientationKey, count int) {
	p.readKeys(count, 20, "orientation keys", orientationKeyParser(out))
}

func (p *parser) readFloatKeys(out *[]FloatKey, count int) {
	p.readKeys(count, 8, "float keys", floatKeyParser(out))
}

func (p *parser) readDetonateKeys(out *[]FloatKey, count int) {
	p.readKeys(count, 8, "detonate keys", detonateKeyParser(out))
}

func (p *parser) readColorKeys(out *[]ColorKey, count int) {
	p.readKeys(count, 16, "color keys", colorKeyParser(out))
}

func (p *parser) readPositionKeysUntilEnd(out *[]PositionKey) {
	p.readKeysUntilEnd(16, "position keys", positionKeyParser(out))
}

func (p *parser) readOrientationKeysUntilEnd(out *[]OrientationKey) {
	p.readKeysUntilEnd(20, "orientation keys", orientationKeyParser(out))
}

func (p *parser) readFloatKeysUntilEnd(out *[]FloatKey) {
	p.readKeysUntilEnd(8, "float keys", floatKeyParser(out))
}

func (p *parser) readColorKeysUntilEnd(out *[]ColorKey) {
	p.readKeysUntilEnd(16, "color keys", colorKeyParser(out))
}

// Utility functions

func tokenize(line string) []string {
	return strings.Fields(line)
}

func parseFloat(s string) (float32, error) {
	v, err := strconv.ParseFloat(s, 32)
	if err != nil {
		return 0, err
	}
	if math.IsNaN(v) || math.IsInf(v, 0) {
		return 0, fmt.Errorf("invalid float value: %s", s)
	}
	return float32(v), nil
}

func parseInt(s string) (int32, error) {
	// Try int first, then float (some MDL files use "1.0" for integer fields)
	if v, err := strconv.ParseInt(s, 10, 32); err == nil {
		return int32(v), nil
	}
	if v, err := strconv.ParseFloat(s, 64); err == nil {
		return int32(math.Round(v)), nil
	}
	return 0, fmt.Errorf("not a number: %s", s)
}

func parseVec3(tokens []string) (Vec3, bool) {
	if len(tokens) < 3 {
		return Vec3{}, false
	}
	x, e1 := parseFloat(tokens[0])
	y, e2 := parseFloat(tokens[1])
	z, e3 := parseFloat(tokens[2])
	if e1 != nil || e2 != nil || e3 != nil {
		return Vec3{}, false
	}
	return Vec3{X: x, Y: y, Z: z}, true
}

func parseVec4(tokens []string) (Vec4, bool) {
	if len(tokens) < 4 {
		return Vec4{}, false
	}
	x, e1 := parseFloat(tokens[0])
	y, e2 := parseFloat(tokens[1])
	z, e3 := parseFloat(tokens[2])
	w, e4 := parseFloat(tokens[3])
	if e1 != nil || e2 != nil || e3 != nil || e4 != nil {
		return Vec4{}, false
	}
	return Vec4{X: x, Y: y, Z: z, W: w}, true
}

func safeIndex(tokens []string, i int) string {
	if i < len(tokens) {
		return tokens[i]
	}
	return ""
}

// validParamsForType returns the set of valid parameter names for a given node type.
// Used for fuzzy matching.
// Ref: load_models.pl paramtype/3 (lines 648-898)
func validParamsForType(nodeType string) []string {
	base := []string{
		"parent", "position", "orientation", "scale", "inheritcolor", "wirecolor",
	}

	meshParams := []string{
		"selfillumcolor", "alpha", "diffuse", "ambient", "specular", "shininess",
		"shadow", "beaming", "render", "transparencyhint", "renderhint",
		"bitmap", "texture0", "texture1", "texture2", "materialname",
		"tilefade", "rotatetexture", "lightmapped", "center",
		"multimaterial", "verts", "faces", "tverts", "tverts1", "tverts2", "tverts3",
		"texindices0", "texindices1", "texindices2", "texindices3",
		"colors", "normals", "tangents", "mirrorlist",
	}

	switch nodeType {
	case "dummy":
		return base
	case "trimesh":
		return append(base, meshParams...)
	case "skin":
		return append(append(base, meshParams...), "weights")
	case "animmesh":
		return append(append(base, meshParams...), "sampleperiod", "clipu", "clipv", "clipw", "cliph", "animverts", "animtverts")
	case "danglymesh":
		return append(append(base, meshParams...), "displacement", "tightness", "period", "constraints", "danglymesh", "showdispl", "displtype", "gizmo")
	case "aabb":
		return append(append(base, meshParams...), "aabb")
	case "emitter":
		return append(base, emitterParams()...)
	case "light":
		return append(base, lightParams()...)
	case "reference":
		return append(base, "refmodel", "reattachable")
	default:
		return base
	}
}

func emitterParams() []string {
	return []string{
		"deadspace", "blastradius", "blastlength", "xgrid", "ygrid", "spawntype",
		"update", "render", "blend", "texture", "chunkname", "twosidedtex",
		"loop", "renderorder", "p2p", "p2p_sel", "affectedbywind", "m_istinted",
		"bounce", "random", "inherit", "inheritvel", "inherit_local", "splat", "inherit_part",
		"p2p_type", "render_sel", "blend_sel", "update_sel", "spawntype_sel",
		"opacity", "iconsize", "lockaxes", "chunky",
		"alphastart", "alphamid", "alphaend",
		"colorstart", "colormid", "colorend",
		"sizestart", "sizemid", "sizeend",
		"sizestart_y", "sizemid_y", "sizeend_y",
		"birthrate", "lifeexp", "mass", "spread", "particlerot", "velocity", "randvel",
		"bounce_co", "blurlength", "fps", "framestart", "frameend",
		"grav", "drag", "threshold", "combinetime",
		"percentstart", "percentmid", "percentend",
		"lightningdelay", "lightningradius", "lightningscale", "lightningsubdiv",
		"p2p_bezier2", "p2p_bezier3", "xsize", "ysize",
	}
}

func lightParams() []string {
	return []string{
		"flareradius", "lensflares", "lightpriority", "ambientonly",
		"isdynamic", "ndynamictype", "affectdynamic", "shadow",
		"generateflare", "fadinglight", "negativelight",
		"color", "radius", "multiplier", "shadowradius", "verticaldisplacement",
		"flaresizes", "flarepositions", "flarecolorshifts", "texturenames",
	}
}
