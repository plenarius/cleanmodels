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
// in dst, read as the game reads it (scanFloat): a token with more after its
// number ("7abc", "1.#INF") gives the number, reported. A missing or unusable
// value leaves dst as it was and is reported with the line number, rather than
// silently keeping the default.
func (p *parser) floatParam(tokens []string, dst *float32) {
	if v, ok := p.floatValue(tokens); ok {
		*dst = v
	}
}

// floatValue reads the value of a one-value line as floatParam does, or
// reports false (reported) when there is no usable value.
func (p *parser) floatValue(tokens []string) (float32, bool) {
	tok := safeIndex(tokens, 1)
	v, n, err := scanFloat(tok)
	if err != nil {
		p.badParam(tokens, "number")
		return 0, false
	}
	if n < len(tok) {
		p.errorf("%q for %q is read as %s, as the game reads it; the rest is ignored", tok, tokens[0], fmtFloat(v))
	}
	return v, true
}

// intParam is floatParam for integer parameters, read as sscanf "%d" does
// (scanInt: "2.9" is 2), then, as the game does when that fails, the words
// TRUE and FALSE (any case) as 1 and 0. A token whose rest changes nothing
// ("1.0") is not reported.
func (p *parser) intParam(tokens []string, dst *int32) {
	tok := safeIndex(tokens, 1)
	v, n, err := scanInt(tok)
	if err != nil {
		switch {
		case strings.EqualFold(tok, "true"):
			*dst = 1
		case strings.EqualFold(tok, "false"):
			*dst = 0
		default:
			p.badParam(tokens, "integer")
		}
		return
	}
	*dst = v
	if n < len(tok) {
		if f, ferr := parseFloat(tok); ferr != nil || float64(f) != float64(v) {
			p.errorf("%q for %q is read as %d, as the game reads it; the rest is ignored", tok, tokens[0], v)
		}
	}
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
	tok := safeIndex(tokens, i)
	v, n, err := scanFloat(tok)
	if err != nil {
		p.errorf("%q is not a usable number for value %d of %q, set to 0", tok, i, tokens[0])
		return 0
	}
	if n < len(tok) {
		p.errorf("%q for value %d of %q is read as %s, as the game reads it; the rest is ignored", tok, i, tokens[0], fmtFloat(v))
	}
	return v
}

// shortParam reports a line with fewer values than its keyword needs.
func (p *parser) shortParam(tokens []string, want int) {
	p.errorf("%q has %d value(s), expected %d; the line is ignored", tokens[0], len(tokens)-1, want)
}

// vec3Param stores a "keyword x y z" line in dst (vecParam).
func (p *parser) vec3Param(tokens []string, dst *Vec3) {
	p.vecParam(tokens, []*float32{&dst.X, &dst.Y, &dst.Z})
}

// vec4Param is vec3Param for "keyword x y z w" lines.
func (p *parser) vec4Param(tokens []string, dst *Vec4) {
	p.vecParam(tokens, []*float32{&dst.X, &dst.Y, &dst.Z, &dst.W})
}

// vecParam reads a line's values into dst as the game does, with sscanf
// "%f %f %f": in order, stopping at the first value that is not a usable
// number or has more after its number ("1.5abc 2 3" sets only the first).
// Values not reached keep what they had, as in the game; each stop is
// reported with the line number.
func (p *parser) vecParam(tokens []string, dst []*float32) {
	vals := tokens[1:]
	for i, d := range dst {
		if i >= len(vals) {
			p.errorf("%q has %d value(s), expected %d; the others keep their values", tokens[0], len(vals), len(dst))
			return
		}
		v, n, err := scanFloat(vals[i])
		if err != nil {
			if n > 0 && n == len(vals[i]) { // a number sscanf reads and this parser refuses
				p.errorf("%q has an unusable number (%q), which keeps its value", tokens[0], vals[i])
				continue
			}
			p.errorf("%q has an unusable number (%q); it and the values after it keep their values", tokens[0], vals[i])
			return
		}
		*d = v
		if n < len(vals[i]) {
			p.errorf("%q: %q is read as %s, as the game reads it; the rest of the line is ignored", tokens[0], vals[i], fmtFloat(v))
			return
		}
	}
}

// countParam reads the entry count that opens a list. When it is unusable the
// list cannot be read, so say so rather than letting its lines fall through.
func (p *parser) countParam(tokens []string) (int32, bool) {
	// A count followed by anything else makes the game read the rows as
	// binary data (mdl-spec E46), so such a count is refused, except one whose
	// rest changes nothing ("3.0").
	tok := safeIndex(tokens, 1)
	v, n, err := scanInt(tok)
	if err == nil && n < len(tok) {
		if f, ferr := parseFloat(tok); ferr != nil || float64(f) != float64(v) {
			err = fmt.Errorf("not a count: %s", tok)
		}
	}
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

// nextWeightLine reads the next entry of a counted weights list the way the
// engine does (Parse<SkinVertexWeight>, 0x44fd00): a line that is truly empty
// is skipped, but a line of only spaces or tabs is an entry, a vertex with no
// weights. NeverBlender writes such a line for a vertex with no weight; skipping
// it, as nextLine does, shifted every later vertex's weights by one and read
// the next node's lines as weights. Comment lines stay skipped, as before.
func (p *parser) nextWeightLine() (string, bool) {
	for p.scanner.Scan() {
		p.line++
		raw := strings.TrimRight(p.scanner.Text(), "\r")
		if raw == "" {
			continue
		}
		line := strings.TrimSpace(raw)
		if strings.HasPrefix(line, "#") {
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
		if caseSensitiveWords[keyword] && tokens[0] != keyword {
			p.errorf("%q: the game reads only lower-case %q and does not read this line; cleanmodels does", tokens[0], keyword)
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

// caseSensitiveWords are the structural words the game matches case-
// sensitively (mdl-spec MDL-ASCII-010; `parent` is handled where it is read).
// cleanmodels reads them in any case, with a warning.
var caseSensitiveWords = map[string]bool{
	"node": true, "endnode": true, "endlist": true, "event": true, "animlength": true,
	"endmodelgeom": true, "doneanim": true, "donemodel": true,
}

// parentLine reports whether tokens is a parent line the game reads: it
// matches "parent" case-sensitively (MdlNode::InternalParseField 0x448bb0), so
// "PARENT" is not read and the node keeps no parent from it; a node with no
// parent the game reads is left out of the tree (golden zspec_numbers).
func (p *parser) parentLine(tokens []string, node string) bool {
	if tokens[0] != "parent" {
		p.errorf("%q is not read by the game, which knows only lower-case \"parent\", so node %q takes no parent from it, as in the game", tokens[0], node)
		return false
	}
	return len(tokens) >= 2
}

func (p *parser) parseNewModel(tokens []string) {
	if len(tokens) >= 2 {
		p.model.Name = tokens[1]
	}
	p.model.FileType = "ascii"
}

// newParsedMeshData is a mesh as the game compiler defaults it for keywords the
// source leaves out (E50 in the mdl-spec engine notes; goldens for every mesh
// type): diffuse and ambient 1 1 1, shininess 1, render 1 and shadow 1, except
// on walkmeshes (aabb), which default to render 0 and shadow 0. Compiled
// against the engine's output for 180 meshes without a "shadow" line (159
// trimesh, 8 skin, 13 danglymesh), it wrote shadow 1 every time, as it does for
// animmesh (zspec_animmesh).
func newParsedMeshData(walkmesh bool) *MeshData {
	m := NewMeshData()
	m.Diffuse = Vec3{X: 1, Y: 1, Z: 1}
	m.Ambient = Vec3{X: 1, Y: 1, Z: 1}
	m.Shininess = 1
	if walkmesh {
		m.Render = 0
	} else {
		m.Shadow = 1
	}
	return m
}

// newParsedLightData is a light as the game compiler defaults it for keywords
// the source leaves out (E50 in the mdl-spec engine notes; goldens zspec_light,
// zspec_animfx): lightpriority 5, nDynamicType 1, affectdynamic 1, shadow 1 and
// fadinglight 1; ambientonly and generateflare 0.
func newParsedLightData() *LightData {
	return &LightData{LightPriority: 5, NDynamicType: 1, AffectDynamic: 1, Shadow: 1, FadingLight: 1}
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
		Orientation: Vec4{},
	}

	switch nodeType {
	case "trimesh":
		node.Mesh = newParsedMeshData(false)
	case "skin":
		node.Mesh = newParsedMeshData(false)
		node.Skin = &SkinData{}
	case "animmesh":
		node.Mesh = newParsedMeshData(false)
		node.AnimMesh = &AnimMeshData{}
	case "danglymesh":
		node.Mesh = newParsedMeshData(false)
		node.Dangly = &DanglyData{}
	case "aabb":
		node.Mesh = newParsedMeshData(true)
		node.Aabb = &AabbData{}
	case "emitter":
		node.Emitter = &EmitterData{}
	case "light":
		node.Light = newParsedLightData()
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
		if t, n, err := scanFloat(tokens[1]); err == nil {
			if n < len(tokens[1]) {
				p.errorf("event time %q is read as %s, as the game reads it; the rest is ignored", tokens[1], fmtFloat(t))
			}
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
		if p.parentLine(tokens, node.Name) {
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

// parseGeomKeyList reads a controller key list (<name>key or
// <name>bezierkey) on a geometry node. The game's compiler stores the list as
// a keyed controller, and geometry is evaluated at time 0, where the linear
// and bezier paths both return the first key's value (mdl-spec MDL-CTRL-023,
// golden zspec_bezier2), so the first key becomes the node's value and the
// rest are dropped with a warning.
func (p *parser) parseGeomKeyList(node *Node, keyword string, tokens []string) bool {
	if !strings.HasSuffix(keyword, "key") || keyword == "detonatekey" || keyword == "detonatebezierkey" {
		return false
	}
	scratch := &AnimNode{Name: node.Name}
	saved := p.currentAnimNode
	p.currentAnimNode = scratch
	p.parseAnimNodeParam(keyword, tokens)
	p.currentAnimNode = saved

	name, vals, n := firstAnimKey(scratch)
	if name == "" {
		return false
	}
	setNodeController(node, name, vals)
	if n > 1 {
		p.errorf("%s on geometry node %q: only the first of its %d keys is used, as in the game, which evaluates geometry at time 0", keyword, node.Name, n)
	}
	return true
}

// firstAnimKey returns the controller an's one key list holds, its first
// key's value and the number of keys, or "" when it holds none.
func firstAnimKey(an *AnimNode) (string, []float32, int) {
	if k := an.PositionKeys; len(k) > 0 {
		return "position", []float32{k[0].Value.X, k[0].Value.Y, k[0].Value.Z}, len(k)
	}
	if k := an.OrientationKeys; len(k) > 0 {
		return "orientation", []float32{k[0].Value.X, k[0].Value.Y, k[0].Value.Z, k[0].Value.W}, len(k)
	}
	for _, e := range animControllerFloatList {
		if k := *e.Getter(an); len(k) > 0 {
			return e.Name, []float32{k[0].Value}, len(k)
		}
	}
	for _, e := range animControllerColorList {
		if k := *e.Getter(an); len(k) > 0 {
			return e.Name, []float32{k[0].Value.X, k[0].Value.Y, k[0].Value.Z}, len(k)
		}
	}
	return "", nil, 0
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

	if p.parseGeomKeyList(node, keyword, tokens) {
		return
	}

	// A keyword this node type's parser does not read. The game ignores it
	// (its parsers compare exact words: mdl-spec E49, goldens zspec_setfill
	// and zspec_light), so it is ignored here too; a near match, such as
	// selfillumcolor for the NWmax typo setfillumcolor, is named in the warning.
	nodeType := node.NodeType()
	validParams := validParamsForType(nodeType)
	if matched, ok := NearMatchParam(keyword, validParams); ok && matched != keyword {
		p.errorf("%q is not a keyword the game reads, so the line is ignored, as in the game; did you mean %q?", keyword, matched)
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
	case "selfillumcolor":
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
	case "detonate":
		// Declared in parseParameter; its controller has no value.
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
	case "ndynamictype":
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

// isBezier reports whether the float or colour controller name is a bezier one.
func (an *AnimNode) isBezier(name string) bool {
	return an.Bezier[name]
}

func (an *AnimNode) setBezier(name string) {
	if an.Bezier == nil {
		an.Bezier = make(map[string]bool)
	}
	an.Bezier[name] = true
}

// tryAnimKeyTable handles table-driven float/color key parsing, linear
// (<name>key) and bezier (<name>bezierkey: value, handle in, handle out).
// Returns true if the keyword was handled.
func (p *parser) tryAnimKeyTable(keyword string, tokens []string, an *AnimNode) bool {
	if base, ok := strings.CutSuffix(keyword, "bezierkey"); ok && base != "detonate" {
		if getter, ok := animControllerFloatTable[base]; ok {
			an.setBezier(base)
			if len(tokens) >= 2 {
				if count, ok := p.countParam(tokens); ok {
					p.readKeys(int(count), 16, "float bezier keys", floatBezierKeyParser(getter(an)))
				}
			} else {
				p.readKeysUntilEnd(16, "float bezier keys", floatBezierKeyParser(getter(an)))
			}
			return true
		}
		if getter, ok := animControllerColorTable[base]; ok {
			an.setBezier(base)
			if len(tokens) >= 2 {
				if count, ok := p.countParam(tokens); ok {
					p.readKeys(int(count), 40, "color bezier keys", colorBezierKeyParser(getter(an)))
				}
			} else {
				p.readKeysUntilEnd(40, "color bezier keys", colorBezierKeyParser(getter(an)))
			}
			return true
		}
	}
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
		if p.parentLine(tokens, an.Name) {
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
		// Bezier float and colour lists are read by tryAnimKeyTable and
		// positionbezierkey above. What is left (orientationbezierkey,
		// detonatebezierkey) is read as linear keys: the leading time and value,
		// handles dropped.
		if strings.HasSuffix(keyword, "bezierkey") {
			base := strings.TrimSuffix(keyword, "bezierkey")
			p.parseAnimNodeParam(base+"key", tokens)
			return
		}
		// Static emitter/light/mesh values on animation nodes become
		// single-keyframe controllers at t=0, matching nwnmdlcomp behavior.
		if getter, ok := animControllerFloatTable[keyword]; ok {
			if len(tokens) >= 2 {
				if v, ok := p.floatValue(tokens); ok {
					if keys := getter(an); len(*keys) == 0 {
						*keys = append(*keys, FloatKey{Time: 0, Value: v})
					}
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
	p       *parser
	shown   int
	total   int
	stopped bool // the current entry stopped being read (row)
}

// row starts a list entry. The game reads an entry's values in order with
// sscanf and stops at the first that is not a number at all or has more after
// its number, so float and int set the values after it to 0. A number this
// parser refuses (NaN, infinity, out of range) is one sscanf reads, so it is
// set to 0 without stopping the entry.
func (w *listWarn) row() { w.stopped = false }

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

// float reads one float component as the game does (scanFloat). One that is
// not a usable number (values outside the float32 range and nan/inf included)
// is set to 0; one with more after its number gives the number. Either stops
// the entry (row), with a warning.
func (w *listWarn) float(tok string) float32 {
	if w.stopped {
		return 0
	}
	v, n, err := scanFloat(tok)
	switch {
	case err != nil && (n == 0 || n < len(tok)):
		w.add("%q is not a usable number, set to 0, as are the values after it in the entry", tok)
		w.stopped = true
		return 0
	case err != nil:
		w.add("%q is not a usable number, set to 0", tok)
		return 0
	case n < len(tok):
		w.add("%q is read as %s, as the game reads it, and the values after it in the entry are set to 0", tok, fmtFloat(v))
		w.stopped = true
	}
	return v
}

// int is float for an integer component (scanInt).
func (w *listWarn) int(tok string) int32 {
	if w.stopped {
		return 0
	}
	v, n, err := scanInt(tok)
	switch {
	case err != nil && (n == 0 || n < len(tok)):
		w.add("%q is not a usable integer, set to 0, as are the values after it in the entry", tok)
		w.stopped = true
		return 0
	case err != nil:
		w.add("%q is not a usable integer, set to 0", tok)
		return 0
	case n < len(tok):
		w.add("%q is read as %d, as the game reads it, and the values after it in the entry are set to 0", tok, v)
		w.stopped = true
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
		w.row()
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
		w.row()
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
		w.row()
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
		w.row()
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
		w.row()
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
		w.row()
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
		w.row()
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
		line, ok := p.nextWeightLine()
		if !ok {
			break
		}
		tokens := tokenize(line)
		w := VertexWeight{}
		// Weight format: bonename1 weight1 bonename2 weight2 ...
		for j := 0; j+1 < len(tokens); j += 2 {
			wt, n, err := scanFloat(tokens[j+1])
			if err != nil {
				ww.add("weight %q for bone %q is not a usable number, that influence is ignored", tokens[j+1], tokens[j])
				continue
			}
			if n < len(tokens[j+1]) {
				ww.add("weight %q for bone %q is read as %s, as the game reads it", tokens[j+1], tokens[j], fmtFloat(wt))
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
	t, tn, err := scanFloat(tokens[0])
	if err != nil {
		w.add("key time %q is not a usable number; the key is skipped", tokens[0])
		return nil, false
	}
	if tn < len(tokens[0]) {
		w.add("key time %q is read as %s, as the game reads it, and the values after it in the key are set to 0", tokens[0], fmtFloat(t))
		w.stopped = true
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
		w.row()
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
		w.row()
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

// floatBezierKeyParser parses "time value [handleIn handleOut]"; the handles
// are optional, as for positionbezierkey.
func floatBezierKeyParser(out *[]FloatKey) keyParser {
	return func(w *listWarn, tokens []string) bool {
		v, ok := w.keyValues(tokens, 2)
		if !ok {
			return false
		}
		var f [4]float32
		copy(f[:2], v)
		for i := 2; i < len(f) && i < len(tokens); i++ {
			f[i] = w.float(tokens[i])
		}
		*out = append(*out, FloatKey{Time: f[0], Value: f[1], TanIn: f[2], TanOut: f[3]})
		return true
	}
}

// colorBezierKeyParser parses "time r g b [in.rgb out.rgb]".
func colorBezierKeyParser(out *[]ColorKey) keyParser {
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
		*out = append(*out, ColorKey{
			Time:   f[0],
			Value:  Vec3{X: f[1], Y: f[2], Z: f[3]},
			TanIn:  Vec3{X: f[4], Y: f[5], Z: f[6]},
			TanOut: Vec3{X: f[7], Y: f[8], Z: f[9]},
		})
		return true
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

// maxNumberLen bounds a number token. strconv.ParseFloat misreads some valid
// numbers past 800 digits ("1" + 900 zeros + "e-900" reads as 0, not 1), and no
// model has numbers anywhere near this long, so longer tokens are refused like
// NaN and Inf: the caller warns and keeps the default.
const maxNumberLen = 400

// ScanNumber and ScanInteger read the number a token starts with the way the
// ASCII parser does (scanFloat, scanInt), returning the bytes consumed too,
// for the Python port's parity test, which must agree on every token.
func ScanNumber(s string) (float32, int, error) { return scanFloat(s) }
func ScanInteger(s string) (int32, int, error)  { return scanInt(s) }

// scanFloat reads the number s starts with as the game's parser does, with C's
// sscanf "%f" (glibc, which the game's Linux build uses; checked against it on
// the parity suite's number strings): an optional sign, then decimal digits
// with an optional point and exponent, a hexadecimal float, or inf, infinity
// or nan, any letter case. It returns the value, the number of bytes sscanf
// consumes (an exponent with no digits is consumed but not used: "1e" reads 1
// and takes both bytes; "1.5abc" takes 3), and an error when s does not start
// with a number or the number is one this parser refuses: NaN, infinity, out
// of the float32 range (the game would store infinity), or longer than
// maxNumberLen.
func scanFloat(s string) (float32, int, error) {
	n, num := scanFloatPrefix(s)
	switch {
	case n == 0:
		return 0, 0, fmt.Errorf("not a number: %s", s)
	case n > maxNumberLen:
		return 0, n, fmt.Errorf("number too long (%d characters)", n)
	case num == "":
		return 0, n, fmt.Errorf("invalid float value: %s", s[:n])
	}
	v, err := strconv.ParseFloat(num, 32)
	if err != nil {
		return 0, n, err
	}
	if math.IsInf(v, 0) {
		return 0, n, fmt.Errorf("invalid float value: %s", s[:n])
	}
	return float32(v), n, nil
}

// scanFloatPrefix returns how many bytes of s sscanf "%f" consumes (0 when it
// fails) and the number they hold in strconv syntax, or "" for inf and nan.
func scanFloatPrefix(s string) (int, string) {
	i := 0
	if i < len(s) && (s[i] == '+' || s[i] == '-') {
		i++
	}
	sign, r := s[:i], s[i:]
	switch {
	case hasPrefixFoldASCII(r, "inf"):
		k := 0
		for k < 5 && 3+k < len(r) && lowerASCII(r[3+k]) == "inity"[k] {
			k++
		}
		switch k {
		case 0:
			return i + 3, ""
		case 5:
			return i + 8, ""
		}
		return 0, "" // "infi": sscanf fails
	case hasPrefixFoldASCII(r, "nan"):
		return i + 3, "" // glibc does not take a "(...)" after it
	}
	digits := func(j int, ok func(byte) bool) int {
		for j < len(r) && ok(r[j]) {
			j++
		}
		return j
	}
	exponent := func(j int, marks string) (int, string) {
		if j < len(r) && strings.IndexByte(marks, r[j]) >= 0 {
			j++
			e0 := j
			if j < len(r) && (r[j] == '+' || r[j] == '-') {
				j++
			}
			d0 := j
			j = digits(j, isDigitASCII)
			if j > d0 {
				return j, r[e0:j]
			}
		}
		return j, ""
	}
	if len(r) >= 2 && r[0] == '0' && (r[1] == 'x' || r[1] == 'X') {
		j := digits(2, isHexASCII)
		ip, fp, dot := r[2:j], "", false
		if j < len(r) && r[j] == '.' {
			dot = true
			f0 := j + 1
			j = digits(f0, isHexASCII)
			fp = r[f0:j]
		}
		if ip == "" && fp == "" {
			if !dot {
				return 0, "" // "0x": sscanf fails
			}
			return i + j, sign + "0" // glibc reads "0x." as 0 and stops there
		}
		j, exp := exponent(j, "pP")
		if exp == "" {
			exp = "0"
		}
		return i + j, sign + "0x" + orZero(ip) + "." + orZero(fp) + "p" + exp
	}
	j := digits(0, isDigitASCII)
	ip, fp := r[:j], ""
	if j < len(r) && r[j] == '.' {
		f0 := j + 1
		j = digits(f0, isDigitASCII)
		fp = r[f0:j]
	}
	if ip == "" && fp == "" {
		return 0, ""
	}
	j, exp := exponent(j, "eE")
	num := sign + orZero(ip) + "." + orZero(fp)
	if exp != "" {
		num += "e" + exp
	}
	return i + j, num
}

// scanInt reads the integer s starts with as sscanf "%d" does: an optional
// sign and decimal digits ("2.9" reads 2 and takes one byte). It returns the
// value, the bytes consumed, and an error when there are no digits or the
// value is outside int32 (refused, as in parseInt: glibc would wrap it).
func scanInt(s string) (int32, int, error) {
	i := 0
	if i < len(s) && (s[i] == '+' || s[i] == '-') {
		i++
	}
	j := i
	for j < len(s) && isDigitASCII(s[j]) {
		j++
	}
	switch {
	case j == i:
		return 0, 0, fmt.Errorf("not a number: %s", s)
	case j > maxNumberLen:
		return 0, j, fmt.Errorf("number too long (%d characters)", j)
	}
	v, err := strconv.ParseInt(s[:j], 10, 32)
	if err != nil {
		return 0, j, fmt.Errorf("integer out of range: %s", s[:j])
	}
	return int32(v), j, nil
}

func isDigitASCII(c byte) bool { return c >= '0' && c <= '9' }
func isHexASCII(c byte) bool {
	return isDigitASCII(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
}
func lowerASCII(c byte) byte {
	if c >= 'A' && c <= 'Z' {
		return c + 'a' - 'A'
	}
	return c
}

func hasPrefixFoldASCII(s, prefix string) bool {
	if len(s) < len(prefix) {
		return false
	}
	for k := 0; k < len(prefix); k++ {
		if lowerASCII(s[k]) != prefix[k] {
			return false
		}
	}
	return true
}

func orZero(s string) string {
	if s == "" {
		return "0"
	}
	return s
}

func parseFloat(s string) (float32, error) {
	if len(s) > maxNumberLen {
		return 0, fmt.Errorf("number too long (%d characters)", len(s))
	}
	v, err := strconv.ParseFloat(s, 32)
	if err != nil {
		return 0, err
	}
	if math.IsNaN(v) || math.IsInf(v, 0) {
		return 0, fmt.Errorf("invalid float value: %s", s)
	}
	return float32(v), nil
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
