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
	return &ParseResult{
		Model:  p.model,
		Errors: p.errors,
	}, nil
}

type parser struct {
	scanner *bufio.Scanner
	line    int
	model   *Model
	errors  []ParseError

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

		keyword := strings.ToLower(tokens[0])

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

func (p *parser) parseSuperModel(tokens []string) {
	if len(tokens) >= 3 {
		p.model.SuperModel = tokens[2]
	} else if len(tokens) >= 2 {
		p.model.SuperModel = tokens[1]
	}
}

func (p *parser) parseAnimationScale(tokens []string) {
	if len(tokens) >= 2 {
		if v, err := parseFloat(tokens[1]); err == nil {
			p.model.AnimationScale = v
		}
	}
}

func (p *parser) parseClassification(tokens []string) {
	if len(tokens) >= 2 {
		p.model.Classification = strings.ToUpper(tokens[1])
	}
}

func (p *parser) parseIgnoreFog(tokens []string) {
	if len(tokens) >= 2 {
		if v, err := parseInt(tokens[1]); err == nil {
			p.model.IgnoreFog = v
		}
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
		node.Mesh = NewMeshData()
	case "skin":
		node.Mesh = NewMeshData()
		node.Skin = &SkinData{}
	case "animmesh":
		node.Mesh = NewMeshData()
		node.AnimMesh = &AnimMeshData{}
	case "danglymesh":
		node.Mesh = NewMeshData()
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
	if v, err := parseFloat(tokens[idx]); err == nil {
		p.currentAnim.Length = v
	}
}

func (p *parser) parseAnimTransTime(tokens []string) {
	if len(tokens) >= 2 {
		if v, err := parseFloat(tokens[1]); err == nil {
			p.currentAnim.TransTime = v
		}
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

	switch keyword {
	case "parent":
		if len(tokens) >= 2 {
			node.Parent = tokens[1]
		}
	case "#part-number":
		if len(tokens) >= 2 {
			if v, err := parseInt(tokens[1]); err == nil {
				node.PartNumber = v
			}
		}
	case "position":
		if v, ok := parseVec3(tokens[1:]); ok {
			node.Position = v
		}
	case "orientation":
		if v, ok := parseVec4(tokens[1:]); ok {
			node.Orientation = v
		}
	case "scale":
		if len(tokens) >= 2 {
			if v, err := parseFloat(tokens[1]); err == nil {
				node.Scale = v
			}
		}
	case "inheritcolor":
		if len(tokens) >= 2 {
			if v, err := parseInt(tokens[1]); err == nil {
				node.InheritColor = v
			}
		}
	case "wirecolor":
		if v, ok := parseVec3(tokens[1:]); ok {
			node.WireColor = v
		}
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
		if v, ok := parseVec3(tokens[1:]); ok {
			mesh.Diffuse = v
		}
	case "ambient":
		if v, ok := parseVec3(tokens[1:]); ok {
			mesh.Ambient = v
		}
	case "specular":
		if v, ok := parseVec3(tokens[1:]); ok {
			mesh.Specular = v
		}
	case "shininess":
		if v, err := parseFloat(safeIndex(tokens, 1)); err == nil {
			mesh.Shininess = v
		}
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
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			mesh.Shadow = v
		}
	case "beaming":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			mesh.Beaming = v
		}
	case "render":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			mesh.Render = v
		}
	case "transparencyhint":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			mesh.TransparencyHint = v
		}
	case "alpha":
		if v, err := parseFloat(safeIndex(tokens, 1)); err == nil {
			mesh.Alpha = v
		}
	case "selfillumcolor", "setfillumcolor":
		if v, ok := parseVec3(tokens[1:]); ok {
			mesh.SelfIllumColor = v
		}
	case "tilefade":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			mesh.TileFade = v
		}
	case "rotatetexture":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			mesh.RotateTexture = v
		}
	case "lightmapped":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			mesh.LightMapped = v
		}
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
	default:
		return false
	}
	return true
}

func (p *parser) parseSkinParam(node *Node, keyword string, tokens []string) bool {
	switch keyword {
	case "weights":
		if len(tokens) >= 2 {
			count, err := parseInt(safeIndex(tokens, 1))
			if err == nil && count > 0 {
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
		if v, err := parseFloat(safeIndex(tokens, 1)); err == nil {
			dangly.Displacement = v
		}
	case "tightness":
		if v, err := parseFloat(safeIndex(tokens, 1)); err == nil {
			dangly.Tightness = v
		}
	case "period":
		if v, err := parseFloat(safeIndex(tokens, 1)); err == nil {
			dangly.Period = v
		}
	case "constraints":
		if len(tokens) >= 2 {
			count, err := parseInt(safeIndex(tokens, 1))
			if err == nil && count > 0 {
				p.readFloatList(&dangly.Constraints, int(count))
			}
		}
	case "displtype":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			dangly.DisplType = int32(v)
		}
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
		if v, err := parseFloat(safeIndex(tokens, 1)); err == nil {
			am.SamplePeriod = v
		}
	case "clipu":
		if v, err := parseFloat(safeIndex(tokens, 1)); err == nil {
			am.ClipU = v
		}
	case "clipv":
		if v, err := parseFloat(safeIndex(tokens, 1)); err == nil {
			am.ClipV = v
		}
	case "clipw":
		if v, err := parseFloat(safeIndex(tokens, 1)); err == nil {
			am.ClipW = v
		}
	case "cliph":
		if v, err := parseFloat(safeIndex(tokens, 1)); err == nil {
			am.ClipH = v
		}
	case "animverts":
		if len(tokens) >= 2 {
			count, err := parseInt(safeIndex(tokens, 1))
			if err == nil && count > 0 {
				p.readVec3List(&am.AnimVerts, int(count))
			}
		}
	case "animtverts":
		if len(tokens) >= 2 {
			count, err := parseInt(safeIndex(tokens, 1))
			if err == nil && count > 0 {
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
		if len(tokens) >= 2 {
			count, err := parseInt(safeIndex(tokens, 1))
			if err == nil && count > 0 {
				p.readAabbList(&aabb.Entries, int(count))
			}
		}
	default:
		return false
	}
	return true
}

func (p *parser) parseEmitterParam(em *EmitterData, keyword string, tokens []string) bool {
	switch keyword {
	case "deadspace":
		if v, err := parseFloat(safeIndex(tokens, 1)); err == nil {
			em.DeadSpace = v
		}
	case "blastradius":
		if v, err := parseFloat(safeIndex(tokens, 1)); err == nil {
			em.BlastRadius = v
		}
	case "blastlength":
		if v, err := parseFloat(safeIndex(tokens, 1)); err == nil {
			em.BlastLength = v
		}
	case "xgrid":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.XGrid = v
		}
	case "ygrid":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.YGrid = v
		}
	case "spawntype":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.SpawnType = v
		}
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
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.TwoSidedTex = v
		}
	case "loop":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.Loop = v
		}
	case "renderorder":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.RenderOrder = v
		}
	case "p2p":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.P2P = v
		}
	case "p2p_sel":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.P2PSel = v
		}
	case "affectedbywind":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.AffectedByWind = v
		}
	case "m_istinted":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.IsTinted = v
		}
	case "bounce":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.Bounce = v
		}
	case "random":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.Random = v
		}
	case "inherit":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.Inherit = v
		}
	case "inheritvel":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.InheritVel = v
		}
	case "inherit_local":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.InheritLocal = v
		}
	case "splat":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.Splat = v
		}
	case "inherit_part":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.InheritPart = v
		}
	case "p2p_type":
		if len(tokens) >= 2 {
			em.P2PType = tokens[1]
		}
	case "render_sel":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.RenderSel = v
		}
	case "blend_sel":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.BlendSel = v
		}
	case "update_sel":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.UpdateSel = v
		}
	case "spawntype_sel":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.SpawnTypeSel = v
		}
	case "opacity":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.Opacity = v
		}
	case "iconsize":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.IconSize = v
		}
	case "lockaxes":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.LockAxes = v
		}
	case "chunky":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			em.Chunky = v
		}
	default:
		if getter, ok := emitterFloatFields[keyword]; ok {
			if v, err := parseFloat(safeIndex(tokens, 1)); err == nil {
				*getter(em) = v
			}
			return true
		}
		if getter, ok := emitterColorFields[keyword]; ok {
			if v, ok := parseVec3(tokens[1:]); ok {
				*getter(em) = v
			}
			return true
		}
		return false
	}
	return true
}

func (p *parser) parseLightParam(light *LightData, keyword string, tokens []string) bool {
	switch keyword {
	case "flareradius":
		if v, err := parseFloat(safeIndex(tokens, 1)); err == nil {
			light.FlareRadius = v
		}
	case "lensflares":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			light.LensFlares = v
		}
	case "lightpriority":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			light.LightPriority = v
		}
	case "ambientonly":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			light.AmbientOnly = v
		}
	case "isdynamic":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			light.NDynamicType = v
		}
	case "ndynamictype", "n_dynamic_type":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			light.NDynamicType = v
		}
	case "affectdynamic":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			light.AffectDynamic = v
		}
	case "shadow":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			light.Shadow = v
		}
	case "generateflare":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			light.GenerateFlare = v
		}
	case "fadinglight":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			light.FadingLight = v
		}
	case "negativelight":
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			light.NegativeLight = v
		}
	case "color":
		if v, ok := parseVec3(tokens[1:]); ok {
			light.Color = v
		}
	case "radius":
		if v, err := parseFloat(safeIndex(tokens, 1)); err == nil {
			light.Radius = v
		}
	case "multiplier":
		if v, err := parseFloat(safeIndex(tokens, 1)); err == nil {
			light.Multiplier = v
		}
	case "shadowradius":
		if v, err := parseFloat(safeIndex(tokens, 1)); err == nil {
			light.ShadowRadius = v
		}
	case "verticaldisplacement":
		if v, err := parseFloat(safeIndex(tokens, 1)); err == nil {
			light.VerticalDisplacement = v
		}
	case "flaresizes":
		if len(tokens) >= 2 {
			count, err := parseInt(safeIndex(tokens, 1))
			if err == nil && count > 0 {
				p.readFloatList(&light.FlareSizes, int(count))
			}
		}
	case "flarepositions":
		if len(tokens) >= 2 {
			count, err := parseInt(safeIndex(tokens, 1))
			if err == nil && count > 0 {
				p.readFloatList(&light.FlarePositions, int(count))
			}
		}
	case "flarecolorshifts":
		if len(tokens) >= 2 {
			count, err := parseInt(safeIndex(tokens, 1))
			if err == nil && count > 0 {
				p.readVec3List(&light.FlareColorShifts, int(count))
			}
		}
	case "texturenames":
		if len(tokens) >= 2 {
			count, err := parseInt(safeIndex(tokens, 1))
			if err == nil && count > 0 {
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
		if v, err := parseInt(safeIndex(tokens, 1)); err == nil {
			ref.Reattachable = v
		}
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
			count, err := parseInt(safeIndex(tokens, 1))
			if err == nil {
				p.readDetonateKeys(&an.DetonateKeys, int(count))
			}
		}
		return true
	}
	if getter, ok := animFloatKeyTable[keyword]; ok {
		if len(tokens) >= 2 {
			count, err := parseInt(safeIndex(tokens, 1))
			if err == nil {
				p.readFloatKeys(getter(an), int(count))
			}
		} else {
			p.readFloatKeysUntilEnd(getter(an))
		}
		return true
	}
	if getter, ok := animColorKeyTable[keyword]; ok {
		if len(tokens) >= 2 {
			count, err := parseInt(safeIndex(tokens, 1))
			if err == nil {
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
			x, _ := parseFloat(safeIndex(tokens, 1))
			y, _ := parseFloat(safeIndex(tokens, 2))
			z, _ := parseFloat(safeIndex(tokens, 3))
			if len(an.PositionKeys) == 0 {
				an.PositionKeys = append(an.PositionKeys, PositionKey{Time: 0, Value: Vec3{X: x, Y: y, Z: z}})
			}
		}
	case "orientation":
		if len(tokens) >= 5 {
			x, _ := parseFloat(safeIndex(tokens, 1))
			y, _ := parseFloat(safeIndex(tokens, 2))
			z, _ := parseFloat(safeIndex(tokens, 3))
			w, _ := parseFloat(safeIndex(tokens, 4))
			if len(an.OrientationKeys) == 0 {
				an.OrientationKeys = append(an.OrientationKeys, OrientationKey{Time: 0, Value: Vec4{X: x, Y: y, Z: z, W: w}})
			}
		}

	// Controller key lists — handle both counted ("positionkey 2") and
	// uncounted ("positionkey" + lines + "endlist") formats.
	case "positionkey":
		if len(tokens) >= 2 {
			count, err := parseInt(safeIndex(tokens, 1))
			if err == nil {
				p.readPositionKeys(&an.PositionKeys, int(count))
			}
		} else {
			p.readPositionKeysUntilEnd(&an.PositionKeys)
		}
	case "orientationkey":
		if len(tokens) >= 2 {
			count, err := parseInt(safeIndex(tokens, 1))
			if err == nil {
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
			count, err := parseInt(safeIndex(tokens, 1))
			if err == nil {
				p.readVec3List(&an.Mesh.Verts, int(count))
			}
		}
	case "faces":
		if len(tokens) >= 2 {
			if an.Mesh == nil {
				an.Mesh = NewMeshData()
			}
			count, err := parseInt(safeIndex(tokens, 1))
			if err == nil {
				p.readFaceList(&an.Mesh.Faces, int(count))
			}
		}
	case "tverts":
		if len(tokens) >= 2 {
			if an.Mesh == nil {
				an.Mesh = NewMeshData()
			}
			count, err := parseInt(safeIndex(tokens, 1))
			if err == nil {
				p.readVec3List(&an.Mesh.TVerts, int(count))
			}
		}
	case "sampleperiod":
		if len(tokens) >= 2 {
			if an.AnimMesh == nil {
				an.AnimMesh = &AnimMeshData{}
			}
			an.AnimMesh.SamplePeriod, _ = parseFloat(safeIndex(tokens, 1))
		}
	case "clipu":
		if len(tokens) >= 2 {
			if an.AnimMesh == nil {
				an.AnimMesh = &AnimMeshData{}
			}
			an.AnimMesh.ClipU, _ = parseFloat(safeIndex(tokens, 1))
		}
	case "clipv":
		if len(tokens) >= 2 {
			if an.AnimMesh == nil {
				an.AnimMesh = &AnimMeshData{}
			}
			an.AnimMesh.ClipV, _ = parseFloat(safeIndex(tokens, 1))
		}
	case "clipw":
		if len(tokens) >= 2 {
			if an.AnimMesh == nil {
				an.AnimMesh = &AnimMeshData{}
			}
			an.AnimMesh.ClipW, _ = parseFloat(safeIndex(tokens, 1))
		}
	case "cliph":
		if len(tokens) >= 2 {
			if an.AnimMesh == nil {
				an.AnimMesh = &AnimMeshData{}
			}
			an.AnimMesh.ClipH, _ = parseFloat(safeIndex(tokens, 1))
		}
	case "animverts":
		if len(tokens) >= 2 {
			if an.AnimMesh == nil {
				an.AnimMesh = &AnimMeshData{}
			}
			count, err := parseInt(safeIndex(tokens, 1))
			if err == nil {
				p.readVec3List(&an.AnimMesh.AnimVerts, int(count))
			}
		}
	case "animtverts":
		if len(tokens) >= 2 {
			if an.AnimMesh == nil {
				an.AnimMesh = &AnimMeshData{}
			}
			count, err := parseInt(safeIndex(tokens, 1))
			if err == nil {
				p.readVec3List(&an.AnimMesh.AnimTVerts, int(count))
			}
		}

	case "endlist":

	default:
		if p.tryAnimKeyTable(keyword, tokens, an) {
			return
		}
		if strings.HasSuffix(keyword, "bezierkey") {
			base := strings.TrimSuffix(keyword, "bezierkey")
			p.parseAnimNodeParam(base+"key", tokens)
			return
		}
		// Static emitter/light/mesh values on animation nodes become
		// single-keyframe controllers at t=0, matching nwnmdlcomp behavior.
		if getter, ok := animControllerFloatTable[keyword]; ok {
			if len(tokens) >= 2 {
				v, err := parseFloat(safeIndex(tokens, 1))
				if err == nil {
					keys := getter(an)
					if len(*keys) == 0 {
						*keys = append(*keys, FloatKey{Time: 0, Value: v})
					}
				}
			}
			return
		}
		if getter, ok := animControllerColorTable[keyword]; ok {
			if len(tokens) >= 4 {
				r, _ := parseFloat(safeIndex(tokens, 1))
				g, _ := parseFloat(safeIndex(tokens, 2))
				b, _ := parseFloat(safeIndex(tokens, 3))
				keys := getter(an)
				if len(*keys) == 0 {
					*keys = append(*keys, ColorKey{Time: 0, Value: Vec3{X: r, Y: g, Z: b}})
				}
			}
			return
		}
	}
}

// List reading helpers

// parseCountedVec3 extracts a count from tokens[1] and reads that many Vec3 lines.
func (p *parser) parseCountedVec3(tokens []string, out *[]Vec3) {
	if len(tokens) >= 2 {
		if count, err := parseInt(safeIndex(tokens, 1)); err == nil && count > 0 {
			p.readVec3List(out, int(count))
		}
	}
}

func (p *parser) parseCountedVec4(tokens []string, out *[]Vec4) {
	if len(tokens) >= 2 {
		if count, err := parseInt(safeIndex(tokens, 1)); err == nil && count > 0 {
			p.readVec4List(out, int(count))
		}
	}
}

func (p *parser) parseCountedFaces(tokens []string, out *[]Face) {
	if len(tokens) >= 2 {
		if count, err := parseInt(safeIndex(tokens, 1)); err == nil && count > 0 {
			p.readFaceList(out, int(count))
		}
	}
}

func (p *parser) parseCountedInt3(tokens []string, out *[][3]int32) {
	if len(tokens) >= 2 {
		if count, err := parseInt(safeIndex(tokens, 1)); err == nil && count > 0 {
			p.readInt3List(out, int(count))
		}
	}
}

func (p *parser) parseCountedStrings(tokens []string, out *[]string) {
	if len(tokens) >= 2 {
		if count, err := parseInt(safeIndex(tokens, 1)); err == nil && count > 0 {
			p.readStringList(out, int(count))
		}
	}
}

func (p *parser) readVec3List(out *[]Vec3, count int) {
	if !p.trackAlloc(int64(count) * 12) {
		p.errorf("allocation limit exceeded reading %d vec3 entries", count)
		return
	}
	for i := 0; i < count; i++ {
		line, ok := p.nextLine()
		if !ok {
			break
		}
		tokens := tokenize(line)
		if v, ok := parseVec3(tokens); ok {
			*out = append(*out, v)
		}
	}
}

func (p *parser) readVec4List(out *[]Vec4, count int) {
	if !p.trackAlloc(int64(count) * 16) {
		p.errorf("allocation limit exceeded reading %d vec4 entries", count)
		return
	}
	for i := 0; i < count; i++ {
		line, ok := p.nextLine()
		if !ok {
			break
		}
		tokens := tokenize(line)
		if len(tokens) >= 4 {
			x, _ := parseFloat(tokens[0])
			y, _ := parseFloat(tokens[1])
			z, _ := parseFloat(tokens[2])
			w, _ := parseFloat(tokens[3])
			*out = append(*out, Vec4{X: x, Y: y, Z: z, W: w})
		}
	}
}

func (p *parser) readFaceList(out *[]Face, count int) {
	if !p.trackAlloc(int64(count) * 48) {
		p.errorf("allocation limit exceeded reading %d face entries", count)
		return
	}
	for i := 0; i < count; i++ {
		line, ok := p.nextLine()
		if !ok {
			break
		}
		tokens := tokenize(line)
		if len(tokens) >= 8 {
			v0, _ := parseInt(tokens[0])
			v1, _ := parseInt(tokens[1])
			v2, _ := parseInt(tokens[2])
			sg, _ := parseInt(tokens[3])
			uv0, _ := parseInt(tokens[4])
			uv1, _ := parseInt(tokens[5])
			uv2, _ := parseInt(tokens[6])
			mat, _ := parseInt(tokens[7])
			*out = append(*out, Face{
				Verts:       [3]int32{v0, v1, v2},
				SmoothGroup: sg,
				UVs:         [3]int32{uv0, uv1, uv2},
				Material:    mat,
			})
		}
	}
}

func (p *parser) readFloatList(out *[]float32, count int) {
	if !p.trackAlloc(int64(count) * 4) {
		p.errorf("allocation limit exceeded reading %d float entries", count)
		return
	}
	for i := 0; i < count; i++ {
		line, ok := p.nextLine()
		if !ok {
			break
		}
		tokens := tokenize(line)
		if len(tokens) >= 1 {
			if v, err := parseFloat(tokens[0]); err == nil {
				*out = append(*out, v)
			}
		}
	}
}

func (p *parser) readInt3List(out *[][3]int32, count int) {
	if !p.trackAlloc(int64(count) * 12) {
		p.errorf("allocation limit exceeded reading %d int3 entries", count)
		return
	}
	for i := 0; i < count; i++ {
		line, ok := p.nextLine()
		if !ok {
			break
		}
		tokens := tokenize(line)
		if len(tokens) >= 3 {
			a, _ := parseInt(tokens[0])
			b, _ := parseInt(tokens[1])
			c, _ := parseInt(tokens[2])
			*out = append(*out, [3]int32{a, b, c})
		}
	}
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
	for i := 0; i < count; i++ {
		line, ok := p.nextLine()
		if !ok {
			break
		}
		tokens := tokenize(line)
		if len(tokens) >= 7 {
			x1, _ := parseFloat(tokens[0])
			y1, _ := parseFloat(tokens[1])
			z1, _ := parseFloat(tokens[2])
			x2, _ := parseFloat(tokens[3])
			y2, _ := parseFloat(tokens[4])
			z2, _ := parseFloat(tokens[5])
			face, _ := parseInt(tokens[6])
			*out = append(*out, AabbEntry{
				BoundMin: Vec3{X: x1, Y: y1, Z: z1},
				BoundMax: Vec3{X: x2, Y: y2, Z: z2},
				LeafFace: face,
			})
		}
	}
}

func (p *parser) readWeightList(skin *SkinData, count int) {
	// Per line: VertexWeight plus bone name strings (often long in community models).
	if !p.trackAlloc(int64(count) * 512) {
		p.errorf("allocation limit exceeded reading %d skin weights", count)
		return
	}
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

func (p *parser) readPositionKeys(out *[]PositionKey, count int) {
	if !p.trackAlloc(int64(count) * 16) {
		p.errorf("allocation limit exceeded reading %d position keys", count)
		return
	}
	for i := 0; i < count; i++ {
		line, ok := p.nextLine()
		if !ok {
			break
		}
		tokens := tokenize(line)
		if len(tokens) >= 4 {
			t, _ := parseFloat(tokens[0])
			x, _ := parseFloat(tokens[1])
			y, _ := parseFloat(tokens[2])
			z, _ := parseFloat(tokens[3])
			*out = append(*out, PositionKey{Time: t, Value: Vec3{X: x, Y: y, Z: z}})
		}
	}
}

func (p *parser) readOrientationKeys(out *[]OrientationKey, count int) {
	if !p.trackAlloc(int64(count) * 20) {
		p.errorf("allocation limit exceeded reading %d orientation keys", count)
		return
	}
	for i := 0; i < count; i++ {
		line, ok := p.nextLine()
		if !ok {
			break
		}
		tokens := tokenize(line)
		if len(tokens) >= 5 {
			t, _ := parseFloat(tokens[0])
			x, _ := parseFloat(tokens[1])
			y, _ := parseFloat(tokens[2])
			z, _ := parseFloat(tokens[3])
			w, _ := parseFloat(tokens[4])
			*out = append(*out, OrientationKey{Time: t, Value: Vec4{X: x, Y: y, Z: z, W: w}})
		}
	}
}

func (p *parser) readFloatKeys(out *[]FloatKey, count int) {
	if !p.trackAlloc(int64(count) * 8) {
		p.errorf("allocation limit exceeded reading %d float keys", count)
		return
	}
	for i := 0; i < count; i++ {
		line, ok := p.nextLine()
		if !ok {
			break
		}
		tokens := tokenize(line)
		if len(tokens) >= 2 {
			t, _ := parseFloat(tokens[0])
			v, _ := parseFloat(tokens[1])
			*out = append(*out, FloatKey{Time: t, Value: v})
		}
	}
}

// readDetonateKeys reads detonate key entries that may be time-only (NWN spec:
// NumCols=-1) or time+value. Both formats are accepted for interoperability.
func (p *parser) readDetonateKeys(out *[]FloatKey, count int) {
	if !p.trackAlloc(int64(count) * 8) {
		p.errorf("allocation limit exceeded reading %d detonate keys", count)
		return
	}
	for i := 0; i < count; i++ {
		line, ok := p.nextLine()
		if !ok {
			break
		}
		tokens := tokenize(line)
		if len(tokens) >= 2 {
			t, _ := parseFloat(tokens[0])
			v, _ := parseFloat(tokens[1])
			*out = append(*out, FloatKey{Time: t, Value: v})
		} else if len(tokens) == 1 {
			t, _ := parseFloat(tokens[0])
			*out = append(*out, FloatKey{Time: t, Value: 1})
		}
	}
}

func (p *parser) readColorKeys(out *[]ColorKey, count int) {
	if !p.trackAlloc(int64(count) * 16) {
		p.errorf("allocation limit exceeded reading %d color keys", count)
		return
	}
	for i := 0; i < count; i++ {
		line, ok := p.nextLine()
		if !ok {
			break
		}
		tokens := tokenize(line)
		if len(tokens) >= 4 {
			t, _ := parseFloat(tokens[0])
			r, _ := parseFloat(tokens[1])
			g, _ := parseFloat(tokens[2])
			b, _ := parseFloat(tokens[3])
			*out = append(*out, ColorKey{Time: t, Value: Vec3{X: r, Y: g, Z: b}})
		}
	}
}

// readKeysUntilEnd scans lines until "endlist", calling parseLine for each
// line that has at least minTokens tokens. allocBytes is tracked per entry.
func (p *parser) readKeysUntilEnd(minTokens int, allocBytes int64, label string, parseLine func(tokens []string)) {
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
		if len(tokens) >= minTokens {
			if !p.trackAlloc(allocBytes) {
				p.errorf("allocation limit exceeded reading %s", label)
				break
			}
			parseLine(tokens)
		}
	}
}

func (p *parser) readPositionKeysUntilEnd(out *[]PositionKey) {
	p.readKeysUntilEnd(4, 16, "position keys", func(tokens []string) {
		t, _ := parseFloat(tokens[0])
		x, _ := parseFloat(tokens[1])
		y, _ := parseFloat(tokens[2])
		z, _ := parseFloat(tokens[3])
		*out = append(*out, PositionKey{Time: t, Value: Vec3{X: x, Y: y, Z: z}})
	})
}

func (p *parser) readOrientationKeysUntilEnd(out *[]OrientationKey) {
	p.readKeysUntilEnd(5, 20, "orientation keys", func(tokens []string) {
		t, _ := parseFloat(tokens[0])
		x, _ := parseFloat(tokens[1])
		y, _ := parseFloat(tokens[2])
		z, _ := parseFloat(tokens[3])
		w, _ := parseFloat(tokens[4])
		*out = append(*out, OrientationKey{Time: t, Value: Vec4{X: x, Y: y, Z: z, W: w}})
	})
}

func (p *parser) readFloatKeysUntilEnd(out *[]FloatKey) {
	p.readKeysUntilEnd(2, 8, "float keys", func(tokens []string) {
		t, _ := parseFloat(tokens[0])
		v, _ := parseFloat(tokens[1])
		*out = append(*out, FloatKey{Time: t, Value: v})
	})
}

func (p *parser) readColorKeysUntilEnd(out *[]ColorKey) {
	p.readKeysUntilEnd(4, 16, "color keys", func(tokens []string) {
		t, _ := parseFloat(tokens[0])
		r, _ := parseFloat(tokens[1])
		g, _ := parseFloat(tokens[2])
		b, _ := parseFloat(tokens[3])
		*out = append(*out, ColorKey{Time: t, Value: Vec3{X: r, Y: g, Z: b}})
	})
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
		"colors", "normals", "tangents",
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
