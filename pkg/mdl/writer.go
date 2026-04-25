// ASCII MDL writer.
//
// Produces NWN ASCII .mdl files from Model structs. Parameter ordering follows
// output_models.pl to ensure output is compilable by NWN:EE's compilemodel.
//
// Key rules:
//   - Node tree is written depth-first, matching the game's expected ordering
//   - Parameter ordering within nodes must match what the compiler expects
//   - Floating point values use %g format (matching Prolog's format/2)
//   - Colors from binary are stored as 0-1 floats; output as 0-1
//   - CamelCase preserved for emitter string enums per output_models.pl camel/2
//
// Ref: output_models.pl (full file)
package mdl

import (
	"fmt"
	"io"
	"strings"
)

// WriteFile writes a model to an ASCII MDL file on disk.
func WriteFile(model *Model, path string) error {
	return atomicWriteFile(path, func(w io.Writer) error { return Write(model, w) })
}

// Write writes a model to an ASCII MDL writer.
func Write(model *Model, w io.Writer) error {
	wr := &writer{w: w, useTexture0: model.UseTexture0}

	wr.printf("# Rewritten by cleanmodels-go\n")
	if model.FileDependancy != "" {
		wr.printf("filedependancy %s\n", model.FileDependancy)
	}
	wr.line("#MAXMODEL ASCII")

	wr.printf("newmodel %s\n", model.Name)
	wr.writeModelHeader(model)

	wr.line("#MAXGEOM ASCII")
	wr.printf("beginmodelgeom %s\n", model.Name)
	wr.writeNodeTree(model)
	wr.printf("endmodelgeom %s\n", model.Name)

	for i := range model.Animations {
		wr.line("")
		wr.writeAnimation(model, &model.Animations[i])
	}

	wr.printf("donemodel %s\n", model.Name)

	return wr.err
}

type writer struct {
	w           io.Writer
	err         error
	useTexture0 bool
}

func (w *writer) printf(format string, args ...interface{}) {
	if w.err != nil {
		return
	}
	_, w.err = fmt.Fprintf(w.w, format, args...)
}

func (w *writer) line(s string) {
	w.printf("%s\n", s)
}

func (w *writer) indent(level int, format string, args ...interface{}) {
	if w.err != nil {
		return
	}
	prefix := strings.Repeat("  ", level)
	w.printf("%s", prefix)
	w.printf(format, args...)
	w.printf("\n")
}

func (w *writer) writeModelHeader(m *Model) {
	if m.Classification != "" {
		w.indent(4, "classification %s", m.Classification)
	}
	w.indent(4, "setsupermodel %s %s", m.Name, m.SuperModel)
	if m.IgnoreFog != 0 {
		w.indent(4, "ignorefog %d", m.IgnoreFog)
	}
	w.indent(4, "setanimationscale %s", fmtFloat(m.AnimationScale))
}

const maxNodeDepth = 10000

func (w *writer) writeNodeTree(m *Model) {
	root := m.RootNode()
	if root == nil && len(m.Nodes) > 0 {
		root = m.Nodes[0]
	}
	if root == nil {
		return
	}
	// Parent → children (case-folded parent key) for O(1) child lookup per node.
	children := make(map[string][]*Node)
	for _, n := range m.Nodes {
		if n == nil {
			continue
		}
		key := strings.ToLower(n.Parent)
		children[key] = append(children[key], n)
	}
	visited := make(map[*Node]bool)
	w.writeNodeRecursive(m, root, visited, 0, children)
}

func (w *writer) writeNodeRecursive(m *Model, node *Node, visited map[*Node]bool, depth int, children map[string][]*Node) {
	if visited[node] || depth > maxNodeDepth {
		return
	}
	visited[node] = true
	w.writeNode(node)
	for _, child := range children[strings.ToLower(node.Name)] {
		if child != node && strings.EqualFold(child.Parent, node.Name) {
			w.writeNodeRecursive(m, child, visited, depth+1, children)
		}
	}
}

// writeNode writes a single node's data in the parameter ordering expected
// by the NWN:EE compiler.
// Ref: output_models.pl output_node/4
func (w *writer) writeNode(n *Node) {
	w.indent(2, "node %s %s", n.NodeType(), n.Name)

	if n.Parent != "" {
		w.indent(4, "parent %s", n.Parent)
	} else {
		w.indent(4, "parent NULL")
	}

	if n.PartNumber != 0 {
		w.indent(4, "#part-number %d", n.PartNumber)
	}

	// Common node properties (only write non-default values)
	if n.Position.X != 0 || n.Position.Y != 0 || n.Position.Z != 0 {
		w.indent(4, "position %s", fmtVec3(n.Position))
	}
	if n.Orientation.X != 0 || n.Orientation.Y != 0 || n.Orientation.Z != 0 || (n.Orientation.W != 0 && n.Orientation.W != 1) {
		w.indent(4, "orientation %s %s %s %s", fmtFloat(n.Orientation.X), fmtFloat(n.Orientation.Y), fmtFloat(n.Orientation.Z), fmtFloat(n.Orientation.W))
	}
	if n.Scale != 1.0 {
		w.indent(4, "scale %s", fmtFloat(n.Scale))
	}
	if n.InheritColor != 0 {
		w.indent(4, "inheritcolor %d", n.InheritColor)
	}
	if n.WireColor.X != 0 || n.WireColor.Y != 0 || n.WireColor.Z != 0 {
		w.indent(4, "wirecolor %s", fmtVec3(n.WireColor))
	}

	// Type-specific properties
	if n.Mesh != nil {
		w.writeMeshData(n.Mesh)
	}
	if n.AnimMesh != nil {
		w.writeAnimMeshData(n.AnimMesh)
	}
	if n.Dangly != nil {
		w.writeDanglyData(n.Dangly)
	}
	if n.Skin != nil {
		w.writeSkinData(n.Skin)
	}
	if n.Aabb != nil {
		w.writeAabbData(n.Aabb)
	}
	if n.Light != nil {
		w.writeLightData(n.Light)
	}
	if n.Emitter != nil {
		w.writeEmitterData(n.Emitter)
	}
	if n.Reference != nil {
		w.writeReferenceData(n.Reference)
	}

	w.indent(2, "endnode")
}

func (w *writer) writeMeshData(m *MeshData) {
	// Material properties
	w.indent(4, "ambient %s", fmtVec3(m.Ambient))
	w.indent(4, "diffuse %s", fmtVec3(m.Diffuse))
	w.indent(4, "specular %s", fmtVec3(m.Specular))
	w.indent(4, "shininess %s", fmtFloat(m.Shininess))

	if m.Bitmap != "" {
		if w.useTexture0 {
			w.indent(4, "texture0 %s", m.Bitmap)
		} else {
			w.indent(4, "bitmap %s", m.Bitmap)
		}
	}
	if m.Texture1 != "" {
		w.indent(4, "texture1 %s", m.Texture1)
	}
	if m.Texture2 != "" {
		w.indent(4, "texture2 %s", m.Texture2)
	}
	if m.MaterialName != "" {
		w.indent(4, "materialname %s", m.MaterialName)
	}
	if m.RenderHint != "" {
		w.indent(4, "renderhint %s", m.RenderHint)
	}

	w.indent(4, "render %d", m.Render)
	w.indent(4, "shadow %d", m.Shadow)
	w.indent(4, "beaming %d", m.Beaming)
	w.indent(4, "transparencyhint %d", m.TransparencyHint)

	if m.Alpha != 1.0 {
		w.indent(4, "alpha %s", fmtFloat(m.Alpha))
	}
	if m.SelfIllumColor.X != 0 || m.SelfIllumColor.Y != 0 || m.SelfIllumColor.Z != 0 {
		w.indent(4, "selfillumcolor %s", fmtVec3(m.SelfIllumColor))
	}
	if m.TileFade != 0 {
		w.indent(4, "tilefade %d", m.TileFade)
	}
	w.indent(4, "rotatetexture %d", m.RotateTexture)
	if m.LightMapped != 0 {
		w.indent(4, "lightmapped %d", m.LightMapped)
	}

	// Multimaterial
	if len(m.Multimaterial) > 0 {
		w.indent(4, "multimaterial %d", len(m.Multimaterial))
		for _, mat := range m.Multimaterial {
			w.indent(6, "%s", mat)
		}
	}

	// Vertex data
	if len(m.Verts) > 0 {
		w.indent(4, "verts %d", len(m.Verts))
		for _, v := range m.Verts {
			w.indent(6, "%s", fmtVec3(v))
		}
	}

	w.writeFaces(4, m.Faces)

	// TVerts
	w.writeVec3List("tverts", m.TVerts)
	w.writeVec3List("tverts1", m.TVerts1)
	w.writeVec3List("tverts2", m.TVerts2)
	w.writeVec3List("tverts3", m.TVerts3)

	// Tex indices
	w.writeInt3List("texindices0", m.TexIndices0)
	w.writeInt3List("texindices1", m.TexIndices1)
	w.writeInt3List("texindices2", m.TexIndices2)
	w.writeInt3List("texindices3", m.TexIndices3)

	// Colors
	if len(m.Colors) > 0 {
		w.indent(4, "colors %d", len(m.Colors))
		for _, c := range m.Colors {
			w.indent(6, "%s", fmtVec3(c))
		}
	}

	// Normals
	w.writeVec3List("normals", m.Normals)

	// Tangents
	if len(m.Tangents) > 0 {
		w.indent(4, "tangents %d", len(m.Tangents))
		for _, t := range m.Tangents {
			w.indent(6, "%s %s %s %s", fmtFloat(t.X), fmtFloat(t.Y), fmtFloat(t.Z), fmtFloat(t.W))
		}
	}
}

func (w *writer) writeSkinData(s *SkinData) {
	if len(s.Weights) > 0 {
		w.indent(4, "weights %d", len(s.Weights))
		for _, wt := range s.Weights {
			parts := make([]string, 0, len(wt.Bones)*2)
			for i, bone := range wt.Bones {
				parts = append(parts, bone, fmtFloat(wt.Weights[i]))
			}
			if len(parts) == 0 {
				w.indent(6, "_ 0")
			} else {
				w.indent(6, "%s", strings.Join(parts, " "))
			}
		}
	}
}

func (w *writer) writeDanglyData(d *DanglyData) {
	w.indent(4, "displacement %s", fmtFloat(d.Displacement))
	w.indent(4, "tightness %s", fmtFloat(d.Tightness))
	w.indent(4, "period %s", fmtFloat(d.Period))
	if d.DisplType != 0 {
		w.indent(4, "displtype %d", d.DisplType)
	}
	if len(d.Constraints) > 0 {
		w.indent(4, "constraints %d", len(d.Constraints))
		for _, c := range d.Constraints {
			w.indent(6, "%s", fmtFloat(c))
		}
	}
}

func (w *writer) writeAnimMeshData(am *AnimMeshData) {
	w.indent(4, "sampleperiod %s", fmtFloat(am.SamplePeriod))
	if am.ClipU != 0 || am.ClipV != 0 || am.ClipW != 0 || am.ClipH != 0 {
		w.indent(4, "clipu %s", fmtFloat(am.ClipU))
		w.indent(4, "clipv %s", fmtFloat(am.ClipV))
		w.indent(4, "clipw %s", fmtFloat(am.ClipW))
		w.indent(4, "cliph %s", fmtFloat(am.ClipH))
	}
	if len(am.AnimVerts) > 0 {
		w.writeVec3List("animverts", am.AnimVerts)
	}
	if len(am.AnimTVerts) > 0 {
		w.writeVec3List("animtverts", am.AnimTVerts)
	}
}

func (w *writer) writeAabbData(a *AabbData) {
	if len(a.Entries) > 0 {
		w.indent(4, "aabb %d", len(a.Entries))
		for _, e := range a.Entries {
			w.indent(6, "%s %s %d", fmtVec3(e.BoundMin), fmtVec3(e.BoundMax), e.LeafFace)
		}
	}
}

func (w *writer) writeLightData(l *LightData) {
	if l.Color.X != 0 || l.Color.Y != 0 || l.Color.Z != 0 {
		w.indent(4, "color %s", fmtVec3(l.Color))
	}
	if l.Radius != 0 {
		w.indent(4, "radius %s", fmtFloat(l.Radius))
	}
	if l.Multiplier != 0 {
		w.indent(4, "multiplier %s", fmtFloat(l.Multiplier))
	}
	w.indent(4, "ambientonly %d", l.AmbientOnly)
	w.indent(4, "nDynamicType %d", l.NDynamicType)
	w.indent(4, "affectdynamic %d", l.AffectDynamic)
	w.indent(4, "shadow %d", l.Shadow)
	w.indent(4, "lightpriority %d", l.LightPriority)
	w.indent(4, "fadinglight %d", l.FadingLight)
	if l.NegativeLight != 0 {
		w.indent(4, "negativelight %d", l.NegativeLight)
	}
	if l.GenerateFlare != 0 {
		w.indent(4, "generateflare %d", l.GenerateFlare)
	}
	if l.FlareRadius != 0 {
		w.indent(4, "flareradius %s", fmtFloat(l.FlareRadius))
	}
	if l.LensFlares != 0 {
		w.indent(4, "lensflares %d", l.LensFlares)
	}

	if len(l.TextureNames) > 0 {
		w.indent(4, "texturenames %d", len(l.TextureNames))
		for _, t := range l.TextureNames {
			w.indent(6, "%s", t)
		}
	}
	if len(l.FlareSizes) > 0 {
		w.indent(4, "flaresizes %d", len(l.FlareSizes))
		for _, s := range l.FlareSizes {
			w.indent(6, "%s", fmtFloat(s))
		}
	}
	if len(l.FlarePositions) > 0 {
		w.indent(4, "flarepositions %d", len(l.FlarePositions))
		for _, p := range l.FlarePositions {
			w.indent(6, "%s", fmtFloat(p))
		}
	}
	if len(l.FlareColorShifts) > 0 {
		w.indent(4, "flarecolorshifts %d", len(l.FlareColorShifts))
		for _, c := range l.FlareColorShifts {
			w.indent(6, "%s", fmtVec3(c))
		}
	}
	if l.ShadowRadius != 0 {
		w.indent(4, "shadowradius %s", fmtFloat(l.ShadowRadius))
	}
	if l.VerticalDisplacement != 0 {
		w.indent(4, "verticaldisplacement %s", fmtFloat(l.VerticalDisplacement))
	}
}

func (w *writer) writeEmitterData(e *EmitterData) {
	w.indent(4, "deadspace %s", fmtFloat(e.DeadSpace))
	w.indent(4, "blastRadius %s", fmtFloat(e.BlastRadius))
	w.indent(4, "blastLength %s", fmtFloat(e.BlastLength))
	w.indent(4, "xgrid %d", e.XGrid)
	w.indent(4, "ygrid %d", e.YGrid)
	w.indent(4, "spawntype %d", e.SpawnType)
	w.indent(4, "update %s", camelCase(e.Update))
	w.indent(4, "render %s", camelCase(e.Render))
	w.indent(4, "blend %s", camelCase(e.Blend))
	if e.Texture != "" {
		w.indent(4, "texture %s", e.Texture)
	}
	if e.ChunkName != "" {
		w.indent(4, "chunkName %s", e.ChunkName)
	}
	w.indent(4, "twosidedtex %d", e.TwoSidedTex)
	w.indent(4, "loop %d", e.Loop)
	w.indent(4, "renderorder %d", e.RenderOrder)

	// Flags
	w.indent(4, "p2p %d", e.P2P)
	w.indent(4, "p2p_sel %d", e.P2PSel)
	w.indent(4, "affectedByWind %d", e.AffectedByWind)
	w.indent(4, "m_isTinted %d", e.IsTinted)
	w.indent(4, "bounce %d", e.Bounce)
	w.indent(4, "random %d", e.Random)
	w.indent(4, "inherit %d", e.Inherit)
	w.indent(4, "inheritvel %d", e.InheritVel)
	w.indent(4, "inherit_local %d", e.InheritLocal)
	w.indent(4, "splat %d", e.Splat)
	w.indent(4, "inherit_part %d", e.InheritPart)

	// Controller values
	w.indent(4, "alphaStart %s", fmtFloat(e.AlphaStart))
	w.indent(4, "alphaMid %s", fmtFloat(e.AlphaMid))
	w.indent(4, "alphaEnd %s", fmtFloat(e.AlphaEnd))
	w.indent(4, "colorStart %s", fmtVec3(e.ColorStart))
	w.indent(4, "colorMid %s", fmtVec3(e.ColorMid))
	w.indent(4, "colorEnd %s", fmtVec3(e.ColorEnd))
	w.indent(4, "sizeStart %s", fmtFloat(e.SizeStart))
	w.indent(4, "sizeMid %s", fmtFloat(e.SizeMid))
	w.indent(4, "sizeEnd %s", fmtFloat(e.SizeEnd))
	w.indent(4, "sizeStart_y %s", fmtFloat(e.SizeStartY))
	w.indent(4, "sizeMid_y %s", fmtFloat(e.SizeMidY))
	w.indent(4, "sizeEnd_y %s", fmtFloat(e.SizeEndY))
	w.indent(4, "birthrate %s", fmtFloat(e.BirthRate))
	w.indent(4, "lifeExp %s", fmtFloat(e.LifeExp))
	w.indent(4, "mass %s", fmtFloat(e.Mass))
	w.indent(4, "spread %s", fmtFloat(e.Spread))
	w.indent(4, "particleRot %s", fmtFloat(e.ParticleRot))
	w.indent(4, "velocity %s", fmtFloat(e.Velocity))
	w.indent(4, "randvel %s", fmtFloat(e.RandVel))
	w.indent(4, "bounce_co %s", fmtFloat(e.BounceCo))
	w.indent(4, "blurlength %s", fmtFloat(e.BlurLength))
	w.indent(4, "fps %s", fmtFloat(e.FPS))
	w.indent(4, "frameStart %s", fmtFloat(e.FrameStart))
	w.indent(4, "frameEnd %s", fmtFloat(e.FrameEnd))
	w.indent(4, "grav %s", fmtFloat(e.Grav))
	w.indent(4, "drag %s", fmtFloat(e.Drag))
	w.indent(4, "threshold %s", fmtFloat(e.Threshold))
	w.indent(4, "combinetime %s", fmtFloat(e.CombineTime))
	w.indent(4, "percentStart %s", fmtFloat(e.PercentStart))
	w.indent(4, "percentMid %s", fmtFloat(e.PercentMid))
	w.indent(4, "percentEnd %s", fmtFloat(e.PercentEnd))
	if e.LightningDelay != 0 || e.LightningRadius != 0 || e.LightningScale != 0 {
		w.indent(4, "lightningDelay %s", fmtFloat(e.LightningDelay))
		w.indent(4, "lightningRadius %s", fmtFloat(e.LightningRadius))
		w.indent(4, "lightningScale %s", fmtFloat(e.LightningScale))
	}
	if e.LightningSubDiv != 0 {
		w.indent(4, "lightningSubDiv %s", fmtFloat(e.LightningSubDiv))
	}
	w.indent(4, "p2p_bezier2 %s", fmtFloat(e.P2PBezier2))
	w.indent(4, "p2p_bezier3 %s", fmtFloat(e.P2PBezier3))
	w.indent(4, "xsize %s", fmtFloat(e.XSize))
	w.indent(4, "ysize %s", fmtFloat(e.YSize))
}

func (w *writer) writeReferenceData(r *ReferenceData) {
	w.indent(4, "refModel %s", r.RefModel)
	w.indent(4, "reattachable %d", r.Reattachable)
}

// writeAnimation writes a single animation block.
// Ref: output_models.pl output_anim/3
func (w *writer) writeAnimation(m *Model, anim *Animation) {
	w.line("#MAXANIM ASCII")
	w.printf("newanim %s %s\n", anim.Name, m.Name)

	w.indent(4, "length %s", fmtFloat(anim.Length))
	w.indent(4, "transtime %s", fmtFloat(anim.TransTime))

	for _, evt := range anim.Events {
		w.indent(4, "event %s %s", fmtFloat(evt.Time), evt.Name)
	}

	if anim.Root != "" {
		w.indent(4, "animroot %s", anim.Root)
	}

	for i := range anim.Nodes {
		w.writeAnimNode(m, &anim.Nodes[i])
	}

	w.printf("doneanim %s %s\n", anim.Name, m.Name)
}

func (w *writer) writeAnimNode(m *Model, an *AnimNode) {
	// Determine node type from the model geometry
	nodeType := "dummy"
	if n := m.FindNode(an.Name); n != nil {
		nodeType = n.NodeType()
	}

	w.indent(4, "node %s %s", nodeType, an.Name)
	w.indent(6, "parent %s", an.Parent)

	// Mesh data for animmesh animation nodes
	if an.Mesh != nil {
		mesh := an.Mesh
		w.writeVec3ListAt(6, "verts", mesh.Verts)
		w.writeFaces(6, mesh.Faces)
		w.writeVec3ListAt(6, "tverts", mesh.TVerts)
	}

	// AnimMesh parameters (sampleperiod, clip*, animverts, animtverts)
	if an.AnimMesh != nil {
		am := an.AnimMesh
		if am.ClipU != 0 || am.ClipV != 0 || am.ClipW != 0 || am.ClipH != 0 {
			w.indent(6, "clipu %s", fmtFloat(am.ClipU))
			w.indent(6, "clipv %s", fmtFloat(am.ClipV))
			w.indent(6, "clipw %s", fmtFloat(am.ClipW))
			w.indent(6, "cliph %s", fmtFloat(am.ClipH))
		}
		if am.SamplePeriod != 0 {
			w.indent(6, "sampleperiod %s", fmtFloat(am.SamplePeriod))
		}
		w.writeVec3ListAt(6, "animverts", am.AnimVerts)
		w.writeVec3ListAt(6, "animtverts", am.AnimTVerts)
	}

	w.writePositionKeys(an.PositionKeys)
	w.writeOrientationKeys(an.OrientationKeys)
	for _, e := range animControllerFloatList {
		if e.Name == "detonate" {
			w.writeDetonateKeys(an.DetonateKeys)
			continue
		}
		w.writeFloatKeyList(e.Name+"key", *e.Getter(an))
	}
	for _, e := range animControllerColorList {
		w.writeColorKeyList(e.Name+"key", *e.Getter(an))
	}

	w.indent(4, "endnode")
}

func (w *writer) writePositionKeys(keys []PositionKey) {
	if len(keys) == 0 {
		return
	}
	w.indent(6, "positionkey %d", len(keys))
	for _, k := range keys {
		w.indent(3, "%s %s", fmtFloat(k.Time), fmtVec3(k.Value))
	}
	w.indent(6, "endlist")
}

func (w *writer) writeOrientationKeys(keys []OrientationKey) {
	if len(keys) == 0 {
		return
	}
	w.indent(6, "orientationkey %d", len(keys))
	for _, k := range keys {
		w.indent(3, "%s %s %s", fmtFloat(k.Time), fmtVec3(Vec3{X: k.Value.X, Y: k.Value.Y, Z: k.Value.Z}), fmtFloat(k.Value.W))
	}
	w.indent(6, "endlist")
}

// writeDetonateKeys emits detonatekey in NWN-spec time-only format (NumCols=-1).
func (w *writer) writeDetonateKeys(keys []FloatKey) {
	if len(keys) == 0 {
		return
	}
	w.indent(6, "detonatekey %d", len(keys))
	for _, k := range keys {
		w.indent(3, "%s", fmtFloat(k.Time))
	}
	w.indent(6, "endlist")
}

func (w *writer) writeFloatKeyList(name string, keys []FloatKey) {
	if len(keys) == 0 {
		return
	}
	w.indent(6, "%s %d", name, len(keys))
	for _, k := range keys {
		w.indent(3, "%s %s", fmtFloat(k.Time), fmtFloat(k.Value))
	}
	w.indent(6, "endlist")
}

func (w *writer) writeColorKeyList(name string, keys []ColorKey) {
	if len(keys) == 0 {
		return
	}
	w.indent(6, "%s %d", name, len(keys))
	for _, k := range keys {
		w.indent(3, "%s %s", fmtFloat(k.Time), fmtVec3(k.Value))
	}
	w.indent(6, "endlist")
}

func (w *writer) writeFaces(indent int, faces []Face) {
	if len(faces) == 0 {
		return
	}
	w.indent(indent, "faces %d", len(faces))
	for _, f := range faces {
		w.indent(indent+2, "%d %d %d %d %d %d %d %d",
			f.Verts[0], f.Verts[1], f.Verts[2],
			f.SmoothGroup,
			f.UVs[0], f.UVs[1], f.UVs[2],
			f.Material)
	}
}

func (w *writer) writeVec3List(name string, vecs []Vec3) {
	w.writeVec3ListAt(4, name, vecs)
}

func (w *writer) writeVec3ListAt(indent int, name string, vecs []Vec3) {
	if len(vecs) == 0 {
		return
	}
	w.indent(indent, "%s %d", name, len(vecs))
	for _, v := range vecs {
		w.indent(indent+2, "%s", fmtVec3(v))
	}
}

func (w *writer) writeInt3List(name string, vals [][3]int32) {
	if len(vals) == 0 {
		return
	}
	w.indent(4, "%s %d", name, len(vals))
	for _, v := range vals {
		w.indent(6, "%d %d %d", v[0], v[1], v[2])
	}
}
