package mdl

import (
	"fmt"
	"hash/fnv"
	"math/rand"
	"strings"
)

// ConvertWateryToTrimesh reclassifies every watery animmesh node back to a
// plain trimesh by dropping its AnimMeshData (animverts, animtverts,
// sampleperiod, clip rects). Static water.
//
// Mirrors the dynamic_water=no branch of make_checks.pl wavy_water (line
// 1183).
func ConvertWateryToTrimesh(model *Model, waterKey string) []string {
	var msgs []string
	for _, n := range model.Nodes {
		if n == nil || n.AnimMesh == nil {
			continue
		}
		if !IsWateryNode(n, waterKey) {
			continue
		}
		n.AnimMesh = nil
		msgs = append(msgs, fmt.Sprintf("animmesh %q reclassified to trimesh (dynamic-water=no)", n.Name))
	}
	stripWateryAnimNodes(model, waterKey)
	return msgs
}

// stripWateryAnimNodes drops AnimMesh per-animation data on watery nodes that
// have just been reclassified to trimesh, so the writer doesn't emit dangling
// animation streams.
func stripWateryAnimNodes(model *Model, waterKey string) {
	wateryNames := make(map[string]bool)
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil {
			continue
		}
		if IsWateryBitmap(n.Mesh.Bitmap, waterKey) {
			wateryNames[lowerName(n.Name)] = true
		}
	}
	if len(wateryNames) == 0 {
		return
	}
	for ai := range model.Animations {
		for ni := range model.Animations[ai].Nodes {
			an := &model.Animations[ai].Nodes[ni]
			if wateryNames[lowerName(an.Name)] && an.AnimMesh != nil {
				an.AnimMesh = nil
			}
		}
	}
}

// WavyWaterOptions controls --dynamic-water wavy generation.
type WavyWaterOptions struct {
	WaterKey   string  // optional substring key for is_watery match
	WaveHeight float64 // 0 disables wave amplitude (water becomes flat anim mesh)
	TileWaterZ float32 // Z-plane to raise water vertices to (default 0)
	EdgePitch  float32 // tessellator edge max (default 2.0)
	AnimLength float32 // default-animation length when none exists (default 10.0)
	TransTime  float32 // default-animation transtime (default 0.25)
}

// withDefaults returns o with zero-fields filled in from the legacy CM3
// defaults (line 1229: pitch 2m, line 1245: 10s/0.25s, line 5575: WaveHeight
// in /105.0 units).
func (o WavyWaterOptions) withDefaults() WavyWaterOptions {
	if o.EdgePitch == 0 {
		o.EdgePitch = 2.0
	}
	if o.AnimLength == 0 {
		o.AnimLength = 10.0
	}
	if o.TransTime == 0 {
		o.TransTime = 0.25
	}
	return o
}

// ApplyWavyWater turns each watery node on a TILE-classified model into an
// animated wavy water plane. The work is split into per-phase helpers below;
// see each helper's doc for the legacy Prolog reference.
//
// Mirrors the dynamic_water=wavy branches of wavy_water in make_checks.pl
// (lines 1195-1310). Returns one message per node touched.
//
// The RNG used for the per-tile wave amplitudes is seeded from the model
// name plus the node name so two runs over the same input produce
// byte-identical output.
func ApplyWavyWater(model *Model, opts WavyWaterOptions) []string {
	if model == nil {
		return nil
	}
	if !strings.EqualFold(model.Classification, "TILE") {
		return nil
	}
	opts = opts.withDefaults()

	idx := nodeIndex(model)
	var msgs []string
	for _, n := range model.Nodes {
		if !IsWateryNode(n, opts.WaterKey) {
			continue
		}
		if msg, ok := wavifyWaterNode(model, idx, n, opts); ok {
			msgs = append(msgs, msg)
		}
	}
	return msgs
}

// wavifyWaterNode applies the full wavy-water pipeline to a single node and
// returns a human-readable summary. Returns ("", false) when the node has
// nothing to wavify (no mesh, or empty after welding).
func wavifyWaterNode(model *Model, idx map[string]*Node, n *Node, opts WavyWaterOptions) (string, bool) {
	if n.Mesh == nil {
		return "", false
	}

	raisedFromTrimesh := ensureWaterAnimMesh(n)
	if !nodeAtOrigin(n) || !isModelChild(model, n) {
		bakeNodeTransformIntoMesh(model, idx, n)
	}
	flattenWaterPlane(n, opts.TileWaterZ)
	WeldVertices(n, WeldOptions{Eps: 0, DropUnused: true})
	TessellateMesh(n, opts.EdgePitch)

	nv := len(n.Mesh.Verts)
	if nv == 0 {
		// Welded to nothing; skip animation generation rather than divide by zero.
		return "", false
	}

	amps := wavyAmplitudes(model.Name, n.Name, opts.WaveHeight)
	animverts := buildWavyAnimVerts(n.Mesh.Verts, amps)
	animtverts := buildWavyAnimTVerts(n.Mesh.TVerts)

	anim := ensureAnimation(model, "default", opts.AnimLength, opts.TransTime)
	attachWavyAnimMesh(n, anim, animverts, animtverts, opts.AnimLength)

	verb := "animmesh"
	if raisedFromTrimesh {
		verb = "trimesh reclassified to animmesh"
	}
	return fmt.Sprintf(
		"%s %q tessellated to %.1fm pitch with %d wavy keyframes",
		verb, n.Name, opts.EdgePitch, len(animverts)/nv,
	), true
}

// ensureWaterAnimMesh promotes a static trimesh to animmesh by attaching an
// empty AnimMeshData. Returns true if a new AnimMesh was created (so the
// caller can report the trimesh→animmesh reclassification in its message).
func ensureWaterAnimMesh(n *Node) bool {
	if n.AnimMesh != nil {
		return false
	}
	n.AnimMesh = &AnimMeshData{}
	return true
}

// flattenWaterPlane snaps every vertex Z to tileWaterZ so the wave displacement
// builds on a flat reference plane. Mirrors raise_to_tile in line 1212.
func flattenWaterPlane(n *Node, tileWaterZ float32) {
	if n == nil || n.Mesh == nil {
		return
	}
	for i := range n.Mesh.Verts {
		n.Mesh.Verts[i].Z = tileWaterZ
	}
}

// wavyAmps holds the five tile-random amplitude scalars used by perturbWave.
type wavyAmps struct {
	R0, R1, R2, R3, R4 float64
}

// wavyAmplitudes draws five seeded random amplitudes for the wave model.
// Mirrors the per-tile RNG calls in line 1262-1268.
func wavyAmplitudes(modelName, nodeName string, waveHeight float64) wavyAmps {
	rng := rand.New(rand.NewSource(wavySeed(modelName, nodeName)))
	return wavyAmps{
		R0: wavyAmplitude(rng) * waveHeight / 105.0,
		R1: wavyAmplitude(rng) / 21.0,
		R2: wavyAmplitude(rng) / 21.0,
		R3: wavyAmplitude(rng) / 21.0,
		R4: wavyAmplitude(rng) / 21.0,
	}
}

// buildWavyAnimVerts emits 6 keyframes per source vertex: rest, four perturbed
// states, then rest again. The 6-frame layout mirrors the legacy stream so
// SamplePeriod = AnimLength/5 yields a smooth loop.
func buildWavyAnimVerts(verts []Vec3, amps wavyAmps) []Vec3 {
	out := make([]Vec3, 0, 6*len(verts))
	for _, v := range verts {
		z1, z2, z3, z4 := perturbWave(amps.R0, amps.R1, amps.R2, amps.R3, amps.R4,
			float64(v.X), float64(v.Y), float64(v.Z))
		out = append(out,
			v,
			Vec3{X: v.X, Y: v.Y, Z: float32(z1)},
			Vec3{X: v.X, Y: v.Y, Z: float32(z2)},
			Vec3{X: v.X, Y: v.Y, Z: float32(z3)},
			Vec3{X: v.X, Y: v.Y, Z: float32(z4)},
			v,
		)
	}
	return out
}

// buildWavyAnimTVerts repeats each TVert 6 times to match the animvert frame
// count. UVs don't animate for wavy water; the duplication keeps the writer
// happy. Returns nil for meshes with no TVerts.
func buildWavyAnimTVerts(tverts []Vec3) []Vec3 {
	if len(tverts) == 0 {
		return nil
	}
	out := make([]Vec3, 0, 6*len(tverts))
	for _, t := range tverts {
		for k := 0; k < 6; k++ {
			out = append(out, t)
		}
	}
	return out
}

// attachWavyAnimMesh wires the generated streams into the node's AnimMesh and
// the model-level Animation, deriving SamplePeriod from animLength so the
// 5-step interpolation aligns with the loop length.
func attachWavyAnimMesh(n *Node, anim *Animation, animverts, animtverts []Vec3, animLength float32) {
	am := n.AnimMesh
	am.SamplePeriod = animLength / 5.0
	am.ClipU = 0
	am.ClipV = 0
	am.ClipW = 1
	am.ClipH = 1
	am.AnimVerts = animverts
	am.AnimTVerts = animtverts
	setAnimNodeMesh(anim, n.Name, n.Parent, am)
}

func nodeAtOrigin(n *Node) bool {
	return n.Position.X == 0 && n.Position.Y == 0 && n.Position.Z == 0 &&
		n.Orientation.W == 0
}

func isModelChild(model *Model, n *Node) bool {
	return strings.EqualFold(n.Parent, model.Name) || strings.EqualFold(n.Parent, "NULL")
}

// bakeNodeTransformIntoMesh rewrites the node's mesh vertices and normals
// into world space (relative to the model root), then zeroes the node's local
// position and orientation and reparents it directly under the model root.
// Mirrors raise_to_tile + set_zero_orientation + set_zero_position for
// animmesh nodes in line 1212-1214.
//
// Normals are rotated by the orientation chain only (no translation, no
// scale); without this the per-vertex lighting on the baked mesh would point
// in the wrong direction once the node's own orientation is zeroed out.
func bakeNodeTransformIntoMesh(model *Model, idx map[string]*Node, n *Node) {
	if n == nil || n.Mesh == nil {
		return
	}
	for i, v := range n.Mesh.Verts {
		n.Mesh.Verts[i] = LocalToWorld(idx, n, v)
	}
	for i, nv := range n.Mesh.Normals {
		n.Mesh.Normals[i] = LocalNormalToWorld(idx, n, nv)
	}
	n.Position = Vec3{}
	n.Orientation = Vec4{}
	if model != nil && model.Name != "" {
		n.Parent = model.Name
	}
}

// perturbWave returns four perturbed Z values for input vertex (X, Y, Z),
// driven by the per-tile random amplitudes (r0..r4). Direct port of
// make_checks.pl perturb/12 (line 5575).
func perturbWave(r0, r1, r2, r3, r4, x, y, z float64) (z1, z2, z3, z4 float64) {
	f0 := r0 * 2 * (5 - x) * (5 + x) * (5 - y) * (5 + y) / 625
	f1 := 0.2 * r1 * f0 * (x + 1.25) * (y + 1.25)
	f2 := 0.2 * r2 * f0 * (x - 1.25) * (y + 1.25)
	f3 := 0.2 * r3 * f0 * (x + 1.25) * (y - 1.25)
	f4 := 0.2 * r4 * f0 * (x - 1.25) * (y - 1.25)
	z1 = z + 0.66*f0 - 0.71*f1 + 0.66*f2 + 0.50*f3 + 0.16*f4
	z2 = z + 0.14*f0 + 0.05*f1 + 0.37*f2 - 0.70*f3 + 0.61*f4
	z3 = z - 0.22*f0 + 0.52*f1 - 0.60*f2 - 0.30*f3 - 0.16*f4
	z4 = z - 0.58*f0 + 0.14*f1 - 0.43*f2 + 0.50*f3 - 0.61*f4
	return
}

// wavyAmplitude returns 3 + sum-of-three-d6, mirroring the legacy random
// expression `(3 + random(6) + random(6) + random(6))` (line 1263). The
// random source is an injected seeded RNG so output is deterministic.
func wavyAmplitude(r *rand.Rand) float64 {
	return float64(3 + r.Intn(6) + r.Intn(6) + r.Intn(6))
}

// wavySeed derives a stable RNG seed from the model + node names so two runs
// produce identical wavy-water output.
func wavySeed(modelName, nodeName string) int64 {
	h := fnv.New64a()
	h.Write([]byte(strings.ToLower(modelName)))
	h.Write([]byte{'/'})
	h.Write([]byte(strings.ToLower(nodeName)))
	return int64(h.Sum64())
}

// ensureAnimation returns a *Animation matching the given name on the model,
// creating one with the supplied length / transtime / animroot=ModelName if
// none exists. Mirrors the "create default anim" branch (line 1242-1247).
func ensureAnimation(model *Model, name string, length, transTime float32) *Animation {
	for i := range model.Animations {
		if strings.EqualFold(model.Animations[i].Name, name) {
			a := &model.Animations[i]
			if a.Length <= 0 {
				a.Length = length
			}
			if a.TransTime == 0 {
				a.TransTime = transTime
			}
			if a.Root == "" {
				a.Root = model.Name
			}
			return a
		}
	}
	model.Animations = append(model.Animations, Animation{
		Name:      name,
		Length:    length,
		TransTime: transTime,
		Root:      model.Name,
	})
	return &model.Animations[len(model.Animations)-1]
}

// setAnimNodeMesh attaches the given AnimMeshData to the named node within
// the animation, creating an AnimNode for that node if missing.
func setAnimNodeMesh(anim *Animation, name, parent string, am *AnimMeshData) {
	for i := range anim.Nodes {
		if strings.EqualFold(anim.Nodes[i].Name, name) {
			anim.Nodes[i].AnimMesh = am
			return
		}
	}
	anim.Nodes = append(anim.Nodes, AnimNode{
		Name:     name,
		Parent:   parent,
		AnimMesh: am,
	})
}
