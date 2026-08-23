package mdl

import (
	"bytes"
	"math"
	"strings"
	"testing"
)

const rtTol = 0.01

func TestRoundtripLight(t *testing.T) {
	src := `newmodel lighttest
setsupermodel lighttest NULL
classification CHARACTER
setanimationscale 1.00
beginmodelgeom lighttest
  node dummy lighttest
    parent NULL
  endnode
  node light mylight
    parent lighttest
    position 0 0 3
    color 1 0.5 0.2
    radius 15.5
    multiplier 2.0
  endnode
endmodelgeom
donemodel lighttest
`
	m := mustParseASCII(t, src)
	m2 := mustDecompile(t, mustCompile(t, m))

	n := m2.FindNode("mylight")
	if n == nil {
		t.Fatal("light node not found")
	}
	if n.Light == nil {
		t.Fatal("Light data is nil")
	}
	assertNear(t, "Color.X", float64(n.Light.Color.X), 1.0)
	assertNear(t, "Color.Y", float64(n.Light.Color.Y), 0.5)
	assertNear(t, "Color.Z", float64(n.Light.Color.Z), 0.2)
	assertNear(t, "Radius", float64(n.Light.Radius), 15.5)
	assertNear(t, "Multiplier", float64(n.Light.Multiplier), 2.0)
}

func TestRoundtripEmitter(t *testing.T) {
	src := `newmodel emittest
setsupermodel emittest NULL
classification CHARACTER
setanimationscale 1.00
beginmodelgeom emittest
  node dummy emittest
    parent NULL
  endnode
  node emitter myemitter
    parent emittest
    position 0 0 1
    update Fountain
    render Normal
    blend Normal
    texture fxpa_default
    birthrate 10
    lifeexp 5
    velocity 3.5
    alphastart 0.8
    alphamid 1.0
    alphaend 0
    lightningsubdiv 4
  endnode
endmodelgeom
donemodel emittest
`
	m := mustParseASCII(t, src)
	m2 := mustDecompile(t, mustCompile(t, m))

	n := m2.FindNode("myemitter")
	if n == nil {
		t.Fatal("emitter node not found")
	}
	if n.Emitter == nil {
		t.Fatal("Emitter data is nil")
	}
	assertNear(t, "BirthRate", float64(n.Emitter.BirthRate), 10)
	assertNear(t, "LifeExp", float64(n.Emitter.LifeExp), 5)
	assertNear(t, "Velocity", float64(n.Emitter.Velocity), 3.5)
	assertNear(t, "LightningSubDiv", float64(n.Emitter.LightningSubDiv), 4)
}

func TestRoundtripDanglymesh(t *testing.T) {
	src := `newmodel dangletest
setsupermodel dangletest NULL
classification CHARACTER
setanimationscale 1.00
beginmodelgeom dangletest
  node dummy dangletest
    parent NULL
  endnode
  node danglymesh dangle
    parent dangletest
    bitmap blank
    render 1
    displacement 0.5
    tightness 10
    period 1
    verts 3
      0 0 0
      1 0 0
      0 1 0
    tverts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2  0 0  0 0 1 2
    constraints 3
      0.5
      1.0
      0.25
  endnode
endmodelgeom
donemodel dangletest
`
	m := mustParseASCII(t, src)
	m2 := mustDecompile(t, mustCompile(t, m))

	n := m2.FindNode("dangle")
	if n == nil {
		t.Fatal("dangly node not found")
	}
	if n.Dangly == nil {
		t.Fatal("Dangly data is nil")
	}
	if len(n.Dangly.Constraints) != 3 {
		t.Fatalf("Constraints: got %d, want 3", len(n.Dangly.Constraints))
	}
	assertNear(t, "Displacement", float64(n.Dangly.Displacement), 0.5)
}

func TestRoundtripSkin(t *testing.T) {
	src := `newmodel skintest
setsupermodel skintest NULL
classification CHARACTER
setanimationscale 1.00
beginmodelgeom skintest
  node dummy skintest
    parent NULL
  endnode
  node dummy bone1
    parent skintest
    position 0 0 0
  endnode
  node dummy bone2
    parent skintest
    position 1 0 0
  endnode
  node skin myskin
    parent skintest
    bitmap blank
    render 1
    verts 3
      0 0 0
      1 0 0
      0 1 0
    tverts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2  0 0  0 0 1 2
    weights 3
      bone1 1.0 bone2 0.0
      bone1 0.5 bone2 0.5
      bone1 0.0 bone2 1.0
  endnode
endmodelgeom
donemodel skintest
`
	m := mustParseASCII(t, src)
	m2 := mustDecompile(t, mustCompile(t, m))

	n := m2.FindNode("myskin")
	if n == nil {
		t.Fatal("skin node not found")
	}
	if n.Skin == nil {
		t.Fatal("Skin data is nil")
	}
	if n.Mesh == nil {
		t.Fatal("Mesh data is nil on skin node")
	}
	if len(n.Skin.Weights) != len(n.Mesh.Verts) {
		t.Errorf("Weights count %d != Verts count %d", len(n.Skin.Weights), len(n.Mesh.Verts))
	}
	hasBone := false
	for _, w := range n.Skin.Weights {
		for _, b := range w.Bones {
			if b != "" {
				hasBone = true
			}
		}
	}
	if !hasBone {
		t.Error("no bone names found in skin weights")
	}
}

func TestRoundtripAlphaZero(t *testing.T) {
	src := `newmodel alphatest0
setsupermodel alphatest0 NULL
classification CHARACTER
setanimationscale 1.00
beginmodelgeom alphatest0
  node dummy alphatest0
    parent NULL
  endnode
  node trimesh alphamesh
    parent alphatest0
    bitmap blank
    render 1
    alpha 0
    verts 3
      0 0 0
      1 0 0
      0 1 0
    tverts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2  0 0  0 0 1 2
  endnode
endmodelgeom
donemodel alphatest0
`
	m := mustParseASCII(t, src)
	m2 := mustDecompile(t, mustCompile(t, m))

	n := m2.FindNode("alphamesh")
	if n == nil {
		t.Fatal("mesh node not found")
	}
	if n.Mesh == nil {
		t.Fatal("Mesh data is nil")
	}
	assertNear(t, "Alpha", float64(n.Mesh.Alpha), 0.0)
}

func TestRoundtripAlphaDefault(t *testing.T) {
	src := `newmodel alphatest1
setsupermodel alphatest1 NULL
classification CHARACTER
setanimationscale 1.00
beginmodelgeom alphatest1
  node dummy alphatest1
    parent NULL
  endnode
  node trimesh alphamesh
    parent alphatest1
    bitmap blank
    render 1
    alpha 1
    verts 3
      0 0 0
      1 0 0
      0 1 0
    tverts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2  0 0  0 0 1 2
  endnode
endmodelgeom
donemodel alphatest1
`
	m := mustParseASCII(t, src)
	m2 := mustDecompile(t, mustCompile(t, m))

	n := m2.FindNode("alphamesh")
	if n == nil {
		t.Fatal("mesh node not found")
	}
	if n.Mesh == nil {
		t.Fatal("Mesh data is nil")
	}
	assertNear(t, "Alpha", float64(n.Mesh.Alpha), 1.0)
}

func TestRoundtripAnimationControllers(t *testing.T) {
	src := `newmodel animctrl
setsupermodel animctrl NULL
classification CHARACTER
setanimationscale 1.00
beginmodelgeom animctrl
  node dummy animctrl
    parent NULL
  endnode
  node dummy child
    parent animctrl
  endnode
endmodelgeom

newanim walk animctrl
  animroot animctrl
  length 1.0
  transtime 0.25
  node dummy animctrl
    parent NULL
  endnode
  node dummy child
    parent animctrl
    positionkey 2
      0.0 0 0 0
      1.0 0 0 1
    orientationkey 2
      0.0 0 0 0 1
      1.0 0 0 1 0.7071
  endnode
doneanim walk animctrl

donemodel animctrl
`
	m := mustParseASCII(t, src)
	if len(m.Animations) == 0 {
		t.Skip("parser did not read animations")
	}
	m2 := mustDecompile(t, mustCompile(t, m))

	var walkAnim *Animation
	for i := range m2.Animations {
		if strings.EqualFold(m2.Animations[i].Name, "walk") {
			walkAnim = &m2.Animations[i]
			break
		}
	}
	if walkAnim == nil {
		t.Fatal("animation 'walk' not found after roundtrip")
	}
	if len(walkAnim.Nodes) == 0 {
		t.Fatal("animation has no nodes")
	}

	var childAnim *AnimNode
	for i := range walkAnim.Nodes {
		if strings.EqualFold(walkAnim.Nodes[i].Name, "child") {
			childAnim = &walkAnim.Nodes[i]
			break
		}
	}
	if childAnim == nil {
		t.Fatal("anim node 'child' not found")
	}
	if len(childAnim.PositionKeys) == 0 {
		t.Error("no position keys on child anim node")
	}
	if len(childAnim.OrientationKeys) == 0 {
		t.Error("no orientation keys on child anim node")
	}
}

// TestRoundtripTrimeshAlphaAnim is the regression guard for issue #12.
// A plain trimesh animation node that carries an alphakey (mesh alpha
// controller) used to lose it on compile: the compiler only set the mesh
// content bit for animmesh nodes, so the type-128 controller was never
// emitted, and the mesh — whose base alpha is 0 and whose visibility comes
// entirely from the alpha animation — rendered invisible in-game.
// vdr_magearmor's eight "shield" meshes are the canonical victims; BioWare's
// own vdr_magearmor2 mesh-flags these anim nodes and stores alphakey.
func TestRoundtripTrimeshAlphaAnim(t *testing.T) {
	src := `newmodel shieldtest
setsupermodel shieldtest NULL
classification Effects
setanimationscale 1.00
beginmodelgeom shieldtest
  node dummy shieldtest
    parent NULL
  endnode
  node trimesh shield
    parent shieldtest
    bitmap fxpa_shield
    render 1
    alpha 0.0
    verts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2 1 0 1 2 1
    tverts 3
      0 0 0
      1 0 0
      0 1 0
  endnode
endmodelgeom

newanim duration shieldtest
  animroot shieldtest
  length 4.0
  transtime 0.25
  node dummy shieldtest
    parent NULL
  endnode
  node trimesh shield
    parent shieldtest
    alphakey 5
      0.0   1.0
      0.8   0.6
      1.8   1.0
      2.8   0.6
      4.0   1.0
    endlist
  endnode
doneanim duration shieldtest

donemodel shieldtest
`
	m := mustParseASCII(t, src)
	if len(m.Animations) == 0 {
		t.Skip("parser did not read animations")
	}
	m2 := mustDecompile(t, mustCompile(t, m))

	var durAnim *Animation
	for i := range m2.Animations {
		if strings.EqualFold(m2.Animations[i].Name, "duration") {
			durAnim = &m2.Animations[i]
			break
		}
	}
	if durAnim == nil {
		t.Fatal("animation 'duration' not found after roundtrip")
	}

	var shieldAnim *AnimNode
	for i := range durAnim.Nodes {
		if strings.EqualFold(durAnim.Nodes[i].Name, "shield") {
			shieldAnim = &durAnim.Nodes[i]
			break
		}
	}
	if shieldAnim == nil {
		t.Fatal("anim node 'shield' not found after roundtrip")
	}
	if len(shieldAnim.AlphaKeys) != 5 {
		t.Fatalf("alpha keys dropped on trimesh anim node: got %d keys, want 5", len(shieldAnim.AlphaKeys))
	}
	// Spot-check the first and last keyframes survived intact.
	assertNear(t, "AlphaKeys[0].Value", float64(shieldAnim.AlphaKeys[0].Value), 1.0)
	assertNear(t, "AlphaKeys[1].Value", float64(shieldAnim.AlphaKeys[1].Value), 0.6)
	assertNear(t, "AlphaKeys[4].Time", float64(shieldAnim.AlphaKeys[4].Time), 4.0)
}

func TestRoundtripEmitterControllerOrder(t *testing.T) {
	src := `newmodel emitdet
setsupermodel emitdet NULL
classification CHARACTER
setanimationscale 1.00
beginmodelgeom emitdet
  node dummy emitdet
    parent NULL
  endnode
  node emitter myemitter
    parent emitdet
    position 0 0 1
    update Fountain
    render Normal
    blend Normal
    texture fxpa_default
    birthrate 10
    lifeexp 5
    velocity 3.5
    alphastart 0.8
    alphamid 1.0
    alphaend 0
    lightningsubdiv 4
  endnode
endmodelgeom
donemodel emitdet
`
	m := mustParseASCII(t, src)
	bin1 := mustCompile(t, m)

	m2 := mustParseASCII(t, src)
	bin2 := mustCompile(t, m2)

	if !bytes.Equal(bin1, bin2) {
		t.Errorf("two compiles of the same model differ: len1=%d len2=%d", len(bin1), len(bin2))
	}
}

func TestRoundtripNodeNameCase(t *testing.T) {
	src := `newmodel MyModel
setsupermodel MyModel NULL
classification CHARACTER
setanimationscale 1.00
beginmodelgeom MyModel
  node dummy MyModel
    parent NULL
  endnode
  node trimesh MyMesh
    parent MyModel
    bitmap blank
    render 1
    verts 3
      0 0 0
      1 0 0
      0 1 0
    tverts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2  0 0  0 0 1 2
  endnode
endmodelgeom
donemodel MyModel
`
	m := mustParseASCII(t, src)
	m2 := mustDecompile(t, mustCompile(t, m))

	n := m2.FindNode("mymesh")
	if n == nil {
		t.Fatal("node not found by case-insensitive lookup")
	}
	if n.Mesh == nil {
		t.Fatal("Mesh data is nil")
	}

	root := m2.FindNode("mymodel")
	if root == nil {
		t.Fatal("root node not found")
	}
	if !strings.EqualFold(n.Parent, root.Name) {
		t.Errorf("parent mismatch: got %q, want %q (case-insensitive)", n.Parent, root.Name)
	}
}

func TestRoundtripReference(t *testing.T) {
	src := `newmodel reftest
setsupermodel reftest NULL
classification CHARACTER
setanimationscale 1.00
beginmodelgeom reftest
  node dummy reftest
    parent NULL
  endnode
  node reference myref
    parent reftest
    refmodel plc_a01
    reattachable 1
  endnode
endmodelgeom
donemodel reftest
`
	m := mustParseASCII(t, src)
	m2 := mustDecompile(t, mustCompile(t, m))

	n := m2.FindNode("myref")
	if n == nil {
		t.Fatal("reference node not found")
	}
	if n.Reference == nil {
		t.Fatal("Reference data is nil")
	}
	if !strings.EqualFold(n.Reference.RefModel, "plc_a01") {
		t.Errorf("RefModel: got %q, want %q", n.Reference.RefModel, "plc_a01")
	}
	if n.Reference.Reattachable != 1 {
		t.Errorf("Reattachable: got %d, want 1", n.Reference.Reattachable)
	}
}

func assertNear(t *testing.T, label string, got, want float64) {
	t.Helper()
	if math.Abs(got-want) >= rtTol {
		t.Errorf("%s: got %g, want %g", label, got, want)
	}
}

// emitterChunkSrc builds a minimal emitter model. chunkName is written only
// when name is non-empty, so the same source serves both the absent and the
// present case.
func emitterChunkSrc(name string) string {
	chunk := ""
	if name != "" {
		chunk = "    chunkName " + name + "\n"
	}
	return `newmodel chunktest
setsupermodel chunktest NULL
classification EFFECT
setanimationscale 1.00
beginmodelgeom chunktest
  node dummy chunktest
    parent NULL
  endnode
  node emitter myemitter
    parent chunktest
    update Fountain
    render Normal
    blend Normal
    texture fxpa_flare
    birthrate 10
    lifeexp 5
` + chunk + `  endnode
endmodelgeom
donemodel chunktest
`
}

// chunkNameField returns the 16-byte chunkName field of the first emitter
// header, which directly follows the 64-byte texture name.
func chunkNameField(t *testing.T, bin []byte, texture string) []byte {
	t.Helper()
	i := bytes.Index(bin, append([]byte(texture), 0))
	if i < 0 {
		t.Fatalf("texture %q not found in compiled output", texture)
	}
	return bin[i+64 : i+80]
}

// An emitter with no chunkName must compile to a zero-filled field. Bioware's
// own binaries store zeros here; writing a placeholder makes the engine treat
// the emitter as chunk-spawning and it renders no particles.
func TestRoundtripEmitterChunkNameAbsentStaysAbsent(t *testing.T) {
	bin := mustCompile(t, mustParseASCII(t, emitterChunkSrc("")))

	for _, b := range chunkNameField(t, bin, "fxpa_flare") {
		if b != 0 {
			t.Fatalf("chunkName field is not zero-filled: %x", chunkNameField(t, bin, "fxpa_flare"))
		}
	}
	if got := mustDecompile(t, bin).FindNode("myemitter").Emitter.ChunkName; got != "" {
		t.Errorf("ChunkName = %q after roundtrip, want empty", got)
	}
}

// The converse: a chunkName that was present must survive, so the fix above
// cannot be satisfied by dropping the field altogether.
func TestRoundtripEmitterChunkNamePresentSurvives(t *testing.T) {
	bin := mustCompile(t, mustParseASCII(t, emitterChunkSrc("mychunk")))

	if got := mustDecompile(t, bin).FindNode("myemitter").Emitter.ChunkName; got != "mychunk" {
		t.Errorf("ChunkName = %q after roundtrip, want %q", got, "mychunk")
	}
}
