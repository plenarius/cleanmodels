package mdl

import (
	"bytes"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

// bezierProbe has a bezier list for every kind of animated controller: float
// and colour on a mesh, a light and an emitter, and orientation, which keeps
// only its linear form.
const bezierProbe = `newmodel beztest
setsupermodel beztest NULL
classification PLACEABLE
setanimationscale 1.00
beginmodelgeom beztest
  node dummy beztest
    parent NULL
  endnode
  node trimesh quad
    parent beztest
    bitmap brick
    verts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2 1 0 0 0 0
  endnode
  node light lamp
    parent beztest
    radius 5
    color 1 1 1
  endnode
  node emitter spark
    parent beztest
    update Fountain
    texture fxpa_flare
    birthrate 10
  endnode
  node dummy spinner
    parent beztest
  endnode
endmodelgeom beztest

newanim walk beztest
  animroot beztest
  length 2.0
  transtime 0.25
  node dummy beztest
    parent NULL
  endnode
  node trimesh quad
    parent beztest
    scalebezierkey 3
      0.0  1.0  0.0  0.25
      0.5  1.5  0.25 -0.25
      1.0  2.0 -0.25  0.0
    endlist
    selfillumcolorbezierkey 2
      0 0 0 0 0.1 0.2 0.3 0.4 0.5 0.6
      2 1 0.5 0.25 0.7 0.8 0.9 -0.1 -0.2 -0.3
    endlist
  endnode
  node light lamp
    parent beztest
    radiusbezierkey 2
      0 5 0 2
      2 8 -2 0
    endlist
    colorbezierkey 2
      0 1 0 0 0 0.1 0 0 0.2 0
      2 0 0 1 0.3 0 0 0 0 0.4
    endlist
  endnode
  node emitter spark
    parent beztest
    birthratebezierkey 2
      0 10 0 5
      2 20 -5 0
    endlist
    colorstartbezierkey 2
      0 1 1 1 0 0 0 0.1 0.1 0.1
      2 1 0 0 0.2 0.2 0.2 0 0 0
    endlist
  endnode
  node dummy spinner
    parent beztest
    orientationbezierkey 2
      0 0 0 1 0 0 0 1 0.1 0 0 1 0.2
      2 0 0 1 3.14 0 0 1 0.3 0 0 1 0.4
    endlist
  endnode
doneanim walk beztest

donemodel beztest
`

func bezierProbeNode(t *testing.T, m *Model, name string) *AnimNode {
	t.Helper()
	if len(m.Animations) == 0 {
		t.Fatal("no animations")
	}
	for i := range m.Animations[0].Nodes {
		if strings.EqualFold(m.Animations[0].Nodes[i].Name, name) {
			return &m.Animations[0].Nodes[i]
		}
	}
	t.Fatalf("anim node %q not found", name)
	return nil
}

// checkBezierProbe checks the probe's keys, handles and bezier marks.
func checkBezierProbe(t *testing.T, label string, m *Model) {
	t.Helper()
	quad := bezierProbeNode(t, m, "quad")
	if !quad.isBezier("scale") || !reflect.DeepEqual(quad.ScaleKeys, []FloatKey{
		{0, 1, 0, 0.25}, {0.5, 1.5, 0.25, -0.25}, {1, 2, -0.25, 0},
	}) {
		t.Errorf("%s: scale bezier keys = %+v (bezier %v)", label, quad.ScaleKeys, quad.isBezier("scale"))
	}
	if !quad.isBezier("selfillumcolor") || len(quad.SelfIllumColorKeys) != 2 ||
		quad.SelfIllumColorKeys[1] != (ColorKey{2, Vec3{1, 0.5, 0.25}, Vec3{0.7, 0.8, 0.9}, Vec3{-0.1, -0.2, -0.3}}) {
		t.Errorf("%s: selfillumcolor bezier keys = %+v", label, quad.SelfIllumColorKeys)
	}
	lamp := bezierProbeNode(t, m, "lamp")
	if !lamp.isBezier("radius") || !reflect.DeepEqual(lamp.RadiusKeys, []FloatKey{{0, 5, 0, 2}, {2, 8, -2, 0}}) {
		t.Errorf("%s: radius bezier keys = %+v", label, lamp.RadiusKeys)
	}
	if !lamp.isBezier("color") || len(lamp.ColorKeys) != 2 || lamp.ColorKeys[0].TanOut != (Vec3{0, 0.2, 0}) {
		t.Errorf("%s: light color bezier keys = %+v", label, lamp.ColorKeys)
	}
	spark := bezierProbeNode(t, m, "spark")
	if !spark.isBezier("birthrate") || !reflect.DeepEqual(spark.BirthRateKeys, []FloatKey{{0, 10, 0, 5}, {2, 20, -5, 0}}) {
		t.Errorf("%s: birthrate bezier keys = %+v", label, spark.BirthRateKeys)
	}
	if !spark.isBezier("colorstart") || len(spark.ColorStartKeys) != 2 || spark.ColorStartKeys[1].TanIn != (Vec3{0.2, 0.2, 0.2}) {
		t.Errorf("%s: colorstart bezier keys = %+v", label, spark.ColorStartKeys)
	}
	if spark.isBezier("alphastart") || quad.isBezier("alpha") {
		t.Errorf("%s: controllers without bezier keys marked bezier", label)
	}
	// orientationbezierkey has no bezier form we can store (the binary keeps
	// quaternions): it is read as linear keys, time and axis-angle.
	spinner := bezierProbeNode(t, m, "spinner")
	if len(spinner.OrientationKeys) != 2 || spinner.OrientationKeys[1].Value != (Vec4{0, 0, 1, 3.14}) {
		t.Errorf("%s: orientation keys = %+v", label, spinner.OrientationKeys)
	}
}

// Bezier float and colour lists keep their handles through parse, write and
// parse, and through compile and decompile.
func TestBezierControllersRoundTrip(t *testing.T) {
	m := mustParseASCII(t, bezierProbe)
	checkBezierProbe(t, "parsed", m)

	var out strings.Builder
	if err := Write(m, &out); err != nil {
		t.Fatal(err)
	}
	for _, kw := range []string{"scalebezierkey 3", "selfillumcolorbezierkey 2", "radiusbezierkey 2",
		"colorbezierkey 2", "birthratebezierkey 2", "colorstartbezierkey 2", "orientationkey 2"} {
		if !strings.Contains(out.String(), kw) {
			t.Errorf("written ASCII lacks %q", kw)
		}
	}
	checkBezierProbe(t, "rewritten", mustParseASCII(t, out.String()))

	bin := mustCompile(t, m)
	dec := mustDecompile(t, bin)
	checkBezierProbe(t, "compiled", dec)
	for _, w := range dec.Warnings {
		t.Errorf("decompile warning: %s", w.Message)
	}
}

// A bezier controller's binary keys are the value's columns with 0x10 and the
// value then two handles; the column count comes from the controller (the
// game's compiler writes 0x11 for a float, nwnmdlcomp the row's width, 0x13).
// Orientation has no readable bezier form and is dropped with a warning.
func TestBezierControllerBinaryStride(t *testing.T) {
	d := &decompiler{model: &Model{}}
	got := d.resolveControllerDefs([]binControllerKey{
		{Type: 36, ValueCount: 1, TimeStart: 0, DataStart: 1, ColumnCount: 0x11},  // scale, game
		{Type: 36, ValueCount: 1, TimeStart: 0, DataStart: 1, ColumnCount: 0x13},  // scale, nwnmdlcomp
		{Type: 100, ValueCount: 1, TimeStart: 0, DataStart: 1, ColumnCount: 0x13}, // selfillumcolor
		{Type: 20, ValueCount: 1, TimeStart: 0, DataStart: 1, ColumnCount: 0x14},  // orientation
	}, 0x21, "node")
	if len(got) != 3 {
		t.Fatalf("resolved %d controllers, want 3 (orientation dropped)", len(got))
	}
	for i, want := range []int{3, 3, 9} {
		if !got[i].bezier || got[i].numCols != want {
			t.Errorf("controller %d: bezier %v, %d columns; want bezier, %d", i, got[i].bezier, got[i].numCols, want)
		}
	}
	if len(d.model.Warnings) != 1 || !strings.Contains(d.model.Warnings[0].Message, "orientation") {
		t.Errorf("warnings = %+v, want one for orientation", d.model.Warnings)
	}
}

// TestPositionBezierControllerBinaryStride pins the position bezier layout
// observed in the engine's own compile of plc_a01 (issue #15): numfloats 0x13,
// nine floats per key.
func TestPositionBezierControllerBinaryStride(t *testing.T) {
	d := &decompiler{model: &Model{}}
	got := d.resolveControllerDefs([]binControllerKey{
		{Type: 8, ValueCount: 5, TimeStart: 0, DataStart: 5, ColumnCount: 0x13},
	}, 1, "node")
	if len(got) != 1 || !got[0].bezier || got[0].numCols != 9 {
		t.Fatalf("got %+v, want one bezier position controller with 9 columns", got)
	}
}

// TestPositionBezierKeyASCIIRoundTrip pins preservation of positionbezierkey
// (issue #15). Rows are 10 columns — time, value, tangent in, tangent out —
// taken from the NWmax-exported plc_a01.mdl in the issue.
func TestPositionBezierKeyASCIIRoundTrip(t *testing.T) {
	src := `newmodel beztest
setsupermodel beztest NULL
classification CHARACTER
setanimationscale 1.00
beginmodelgeom beztest
  node dummy beztest
    parent NULL
  endnode
  node dummy child
    parent beztest
  endnode
endmodelgeom

newanim walk beztest
  animroot beztest
  length 6.0
  transtime 0.25
  node dummy beztest
    parent NULL
  endnode
  node dummy child
    parent beztest
    positionbezierkey
      0.0   0.0    0.0  0.0    0.0    0.0  0.0   0.0  -10.0  0.0
      1.5 -10.0  -10.0  0.0  -10.0   20.0  0.0  10.0    0.0  0.0
      6.0   0.0    0.0  0.0   20.0   10.0  0.0   0.0    0.0  0.0
    endlist
  endnode
doneanim walk beztest

donemodel beztest
`
	check := func(m *Model) {
		t.Helper()
		var child *AnimNode
		for i := range m.Animations[0].Nodes {
			if m.Animations[0].Nodes[i].Name == "child" {
				child = &m.Animations[0].Nodes[i]
			}
		}
		if child == nil || !child.PositionBezier || len(child.PositionKeys) != 3 {
			t.Fatalf("bezier position keys not preserved: %+v", child)
		}
		k := child.PositionKeys[1]
		if k.Time != 1.5 || k.Value != (Vec3{-10, -10, 0}) ||
			k.TanIn != (Vec3{-10, 20, 0}) || k.TanOut != (Vec3{10, 0, 0}) {
			t.Errorf("key 1 = %+v", k)
		}
	}
	m := mustParseASCII(t, src)
	check(m)

	var out strings.Builder
	if err := Write(m, &out); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(out.String(), "positionbezierkey 3") {
		t.Fatalf("output lost positionbezierkey:\n%s", out.String())
	}
	check(mustParseASCII(t, out.String()))
}

// TestPositionBezierEngineCompiledFixture checks the decoder and compiler
// against ground truth: tests/fixtures/bezier/plc_a01.mdl is the binary the
// NWN:EE in-game `compilemodel plc_a01` console command produced from
// plc_a01.ascii.mdl (the NWmax sample from issue #15). Our decode of the
// engine's output must recover the ASCII keys with their tangents, and our own
// compile of the ASCII must decode back to the same keys.
func TestPositionBezierEngineCompiledFixture(t *testing.T) {
	dir := filepath.Join("..", "..", "tests", "fixtures", "bezier")

	asciiRes, err := ParseFile(filepath.Join(dir, "plc_a01.ascii.mdl"))
	if err != nil {
		t.Fatal(err)
	}
	engine, err := DecompileFile(filepath.Join(dir, "plc_a01.mdl"))
	if err != nil {
		t.Fatal(err)
	}

	var buf bytes.Buffer
	if err := Compile(asciiRes.Model, &buf); err != nil {
		t.Fatal(err)
	}
	ours, err := Decompile(bytes.NewReader(buf.Bytes()), int64(buf.Len()))
	if err != nil {
		t.Fatal(err)
	}

	find := func(m *Model, name string) *AnimNode {
		for i := range m.Animations {
			for j := range m.Animations[i].Nodes {
				if m.Animations[i].Nodes[j].Name == name {
					return &m.Animations[i].Nodes[j]
				}
			}
		}
		return nil
	}
	for _, name := range []string{"dbxy", "dbxy1", "dbxy2", "dbxy3"} {
		want := find(asciiRes.Model, name)
		if want == nil || !want.PositionBezier || len(want.PositionKeys) != 5 {
			t.Fatalf("%s: ascii source did not parse as 5 bezier keys: %+v", name, want)
		}
		for label, m := range map[string]*Model{"engine": engine, "ours": ours} {
			got := find(m, name)
			if got == nil || !got.PositionBezier {
				t.Errorf("%s/%s: bezier position keys missing", label, name)
				continue
			}
			if !reflect.DeepEqual(got.PositionKeys, want.PositionKeys) {
				t.Errorf("%s/%s: keys\n got  %+v\n want %+v", label, name, got.PositionKeys, want.PositionKeys)
			}
		}
	}
}

// TestBezierControllersEngineCompiledFixture checks every float and colour
// bezier controller against ground truth: tests/fixtures/bezier/cmbez01.mdl is
// what NWN:EE's own compiler (`nwmain-linux compilemodel cmbez01`) wrote for
// cmbez01.ascii.mdl. Decoding it, and decoding our compile of the ASCII, must
// both give the source's keys and handles. The engine stores
// orientationbezierkey as a 3-column (0x13) controller, which is dropped with a
// warning; we compile the same rows as linear orientation keys.
func TestBezierControllersEngineCompiledFixture(t *testing.T) {
	dir := filepath.Join("..", "..", "tests", "fixtures", "bezier")
	src, err := ParseFile(filepath.Join(dir, "cmbez01.ascii.mdl"))
	if err != nil {
		t.Fatal(err)
	}
	engine, err := DecompileFile(filepath.Join(dir, "cmbez01.mdl"))
	if err != nil {
		t.Fatal(err)
	}
	ours := mustDecompile(t, mustCompile(t, src.Model))

	type ctrl struct {
		float  []FloatKey
		color  []ColorKey
		bezier bool
	}
	controllers := func(an *AnimNode) map[string]ctrl {
		out := map[string]ctrl{}
		for _, e := range animControllerFloatList {
			if k := *e.Getter(an); len(k) > 0 {
				out[e.Name] = ctrl{float: k, bezier: an.isBezier(e.Name)}
			}
		}
		for _, e := range animControllerColorList {
			if k := *e.Getter(an); len(k) > 0 {
				out[e.Name] = ctrl{color: k, bezier: an.isBezier(e.Name)}
			}
		}
		return out
	}
	want := map[string]int{"quad": 3, "lamp": 3, "spark": 3}
	for node, n := range want {
		w := controllers(bezierProbeNode(t, src.Model, node))
		if len(w) != n {
			t.Fatalf("%s: source has %d controllers, want %d", node, len(w), n)
		}
		for label, m := range map[string]*Model{"engine": engine, "ours": ours} {
			got := controllers(bezierProbeNode(t, m, node))
			if !reflect.DeepEqual(got, w) {
				t.Errorf("%s/%s: controllers\n got  %+v\n want %+v", label, node, got, w)
			}
		}
	}
	if k := bezierProbeNode(t, engine, "spinner").OrientationKeys; len(k) != 0 {
		t.Errorf("engine: orientation keys %+v, want the 0x13 controller dropped", k)
	}
	if len(engine.Warnings) != 1 || !strings.Contains(engine.Warnings[0].Message, "orientation") {
		t.Errorf("engine warnings = %+v, want one for orientation", engine.Warnings)
	}
	if k := bezierProbeNode(t, ours, "spinner").OrientationKeys; len(k) != 2 {
		t.Errorf("ours: %d orientation keys, want 2 linear ones", len(k))
	}
}
