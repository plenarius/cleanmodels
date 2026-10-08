package mdl

import (
	"bytes"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

// TestBezierControllerASCIIDegradesToLinear pins the ASCII behaviour of non-position
// bezier lists (positionbezierkey is preserved, see below): a
// bezier key list is read as linear keyframes, keeping each key's time and
// value and dropping its tangents. The key readers are line-based, so the
// trailing tangent columns are simply surplus tokens — the rows are not
// misaligned. We do not re-emit bezier controllers, so this is a deliberate
// lossy passthrough rather than support.
func TestBezierControllerASCIIDegradesToLinear(t *testing.T) {
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
  length 1.0
  transtime 0.25
  node dummy beztest
    parent NULL
  endnode
  node dummy child
    parent beztest
    scalebezierkey 3
      0.0  1.0  0.0  0.25
      0.5  1.5  0.25 -0.25
      1.0  2.0 -0.25  0.0
    endlist
  endnode
doneanim walk beztest

donemodel beztest
`
	m := mustParseASCII(t, src)
	if len(m.Animations) == 0 {
		t.Skip("parser did not read animations")
	}

	var child *AnimNode
	for i := range m.Animations[0].Nodes {
		if strings.EqualFold(m.Animations[0].Nodes[i].Name, "child") {
			child = &m.Animations[0].Nodes[i]
			break
		}
	}
	if child == nil {
		t.Fatal("anim node 'child' not found")
	}
	if len(child.ScaleKeys) != 3 {
		t.Fatalf("got %d scale keys, want 3 (one per bezier row)", len(child.ScaleKeys))
	}
	// Time and value must come from the first two columns; the tangent
	// columns must not be mistaken for either.
	want := []struct{ time, value float32 }{{0, 1}, {0.5, 1.5}, {1, 2}}
	for i, w := range want {
		if child.ScaleKeys[i].Time != w.time || child.ScaleKeys[i].Value != w.value {
			t.Errorf("key %d = (t=%v, v=%v), want (t=%v, v=%v)",
				i, child.ScaleKeys[i].Time, child.ScaleKeys[i].Value, w.time, w.value)
		}
	}
}

// TestBezierControllerBinaryNonPositionIsDropped pins the binary read guard
// for bezier controllers other than position. Their keys are three times as
// wide as a linear one's, so reading one with the linear stride would return
// values lifted from the middle of the previous key. We have no engine-compiled
// sample of any but position, so the reader drops them and warns instead of
// inventing plausible-looking numbers.
func TestBezierControllerBinaryNonPositionIsDropped(t *testing.T) {
	d := &decompiler{model: &Model{}}
	keys := []binControllerKey{
		// Linear scale controller — must survive.
		{Type: 36, ValueCount: 1, TimeStart: 0, DataStart: 1, ColumnCount: 1},
		// Same controller marked bezier (0x10 | 1) — must be dropped.
		{Type: 36, ValueCount: 1, TimeStart: 0, DataStart: 1, ColumnCount: 0x11},
	}

	got := d.resolveControllerDefs(keys, 1, "node")
	if len(got) != 1 {
		t.Fatalf("resolved %d controllers, want 1 (the bezier one must be dropped)", len(got))
	}
	if got[0].key.ColumnCount != 1 || got[0].bezier {
		t.Errorf("surviving controller has numfloats 0x%02x bezier=%v, want the linear one", got[0].key.ColumnCount, got[0].bezier)
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
