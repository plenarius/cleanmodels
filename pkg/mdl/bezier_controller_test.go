package mdl

import (
	"strings"
	"testing"
)

// TestBezierControllerASCIIDegradesToLinear pins the ASCII behaviour: a
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

// TestBezierControllerBinaryIsDropped pins the binary read guard. A bezier
// controller's keys are three times as wide as a linear one's, so reading one
// with the linear stride would return values lifted from the middle of the
// previous key. Since we have no sample of one to validate against (0 of
// ~347k controller keys across 1910 retail binaries), the reader drops it and
// warns instead of inventing plausible-looking numbers.
func TestBezierControllerBinaryIsDropped(t *testing.T) {
	d := &decompiler{model: &Model{}}
	keys := []binControllerKey{
		// Linear position controller — must survive.
		{Type: 8, ValueCount: 1, TimeStart: 0, DataStart: 1, ColumnCount: 3},
		// Same controller marked bezier (0x10 | 3) — must be dropped.
		{Type: 8, ValueCount: 1, TimeStart: 0, DataStart: 1, ColumnCount: 0x13},
	}

	got := d.resolveControllerDefs(keys, 1, "node")
	if len(got) != 1 {
		t.Fatalf("resolved %d controllers, want 1 (the bezier one must be dropped)", len(got))
	}
	if got[0].key.ColumnCount != 3 {
		t.Errorf("surviving controller has numfloats 0x%02x, want 0x03 (the linear one)", got[0].key.ColumnCount)
	}
	if got[0].numCols != 3 {
		t.Errorf("numCols = %d, want 3", got[0].numCols)
	}
}
