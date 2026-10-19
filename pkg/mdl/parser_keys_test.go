package mdl

import (
	"strings"
	"testing"
)

// animNode parses a one-node animation whose node body is `body` and returns
// that node and the parse warnings.
func animNode(t *testing.T, nodeType, body string) (AnimNode, []ParseError) {
	t.Helper()
	src := `newmodel keys
setsupermodel keys NULL
classification CHARACTER
setanimationscale 1.00
beginmodelgeom keys
  node dummy keys
    parent NULL
  endnode
endmodelgeom

newanim a keys
  length 2.0
  transtime 0.25
  animroot keys
  node ` + nodeType + ` keys
    parent NULL
` + body + `
  endnode
doneanim a keys

donemodel keys
`
	pr, err := Parse(strings.NewReader(src))
	if err != nil {
		t.Fatal(err)
	}
	if len(pr.Model.Animations) != 1 || len(pr.Model.Animations[0].Nodes) != 1 {
		t.Fatalf("animation not parsed: %+v", pr.Model.Animations)
	}
	return pr.Model.Animations[0].Nodes[0], pr.Errors
}

// A key cannot be kept with an invented time or pose, so a key with no usable
// time or too few values is skipped (and reported). Inside an otherwise good
// key, the values are read as the game reads them (sscanf): "0/9" is 0, and
// the values after it are set to 0 (reported). Neighbours stay in order.
func TestCountedKeysSkipBadKeysAndKeepOrder(t *testing.T) {
	node, errs := animNode(t, "dummy", `    positionkey 5
      0.0 0 0 0
      0.25 1 2
      x 1 2 3
      0.75 1 0/9 3
      1.0 4 5 6`)
	keys := node.PositionKeys
	if len(keys) != 3 {
		t.Fatalf("kept %d keys, want 3 (the short one and the bad-time one skipped): %+v", len(keys), keys)
	}
	if keys[0].Time != 0 || keys[1].Time != 0.75 || keys[2].Time != 1.0 {
		t.Errorf("times out of order or wrong: %+v", keys)
	}
	if keys[1].Value.X != 1 || keys[1].Value.Y != 0 || keys[1].Value.Z != 0 {
		t.Errorf("the value read as 0 and the one after it should be 0: %+v", keys[1].Value)
	}
	for _, want := range []string{"key has 3 value(s), expected 4; the key is skipped", `key time "x" is not a usable number`, `"0/9" is read as 0, as the game reads it, and the values after it in the entry are set to 0`} {
		if !hasError(errs, want) {
			t.Errorf("missing warning %q in %+v", want, errs)
		}
	}
}

// The same rules apply to the uncounted "positionkey ... endlist" form.
func TestUncountedKeysSkipBadKeys(t *testing.T) {
	node, errs := animNode(t, "dummy", `    positionkey
      0.0 0 0 0
      oops
      0.5 1 1 1
      endlist`)
	if len(node.PositionKeys) != 2 || node.PositionKeys[1].Time != 0.5 {
		t.Fatalf("keys = %+v", node.PositionKeys)
	}
	if !hasError(errs, "key has 1 value(s), expected 4; the key is skipped") {
		t.Errorf("no warning: %+v", errs)
	}
}

func TestOrientationAndFloatKeys(t *testing.T) {
	node, errs := animNode(t, "trimesh", `    orientationkey 3
      0.0 0 0 1 0
      0.5 0 0 1
      1.0 0 0 1 1.5
    alphakey 3
      0.0 1.0
      0.5 nan
      1.0 0.5`)
	if len(node.OrientationKeys) != 2 || node.OrientationKeys[1].Time != 1.0 {
		t.Errorf("orientation keys = %+v", node.OrientationKeys)
	}
	if len(node.AlphaKeys) != 3 || node.AlphaKeys[1].Value != 0 {
		t.Errorf("alpha keys = %+v (a bad value keeps the key with 0)", node.AlphaKeys)
	}
	if !hasError(errs, "key has 4 value(s), expected 5") || !hasError(errs, `"nan" is not a usable number`) {
		t.Errorf("warnings = %+v", errs)
	}
}

// Bezier rows may omit the tangents; a bad tangent is zeroed and reported, and a
// row missing the position is skipped.
func TestBezierKeys(t *testing.T) {
	node, errs := animNode(t, "dummy", `    positionbezierkey 3
      0.0 1 2 3
      0.5 1 2 3 0.1 0.2 q 0.4 0.5 0.6
      1.0 1 2`)
	keys := node.PositionKeys
	if len(keys) != 2 {
		t.Fatalf("kept %d keys, want 2: %+v", len(keys), keys)
	}
	if keys[0].TanIn != (Vec3{}) {
		t.Errorf("row without tangents should have zero tangents: %+v", keys[0])
	}
	// The game stops reading the row at "q", so the values after it are 0 too.
	if keys[1].TanIn != (Vec3{X: 0.1, Y: 0.2}) || keys[1].TanOut != (Vec3{}) {
		t.Errorf("tangents = in %+v out %+v", keys[1].TanIn, keys[1].TanOut)
	}
	if !hasError(errs, `"q" is not a usable number, set to 0, as are the values after it`) || !hasError(errs, "key has 3 value(s), expected 4") {
		t.Errorf("warnings = %+v", errs)
	}
}

// A well-formed animation produces no warnings.
func TestCleanKeysProduceNoWarnings(t *testing.T) {
	_, errs := animNode(t, "dummy", `    positionkey 2
      0.0 0 0 0
      1.0 1 1 1
    orientationkey
      0.0 0 0 1 0
      1.0 0 0 1 1
      endlist`)
	if len(errs) != 0 {
		t.Fatalf("clean animation produced warnings: %+v", errs)
	}
}

// A damaged track reports a few lines and then one summary.
func TestKeyWarningsAreCapped(t *testing.T) {
	var b strings.Builder
	b.WriteString("    positionkey 12\n")
	for i := 0; i < 12; i++ {
		b.WriteString("      bad 0 0 0\n")
	}
	node, errs := animNode(t, "dummy", strings.TrimRight(b.String(), "\n"))
	if len(node.PositionKeys) != 0 {
		t.Fatalf("kept %d keys, want 0", len(node.PositionKeys))
	}
	if len(errs) != maxListWarnings+1 || !hasError(errs, "7 more damaged value(s)") {
		t.Errorf("want %d warnings plus a summary, got %d: %+v", maxListWarnings, len(errs), errs)
	}
}

// Detonate keys may be time-only (value 1) or time plus value.
func TestDetonateKeys(t *testing.T) {
	node, errs := animNode(t, "emitter", `    detonatekey 4
      1.5
      2.0 0.5
      zz
      3.0 7`)
	if len(node.DetonateKeys) != 3 {
		t.Fatalf("kept %d keys, want 3 (the unusable time skipped): %+v", len(node.DetonateKeys), node.DetonateKeys)
	}
	if node.DetonateKeys[0].Time != 1.5 || node.DetonateKeys[0].Value != 1 {
		t.Errorf("time-only key = %+v, want value 1", node.DetonateKeys[0])
	}
	if node.DetonateKeys[1].Value != 0.5 || node.DetonateKeys[2].Time != 3.0 {
		t.Errorf("keys = %+v", node.DetonateKeys)
	}
	if !hasError(errs, `key time "zz" is not a usable number`) {
		t.Errorf("no warning: %+v", errs)
	}
}
