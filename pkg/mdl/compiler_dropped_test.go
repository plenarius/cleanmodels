package mdl

import (
	"bytes"
	"strings"
	"testing"
)

const droppedSrc = `newmodel drp
setsupermodel drp NULL
classification CHARACTER
setanimationscale 1.0
beginmodelgeom drp
  node dummy drp
    parent NULL
  endnode
  node dummy fine
    parent drp
  endnode
  node dummy orphan
    parent nosuchnode
  endnode
  node dummy other_root
    parent NULL
  endnode
  node dummy under_orphan
    parent orphan
  endnode
endmodelgeom
newanim walk drp
  length 1.0
  transtime 0.25
  animroot drp
  node dummy drp
    parent NULL
  endnode
  node dummy fine
    parent drp
  endnode
  node dummy anim_orphan
    parent nosuchnode
  endnode
doneanim walk drp
donemodel drp
`

func compileWarnings(t *testing.T, src string) ([]string, []byte) {
	t.Helper()
	m := mustParseASCII(t, src)
	var warns []string
	var buf bytes.Buffer
	if err := CompileWithOptions(m, &buf, CompileOptions{Warn: func(s string) { warns = append(warns, s) }}); err != nil {
		t.Fatal(err)
	}
	return warns, buf.Bytes()
}

// TestDroppedNodesAreReported: a node whose parent does not exist, a second
// root, and anything below them never reaches the binary. That used to be
// silent. The compile still succeeds and the output is unchanged, but each
// kind is now named with its reason.
func TestDroppedNodesAreReported(t *testing.T) {
	warns, _ := compileWarnings(t, droppedSrc)
	all := strings.Join(warns, "\n")
	for _, want := range []string{
		`orphan (parent "nosuchnode" does not exist)`,
		"other_root (a second root)",
		`under_orphan (parent "orphan" is itself not connected)`,
		`animation "walk"`,
		"anim_orphan",
	} {
		if !strings.Contains(all, want) {
			t.Errorf("missing %q in warnings:\n%s", want, all)
		}
	}
	if strings.Contains(all, "fine") {
		t.Errorf("a connected node was reported:\n%s", all)
	}
}

// TestConnectedModelWarnsNothing guards against noise: across the corpus only
// 6 of 49,213 standalone models warn.
func TestConnectedModelWarnsNothing(t *testing.T) {
	clean := strings.NewReplacer("  node dummy orphan\n    parent nosuchnode\n  endnode\n", "",
		"  node dummy other_root\n    parent NULL\n  endnode\n", "",
		"  node dummy under_orphan\n    parent orphan\n  endnode\n", "",
		"  node dummy anim_orphan\n    parent nosuchnode\n  endnode\n", "").Replace(droppedSrc)
	if warns, _ := compileWarnings(t, clean); len(warns) != 0 {
		t.Errorf("unexpected warnings on a connected model: %v", warns)
	}
}
