package main

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

// Three faces on one edge, shadow on by default.
const multiEdgeFan = `newmodel fan
setsupermodel fan NULL
classification PLACEABLE
setanimationscale 1.0
beginmodelgeom fan
  node dummy fan
    parent NULL
  endnode
  node trimesh trunk
    parent fan
    bitmap blank
    verts 5
      0 0 0
      1 0 0
      0.5 1 0
      0.5 -1 0
      0.5 0 1
    tverts 1
      0 0 0
    faces 3
      0 1 2  1  0 0 0  0
      1 0 3  1  0 0 0  0
      0 1 4  1  0 0 0  0
  endnode
endmodelgeom
donemodel fan
`

func TestRepairOpenMultiEdges(t *testing.T) {
	dir := t.TempDir()
	ascii := filepath.Join(dir, "fan_ascii.mdl")
	bin := filepath.Join(dir, "fan.mdl")
	out := filepath.Join(dir, "out.mdl")
	if err := os.WriteFile(ascii, []byte(multiEdgeFan), 0o644); err != nil {
		t.Fatal(err)
	}
	pr, err := mdl.ParseFile(ascii)
	if err != nil {
		t.Fatal(err)
	}
	if err := mdl.CompileFile(pr.Model, bin); err != nil {
		t.Fatal(err)
	}

	var code int
	stdout, _ := captureStdio(t, func() {
		code = cmdRepair([]string{"--open-multiedges", "--color", "never", bin, out})
	})
	if code != exitOK {
		t.Fatalf("exit %d:\n%s", code, stdout)
	}
	if !strings.Contains(stdout, `opened 1 multiple edge(s) on shadow node "trunk"`) {
		t.Errorf("repair not reported:\n%s", stdout)
	}
	before, _ := os.ReadFile(bin)
	after, err := os.ReadFile(out)
	if err != nil {
		t.Fatal(err)
	}
	if len(after) != len(before) || bytes.Equal(after, before) {
		t.Fatalf("want the same file with a few bytes changed: %d -> %d bytes", len(before), len(after))
	}
	m, err := mdl.DecompileFile(out)
	if err != nil {
		t.Fatal(err)
	}
	if _, edges, _ := mdl.StoredMultiEdges(m.FindNode("trunk").Mesh); edges != 0 {
		t.Errorf("%d multiple edges left", edges)
	}

	// An ASCII model has no stored neighbours to open.
	stdout, _ = captureStdio(t, func() {
		code = cmdRepair([]string{"--open-multiedges", "--color", "never", ascii, filepath.Join(dir, "x.mdl")})
	})
	if code == exitOK || !strings.Contains(stdout, "--split-multiedge") {
		t.Errorf("ASCII input: exit %d, want a refusal naming --split-multiedge:\n%s", code, stdout)
	}

	// It patches the file as it is, so it does not combine with repairs that rewrite it.
	_, stderr := captureStdio(t, func() {
		code = cmdRepair([]string{"--open-multiedges", "--all", bin, filepath.Join(dir, "y.mdl")})
	})
	if code != exitUsage || !strings.Contains(stderr, "can't be combined") {
		t.Errorf("with --all: exit %d, stderr %q", code, stderr)
	}
}
