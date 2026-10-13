package main

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// A damaged list entry (the real case: a tvert of "0/934729") must reach the
// JSON result, not only the text output, so the GUI and scripts can show it.
func TestParseWarningsReachTheResult(t *testing.T) {
	src := `newmodel pw
setsupermodel pw NULL
classification PLACEABLE
setanimationscale 1.00
beginmodelgeom pw
  node dummy pw
    parent NULL
  endnode
  node trimesh m
    parent pw
    bitmap NULL
    render 1
    verts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2 1 0 1 2 0
    tverts 3
      0 0 0
      0.0809014 0/934729 0
      1 1 0
  endnode
endmodelgeom
donemodel pw
`
	path := filepath.Join(t.TempDir(), "pw.mdl")
	if err := os.WriteFile(path, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	res, _, parseErrs, err := processOne(path, "", procOpts{dryRun: true})
	if err != nil {
		t.Fatal(err)
	}
	if len(parseErrs) != 1 || len(res.ParseWarnings) != 1 {
		t.Fatalf("parse errors = %v, result warnings = %v", parseErrs, res.ParseWarnings)
	}
	if !strings.Contains(res.ParseWarnings[0], `"0/934729" is not a usable number`) || !strings.HasPrefix(res.ParseWarnings[0], "line ") {
		t.Errorf("warning = %q", res.ParseWarnings[0])
	}
	data, err := json.Marshal(res)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(data), `"parse_warnings":["line `) {
		t.Errorf("JSON has no parse_warnings: %s", data)
	}
}

// A clean file adds nothing to the JSON.
func TestParseWarningsOmittedWhenClean(t *testing.T) {
	data, _ := json.Marshal(Result{File: "x.mdl"})
	if strings.Contains(string(data), "parse_warnings") {
		t.Errorf("clean result should omit the field: %s", data)
	}
}
