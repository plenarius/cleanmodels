package main

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// A trimesh with no faces and no bitmap, drawn (render 1): faceless_trimesh
// and null_bitmaps both apply to it. faceless_trimesh runs first, and its fix
// (render 0) hides the null_bitmaps finding.
const facelessNoBitmap = `newmodel box
setsupermodel box NULL
classification PLACEABLE
beginmodelgeom box
  node dummy box
    parent NULL
  endnode
  node trimesh plane
    parent box
    render 1
    shadow 0
    verts 1
      0 0 0
  endnode
endmodelgeom box
donemodel box
`

// checkJSONLines runs the given subcommand with --json-lines and returns the
// checks of the one model's done event.
func checkJSONLines(t *testing.T, run func([]string) int, args ...string) []map[string]any {
	t.Helper()
	stdout, stderr := captureStdio(t, func() { run(append([]string{"--json-lines", "--color", "never"}, args...)) })
	for _, line := range strings.Split(stdout, "\n") {
		var evt struct {
			Type   string `json:"type"`
			Result struct {
				Checks []map[string]any `json:"checks"`
			} `json:"result"`
		}
		if json.Unmarshal([]byte(line), &evt) == nil && evt.Type == "done" {
			return evt.Result.Checks
		}
	}
	t.Fatalf("no done event\nstdout:\n%s\nstderr:\n%s", stdout, stderr)
	return nil
}

func byCheck(checks []map[string]any) map[string]map[string]any {
	m := map[string]map[string]any{}
	for _, c := range checks {
		m[c["check"].(string)] = c
	}
	return m
}

// Plain check reports on the model as written: no fix is applied in memory,
// so a later rule still sees what an earlier rule's fix would have changed,
// and no finding says it fixed anything.
func TestCheckWithoutFixDoesNotChangeTheModel(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "box.mdl")
	if err := os.WriteFile(path, []byte(facelessNoBitmap), 0o644); err != nil {
		t.Fatal(err)
	}
	include := "--include-checks=faceless_trimesh,null_bitmaps"

	got := byCheck(checkJSONLines(t, cmdCheck, include, path))
	for _, name := range []string{"faceless_trimesh", "null_bitmaps"} {
		c, ok := got[name]
		if !ok {
			t.Errorf("check: no %s finding (hidden by an earlier rule's fix?): %v", name, got)
			continue
		}
		if c["fixed"] != false {
			t.Errorf("check: %s fixed = %v, want false", name, c["fixed"])
		}
		if msg := c["message"].(string); !strings.Contains(msg, "would") {
			t.Errorf("check: %s message should say what the fix would do: %q", name, msg)
		}
	}

	// check --fix and repair --check still fix, as before: the first rule's
	// fix is applied and reported as made.
	for _, tc := range []struct {
		name string
		run  func([]string) int
		args []string
	}{
		{"check --fix", cmdCheck, []string{"--fix", include, path, filepath.Join(dir, "fixed.mdl")}},
		{"repair --check", cmdRepair, []string{"--check", include, path, filepath.Join(dir, "repaired.mdl")}},
	} {
		got := byCheck(checkJSONLines(t, tc.run, tc.args...))
		c, ok := got["faceless_trimesh"]
		if !ok || c["fixed"] != true || strings.Contains(c["message"].(string), "would") {
			t.Errorf("%s: faceless_trimesh = %v", tc.name, c)
		}
		if _, ok := got["null_bitmaps"]; ok {
			t.Errorf("%s: null_bitmaps still reported after render was set to 0: %v", tc.name, got)
		}
	}
}
