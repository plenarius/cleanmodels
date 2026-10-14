package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// A walkmesh with a face that names a vertex that is not there: the rebuild cannot
// work from it.
const brokenWalkmesh = `newmodel brokenwok
setsupermodel brokenwok NULL
classification TILE
setanimationscale 1.00
beginmodelgeom brokenwok
  node dummy brokenwok
    parent NULL
  endnode
  node aabb wok
    parent brokenwok
    verts 3
      0 0 0
      1 0 0
      0 1 0
    faces 2
      0 1 2 1 0 1 2 4
      0 1 9 1 0 1 2 4
    aabb 3
      0 0 0 1 1 0 -1
      0 0 0 1 1 0 0
      0 0 0 1 1 0 1
  endnode
endmodelgeom
donemodel brokenwok
`

// A repair that fails is an error: non-zero exit, the message in the result, and not
// listed among the repairs (it used to be, and the command exited 0).
func TestFailedAabbRebuildIsAnErrorNotARepair(t *testing.T) {
	dir := t.TempDir()
	in, out := filepath.Join(dir, "in.mdl"), filepath.Join(dir, "out.mdl")
	if err := os.WriteFile(in, []byte(brokenWalkmesh), 0o644); err != nil {
		t.Fatal(err)
	}
	var code int
	stdout, _ := captureStdio(t, func() {
		code = cmdRepair([]string{"--fix-aabb", "--color", "never", in, out})
	})
	if code != exitErrors {
		t.Errorf("exit %d, want %d (stdout: %s)", code, exitErrors, stdout)
	}
	if !strings.Contains(stdout, "aabb rebuild wok") || strings.Contains(stdout, "[REPAIR] aabb rebuild") {
		t.Errorf("failure not reported as an error, or still listed as a repair:\n%s", stdout)
	}
	if _, err := os.Stat(out); err != nil {
		t.Errorf("the rest of the repair should still be written: %v", err)
	}

	// JSON carries it in "error".
	jsonOut, _ := captureStdio(t, func() {
		code = cmdRepair([]string{"--fix-aabb", "--json", "--overwrite", in, out})
	})
	if code != exitErrors || !strings.Contains(jsonOut, `"error": "aabb rebuild wok`) {
		t.Errorf("--json: exit %d, output:\n%s", code, jsonOut)
	}
}
