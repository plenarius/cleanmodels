package main

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"testing"
)

// The --json array must list files in input order, whatever order the workers
// finish in; otherwise two runs over the same directory print different output.
func TestBatchJSONFollowsInputOrder(t *testing.T) {
	src, err := os.ReadFile("../../tests/fixtures/oracle/ascii/plc_dummy_only.mdl")
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	var want []string
	for i := 0; i < 40; i++ {
		name := fmt.Sprintf("m%02d.mdl", i)
		if err := os.WriteFile(filepath.Join(dir, name), src, 0o644); err != nil {
			t.Fatal(err)
		}
		want = append(want, filepath.Join(dir, name))
	}
	sort.Strings(want)

	for run := 0; run < 10; run++ {
		opts := procOpts{check: true, jsonOut: true, workers: 8, colorMode: "never"}
		stdout, _ := captureStdio(t, func() {
			tw := newTermWriter(os.Stdout, false)
			errTw := newTermWriter(os.Stderr, false)
			runBatch(dir, "", opts, tw, errTw)
		})
		var got []Result
		if err := json.Unmarshal([]byte(stdout), &got); err != nil {
			t.Fatalf("run %d: stdout is not a JSON array: %v\n%s", run, err, stdout)
		}
		if len(got) != len(want) {
			t.Fatalf("run %d: %d results, want %d", run, len(got), len(want))
		}
		for i := range got {
			if got[i].File != want[i] {
				t.Fatalf("run %d: result %d is %s, want %s (not in input order)", run, i, got[i].File, want[i])
			}
		}
	}
}
