package main

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"testing"
)

// The [n/total] counter on each plain progress line must count up in the order
// the lines appear, whichever order the workers finish in. It used to carry the
// number a file was given when it started, so lines read [4/40] [7/40] [6/40].
func TestBatchProgressCounterCountsUp(t *testing.T) {
	src, err := os.ReadFile("../../tests/fixtures/oracle/ascii/plc_dummy_only.mdl")
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	const n = 40
	for i := 0; i < n; i++ {
		if err := os.WriteFile(filepath.Join(dir, fmt.Sprintf("m%02d.mdl", i)), src, 0o644); err != nil {
			t.Fatal(err)
		}
	}
	line := regexp.MustCompile(`\[\s*(\d+)/(\d+)\]`)
	for run := 0; run < 10; run++ {
		opts := procOpts{check: true, workers: 8, colorMode: "never"}
		stdout, _ := captureStdio(t, func() {
			runBatch([]string{dir}, "", opts, newTermWriter(os.Stdout, false), newTermWriter(os.Stderr, false))
		})
		want := 1
		for _, m := range line.FindAllStringSubmatch(stdout, -1) {
			got, _ := strconv.Atoi(m[1])
			if got != want {
				t.Fatalf("run %d: progress line %d is numbered %d\n%s", run, want, got, stdout)
			}
			if m[2] != strconv.Itoa(n) {
				t.Fatalf("run %d: total is %s, want %d", run, m[2], n)
			}
			want++
		}
		if want != n+1 {
			t.Fatalf("run %d: %d progress lines, want %d\n%s", run, want-1, n, stdout)
		}
	}
}
