package main

import (
	"bytes"
	"strings"
	"testing"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

// stripANSI removes ANSI CSI escape sequences from s for visible-length checks.
func stripANSI(s string) string {
	var out bytes.Buffer
	i := 0
	for i < len(s) {
		if s[i] == '\x1b' && i+1 < len(s) && s[i+1] == '[' {
			j := i + 2
			for j < len(s) {
				c := s[j]
				j++
				if c >= 0x40 && c <= 0x7e {
					break
				}
			}
			i = j
			continue
		}
		out.WriteByte(s[i])
		i++
	}
	return out.String()
}

// leaderDots returns the number of dots in the contiguous dot run between
// the basename and the status (i.e. excludes any dots inside the basename
// or the status itself).
func leaderDots(line, baseName string) int {
	i := strings.Index(line, baseName)
	if i < 0 {
		return 0
	}
	rest := line[i+len(baseName):]
	rest = strings.TrimLeft(rest, " ")
	n := 0
	for _, r := range rest {
		if r != '.' {
			break
		}
		n++
	}
	return n
}

func TestFormatBatchLine_PipedKeepsFiftyCharRegion(t *testing.T) {
	tw := &termWriter{w: nil, color: false, cols: 0}
	const name = "tile.mdl"
	line := tw.formatBatchLine(1, 1, name, Result{}, nil)
	visible := stripANSI(line)
	dots := leaderDots(visible, name)
	wantDots := 50 - len(name)
	if dots != wantDots {
		t.Fatalf("piped mode: got %d dots, want %d. line=%q", dots, wantDots, visible)
	}
}

func TestFormatBatchLine_NarrowTTYShrinksLeader(t *testing.T) {
	tw := &termWriter{w: nil, color: false, cols: 60}
	name := "very_long_tile_name_indeed.mdl"
	line := tw.formatBatchLine(1, 1, name, Result{}, nil)
	visible := stripANSI(line)
	if len(visible) > tw.cols {
		t.Fatalf("narrow tty: line len %d > cols %d. line=%q", len(visible), tw.cols, visible)
	}
}

func TestFormatBatchLine_NarrowTTYKeepsMinimumDots(t *testing.T) {
	tw := &termWriter{w: nil, color: false, cols: 30}
	const name = "huge_basename_that_eats_the_window.mdl"
	line := tw.formatBatchLine(1, 1, name, Result{}, nil)
	visible := stripANSI(line)
	dots := leaderDots(visible, name)
	if dots < 3 {
		t.Fatalf("narrow tty + huge name: got %d leader dots, want >= 3. line=%q", dots, visible)
	}
}

func TestFormatBatchLine_WideTTYDoesNotExceedHistoricalRegion(t *testing.T) {
	tw := &termWriter{w: nil, color: false, cols: 200}
	const name = "x.mdl"
	line := tw.formatBatchLine(1, 1, name, Result{}, nil)
	visible := stripANSI(line)
	dots := leaderDots(visible, name)
	wantDots := 50 - len(name)
	if dots != wantDots {
		t.Fatalf("wide tty: got %d leader dots, want %d (historical cap preserved). line=%q", dots, wantDots, visible)
	}
}

// A file with only warnings (no errors, repairs, or actions) used to fall
// through to "clean" because formatBatchLine had no warning branch — even
// though the batch summary correctly tallied them. Status now reads as
// "N warnings" so the per-line view matches the summary.
func TestFormatBatchLine_WarningsOnly_NotClean(t *testing.T) {
	tw := &termWriter{w: nil, color: false, cols: 0}
	res := Result{
		Checks: []mdl.CheckResult{
			{Check: "ex", Severity: mdl.SevWarning, Message: "x"},
			{Check: "ex", Severity: mdl.SevWarning, Message: "y"},
		},
	}
	line := stripANSI(tw.formatBatchLine(1, 1, "f.mdl", res, nil))
	if strings.Contains(line, "clean") {
		t.Fatalf("warnings-only file rendered as clean: %q", line)
	}
	if !strings.Contains(line, "2 warnings") {
		t.Fatalf("expected `2 warnings` in line, got %q", line)
	}
}

// Parse errors are tallied as warnings (not errors) by tally(); they must
// also flow through to the batch line so users see them on a single line.
func TestFormatBatchLine_ParseErrorsCountAsWarnings(t *testing.T) {
	tw := &termWriter{w: nil, color: false, cols: 0}
	parseErrs := []mdl.ParseError{{}, {}, {}}
	line := stripANSI(tw.formatBatchLine(1, 1, "f.mdl", Result{}, parseErrs))
	if !strings.Contains(line, "3 warnings") {
		t.Fatalf("expected `3 warnings`, got %q", line)
	}
}
