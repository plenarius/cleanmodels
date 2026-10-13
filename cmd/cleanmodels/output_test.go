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

func singleResultOutput(res Result, parseErrs []mdl.ParseError, verbose bool) string {
	var buf bytes.Buffer
	tw := &termWriter{w: &buf, color: false, cols: 0}
	tw.printSingleResult("/tmp/f.mdl", res, nil, parseErrs, verbose, false)
	return buf.String()
}

// A single file whose only findings are check warnings (the most common kind)
// used to print nothing, or "clean" under -v, while the summary line counted
// the warnings. The batch line had the same bug and was fixed earlier; the
// single-file path was missed.
func TestPrintSingleResult_WarningsOnlyShowsDetails(t *testing.T) {
	res := Result{Checks: []mdl.CheckResult{
		{Check: "degenerate_faces", Node: "Mesh1", Severity: mdl.SevWarning, Message: "face 0 has repeated vertex indices"},
	}}
	for _, verbose := range []bool{false, true} {
		out := stripANSI(singleResultOutput(res, nil, verbose))
		if strings.Contains(out, "clean") {
			t.Errorf("verbose=%v: warnings-only file rendered as clean: %q", verbose, out)
		}
		if !strings.Contains(out, "1 warning") || !strings.Contains(out, "degenerate_faces (Mesh1)") || !strings.Contains(out, "face 0 has repeated vertex indices") {
			t.Errorf("verbose=%v: header and detail expected, got %q", verbose, out)
		}
	}
}

// A file with nothing to report stays quiet, and says "clean" only under -v.
func TestPrintSingleResult_CleanStaysQuiet(t *testing.T) {
	if out := singleResultOutput(Result{}, nil, false); out != "" {
		t.Errorf("clean, not verbose: want no output, got %q", out)
	}
	if out := stripANSI(singleResultOutput(Result{}, nil, true)); !strings.Contains(out, "clean") {
		t.Errorf("clean, verbose: want \"clean\", got %q", out)
	}
	// Info-level findings are filtered unless verbose, so they do not make a file noisy.
	info := Result{Checks: []mdl.CheckResult{{Check: "x", Severity: mdl.SevInfo, Message: "note"}}}
	if out := singleResultOutput(info, nil, false); out != "" {
		t.Errorf("info only, not verbose: want no output, got %q", out)
	}
}

// Check errors and parse warnings still show, as before.
func TestPrintSingleResult_ErrorsAndParseWarningsStillShow(t *testing.T) {
	errRes := Result{Checks: []mdl.CheckResult{{Check: "missing_parents", Node: "n", Severity: mdl.SevError, Message: "boom"}}}
	if out := stripANSI(singleResultOutput(errRes, nil, false)); !strings.Contains(out, "1 error") || !strings.Contains(out, "boom") {
		t.Errorf("error not shown: %q", out)
	}
	pe := []mdl.ParseError{{Line: 352, Message: `"0/934729" is not a usable number, set to 0`}}
	if out := stripANSI(singleResultOutput(Result{}, pe, false)); !strings.Contains(out, "line 352") {
		t.Errorf("parse warning not shown: %q", out)
	}
}

// The check messages start with "<file>: ", and the text output already names
// the file in the line above them, so the prefix is dropped when printing.
func TestWithoutFilePrefix(t *testing.T) {
	cases := []struct{ msg, file, want string }{
		{"x.mdl: node \"a\" has no faces", "x.mdl", "node \"a\" has no faces"},
		{"in/x.mdl: node \"a\" has no faces", "in/x.mdl", "node \"a\" has no faces"},
		{"x.mdl: node \"a\" has no faces", "in/x.mdl", "node \"a\" has no faces"},             // message carries the base name only
		{"other.mdl: node \"a\" has no faces", "x.mdl", "other.mdl: node \"a\" has no faces"}, // someone else's name stays
		{"node \"a\" has no faces", "x.mdl", "node \"a\" has no faces"},
		{"x.mdl:but no space", "x.mdl", "x.mdl:but no space"},
		{"x.mdl: msg", "", "x.mdl: msg"},
	}
	for _, c := range cases {
		if got := withoutFilePrefix(c.msg, c.file); got != c.want {
			t.Errorf("withoutFilePrefix(%q, %q) = %q, want %q", c.msg, c.file, got, c.want)
		}
	}
}

func TestResultDetailDoesNotRepeatTheFileName(t *testing.T) {
	res := Result{File: "tiles/x.mdl", Checks: []mdl.CheckResult{
		{Check: "aabb_fields", Node: "wok", Severity: mdl.SevWarning, Message: "tiles/x.mdl: AABB node \"wok\" has empty AABB tree", Fixed: true},
	}}
	var buf bytes.Buffer
	newTermWriter(&buf, false).printResultDetail(res, nil, false)
	got := buf.String()
	if strings.Contains(got, "x.mdl") {
		t.Errorf("file name repeated in %q", got)
	}
	if !strings.Contains(got, "aabb_fields (wok): AABB node \"wok\" has empty AABB tree") {
		t.Errorf("message lost: %q", got)
	}
}
