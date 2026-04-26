package main

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"github.com/plenarius/cleanmodels/pkg/mdl"
)

// ANSI escape sequences for terminal color output.
const (
	ansiReset   = "\033[0m"
	ansiBold    = "\033[1m"
	ansiDim     = "\033[2m"
	ansiRed     = "\033[31m"
	ansiYellow  = "\033[33m"
	ansiGreen   = "\033[32m"
)

// termWriter wraps an io.Writer with optional ANSI color support.
//
// cols is the terminal width when stdout is a real TTY, or 0 when
// piped/unknown. formatBatchLine uses it to keep diagnostic lines on
// a single visual line on narrow terminals.
type termWriter struct {
	w     io.Writer
	color bool
	cols  int
}

func newTermWriter(w io.Writer, color bool) *termWriter {
	return &termWriter{w: w, color: color}
}

func (tw *termWriter) Write(p []byte) (int, error) { return tw.w.Write(p) }

func (tw *termWriter) styled(style, s string) string {
	if !tw.color {
		return s
	}
	return style + s + ansiReset
}

func (tw *termWriter) bold(s string) string    { return tw.styled(ansiBold, s) }
func (tw *termWriter) dim(s string) string     { return tw.styled(ansiDim, s) }
func (tw *termWriter) red(s string) string     { return tw.styled(ansiRed, s) }
func (tw *termWriter) yellow(s string) string  { return tw.styled(ansiYellow, s) }
func (tw *termWriter) green(s string) string   { return tw.styled(ansiGreen, s) }

// severityTag returns a color-coded severity label.
func (tw *termWriter) severityTag(s mdl.Severity) string {
	tag := severityString(s)
	switch s {
	case mdl.SevError, mdl.SevFatal:
		return tw.red(tag)
	case mdl.SevWarning:
		return tw.yellow(tag)
	case mdl.SevInfo:
		return tw.dim(tag)
	default:
		return tag
	}
}

// printSingleResult formats output for a single file in human-readable mode.
func (tw *termWriter) printSingleResult(path string, res Result, model *mdl.Model, parseErrs []mdl.ParseError, verbose, quiet bool) {
	if quiet {
		return
	}

	baseName := filepath.Base(path)
	fixes := countFixes(res)

	if res.Error != "" {
		fmt.Fprintf(tw.w, "%s %s\n", tw.bold(baseName), tw.red("ERROR: "+res.Error))
		return
	}

	if fixes == 0 && len(res.Actions) == 0 && len(res.Warnings) == 0 && len(parseErrs) == 0 && countCheckErrors(res.Checks) == 0 {
		if verbose {
			fmt.Fprintf(tw.w, "%s %s\n", tw.bold(baseName), tw.green("clean"))
		}
		return
	}

	// Header line
	parts := []string{}
	if fixes > 0 {
		parts = append(parts, tw.green(fmt.Sprintf("%d %s", fixes, pluralize(fixes, "repair", "repairs"))))
	}
	warnCount := len(res.Warnings) + len(parseErrs) + countCheckBySeverity(res.Checks, mdl.SevWarning)
	if warnCount > 0 {
		parts = append(parts, tw.yellow(fmt.Sprintf("%d %s", warnCount, pluralize(warnCount, "warning", "warnings"))))
	}
	errCount := countCheckErrors(res.Checks)
	if errCount > 0 {
		parts = append(parts, tw.red(fmt.Sprintf("%d %s", errCount, pluralize(errCount, "error", "errors"))))
	}
	if len(parts) == 0 && len(res.Actions) > 0 {
		parts = append(parts, tw.green("ok"))
	}
	fmt.Fprintf(tw.w, "%s  %s\n", tw.bold(baseName), strings.Join(parts, ", "))

	// Detail lines
	for _, a := range res.Actions {
		fmt.Fprintf(tw.w, "  %s %s\n", tw.dim("[ACTION]"), a)
	}
	for _, r := range res.Repairs {
		fmt.Fprintf(tw.w, "  %s %s\n", tw.green("[REPAIR]"), r)
	}
	for _, wn := range res.Warnings {
		fmt.Fprintf(tw.w, "  %s decompile [%s] off=%d: %s\n", tw.yellow("[WARN]"), wn.Node, wn.Offset, wn.Message)
	}
	for _, pe := range parseErrs {
		fmt.Fprintf(tw.w, "  %s parse: %v\n", tw.yellow("[WARN]"), pe)
	}
	for _, r := range res.Checks {
		if !verbose && r.Severity == mdl.SevInfo {
			continue
		}
		node := r.Node
		if node == "" {
			node = "-"
		}
		fixTag := ""
		if r.Fixed {
			fixTag = tw.green(" [FIXED]")
		}
		fmt.Fprintf(tw.w, "  [%s] %s (%s): %s%s\n", tw.severityTag(r.Severity), r.Check, node, r.Message, fixTag)
	}

	if verbose && model != nil {
		tw.printModelStats(model)
	}
}

// printBatchLine formats a single line of batch progress output.
func (tw *termWriter) printBatchLine(idx, total int, baseName string, res Result, parseErrs []mdl.ParseError) {
	fmt.Fprintln(tw.w, tw.formatBatchLine(idx, total, baseName, res))
}

// printBatchSummary formats the final summary line for batch processing.
func (tw *termWriter) printBatchSummary(totalFiles, totalRepairs, totalWarnings, totalErrors int) {
	parts := []string{fmt.Sprintf("%d %s", totalFiles, pluralize(totalFiles, "file", "files"))}
	if totalRepairs > 0 {
		parts = append(parts, tw.green(fmt.Sprintf("%d %s", totalRepairs, pluralize(totalRepairs, "repair", "repairs"))))
	}
	if totalWarnings > 0 {
		parts = append(parts, tw.yellow(fmt.Sprintf("%d %s", totalWarnings, pluralize(totalWarnings, "warning", "warnings"))))
	}
	if totalErrors > 0 {
		parts = append(parts, tw.red(fmt.Sprintf("%d %s", totalErrors, pluralize(totalErrors, "error", "errors"))))
	}
	fmt.Fprintf(tw.w, "\n%s\n", strings.Join(parts, ", "))
}

func pluralize(n int, singular, plural string) string {
	if n == 1 {
		return singular
	}
	return plural
}

// ANSI cursor control for in-place terminal rewriting.
const (
	ansiClearLine = "\033[2K"
	ansiCursorUp  = "\033[%dA"
	ansiHideCursor = "\033[?25l"
	ansiShowCursor = "\033[?25h"
)

// liveProgress manages in-place terminal output for batch processing.
// Lines are kept sorted by file index, and a summary counter updates live.
type liveProgress struct {
	tw         *termWriter
	total      int
	lines      []string // rendered line per file slot (0-indexed)
	done       int
	repairs    int
	warnings   int
	errors     int
	linesDrawn int // how many lines we last wrote to terminal
}

func newLiveProgress(tw *termWriter, total int) *liveProgress {
	lp := &liveProgress{
		tw:    tw,
		total: total,
		lines: make([]string, total),
	}
	width := len(fmt.Sprintf("%d", total))
	for i := 0; i < total; i++ {
		lp.lines[i] = fmt.Sprintf("[%*d/%d] %s", width, i+1, total, tw.dim("..."))
	}
	return lp
}

func (lp *liveProgress) update(fileIdx int, baseName string, res Result, parseErrs []mdl.ParseError) {
	lp.lines[fileIdx] = lp.tw.formatBatchLine(fileIdx+1, lp.total, baseName, res)
	fixes := countFixes(res)
	w, e := tally(res, len(parseErrs))
	lp.done++
	lp.repairs += fixes
	lp.warnings += w
	lp.errors += e
}

func (lp *liveProgress) render() {
	if lp.linesDrawn > 0 {
		fmt.Fprintf(lp.tw.w, ansiCursorUp, lp.linesDrawn)
	}

	// Determine visible window: show up to 20 lines around the action
	maxVisible := 20
	if lp.total <= maxVisible {
		for _, line := range lp.lines {
			fmt.Fprintf(lp.tw.w, "%s%s\n", ansiClearLine, line)
		}
		lp.linesDrawn = lp.total + 1
	} else {
		// Show first few, last completed, and summary
		shown := 0
		for i, line := range lp.lines {
			if i < maxVisible-1 {
				fmt.Fprintf(lp.tw.w, "%s%s\n", ansiClearLine, line)
				shown++
			} else if i == maxVisible-1 {
				remaining := lp.total - maxVisible
				fmt.Fprintf(lp.tw.w, "%s  %s\n", ansiClearLine,
					lp.tw.dim(fmt.Sprintf("... and %d more", remaining)))
				shown++
				break
			}
		}
		lp.linesDrawn = shown + 1
	}

	// Summary line
	pct := 0
	if lp.total > 0 {
		pct = lp.done * 100 / lp.total
	}
	summary := fmt.Sprintf("%s%d/%d (%d%%)", ansiClearLine, lp.done, lp.total, pct)
	if lp.repairs > 0 {
		summary += "  " + lp.tw.green(fmt.Sprintf("%d %s", lp.repairs, pluralize(lp.repairs, "repair", "repairs")))
	}
	if lp.errors > 0 {
		summary += "  " + lp.tw.red(fmt.Sprintf("%d %s", lp.errors, pluralize(lp.errors, "error", "errors")))
	}
	fmt.Fprintf(lp.tw.w, "%s\n", summary)
}

func (lp *liveProgress) finish() {
	// Final render with all lines settled
	lp.render()
}

// formatBatchLine returns a formatted batch line string (no trailing newline).
//
// When the term writer knows the terminal width (cols > 0), the dot leader
// shrinks so the assembled line fits within cols-1 columns and avoids the
// trailing wrap that doubles every progress line on narrow terminals. When
// cols is 0 (piped, unknown), the historical 50-char filename region is
// preserved.
func (tw *termWriter) formatBatchLine(idx, total int, baseName string, res Result) string {
	width := len(fmt.Sprintf("%d", total))
	prefix := fmt.Sprintf("[%*d/%d]", width, idx, total)

	fixes := countFixes(res)

	var statusPlain string
	var statusColored string
	switch {
	case res.Error != "":
		statusPlain = "ERROR: " + truncate(res.Error, 40)
		statusColored = tw.red(statusPlain)
	case countCheckErrors(res.Checks) > 0:
		errCount := countCheckErrors(res.Checks)
		statusPlain = fmt.Sprintf("%d %s", errCount, pluralize(errCount, "error", "errors"))
		statusColored = tw.red(statusPlain)
	case fixes > 0:
		statusPlain = fmt.Sprintf("%d %s", fixes, pluralize(fixes, "repair", "repairs"))
		statusColored = tw.green(statusPlain)
	case len(res.Actions) > 0:
		statusPlain = "ok"
		statusColored = tw.green(statusPlain)
	default:
		statusPlain = "clean"
		statusColored = tw.dim(statusPlain)
	}

	nameLen := len(baseName)
	// Default historical region: 50 chars between prefix and status.
	leaderLen := 50 - nameLen
	if leaderLen < 3 {
		leaderLen = 3
	}
	if tw.cols > 0 {
		// Visible budget: prefix + " " + baseName + " " + dots + " " + status.
		// Reserve one trailing column to avoid edge-of-window wrap glyphs.
		fixed := len(prefix) + 1 + nameLen + 2 + len(statusPlain) + 1
		if budget := tw.cols - fixed; budget < leaderLen {
			leaderLen = budget
		}
		if leaderLen < 3 {
			leaderLen = 3
		}
	}
	leader := " " + strings.Repeat(".", leaderLen) + " "

	return fmt.Sprintf("%s %s%s%s", prefix, baseName, tw.dim(leader), statusColored)
}

func truncate(s string, max int) string {
	if len(s) <= max {
		return s
	}
	return s[:max-3] + "..."
}

// shouldColorize returns true if output should use ANSI color codes.
// mode: "always" forces on, "never" forces off, "auto" (or empty)
// honors NO_COLOR (https://no-color.org/) and FORCE_COLOR env vars,
// then falls back to TTY detection on the supplied descriptor.
// Explicit mode beats env vars; env vars beat TTY detection.
func shouldColorize(fd uintptr, mode string) bool {
	switch mode {
	case "always":
		return true
	case "never":
		return false
	}
	if os.Getenv("NO_COLOR") != "" {
		return false
	}
	if os.Getenv("FORCE_COLOR") != "" {
		return true
	}
	return isTerminal(int(fd))
}

// printModelStats prints a compact structural overview of a model.
func (tw *termWriter) printModelStats(m *mdl.Model) {
	if m == nil {
		return
	}

	var meshCount, faceTotal, vertTotal, lightCount, emitterCount, dummyCount, skinCount int
	maxDepth := 0
	depthOf := map[string]int{}

	for _, n := range m.Nodes {
		if n == nil {
			continue
		}
		d := 0
		if pd, ok := depthOf[strings.ToLower(n.Parent)]; ok {
			d = pd + 1
		}
		depthOf[strings.ToLower(n.Name)] = d
		if d > maxDepth {
			maxDepth = d
		}

		switch {
		case n.Skin != nil:
			skinCount++
			meshCount++
		case n.Mesh != nil:
			meshCount++
		case n.Light != nil:
			lightCount++
		case n.Emitter != nil:
			emitterCount++
		default:
			dummyCount++
		}
		if n.Mesh != nil {
			faceTotal += len(n.Mesh.Faces)
			vertTotal += len(n.Mesh.Verts)
		}
	}

	animTotal := len(m.Animations)
	var maxAnimLen float32
	for _, a := range m.Animations {
		if a.Length > maxAnimLen {
			maxAnimLen = a.Length
		}
	}

	fmt.Fprintf(tw.w, "\n")
	fmt.Fprintf(tw.w, "  %s  %s", tw.bold(m.Name), tw.dim(m.Classification))
	if m.SuperModel != "" && !strings.EqualFold(m.SuperModel, "NULL") {
		fmt.Fprintf(tw.w, "  super:%s", m.SuperModel)
	}
	fmt.Fprintf(tw.w, "\n")

	// Node breakdown
	nodeParts := []string{fmt.Sprintf("%d nodes", len(m.Nodes))}
	if meshCount > 0 {
		nodeParts = append(nodeParts, fmt.Sprintf("%d mesh", meshCount))
	}
	if skinCount > 0 {
		nodeParts = append(nodeParts, fmt.Sprintf("%d skin", skinCount))
	}
	if lightCount > 0 {
		nodeParts = append(nodeParts, fmt.Sprintf("%d light", lightCount))
	}
	if emitterCount > 0 {
		nodeParts = append(nodeParts, fmt.Sprintf("%d emitter", emitterCount))
	}
	fmt.Fprintf(tw.w, "  %s  depth %d\n", strings.Join(nodeParts, ", "), maxDepth)

	// Geometry
	if faceTotal > 0 {
		fmt.Fprintf(tw.w, "  %s faces, %s verts\n", formatCount(faceTotal), formatCount(vertTotal))
	}

	// Animations
	if animTotal > 0 {
		fmt.Fprintf(tw.w, "  %d %s", animTotal, pluralize(animTotal, "animation", "animations"))
		if maxAnimLen > 0 {
			fmt.Fprintf(tw.w, "  longest %.1fs", maxAnimLen)
		}
		fmt.Fprintf(tw.w, "\n")
	}
}

func formatCount(n int) string {
	if n >= 1000000 {
		return fmt.Sprintf("%.1fM", float64(n)/1000000)
	}
	if n >= 1000 {
		return fmt.Sprintf("%.1fk", float64(n)/1000)
	}
	return fmt.Sprintf("%d", n)
}

