package main

import (
	"encoding/json"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"sync"
)

func collectMDLFiles(root string, recursive bool) ([]string, error) {
	if recursive {
		var out []string
		err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
			if err != nil {
				return err
			}
			if d.IsDir() {
				return nil
			}
			if strings.EqualFold(filepath.Ext(path), ".mdl") {
				out = append(out, path)
			}
			return nil
		})
		return out, err
	}
	var out []string
	entries, err := os.ReadDir(root)
	if err != nil {
		return nil, err
	}
	for _, e := range entries {
		if !e.IsDir() && strings.EqualFold(filepath.Ext(e.Name()), ".mdl") {
			out = append(out, filepath.Join(root, e.Name()))
		}
	}
	return out, nil
}

// batchFile is one model to process and the input it came from, which decides
// where it lands under the output directory: a directory input keeps the path
// below it, a file input goes in by its base name.
type batchFile struct{ path, root string }

// collectInputs expands the inputs (directories and files, in the order given)
// into the models to process. The same file named twice is processed once.
// With an output directory, two models that would land on the same output path
// are an error, since one would silently replace the other.
func collectInputs(inputs []string, outputDir string, recursive bool) ([]batchFile, error) {
	var out []batchFile
	seen := map[string]bool{}
	landing := map[string]string{}
	for _, in := range inputs {
		fi, err := os.Stat(in)
		if err != nil {
			return nil, wrapWithSuggestion(err, in)
		}
		paths, root := []string{in}, filepath.Dir(in)
		if fi.IsDir() {
			root = in
			if paths, err = collectMDLFiles(in, recursive); err != nil {
				return nil, err
			}
		}
		for _, p := range paths {
			abs, err := filepath.Abs(p)
			if err != nil {
				abs = p
			}
			if seen[abs] {
				continue
			}
			seen[abs] = true
			if outputDir != "" {
				dest := filepath.Join(outputDir, batchRelPath(root, p))
				if other, ok := landing[dest]; ok {
					return nil, fmt.Errorf("%q and %q would both be written to %q", other, p, dest)
				}
				landing[dest] = p
			}
			out = append(out, batchFile{path: p, root: root})
		}
	}
	return out, nil
}

func batchRelPath(root, filePath string) string {
	rel, err := filepath.Rel(root, filePath)
	if err != nil {
		return filepath.Base(filePath)
	}
	return rel
}

func batchOutputPath(root, outputDir, filePath string) (string, error) {
	if outputDir == "" {
		return "", nil
	}
	out := filepath.Join(outputDir, batchRelPath(root, filePath))
	if err := os.MkdirAll(filepath.Dir(out), 0755); err != nil {
		return "", fmt.Errorf("create output dir: %w", err)
	}
	return out, nil
}

// runBatch processes every MDL under root.
//
// Stream routing rule (single source of truth):
//
//   - Per-file diagnostic lines and the final human summary go to stdout in
//     plain mode (errTw is unused there) so a `> log.txt` redirect captures
//     the whole batch report.
//   - In --json mode the JSON array goes to stdout; the human summary moves
//     to stderr (via errTw) to keep stdout machine-clean.
//   - In --json-lines mode every event (including a "summary" event) is on
//     stdout; no human summary is emitted.
//   - Errors and the empty-batch hint always go to stderr regardless of mode.
func runBatch(inputs []string, outputDir string, o procOpts, tw, errTw *termWriter) int {
	items, err := collectInputs(inputs, outputDir, o.recursive)
	if err != nil {
		fmt.Fprintf(os.Stderr, "cleanmodels: %v\n", err)
		return exitErrors
	}

	if len(items) == 0 {
		if !o.quiet {
			if len(inputs) == 1 {
				fmt.Fprintf(os.Stderr, "cleanmodels: %s\n", suggestRecursive(inputs[0]))
			} else {
				fmt.Fprintf(os.Stderr, "cleanmodels: no .mdl files found in the given inputs\n")
			}
		}
		return exitUsage
	}
	files := make([]string, len(items))
	rootOf := make(map[string]string, len(items))
	for i, it := range items {
		files[i] = it.path
		rootOf[it.path] = it.root
	}

	total := len(files)

	var (
		mu           sync.Mutex
		jsonSlots    = make([]*Result, total) // --json results by input position, not completion order
		totalW       int
		totalE       int
		totalRepairs int
		procN        int // files started; numbers the JSON-lines start/done events
		doneN        int // files finished; numbers the plain progress lines so they count up in order
		exitCode     = exitOK
	)

	bo := o
	if !o.jsonLines {
		bo.jsonOut = true
	}

	// Use live progress (in-place rewriting) only when stdout is a real TTY,
	// not in machine-output modes, and only above a small-batch threshold —
	// for 2 or 3 files the streaming line-per-file form is calmer and avoids
	// flashing cursor moves for an operation that completes in <1s.
	// FORCE_COLOR alone is not enough — cursor control requires a real terminal.
	// In verbose mode we stream a per-file detail block under each batch
	// line. That's incompatible with the live-progress UI, which rewrites
	// lines in place and caps the visible viewport at ~20 lines — verbose
	// has to mean "show me everything", so we drop the in-place renderer
	// and fall through to streaming output instead.
	const liveProgressMinFiles = 4
	useLive := total >= liveProgressMinFiles &&
		isTerminal(int(os.Stdout.Fd())) &&
		!o.jsonOut && !o.jsonLines && !o.quiet && !o.verbose
	var lp *liveProgress
	if useLive {
		lp = newLiveProgress(tw, total)
		fmt.Fprintf(tw.w, "%s", ansiHideCursor)
		defer fmt.Fprintf(tw.w, "%s", ansiShowCursor)
		lp.render()
	}

	// Build a file-index map so workers can report by sorted position.
	fileIdx := make(map[string]int, total)
	for i, p := range files {
		fileIdx[p] = i
	}

	jobs := make(chan string, total)
	var wg sync.WaitGroup

	worker := func() {
		defer wg.Done()
		for path := range jobs {
			baseName := filepath.Base(path)
			idx := fileIdx[path]

			outPath, mkdirErr := batchOutputPath(rootOf[path], outputDir, path)
			if mkdirErr != nil {
				mu.Lock()
				totalE++
				procN++
				doneN++
				if o.jsonLines {
					emitEvent(Event{Type: "error", File: baseName, Message: mkdirErr.Error(), Index: procN, Total: total})
				} else if o.jsonOut {
					jsonSlots[idx] = &Result{File: path, Error: mkdirErr.Error()}
				} else if lp != nil {
					lp.update(idx, baseName, Result{Error: mkdirErr.Error()}, nil)
					lp.render()
				} else if !bo.quiet {
					tw.printBatchLine(doneN, total, baseName, Result{Error: mkdirErr.Error()}, nil)
				}
				mu.Unlock()
				continue
			}

			mu.Lock()
			procN++
			seqIdx := procN
			mu.Unlock()

			if o.jsonLines {
				emitEvent(Event{Type: "start", File: baseName, Index: seqIdx, Total: total})
			}

			res, _, parseErrs, err := processOne(path, outPath, bo)
			if err != nil && res.Error == "" {
				res.Error = err.Error()
			}
			w, e := tally(res, len(parseErrs))
			fixes := countFixes(res)

			mu.Lock()
			totalW += w
			totalE += e
			totalRepairs += fixes
			doneN++
			if e > 0 {
				exitCode = exitErrors
			}
			if o.jsonLines {
				if res.Error != "" {
					emitEvent(Event{Type: "error", File: baseName, Message: res.Error, Index: seqIdx, Total: total})
				} else {
					emitEvent(Event{Type: "done", File: baseName, Fixes: fixes, Index: seqIdx, Total: total, Result: &res})
				}
			} else if o.jsonOut && !o.jsonLines {
				jsonSlots[idx] = &res
			} else if lp != nil {
				lp.update(idx, baseName, res, parseErrs)
				lp.render()
			} else if !o.quiet {
				tw.printBatchLine(doneN, total, baseName, res, parseErrs)
				if o.verbose {
					tw.printResultDetail(res, parseErrs, true)
				}
			}
			mu.Unlock()
		}
	}

	nw := o.workers
	if nw < 1 {
		nw = 1
	}
	if nw > len(files) {
		nw = len(files)
	}
	for i := 0; i < nw; i++ {
		wg.Add(1)
		go worker()
	}
	for _, p := range files {
		jobs <- p
	}
	close(jobs)
	wg.Wait()

	if o.jsonLines {
		emitEvent(Event{Type: "summary", Total: procN, Fixes: totalRepairs,
			Message: fmt.Sprintf("Processed %d files, %d warnings, %d errors", procN, totalW, totalE)})
	} else if o.jsonOut {
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		// Workers finish in any order; the array follows the sorted input order
		// so two runs over the same files print the same thing.
		jsonBuf := make([]Result, 0, total)
		for _, r := range jsonSlots {
			if r != nil {
				jsonBuf = append(jsonBuf, *r)
			}
		}
		if err := enc.Encode(jsonBuf); err != nil {
			fmt.Fprintf(os.Stderr, "cleanmodels: json encode: %v\n", err)
			return exitErrors
		}
	}

	if lp != nil {
		lp.finish()
	}

	switch {
	case o.quiet, o.jsonLines:
		// Quiet emits nothing; json-lines already emitted a summary event.
	case o.jsonOut:
		// Stdout carries the JSON array; route the human counts to stderr.
		errTw.printBatchSummary(procN, totalRepairs, totalW, totalE)
	default:
		// Plain mode: per-file lines went to stdout, summary follows them.
		tw.printBatchSummary(procN, totalRepairs, totalW, totalE)
	}
	return exitCode
}
