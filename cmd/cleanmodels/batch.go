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

func batchOutputPath(root, outputDir, filePath string) (string, error) {
	if outputDir == "" {
		return "", nil
	}
	rel, err := filepath.Rel(root, filePath)
	if err != nil {
		rel = filepath.Base(filePath)
	}
	out := filepath.Join(outputDir, rel)
	if err := os.MkdirAll(filepath.Dir(out), 0755); err != nil {
		return "", fmt.Errorf("create output dir: %w", err)
	}
	return out, nil
}

func runBatch(root, outputDir string, o procOpts, tw *termWriter) int {
	files, err := collectMDLFiles(root, o.recursive)
	if err != nil {
		fmt.Fprintf(os.Stderr, "cleanmodels: %v\n", err)
		return exitErrors
	}

	if len(files) == 0 {
		if !o.quiet {
			fmt.Fprintf(os.Stderr, "%s\n", suggestRecursive(root))
		}
		return exitOK
	}

	total := len(files)

	var (
		mu           sync.Mutex
		jsonBuf      []Result
		totalW       int
		totalE       int
		totalRepairs int
		procN        int
		exitCode     = exitOK
	)

	bo := o
	if !o.jsonLines {
		bo.jsonOut = true
	}

	// Use live progress (in-place rewriting) only when stdout is a real TTY.
	// FORCE_COLOR alone is not enough — cursor control requires a real terminal.
	useLive := isTerminal(int(os.Stdout.Fd())) && !o.jsonOut && !o.jsonLines && !o.quiet
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

			outPath, mkdirErr := batchOutputPath(root, outputDir, path)
			if mkdirErr != nil {
				mu.Lock()
				totalE++
				procN++
				if o.jsonLines {
					emitEvent(Event{Type: "error", File: baseName, Message: mkdirErr.Error(), Index: procN, Total: total})
				} else if lp != nil {
					lp.update(idx, baseName, Result{Error: mkdirErr.Error()}, nil)
					lp.render()
				} else if !bo.quiet {
					tw.printBatchLine(procN, total, baseName, Result{Error: mkdirErr.Error()}, nil)
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
				jsonBuf = append(jsonBuf, res)
			} else if lp != nil {
				lp.update(idx, baseName, res, parseErrs)
				lp.render()
			} else if !o.quiet {
				tw.printBatchLine(seqIdx, total, baseName, res, parseErrs)
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
		if err := enc.Encode(jsonBuf); err != nil {
			fmt.Fprintf(os.Stderr, "cleanmodels: json encode: %v\n", err)
			return exitErrors
		}
	}

	if lp != nil {
		lp.finish()
	}

	if !o.quiet && !o.jsonOut && !o.jsonLines {
		tw.printBatchSummary(procN, totalRepairs, totalW, totalE)
	} else if !o.quiet && !o.jsonLines {
		fmt.Fprintf(os.Stderr, "Processed %d files, %d warnings, %d errors\n", procN, totalW, totalE)
	}
	return exitCode
}
