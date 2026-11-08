package main

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"

	"github.com/plenarius/cleanmodels/pkg/buildinfo"
	"github.com/plenarius/cleanmodels/pkg/mdl"
)

// The compile cache (compile --cache-dir) keeps each compile's output so the
// same model is not compiled twice. It is content-addressed:
//
//	entries/ab/<key>.json   what is known about one model: its key covers the
//	                        compiler, the model's bytes and file name, and the
//	                        search paths; it lists up to cacheCandidates
//	                        outputs, each with the resources the compile looked
//	                        up (mdl.Dependency) and what it reported
//	objects/ab/<sha256>     compiled models and walkmeshes, by their hash
//
// A stored output is used only when every resource its compile looked up
// resolves to the same thing again (mdl.ResolveDependency): a changed or newly
// added supermodel, skeleton, capart.2da or material compiles the model again,
// as a changed model or compiler does. An entry's modification time is when
// it was last used; `cleanmodels cache prune` removes the ones not used for a
// while and the objects nothing refers to any more.

const (
	cacheFormat     = "cleanmodels-compile-cache/1"
	cacheCandidates = 4
)

type cacheEntry struct {
	Format     string           `json:"format"`
	Candidates []cacheCandidate `json:"candidates"`
}

// cacheCandidate is one stored compile of a model.
type cacheCandidate struct {
	Deps []mdl.Dependency `json:"deps"`
	// MDL and Wok are the objects written: the binary model and, for a model
	// with an aabb node, its walkmesh ("" when none).
	MDL string `json:"mdl"`
	Wok string `json:"wok,omitempty"`
	// What the compile reported, given back on a hit so a cached run reports
	// the same as one that compiles.
	ParseErrors []mdl.ParseError       `json:"parse_errors,omitempty"`
	Warnings    []mdl.DecompileWarning `json:"warnings,omitempty"`
	Notes       []mdl.DecompileWarning `json:"notes,omitempty"`
}

var (
	compilerIDOnce sync.Once
	compilerID     string
)

// cacheCompilerID identifies this build of cleanmodels: its version and a hash
// of its own executable, so a rebuild with the same version string (a dev
// build) does not reuse another build's output.
func cacheCompilerID() string {
	compilerIDOnce.Do(func() {
		compilerID = buildinfo.Version(version)
		if exe, err := os.Executable(); err == nil {
			if data, err := os.ReadFile(exe); err == nil {
				sum := sha256.Sum256(data)
				compilerID += " " + hex.EncodeToString(sum[:])
			}
		}
	})
	return compilerID
}

// cacheKey is the entry key for compiling input (the model's bytes, read from
// path) with the given search paths.
func cacheKey(path string, input []byte, copts mdl.CompileOptions, force bool) string {
	h := sha256.New()
	field := func(s string) {
		h.Write([]byte(s))
		h.Write([]byte{0})
	}
	field(cacheFormat)
	field(cacheCompilerID())
	field(fmt.Sprintf("force=%t", force))
	field(strings.ToLower(filepath.Base(path)))
	for _, list := range [][]string{copts.SupermodelDirs, copts.ResourceDirs} {
		field(fmt.Sprintf("%d", len(list)))
		for _, d := range list {
			if abs, err := filepath.Abs(d); err == nil {
				d = abs
			}
			field(d)
		}
	}
	sum := sha256.Sum256(input)
	field(hex.EncodeToString(sum[:]))
	return hex.EncodeToString(h.Sum(nil))
}

func cacheEntryPath(dir, key string) string {
	return filepath.Join(dir, "entries", key[:2], key+".json")
}

func cacheObjectPath(dir, sum string) string {
	return filepath.Join(dir, "objects", sum[:2], sum)
}

// writeFileAtomic writes data to path through a temporary file in the same
// directory, so a reader never sees half a file.
func writeFileAtomic(path string, data []byte) error {
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return err
	}
	tmp, err := os.CreateTemp(filepath.Dir(path), ".tmp-*")
	if err != nil {
		return err
	}
	_, werr := tmp.Write(data)
	cerr := tmp.Close()
	if werr == nil {
		werr = cerr
	}
	if werr == nil {
		werr = os.Rename(tmp.Name(), path)
	}
	if werr != nil {
		os.Remove(tmp.Name())
	}
	return werr
}

func readCacheEntry(dir, key string) (*cacheEntry, error) {
	data, err := os.ReadFile(cacheEntryPath(dir, key))
	if err != nil {
		return nil, err
	}
	var e cacheEntry
	if err := json.Unmarshal(data, &e); err != nil || e.Format != cacheFormat {
		return nil, fmt.Errorf("not a cache entry")
	}
	return &e, nil
}

// cacheLookup returns the stored compile of key whose dependencies all still
// resolve as they did, or nil.
func cacheLookup(dir, key string, copts mdl.CompileOptions) *cacheCandidate {
	e, err := readCacheEntry(dir, key)
	if err != nil {
		return nil
	}
	for i := range e.Candidates {
		c := &e.Candidates[i]
		if c.MDL == "" || !depsHold(c.Deps, copts) {
			continue
		}
		now := time.Now()
		os.Chtimes(cacheEntryPath(dir, key), now, now)
		return c
	}
	return nil
}

func depsHold(deps []mdl.Dependency, copts mdl.CompileOptions) bool {
	for _, d := range deps {
		if mdl.ResolveDependency(d, copts) != d {
			return false
		}
	}
	return true
}

func putCacheObject(dir string, data []byte) (string, error) {
	sum := sha256.Sum256(data)
	h := hex.EncodeToString(sum[:])
	p := cacheObjectPath(dir, h)
	if _, err := os.Stat(p); err == nil {
		now := time.Now()
		os.Chtimes(p, now, now)
		return h, nil
	}
	return h, writeFileAtomic(p, data)
}

// cacheStore adds a compile of key to its entry, most recent first.
func cacheStore(dir, key string, c cacheCandidate) error {
	e, err := readCacheEntry(dir, key)
	if err != nil {
		e = &cacheEntry{Format: cacheFormat}
	}
	cands := []cacheCandidate{c}
	for _, old := range e.Candidates {
		if len(cands) == cacheCandidates {
			break
		}
		if old.MDL == c.MDL && old.Wok == c.Wok && sameDeps(old.Deps, c.Deps) {
			continue
		}
		cands = append(cands, old)
	}
	e.Candidates = cands
	data, err := json.Marshal(e)
	if err != nil {
		return err
	}
	return writeFileAtomic(cacheEntryPath(dir, key), data)
}

func sameDeps(a, b []mdl.Dependency) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// compileCacheRun is one model's trip through the cache: lookup before the
// compile, store after it.
type compileCacheRun struct {
	dir   string
	key   string
	copts mdl.CompileOptions
	mu    sync.Mutex
	deps  []mdl.Dependency
}

// startCompileCache reads the model and looks it up. On a hit it writes the
// stored output to outPath (and its walkmesh beside it), fills res and returns
// hit true. On a miss it returns the run to finish with store after compiling;
// a nil run means the cache is not used for this model (it could not be read).
func startCompileCache(path, outPath string, o procOpts, res *Result) (run *compileCacheRun, parseErrs []mdl.ParseError, hit bool, err error) {
	input, err := os.ReadFile(path)
	if err != nil {
		return nil, nil, false, nil
	}
	superDirs, resDirs := lookupDirs(path, o)
	copts := mdl.CompileOptions{SupermodelDirs: superDirs, ResourceDirs: resDirs}
	run = &compileCacheRun{dir: o.cacheDir, key: cacheKey(path, input, copts, o.force), copts: copts}
	c := cacheLookup(run.dir, run.key, copts)
	if c == nil {
		return run, nil, false, nil
	}
	data, err := os.ReadFile(cacheObjectPath(run.dir, c.MDL))
	if err != nil {
		return run, nil, false, nil // the object is gone (pruned): compile
	}
	var wok []byte
	if c.Wok != "" {
		if wok, err = os.ReadFile(cacheObjectPath(run.dir, c.Wok)); err != nil {
			return run, nil, false, nil
		}
	}
	if err := writeFileAtomic(outPath, data); err != nil {
		res.Error = err.Error()
		return nil, nil, true, err
	}
	for _, pe := range c.ParseErrors {
		res.ParseWarnings = append(res.ParseWarnings, pe.Error())
	}
	res.Warnings = c.Warnings
	res.Notes = c.Notes
	res.Cached = true
	res.Actions = append(res.Actions, fmt.Sprintf("compiled to binary: %s (from the cache)", outPath))
	if err := writeWok(c.Wok != "", wok, outPath, res); err != nil {
		res.Error = err.Error()
		return nil, c.ParseErrors, true, err
	}
	return nil, c.ParseErrors, true, nil
}

// dependency collects the compile's lookups (mdl.CompileOptions.Dependency).
func (r *compileCacheRun) dependency(d mdl.Dependency) {
	r.mu.Lock()
	r.deps = append(r.deps, d)
	r.mu.Unlock()
}

// store keeps a finished compile: the model written to outPath, its walkmesh
// (nil when none) and what was reported. A failure to store is not the
// compile's failure: the model was written, it just isn't cached.
func (r *compileCacheRun) store(outPath string, wok []byte, parseErrs []mdl.ParseError, res *Result) {
	data, err := os.ReadFile(outPath)
	if err != nil {
		return
	}
	c := cacheCandidate{Deps: r.deps, ParseErrors: parseErrs, Warnings: res.Warnings, Notes: res.Notes}
	if c.MDL, err = putCacheObject(r.dir, data); err != nil {
		return
	}
	if wok != nil {
		if c.Wok, err = putCacheObject(r.dir, wok); err != nil {
			return
		}
	}
	cacheStore(r.dir, r.key, c)
}

// ---------------------------------------------------------------- cache prune

func cmdCache(args []string) int {
	if len(args) == 0 || args[0] != "prune" {
		fmt.Fprintf(os.Stderr, "Usage: cleanmodels cache prune [--unused-for DURATION] <cache-dir>\n")
		if len(args) > 0 && (args[0] == "--help" || args[0] == "-h") {
			return exitOK
		}
		return exitUsage
	}
	return cmdCachePrune(args[1:])
}

func cmdCachePrune(args []string) int {
	fs := flag.NewFlagSet("cleanmodels cache prune", flag.ContinueOnError)
	fs.SetOutput(os.Stderr)
	unused := fs.String("unused-for", "30d", "remove the models not compiled or used from the cache for this long (such as 30d or 12h)")
	quiet := fs.Bool("quiet", false, "print nothing but errors")
	fs.BoolVar(quiet, "q", false, "alias for --quiet")
	fs.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: cleanmodels cache prune [flags] <cache-dir>\n\n")
		fmt.Fprintf(os.Stderr, "Remove the compiled models that compile --cache-dir has not used for a while, and the files nothing refers to any more.\n\nFlags:\n")
		fs.PrintDefaults()
	}
	if err := parseArgs(fs, args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return exitOK
		}
		return exitUsage
	}
	if fs.NArg() != 1 {
		fs.Usage()
		return exitUsage
	}
	age, err := parseAge(*unused)
	if err != nil {
		fmt.Fprintf(os.Stderr, "cleanmodels: --unused-for: %v\n", err)
		return exitUsage
	}
	st, err := pruneCache(fs.Arg(0), time.Now().Add(-age))
	if err != nil {
		fmt.Fprintf(os.Stderr, "cleanmodels: %v\n", err)
		return exitErrors
	}
	if !*quiet {
		fmt.Printf("removed %d of %d models and %d files (%s); %d models kept\n",
			st.entriesRemoved, st.entries, st.objectsRemoved, formatBytes(st.bytesRemoved), st.entries-st.entriesRemoved)
	}
	return exitOK
}

// parseAge reads a duration, also in days ("30d").
func parseAge(s string) (time.Duration, error) {
	if strings.HasSuffix(s, "d") {
		var n float64
		if _, err := fmt.Sscanf(strings.TrimSuffix(s, "d"), "%g", &n); err != nil || n < 0 {
			return 0, fmt.Errorf("%q is not a duration", s)
		}
		return time.Duration(n * 24 * float64(time.Hour)), nil
	}
	d, err := time.ParseDuration(s)
	if err != nil || d < 0 {
		return 0, fmt.Errorf("%q is not a duration", s)
	}
	return d, nil
}

type pruneStats struct {
	entries, entriesRemoved, objectsRemoved int
	bytesRemoved                            int64
}

// pruneCache removes entries last used before cutoff, then the objects no
// remaining entry names. An object written in the last hour is kept even when
// nothing names it yet, as a compile running now writes its objects before its
// entry.
func pruneCache(dir string, cutoff time.Time) (pruneStats, error) {
	var st pruneStats
	if _, err := os.Stat(filepath.Join(dir, "entries")); err != nil {
		return st, fmt.Errorf("%s is not a compile cache (no entries folder)", dir)
	}
	keep := map[string]bool{}
	err := filepath.WalkDir(filepath.Join(dir, "entries"), func(p string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() || !strings.HasSuffix(p, ".json") {
			return nil
		}
		st.entries++
		info, err := d.Info()
		if err != nil {
			return nil
		}
		if info.ModTime().Before(cutoff) {
			if os.Remove(p) == nil {
				st.entriesRemoved++
			}
			return nil
		}
		data, err := os.ReadFile(p)
		if err != nil {
			return nil
		}
		var e cacheEntry
		if json.Unmarshal(data, &e) != nil {
			return nil
		}
		for _, c := range e.Candidates {
			keep[c.MDL], keep[c.Wok] = true, true
		}
		return nil
	})
	if err != nil {
		return st, err
	}
	recent := time.Now().Add(-time.Hour)
	err = filepath.WalkDir(filepath.Join(dir, "objects"), func(p string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() || keep[d.Name()] {
			return nil
		}
		info, err := d.Info()
		if err != nil || info.ModTime().After(recent) {
			return nil
		}
		if os.Remove(p) == nil {
			st.objectsRemoved++
			st.bytesRemoved += info.Size()
		}
		return nil
	})
	if errors.Is(err, fs.ErrNotExist) {
		err = nil
	}
	return st, err
}

func formatBytes(n int64) string {
	switch {
	case n >= 1<<30:
		return fmt.Sprintf("%.1f GB", float64(n)/(1<<30))
	case n >= 1<<20:
		return fmt.Sprintf("%.1f MB", float64(n)/(1<<20))
	case n >= 1<<10:
		return fmt.Sprintf("%.1f KB", float64(n)/(1<<10))
	}
	return fmt.Sprintf("%d bytes", n)
}
