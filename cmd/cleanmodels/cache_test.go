package main

import (
	"bytes"
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// compileJSONLines runs compile --json-lines and returns each model's result.
func compileJSONLines(t *testing.T, args ...string) map[string]Result {
	t.Helper()
	var code int
	stdout, stderr := captureStdio(t, func() {
		code = cmdCompile(append([]string{"--json-lines", "--color", "never"}, args...))
	})
	if code != exitOK {
		t.Fatalf("compile exit %d\nstdout:\n%s\nstderr:\n%s", code, stdout, stderr)
	}
	out := map[string]Result{}
	for _, line := range strings.Split(strings.TrimSpace(stdout), "\n") {
		var e Event
		if err := json.Unmarshal([]byte(line), &e); err != nil {
			t.Fatalf("bad event %q: %v", line, err)
		}
		if e.Type == "done" && e.Result != nil {
			out[e.File] = *e.Result
		}
	}
	return out
}

func sameTree(t *testing.T, a, b string) {
	t.Helper()
	entries, err := os.ReadDir(a)
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) == 0 {
		t.Fatalf("%s is empty", a)
	}
	for _, e := range entries {
		x, _ := os.ReadFile(filepath.Join(a, e.Name()))
		y, err := os.ReadFile(filepath.Join(b, e.Name()))
		if err != nil || !bytes.Equal(x, y) {
			t.Errorf("%s differs between %s and %s", e.Name(), a, b)
		}
	}
}

func TestCompileCache(t *testing.T) {
	dir, _ := writeModels(t, "a.mdl", "b.mdl", "c.mdl")
	// c.mdl gets a line the game ignores, so it has a parse warning to replay.
	c, _ := os.ReadFile(filepath.Join(dir, "c.mdl"))
	i := bytes.Index(c, []byte("node trimesh "))
	j := i + bytes.IndexByte(c[i:], '\n') + 1
	if i < 0 || j <= i {
		t.Fatal("no trimesh node in the fixture")
	}
	c = append(c[:j:j], append([]byte("  setfillumcolor 0 0 0\n"), c[j:]...)...)
	if err := os.WriteFile(filepath.Join(dir, "c.mdl"), c, 0o644); err != nil {
		t.Fatal(err)
	}
	work := t.TempDir()
	cache := filepath.Join(work, "cache")
	out := func(n string) string { return filepath.Join(work, n) }

	first := compileJSONLines(t, "--cache-dir", cache, dir, out("one"))
	if len(first) != 3 {
		t.Fatalf("first run: %d results, want 3", len(first))
	}
	for name, r := range first {
		if r.Cached {
			t.Errorf("first run: %s came from an empty cache", name)
		}
	}
	if len(first["c.mdl"].ParseWarnings) == 0 {
		t.Fatal("c.mdl has no parse warning to replay; the fixture edit did not take")
	}

	second := compileJSONLines(t, "--cache-dir", cache, dir, out("two"))
	for name, r := range second {
		if !r.Cached {
			t.Errorf("second run: %s was compiled again", name)
		}
		if strings.Join(r.ParseWarnings, "|") != strings.Join(first[name].ParseWarnings, "|") ||
			len(r.Warnings) != len(first[name].Warnings) || len(r.Notes) != len(first[name].Notes) {
			t.Errorf("second run: %s reports differently from the compile it came from", name)
		}
	}
	sameTree(t, out("one"), out("two"))

	compileJSONLines(t, dir, out("plain"))
	sameTree(t, out("plain"), out("two"))

	// An edited model is compiled again; the others still come from the cache.
	b, _ := os.ReadFile(filepath.Join(dir, "b.mdl"))
	if err := os.WriteFile(filepath.Join(dir, "b.mdl"), append(b, []byte("\n# edited\n")...), 0o644); err != nil {
		t.Fatal(err)
	}
	third := compileJSONLines(t, "--cache-dir", cache, dir, out("three"))
	if third["b.mdl"].Cached || !third["a.mdl"].Cached || !third["c.mdl"].Cached {
		t.Errorf("after editing b.mdl: cached a=%v b=%v c=%v, want true false true",
			third["a.mdl"].Cached, third["b.mdl"].Cached, third["c.mdl"].Cached)
	}
}

// A material added next to the models is a changed dependency for every model
// whose meshes look it up.
func TestCompileCacheNoticesNewMaterial(t *testing.T) {
	dir := t.TempDir()
	crystal, err := os.ReadFile("../../tests/fixtures/oracle/ascii/plc_crysblu.mdl")
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "a.mdl"), crystal, 0o644); err != nil {
		t.Fatal(err)
	}
	work := t.TempDir()
	cache := filepath.Join(work, "cache")
	compileJSONLines(t, "--cache-dir", cache, dir, filepath.Join(work, "one"))

	entries, _ := filepath.Glob(filepath.Join(cache, "entries", "*", "*.json"))
	if len(entries) != 1 {
		t.Fatalf("%d entries, want 1", len(entries))
	}
	data, _ := os.ReadFile(entries[0])
	var e cacheEntry
	if err := json.Unmarshal(data, &e); err != nil {
		t.Fatal(err)
	}
	var missing string
	for _, d := range e.Candidates[0].Deps {
		if d.Kind == "mtr" && !d.Found {
			missing = d.Name
			break
		}
	}
	if missing == "" {
		t.Fatal("the crystal's meshes looked up no material that is missing")
	}
	if err := os.WriteFile(filepath.Join(dir, missing+".mtr"), []byte("renderhint NormalAndSpecMapped\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	// A folder's materials are indexed once per process, as one run sees one
	// state of the disk, so the next run is a new process.
	if r := compileInNewProcess(t, "--cache-dir", cache, dir, filepath.Join(work, "two"))["a.mdl"]; r.Cached {
		t.Errorf("a.mdl came from the cache after %s.mtr was added", missing)
	}
}

// compileInNewProcess is compileJSONLines in a fresh process: the test binary
// run again as TestHelperCompile.
func compileInNewProcess(t *testing.T, args ...string) map[string]Result {
	t.Helper()
	cmd := exec.Command(os.Args[0], "-test.run=^TestHelperCompile$")
	cmd.Env = append(os.Environ(), "CLEANMODELS_HELPER_COMPILE="+strings.Join(append([]string{"--json-lines", "--color", "never"}, args...), "\n"))
	stdout, err := cmd.Output()
	if err != nil {
		t.Fatalf("helper compile: %v\n%s", err, stdout)
	}
	out := map[string]Result{}
	for _, line := range strings.Split(strings.TrimSpace(string(stdout)), "\n") {
		var e Event
		if json.Unmarshal([]byte(line), &e) == nil && e.Type == "done" && e.Result != nil {
			out[e.File] = *e.Result
		}
	}
	return out
}

// TestHelperCompile is not a test: compileInNewProcess runs the test binary
// with it to compile in a new process.
func TestHelperCompile(t *testing.T) {
	args := os.Getenv("CLEANMODELS_HELPER_COMPILE")
	if args == "" {
		return
	}
	os.Exit(cmdCompile(strings.Split(args, "\n")))
}

func TestCachePrune(t *testing.T) {
	dir, _ := writeModels(t, "a.mdl", "b.mdl")
	work := t.TempDir()
	cache := filepath.Join(work, "cache")
	compileJSONLines(t, "--cache-dir", cache, dir, filepath.Join(work, "one"))

	// One model's entry and every object were last used two days ago.
	old := time.Now().Add(-48 * time.Hour)
	entries, _ := filepath.Glob(filepath.Join(cache, "entries", "*", "*.json"))
	objects, _ := filepath.Glob(filepath.Join(cache, "objects", "*", "*"))
	if len(entries) != 2 || len(objects) == 0 {
		t.Fatalf("%d entries and %d objects, want 2 and some", len(entries), len(objects))
	}
	os.Chtimes(entries[0], old, old)
	for _, p := range objects {
		os.Chtimes(p, old, old)
	}

	st, err := pruneCache(cache, time.Now().Add(-24*time.Hour))
	if err != nil {
		t.Fatal(err)
	}
	if st.entries != 2 || st.entriesRemoved != 1 || st.objectsRemoved == 0 {
		t.Errorf("prune: %+v, want 2 entries, 1 removed, and its objects", st)
	}
	left, _ := filepath.Glob(filepath.Join(cache, "objects", "*", "*"))
	if len(left) == 0 {
		t.Error("prune removed the objects the kept entry needs")
	}
	// What is left still serves the kept model, and the pruned one compiles.
	got := compileJSONLines(t, "--cache-dir", cache, dir, filepath.Join(work, "two"))
	cached := 0
	for _, r := range got {
		if r.Cached {
			cached++
		}
	}
	if cached != 1 {
		t.Errorf("after prune %d models came from the cache, want 1", cached)
	}

	if code := cmdCache([]string{"prune", "--unused-for", "nonsense", cache}); code != exitUsage {
		t.Errorf("bad --unused-for: exit %d, want %d", code, exitUsage)
	}
	if _, err := pruneCache(filepath.Join(work, "nothing"), time.Now()); err == nil {
		t.Error("pruning a folder that is not a cache did not fail")
	}
}

func TestParseAge(t *testing.T) {
	for in, want := range map[string]time.Duration{"30d": 30 * 24 * time.Hour, "12h": 12 * time.Hour, "0s": 0, "1.5d": 36 * time.Hour} {
		if got, err := parseAge(in); err != nil || got != want {
			t.Errorf("parseAge(%q) = %v, %v; want %v", in, got, err, want)
		}
	}
	for _, in := range []string{"", "d", "-1d", "soon"} {
		if _, err := parseAge(in); err == nil {
			t.Errorf("parseAge(%q) did not fail", in)
		}
	}
}
