package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// Models for the argument tests: three different ASCII files, so an accidental
// overwrite is visible as changed content.
func writeModels(t *testing.T, names ...string) (dir string, contents map[string][]byte) {
	t.Helper()
	fixtures := []string{"plc_dummy_only.mdl", "ctl_compass.mdl", "plc_crysblu.mdl"}
	dir = t.TempDir()
	contents = map[string][]byte{}
	for i, name := range names {
		b, err := os.ReadFile(filepath.Join("../../tests/fixtures/oracle/ascii", fixtures[i%len(fixtures)]))
		if err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(dir, name), b, 0o644); err != nil {
			t.Fatal(err)
		}
		contents[name] = b
	}
	return dir, contents
}

func unchanged(t *testing.T, dir string, contents map[string][]byte, names ...string) {
	t.Helper()
	for _, n := range names {
		got, err := os.ReadFile(filepath.Join(dir, n))
		if err != nil {
			t.Fatal(err)
		}
		if string(got) != string(contents[n]) {
			t.Errorf("%s was changed", n)
		}
	}
}

// `cleanmodels check *.mdl` must check every file and write none of them.
func TestCheckTakesManyInputsAndWritesNothing(t *testing.T) {
	dir, contents := writeModels(t, "a.mdl", "b.mdl", "c.mdl")
	var code int
	stdout, _ := captureStdio(t, func() {
		code = cmdCheck([]string{"--color", "never", filepath.Join(dir, "a.mdl"), filepath.Join(dir, "b.mdl"), filepath.Join(dir, "c.mdl")})
	})
	if code != exitOK {
		t.Errorf("exit %d, want %d", code, exitOK)
	}
	for _, n := range []string{"a.mdl", "b.mdl", "c.mdl"} {
		if !strings.Contains(stdout, n) {
			t.Errorf("%s was not checked:\n%s", n, stdout)
		}
	}
	unchanged(t, dir, contents, "a.mdl", "b.mdl", "c.mdl")
}

// A second file named after the first used to be taken as the output and
// silently replaced.
func TestRepairRefusesToOverwriteAnExistingFile(t *testing.T) {
	dir, contents := writeModels(t, "a.mdl", "b.mdl")
	var code int
	_, stderr := captureStdio(t, func() {
		code = cmdRepair([]string{"--all", "--color", "never", filepath.Join(dir, "a.mdl"), filepath.Join(dir, "b.mdl")})
	})
	if code != exitUsage {
		t.Errorf("exit %d, want %d", code, exitUsage)
	}
	if !strings.Contains(stderr, "--overwrite") || !strings.Contains(stderr, "\n  hint: ") {
		t.Errorf("message does not say how to proceed on its own hint line: %q", stderr)
	}
	if strings.Count(stderr, "\n") > 3 {
		t.Errorf("refusal should be a short message plus a hint, got: %q", stderr)
	}
	unchanged(t, dir, contents, "a.mdl", "b.mdl")
}

func TestOverwriteFlagReplacesTheOutput(t *testing.T) {
	dir, contents := writeModels(t, "a.mdl", "b.mdl")
	var code int
	captureStdio(t, func() {
		code = cmdRepair([]string{"--all", "--overwrite", "--color", "never", filepath.Join(dir, "a.mdl"), filepath.Join(dir, "b.mdl")})
	})
	if code != exitOK {
		t.Fatalf("exit %d, want %d", code, exitOK)
	}
	got, _ := os.ReadFile(filepath.Join(dir, "b.mdl"))
	if string(got) == string(contents["b.mdl"]) {
		t.Error("b.mdl was not replaced although --overwrite was given")
	}
	unchanged(t, dir, contents, "a.mdl")
}

// Writing the result over the input itself is the in-place repair and is allowed.
func TestRepairInPlaceIsAllowed(t *testing.T) {
	dir, _ := writeModels(t, "a.mdl")
	p := filepath.Join(dir, "a.mdl")
	var code int
	captureStdio(t, func() { code = cmdRepair([]string{"--all", "--color", "never", p, p}) })
	if code != exitOK {
		t.Errorf("exit %d, want %d", code, exitOK)
	}
}

// With several inputs the last argument is the output directory; a file there
// would be overwritten, so it is an error.
func TestSeveralInputsNeedAnOutputDirectory(t *testing.T) {
	dir, contents := writeModels(t, "a.mdl", "b.mdl", "c.mdl")
	var code int
	_, stderr := captureStdio(t, func() {
		code = cmdRepair([]string{"--all", "--color", "never", filepath.Join(dir, "a.mdl"), filepath.Join(dir, "b.mdl"), filepath.Join(dir, "c.mdl")})
	})
	if code != exitUsage {
		t.Errorf("exit %d, want %d", code, exitUsage)
	}
	if !strings.Contains(stderr, "output directory") {
		t.Errorf("message: %q", stderr)
	}
	unchanged(t, dir, contents, "a.mdl", "b.mdl", "c.mdl")
}

func TestSeveralInputsAreWrittenToTheOutputDirectory(t *testing.T) {
	dir, contents := writeModels(t, "a.mdl", "b.mdl")
	out := filepath.Join(t.TempDir(), "cleaned")
	var code int
	captureStdio(t, func() {
		code = cmdRepair([]string{"--all", "--color", "never", filepath.Join(dir, "a.mdl"), filepath.Join(dir, "b.mdl"), out})
	})
	if code != exitOK {
		t.Fatalf("exit %d, want %d", code, exitOK)
	}
	for _, n := range []string{"a.mdl", "b.mdl"} {
		if _, err := os.Stat(filepath.Join(out, n)); err != nil {
			t.Errorf("%s was not written to the output directory: %v", n, err)
		}
	}
	unchanged(t, dir, contents, "a.mdl", "b.mdl")
}

// Two inputs that would land on one output path must not silently replace each other.
func TestInputsWithTheSameNameAreRejected(t *testing.T) {
	d1, _ := writeModels(t, "x.mdl")
	d2, _ := writeModels(t, "x.mdl")
	out := filepath.Join(t.TempDir(), "cleaned")
	var code int
	_, stderr := captureStdio(t, func() {
		code = cmdRepair([]string{"--all", "--color", "never", filepath.Join(d1, "x.mdl"), filepath.Join(d2, "x.mdl"), out})
	})
	if code == exitOK {
		t.Error("exit 0, want an error")
	}
	if !strings.Contains(stderr, "would both be written") {
		t.Errorf("message: %q", stderr)
	}
	if _, err := os.Stat(out); err == nil {
		if entries, _ := os.ReadDir(out); len(entries) > 0 {
			t.Errorf("something was written despite the clash: %v", entries)
		}
	}
}

// check --fix keeps its optional output path.
func TestCheckFixStillWritesItsOutput(t *testing.T) {
	dir, contents := writeModels(t, "a.mdl")
	out := filepath.Join(t.TempDir(), "out.mdl")
	var code int
	captureStdio(t, func() {
		code = cmdCheck([]string{"--fix", "--color", "never", filepath.Join(dir, "a.mdl"), out})
	})
	if code != exitOK {
		t.Fatalf("exit %d, want %d", code, exitOK)
	}
	if _, err := os.Stat(out); err != nil {
		t.Errorf("output not written: %v", err)
	}
	unchanged(t, dir, contents, "a.mdl")
}

// A repair with no output reports repairs but writes nothing; it has to say so.
func TestRepairWithoutAnOutputSaysNothingWasWritten(t *testing.T) {
	dir, contents := writeModels(t, "a.mdl")
	run := func(args ...string) (code int, stderr string) {
		_, stderr = captureStdio(t, func() { code = cmdRepair(append([]string{"--all", "--color", "never"}, args...)) })
		return
	}
	if _, stderr := run(filepath.Join(dir, "a.mdl")); !strings.Contains(stderr, "nothing was written") {
		t.Errorf("no note without an output: %q", stderr)
	}
	unchanged(t, dir, contents, "a.mdl")
	if _, stderr := run("--quiet", filepath.Join(dir, "a.mdl")); strings.Contains(stderr, "nothing was written") {
		t.Errorf("--quiet should stay quiet: %q", stderr)
	}
	out := filepath.Join(t.TempDir(), "out.mdl")
	if _, stderr := run(filepath.Join(dir, "a.mdl"), out); strings.Contains(stderr, "nothing was written") {
		t.Errorf("note shown although an output was given: %q", stderr)
	}
}

// `compile model.mdl` with no output used to overwrite the ASCII source with the
// binary without a word.
func TestCompileWithoutAnOutputIsRefused(t *testing.T) {
	dir, contents := writeModels(t, "a.mdl")
	var code int
	_, stderr := captureStdio(t, func() { code = cmdCompile([]string{"--color", "never", filepath.Join(dir, "a.mdl")}) })
	if code != exitUsage {
		t.Errorf("exit %d, want %d", code, exitUsage)
	}
	if !strings.Contains(stderr, "needs an output") || !strings.Contains(stderr, "\n  hint: ") {
		t.Errorf("message: %q", stderr)
	}
	unchanged(t, dir, contents, "a.mdl")
}

func TestCompileOfAFolderWithoutAnOutputIsRefused(t *testing.T) {
	dir, contents := writeModels(t, "a.mdl", "b.mdl")
	var code int
	captureStdio(t, func() { code = cmdCompile([]string{"--color", "never", dir}) })
	if code != exitUsage {
		t.Errorf("exit %d, want %d", code, exitUsage)
	}
	unchanged(t, dir, contents, "a.mdl", "b.mdl")
}

// Compiling in place stays possible when it is spelled out.
func TestCompileInPlaceStillWorks(t *testing.T) {
	dir, contents := writeModels(t, "a.mdl", "b.mdl")
	p := filepath.Join(dir, "a.mdl")
	var code int
	captureStdio(t, func() { code = cmdCompile([]string{"--color", "never", p, p}) })
	if code != exitOK {
		t.Errorf("in place: exit %d, want %d", code, exitOK)
	}
	if got, _ := os.ReadFile(p); len(got) < 4 || got[0] != 0 || got[1] != 0 || got[2] != 0 || got[3] != 0 {
		t.Error("in place: a.mdl was not compiled to binary")
	}
	unchanged(t, dir, contents, "b.mdl")
}

// The flat-flag mode has the same no-output behaviour and says so too, but only
// when the run asked for repairs (a check-only run is expected to write nothing).
func TestLegacyModeSaysNothingWasWritten(t *testing.T) {
	dir, contents := writeModels(t, "a.mdl")
	p := filepath.Join(dir, "a.mdl")
	run := func(args ...string) string {
		var stderr string
		_, stderr = captureStdio(t, func() { runLegacy(append(args, "--color", "never")) })
		return stderr
	}
	if got := run("--fix", p); !strings.Contains(got, "nothing was written") || !strings.Contains(got, "cleanmodels --fix a.mdl a.mdl") {
		t.Errorf("--fix without an output: %q", got)
	}
	if got := run("--check", p); strings.Contains(got, "nothing was written") {
		t.Errorf("a check-only run should not get the note: %q", got)
	}
	if got := run("--fix", "--quiet", p); strings.Contains(got, "nothing was written") {
		t.Errorf("--quiet should stay quiet: %q", got)
	}
	unchanged(t, dir, contents, "a.mdl")
}
