package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// A file that is not a model is an error and compile writes nothing for it.
func TestNonModelFileIsAnErrorAndCompileWritesNothing(t *testing.T) {
	dir := t.TempDir()
	bad := filepath.Join(dir, "garbage.mdl")
	if err := os.WriteFile(bad, []byte("this is not a model\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	var code int
	stdout, _ := captureStdio(t, func() { code = cmdCheck([]string{"--color", "never", bad}) })
	if code != exitErrors {
		t.Errorf("check exit %d, want %d", code, exitErrors)
	}
	if !strings.Contains(stdout, "not an ASCII MDL model") || !strings.Contains(stdout, "hint:") {
		t.Errorf("message: %q", stdout)
	}
	out := filepath.Join(dir, "out.mdl")
	captureStdio(t, func() { code = cmdCompile([]string{"--color", "never", bad, out}) })
	if code != exitErrors {
		t.Errorf("compile exit %d, want %d", code, exitErrors)
	}
	if _, err := os.Stat(out); err == nil {
		t.Error("compile wrote an output file for a file that is not a model")
	}
}

func TestEmptyFileGivesAClearError(t *testing.T) {
	empty := filepath.Join(t.TempDir(), "empty.mdl")
	if err := os.WriteFile(empty, nil, 0o644); err != nil {
		t.Fatal(err)
	}
	stdout, _ := captureStdio(t, func() { cmdCheck([]string{"--color", "never", empty}) })
	if strings.Contains(stdout, "ERROR: EOF") || !strings.Contains(stdout, "empty") {
		t.Errorf("message: %q", stdout)
	}
}
