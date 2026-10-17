package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func copyBinary(t *testing.T, dir, src, name string) {
	t.Helper()
	b, err := os.ReadFile(filepath.Join("../../tests/fixtures/oracle/game_binary", src))
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, name), b, 0o644); err != nil {
		t.Fatal(err)
	}
}

func isASCII(t *testing.T, path string) bool {
	t.Helper()
	b, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	return len(b) > 0 && b[0] != 0 && strings.Contains(string(b), "beginmodel")
}

// Neverblender's generated command for CleanModels 3: decompile a temporary
// copy in place, the file name given as the pattern.
func TestLegacyCleanModels3InPlace(t *testing.T) {
	dir := t.TempDir()
	copyBinary(t, dir, "plc_crysblu.mdl", "nvbtmp")
	var code int
	captureStdio(t, func() {
		code = runLegacy([]string{"-d", "-i", dir, "-o", dir, "-p", "nvbtmp"})
	})
	if code != 0 {
		t.Fatalf("exit %d", code)
	}
	if !isASCII(t, filepath.Join(dir, "nvbtmp")) {
		t.Error("nvbtmp was not decompiled in place")
	}
}

// Folder to folder with the default pattern: every .mdl, same names, the
// output folder created, an existing result replaced.
func TestLegacyCleanModels3Folders(t *testing.T) {
	in := t.TempDir()
	copyBinary(t, in, "plc_crysblu.mdl", "a.mdl")
	copyBinary(t, in, "ctl_compass.mdl", "b.mdl")
	copyBinary(t, in, "plc_crysblu.mdl", "c.txt")
	out := filepath.Join(t.TempDir(), "new")
	if err := os.MkdirAll(out, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(out, "a.mdl"), []byte("old"), 0o644); err != nil {
		t.Fatal(err)
	}
	var code int
	captureStdio(t, func() {
		code = runLegacy([]string{"--decompile-only", "--indir", in, "--outdir", out})
	})
	if code != 0 {
		t.Fatalf("exit %d", code)
	}
	for _, n := range []string{"a.mdl", "b.mdl"} {
		if !isASCII(t, filepath.Join(out, n)) {
			t.Errorf("%s was not written as ASCII", n)
		}
	}
	if _, err := os.Stat(filepath.Join(out, "c.txt")); err == nil {
		t.Error("c.txt does not match *.mdl but was processed")
	}
}

func TestLegacyCleanModels3Errors(t *testing.T) {
	dir := t.TempDir()
	for _, args := range [][]string{
		{"-d", "-i", dir, "-p", "missing.mdl"},
		{"-d", "-i", filepath.Join(dir, "nope")},
		{"-d", "-i", dir, "extra.mdl"},
	} {
		var code int
		_, stderr := captureStdio(t, func() { code = runLegacy(args) })
		if code != exitUsage {
			t.Errorf("%q: exit %d, want %d (%s)", args, code, exitUsage, stderr)
		}
	}
}
