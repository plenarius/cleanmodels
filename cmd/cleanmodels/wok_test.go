package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// A model with an aabb node (a tile) gets its walkmesh written beside the model as
// <name>.wok, as the Prolog tool did; a model without one gets no .wok.
func TestRepairWritesTheWalkmesh(t *testing.T) {
	dir := t.TempDir()
	in := filepath.Join(dir, "in.mdl")
	out := filepath.Join(dir, "tilex.mdl")
	flat := filepath.Join(dir, "flat.mdl")
	good := strings.Replace(brokenWalkmesh, "      0 1 9 1 0 1 2 4\n", "      0 1 1 1 0 1 2 4\n", 1)
	if err := os.WriteFile(in, []byte(good), 0o644); err != nil {
		t.Fatal(err)
	}
	var code int
	captureStdio(t, func() { code = cmdRepair([]string{"--color", "never", in, out}) })
	if code != 0 {
		t.Fatalf("exit %d", code)
	}
	wok, err := os.ReadFile(filepath.Join(dir, "tilex.wok"))
	if err != nil {
		t.Fatalf("no .wok written: %v", err)
	}
	for _, want := range []string{"#MAXWALKMESH  ASCII", "beginwalkmeshgeom brokenwok", "node aabb wok", "endwalkmeshgeom brokenwok"} {
		if !strings.Contains(string(wok), want) {
			t.Errorf(".wok lacks %q:\n%s", want, wok)
		}
	}

	noAabb := "newmodel plain\nsetsupermodel plain NULL\nclassification OTHER\nsetanimationscale 1.00\nbeginmodelgeom plain\n  node dummy plain\n    parent NULL\n  endnode\nendmodelgeom plain\ndonemodel plain\n"
	if err := os.WriteFile(in, []byte(noAabb), 0o644); err != nil {
		t.Fatal(err)
	}
	captureStdio(t, func() { code = cmdRepair([]string{"--color", "never", in, flat}) })
	if _, err := os.Stat(filepath.Join(dir, "flat.wok")); err == nil {
		t.Error("a model without an aabb node got a .wok")
	}
}
