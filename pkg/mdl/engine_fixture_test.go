package mdl

import (
	"bytes"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

// engineFixtureLines drops the lines where our compile differs from the
// engine's on purpose: the identity orientation controller we write on every
// non-root node (see compiler_controllers.go, issue #12), which decompiles to
// "orientation 0 0 1 0"; and the mesh lightmapped byte and the emitter xgrid
// and ygrid of a source that does not set them, which the engine leaves
// uninitialised (mdl-spec E55).
var engineFixtureLines = regexp.MustCompile(`(?m)^\s*(orientation 0 0 1 0|lightmapped \d+|[xy]grid \d+)\n`)

// engineFixtureKnownGaps are probes whose engine behaviour cleanmodels does
// not follow, each with the reason.
var engineFixtureKnownGaps = map[string]string{
	"zspec_numbers": "the game writes \"position 1.5abc 2 3\" as a one-column controller, whose other two values it then reads from past the controller's data; cleanmodels writes 1.5 0 0",
}

// TestEngineCompiledFixtures compiles each probe in tests/fixtures/engine
// (<name>.ascii.mdl, our own CC0 models from cleanmodels and the mdl-spec
// conformance suite) and checks that our binary decompiles to the same ASCII
// as <name>.mdl, the game's own compile of it (`nwmain-linux compilemodel`).
// That covers node order and numbering (declaration order, the transparency
// sort, numbering against a supermodel), the header and the defaults the game
// writes for keywords a source leaves out, and orientation conversion.
func TestEngineCompiledFixtures(t *testing.T) {
	dir := filepath.Join("..", "..", "tests", "fixtures", "engine")
	probes, err := filepath.Glob(filepath.Join(dir, "*.ascii.mdl"))
	if err != nil || len(probes) == 0 {
		t.Fatalf("no probes in %s: %v", dir, err)
	}
	decompiled := func(t *testing.T, data []byte) string {
		t.Helper()
		var buf bytes.Buffer
		if err := Write(mustDecompile(t, data), &buf); err != nil {
			t.Fatal(err)
		}
		return engineFixtureLines.ReplaceAllString(buf.String(), "")
	}
	for _, probe := range probes {
		name := strings.TrimSuffix(filepath.Base(probe), ".ascii.mdl")
		t.Run(name, func(t *testing.T) {
			res, err := ParseFile(probe)
			if err != nil {
				t.Fatal(err)
			}
			var ours bytes.Buffer
			if err := CompileWithOptions(res.Model, &ours, CompileOptions{SupermodelDirs: []string{dir}}); err != nil {
				t.Fatal(err)
			}
			golden, err := os.ReadFile(filepath.Join(dir, name+".mdl"))
			if err != nil {
				t.Fatal(err)
			}
			want, got := decompiled(t, golden), decompiled(t, ours.Bytes())
			gap, known := engineFixtureKnownGaps[name]
			if want == got {
				if known {
					t.Errorf("matches the engine now; remove it from engineFixtureKnownGaps")
				}
				return
			}
			if known {
				t.Skip(gap)
			}
			wl, gl := strings.Split(want, "\n"), strings.Split(got, "\n")
			for i := 0; i < len(wl) || i < len(gl); i++ {
				var w, g string
				if i < len(wl) {
					w = wl[i]
				}
				if i < len(gl) {
					g = gl[i]
				}
				if w != g {
					t.Fatalf("line %d: engine %q, ours %q", i+1, w, g)
				}
			}
		})
	}
}
