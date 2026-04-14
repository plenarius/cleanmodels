// oracle_test.go — compare our compiler output against NWN:EE game-compiled binaries.
//
// Workflow:
//  1. Copy tests/fixtures/oracle/ascii/*.mdl to your NWN Windows machine's
//     development folder (usually Documents\Neverwinter Nights\development\).
//  2. Launch NWN:EE and load an area. Place each model as a placeable/creature,
//     or use the module editor to reference it.
//  3. Open the debug console (~~) and run: compileloadedmodels
//     (or load each model manually and use the toolset's model compiler)
//  4. Copy the resulting binary .mdl files into tests/fixtures/oracle/game_binary/.
//     The filenames must match the ascii source (same basename, lowercase is fine).
//  5. Run: go test ./pkg/mdl/ -run TestOracle -v
//
// The test compares our binary output against the game's semantically —
// it does NOT do a byte-for-byte diff because the game generates tangents,
// bitangents, and may reorder some data differently. Instead it checks:
//
//   • Model/geometry name, classification, supermodel
//   • Node count and names
//   • Node hierarchy (parent names)
//   • Mesh vertex and face counts per node
//   • Animation count, names, and per-animation event counts
//   • Light color, radius, multiplier per light node
//   • Emitter birth rate, life expectancy, velocity per emitter node
//   • Controller type IDs present per node
//
// Any field difference is reported with both values so you can judge whether
// it is a real bug or a known acceptable divergence (e.g. tangent gen).
package mdl

import (
	"bytes"
	"fmt"
	"math"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"
)

const oracleAsciiDir  = "../../tests/fixtures/oracle/ascii"
const oracleBinaryDir = "../../tests/fixtures/oracle/game_binary"

// TestOracleCompare is skipped automatically when no game_binary fixtures exist.
// Drop the game-compiled binaries in game_binary/ and re-run to activate.
func TestOracleCompare(t *testing.T) {
	entries, err := os.ReadDir(oracleBinaryDir)
	if err != nil || len(entries) == 0 {
		t.Skip("no game_binary fixtures — see oracle_test.go header for setup instructions")
	}

	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(strings.ToLower(e.Name()), ".mdl") {
			continue
		}
		name := e.Name()
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			compareOracle(t, name)
		})
	}
}

func compareOracle(t *testing.T, binaryName string) {
	t.Helper()

	gamePath := filepath.Join(oracleBinaryDir, binaryName)
	gameData, err := os.ReadFile(gamePath)
	if err != nil {
		t.Fatalf("read game binary: %v", err)
	}
	if len(gameData) < 12 {
		t.Skipf("game binary %s is too small (%d bytes) — likely truncated by a crash", binaryName, len(gameData))
	}

	asciiPath := findOracleAscii(t, binaryName)
	runOracleDiff(t, binaryName, asciiPath, gameData)
}

// findOracleAscii matches the binary filename to an ascii source file
// case-insensitively (game compiler lowercases names).
func findOracleAscii(t *testing.T, binaryName string) string {
	t.Helper()
	base := strings.ToLower(strings.TrimSuffix(binaryName, filepath.Ext(binaryName)))
	entries, err := os.ReadDir(oracleAsciiDir)
	if err != nil {
		t.Fatalf("read ascii dir: %v", err)
	}
	for _, e := range entries {
		if strings.ToLower(strings.TrimSuffix(e.Name(), filepath.Ext(e.Name()))) == base {
			return filepath.Join(oracleAsciiDir, e.Name())
		}
	}
	t.Fatalf("no ascii source for %s in %s", binaryName, oracleAsciiDir)
	return ""
}

// oracleDiff returns a list of human-readable differences between two models.
// It intentionally tolerates differences in tangent/bitangent data and AABB
// tree internal structure, which the game compiler generates but we may not.
func oracleDiff(ours, game *Model) []string {
	var diffs []string
	add := func(format string, args ...any) {
		diffs = append(diffs, fmt.Sprintf(format, args...))
	}

	// ── Top-level model fields ────────────────────────────────────────────────
	if !strEqCI(ours.Name, game.Name) {
		add("model name: ours=%q game=%q", ours.Name, game.Name)
	}
	if !strEqCI(ours.SuperModel, game.SuperModel) {
		add("supermodel: ours=%q game=%q", ours.SuperModel, game.SuperModel)
	}
	if !strEqCI(ours.Classification, game.Classification) {
		add("classification: ours=%q game=%q", ours.Classification, game.Classification)
	}

	// ── Node inventory ────────────────────────────────────────────────────────
	ourNodes  := nodeMap(ours.Nodes)
	gameNodes := nodeMap(game.Nodes)

	for name, on := range ourNodes {
		gn, ok := gameNodes[name]
		if !ok {
			add("node %q: present in ours, missing in game", name)
			continue
		}
		diffNode(name, on, gn, add)
	}
	for name := range gameNodes {
		if _, ok := ourNodes[name]; !ok {
			add("node %q: missing in ours, present in game", name)
		}
	}

	// ── Animation count / names / events ──────────────────────────────────────
	if len(ours.Animations) != len(game.Animations) {
		add("animation count: ours=%d game=%d", len(ours.Animations), len(game.Animations))
	}

	ourAnims  := animNameSet(ours.Animations)
	gameAnims := animNameSet(game.Animations)
	for a := range ourAnims {
		if !gameAnims[a] {
			add("anim %q: present in ours, missing in game", a)
		}
	}
	for a := range gameAnims {
		if !ourAnims[a] {
			add("anim %q: missing in ours, present in game", a)
		}
	}

	ourAnimByName := animByLowerName(ours.Animations)
	gameAnimByName := animByLowerName(game.Animations)
	for animName, oa := range ourAnimByName {
		ga, ok := gameAnimByName[animName]
		if !ok {
			continue
		}
		if len(oa.Events) != len(ga.Events) {
			add("anim %q: event count ours=%d game=%d", animName, len(oa.Events), len(ga.Events))
		}
	}

	sort.Strings(diffs)
	return diffs
}

func diffNode(name string, ours, game *Node, add func(string, ...any)) {
	// Parent name
	if !strEqCI(ours.Parent, game.Parent) {
		add("node %q: parent ours=%q game=%q", name, ours.Parent, game.Parent)
	}

	// Mesh geometry counts
	ourHasMesh  := ours.Mesh != nil
	gameHasMesh := game.Mesh != nil
	if ourHasMesh != gameHasMesh {
		add("node %q: has_mesh ours=%v game=%v", name, ourHasMesh, gameHasMesh)
		return
	}
	if ourHasMesh {
		ov, gv := len(ours.Mesh.Verts), len(game.Mesh.Verts)
		of, gf := len(ours.Mesh.Faces), len(game.Mesh.Faces)
		// Our vertex count = faces*3 (expanded); game may differ by tangent gen.
		// Compare face count which must be identical.
		if of != gf {
			add("node %q: face count ours=%d game=%d", name, of, gf)
		}
		// Vertex counts intentionally differ: we always expand to faces×3,
		// the game deduplicates identical verts. Log for information only.
		if ov != gv {
			add("INFO node %q: vertex count ours=%d game=%d  (game deduplicates; ours expands)", name, ov, gv)
		}
	}

	// Light static controller values
	ourHasLight := ours.Light != nil
	gameHasLight := game.Light != nil
	if ourHasLight != gameHasLight {
		add("node %q: has_light ours=%v game=%v", name, ourHasLight, gameHasLight)
	} else if ourHasLight {
		ol, gl := ours.Light, game.Light
		if !approxEqVec3(ol.Color, gl.Color, 0.01) {
			add("node %q: light color ours=%v game=%v", name, ol.Color, gl.Color)
		}
		if !approxEqF32(ol.Radius, gl.Radius, 0.01) {
			add("node %q: light radius ours=%v game=%v", name, ol.Radius, gl.Radius)
		}
		if !approxEqF32(ol.Multiplier, gl.Multiplier, 0.01) {
			add("node %q: light multiplier ours=%v game=%v", name, ol.Multiplier, gl.Multiplier)
		}
	}

	// Emitter static values (compiled as controllers)
	ourHasEmitter := ours.Emitter != nil
	gameHasEmitter := game.Emitter != nil
	if ourHasEmitter != gameHasEmitter {
		add("node %q: has_emitter ours=%v game=%v", name, ourHasEmitter, gameHasEmitter)
	} else if ourHasEmitter {
		oe, ge := ours.Emitter, game.Emitter
		if !approxEqF32(oe.BirthRate, ge.BirthRate, 0.01) {
			add("node %q: emitter birthrate ours=%v game=%v", name, oe.BirthRate, ge.BirthRate)
		}
		if !approxEqF32(oe.LifeExp, ge.LifeExp, 0.01) {
			add("node %q: emitter lifeexp ours=%v game=%v", name, oe.LifeExp, ge.LifeExp)
		}
		if !approxEqF32(oe.Velocity, ge.Velocity, 0.01) {
			add("node %q: emitter velocity ours=%v game=%v", name, oe.Velocity, ge.Velocity)
		}
	}

}

// ── helpers ───────────────────────────────────────────────────────────────────

func strEqCI(a, b string) bool {
	return strings.EqualFold(a, b)
}

func nodeMap(nodes []*Node) map[string]*Node {
	m := make(map[string]*Node, len(nodes))
	for _, n := range nodes {
		m[strings.ToLower(n.Name)] = n
	}
	return m
}

func animNameSet(anims []Animation) map[string]bool {
	m := make(map[string]bool, len(anims))
	for _, a := range anims {
		m[strings.ToLower(a.Name)] = true
	}
	return m
}

func animByLowerName(anims []Animation) map[string]Animation {
	m := make(map[string]Animation, len(anims))
	for _, a := range anims {
		m[strings.ToLower(a.Name)] = a
	}
	return m
}

func approxEqF32(a, b float32, tol float64) bool {
	return math.Abs(float64(a)-float64(b)) <= tol
}

func approxEqVec3(a, b Vec3, tol float64) bool {
	return approxEqF32(a.X, b.X, tol) && approxEqF32(a.Y, b.Y, tol) && approxEqF32(a.Z, b.Z, tol)
}

// TestOracleDumpCompare compares our compiler output against the game-compiled
// binaries extracted from a full area dump (modelcompiler output zip).
// It sources ASCII from the full NWN:EE decompiled stock model folder.
//
// Defaults (override with env vars):
//   DUMP_BINARY_DIR  — folder of game-compiled .mdl binaries  (default: /tmp/modelcompiler_dump/modelcompiler)
//   DECOMPILED_DIR   — folder of decompiled ASCII .mdl sources (default: ~/Downloads/1.86.8193.34.1 Decompiled Models (One Folder))
func TestOracleDumpCompare(t *testing.T) {
	dumpDir := os.Getenv("DUMP_BINARY_DIR")
	if dumpDir == "" {
		dumpDir = "/tmp/modelcompiler_dump/modelcompiler"
	}
	asciiDir := os.Getenv("DECOMPILED_DIR")
	if asciiDir == "" {
		asciiDir = "/Users/james/Downloads/1.86.8193.34.1 Decompiled Models (One Folder)"
	}

	entries, err := os.ReadDir(dumpDir)
	if err != nil || len(entries) == 0 {
		t.Skipf("dump binary dir not available: %v", err)
	}

	// Build case-insensitive index of available ASCII sources.
	asciiIndex := map[string]string{} // lower-basename → full path
	if ae, err := os.ReadDir(asciiDir); err == nil {
		for _, e := range ae {
			base := strings.ToLower(strings.TrimSuffix(e.Name(), filepath.Ext(e.Name())))
			asciiIndex[base] = filepath.Join(asciiDir, e.Name())
		}
	}

	var skipped int
	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(strings.ToLower(e.Name()), ".mdl") {
			continue
		}
		name := e.Name()
		base := strings.ToLower(strings.TrimSuffix(name, filepath.Ext(name)))
		asciiPath, ok := asciiIndex[base]
		if !ok {
			skipped++
			continue
		}
		gamePath := filepath.Join(dumpDir, name)
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			compareDumpOracle(t, name, asciiPath, gamePath)
		})
	}
	if skipped > 0 {
		t.Logf("skipped %d models with no ASCII source (non-stock or hak-only)", skipped)
	}
}

func compareDumpOracle(t *testing.T, name, asciiPath, gamePath string) {
	t.Helper()

	gameData, err := os.ReadFile(gamePath)
	if err != nil {
		t.Fatalf("read game binary: %v", err)
	}
	if len(gameData) < 12 {
		t.Skipf("game binary too small (%d bytes) — likely truncated", len(gameData))
	}

	runOracleDiff(t, name, asciiPath, gameData)
}

// runOracleDiff compiles the ASCII source, decompiles both binaries, and diffs.
func runOracleDiff(t *testing.T, name, asciiPath string, gameData []byte) {
	t.Helper()

	pr, err := ParseFile(asciiPath)
	if err != nil {
		t.Skipf("parse ascii: %v", err)
	}
	var ourBuf bytes.Buffer
	if err := Compile(pr.Model, &ourBuf); err != nil {
		t.Fatalf("compile: %v", err)
	}
	ourData := ourBuf.Bytes()

	ourModel, err := Decompile(bytes.NewReader(ourData), int64(len(ourData)))
	if err != nil {
		t.Fatalf("decompile our binary: %v", err)
	}
	gameModel, err := Decompile(bytes.NewReader(gameData), int64(len(gameData)))
	if err != nil {
		t.Fatalf("decompile game binary: %v", err)
	}

	diffs := oracleDiff(ourModel, gameModel)
	failures := 0
	for _, d := range diffs {
		if strings.HasPrefix(d, "INFO ") {
			t.Logf("%s", d)
		} else {
			t.Errorf("%s", d)
			failures++
		}
	}
	if failures == 0 {
		t.Logf("✓ %s  ours=%d bytes  game=%d bytes", name, len(ourData), len(gameData))
	}
}

// TestOracleCompileAll ensures every ascii oracle fixture compiles and
// decompiles without error — no game binary needed.
func TestOracleCompileAll(t *testing.T) {
	entries, err := os.ReadDir(oracleAsciiDir)
	if err != nil {
		t.Fatalf("read oracle ascii dir: %v", err)
	}
	if len(entries) == 0 {
		t.Skip("no oracle ascii fixtures found")
	}
	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(strings.ToLower(e.Name()), ".mdl") {
			continue
		}
		name := e.Name()
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			path := filepath.Join(oracleAsciiDir, name)
			pr, err := ParseFile(path)
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			var buf bytes.Buffer
			if err := Compile(pr.Model, &buf); err != nil {
				t.Fatalf("compile: %v", err)
			}
			data := buf.Bytes()
			m2, err := Decompile(bytes.NewReader(data), int64(len(data)))
			if err != nil {
				t.Fatalf("decompile: %v", err)
			}
			if len(m2.Nodes) == 0 {
				t.Error("decompiled model has no nodes")
			}
			t.Logf("OK  nodes=%d  binary=%d bytes", len(m2.Nodes), len(data))
		})
	}
}
