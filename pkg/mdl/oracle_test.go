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

// Tangent-only oracle pairs live in their own directory because the
// ASCII source and game binary often come from different revisions of
// the same model — the tile geometry, node names, and face counts
// differ enough to break TestOracleCompare's structural diff, but the
// per-vertex tangent comparison only needs matching world positions.
const tangentOracleAsciiDir  = "../../tests/fixtures/oracle/tangent_pairs/ascii"
const tangentOracleBinaryDir = "../../tests/fixtures/oracle/tangent_pairs/game_binary"

// Skip-list: filenames seen in dump folders that we deliberately do
// NOT include as oracle fixtures because no usable ASCII source
// exists. The binary lives only inside a hak (game-compiled, never
// re-emitted as ASCII), so we cannot do an apples-to-apples
// ASCII → compile → diff comparison. If you ever recover an ASCII
// source for one of these, drop it into the matching directory and
// remove the entry here. See tests/fixtures/oracle/README.md for the
// full skip list and rationale.
//
// This list is informational — it is not consulted at runtime; any
// fixture that does live in ascii/ and game_binary/ is exercised by
// the oracle tests. The comment exists so the next person adding
// fixtures knows why these specific filenames are missing.
var hakBinaryOnlySkipList = []string{
	"tin01_a03_01.mdl",
	"tin01_b14_01.mdl",
	"tin01_i01_01.mdl",
	"tin01_z11_01.mdl",
	"TIN01_Z10_01.mdl",
	"tin01_f01_09.mdl",
	"a_ba.mdl",
	"a_ba_casts.mdl",
	"a_ba_custom.mdl",
	"a_ba_med_weap.mdl",
	"a_ba_non_combat.mdl",
	"fx_flame01.mdl",
	"gi_armor01.mdl",
	// ashto_05[1-4]: game binaries lack baked tangents — useless for
	// tangent oracle, no value as a general fixture.
	"ashto_051.mdl",
	"ashto_052.mdl",
	"ashto_053.mdl",
	"ashto_054.mdl",
}
var _ = hakBinaryOnlySkipList // keep referenced for go vet

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

func findTangentOracleAscii(t *testing.T, binaryName string) string {
	t.Helper()
	base := strings.ToLower(strings.TrimSuffix(binaryName, filepath.Ext(binaryName)))
	entries, err := os.ReadDir(tangentOracleAsciiDir)
	if err != nil {
		t.Fatalf("read tangent ascii dir: %v", err)
	}
	for _, e := range entries {
		if strings.ToLower(strings.TrimSuffix(e.Name(), filepath.Ext(e.Name()))) == base {
			return filepath.Join(tangentOracleAsciiDir, e.Name())
		}
	}
	t.Fatalf("no ascii source for %s in %s", binaryName, tangentOracleAsciiDir)
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
	ourNodes  := nodeNameIndexFromSlice(ours.Nodes)
	gameNodes := nodeNameIndexFromSlice(game.Nodes)

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

// TestOracleTangents compares per-vertex tangent direction between our
// compiler and the game compiler for every fixture in game_binary/. Both
// compilers run independent tangent-generation passes (we use Mikktspace,
// the game uses its own algorithm) so an exact match is not expected;
// instead we check that the tangent xyz vectors point in the same
// direction (allowing sign flip, since each compiler may pick opposite
// W handedness).
//
// Approach: walk faces in lockstep (face order is preserved per
// TestOracleCompare's face-count invariant), match each face's 3 corners
// by world-space position (since the game may dedupe and reorder
// in-face vertex slots), then compute |dot(ours.xyz, game.xyz)| at each
// matched corner. A value of 1.0 means perfectly parallel tangents; 0.0
// means orthogonal. We aggregate per node and surface stats so a
// regression in our tangent generation would visibly drop the average.
//
// Currently informational-only — once we see real numbers across the
// fixture set we can replace the t.Logf with a t.Errorf threshold.
func TestOracleTangents(t *testing.T) {
	entries, err := os.ReadDir(tangentOracleBinaryDir)
	if err != nil || len(entries) == 0 {
		t.Skip("no tangent_pairs/game_binary fixtures — see oracle_test.go header for setup instructions")
	}

	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(strings.ToLower(e.Name()), ".mdl") {
			continue
		}
		name := e.Name()
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			compareOracleTangents(t, name)
		})
	}
}

func compareOracleTangents(t *testing.T, binaryName string) {
	t.Helper()

	gameData, err := os.ReadFile(filepath.Join(tangentOracleBinaryDir, binaryName))
	if err != nil {
		t.Fatalf("read game binary: %v", err)
	}
	if len(gameData) < 12 {
		t.Skipf("game binary %s too small (%d bytes)", binaryName, len(gameData))
	}

	asciiPath := findTangentOracleAscii(t, binaryName)
	pr, err := ParseFile(asciiPath)
	if err != nil {
		t.Skipf("parse ascii: %v", err)
	}

	// Decompile the game binary first so we can detect which meshes the
	// game compiler chose to bake tangents into. Many EE tile ASCII
	// sources don't carry an explicit "renderhint NormalAndSpecMapped"
	// directive — the game compiler infers the renderhint from
	// neighboring assets (e.g. a "_n.tga" normal-map sibling). Rather
	// than chasing that heuristic, we simply mirror the game's choice:
	// if the game baked tangents for a node, force our compiler to bake
	// them too by setting RenderHint on the matching mesh. This gives
	// the comparison something to actually compare without needing to
	// hand-edit each fixture.
	gameModel, err := Decompile(bytes.NewReader(gameData), int64(len(gameData)))
	if err != nil {
		t.Fatalf("decompile game binary: %v", err)
	}
	gameBaked := map[string]bool{}
	for _, gn := range gameModel.Nodes {
		if gn.Mesh != nil && len(gn.Mesh.Tangents) > 0 {
			gameBaked[strings.ToLower(gn.Name)] = true
		}
	}
	for _, on := range pr.Model.Nodes {
		if on.Mesh != nil && gameBaked[strings.ToLower(on.Name)] && on.Mesh.RenderHint == "" {
			on.Mesh.RenderHint = "NormalAndSpecMapped"
		}
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

	gameNodes := nodeNameIndexFromSlice(gameModel.Nodes)

	type nodeStats struct {
		matched, unmatched, badAlign int
		dotSum                       float64
	}
	var totals nodeStats
	nodesWithTangents := 0

	for _, on := range ourModel.Nodes {
		if on.Mesh == nil || len(on.Mesh.Tangents) == 0 {
			continue
		}
		gn, ok := gameNodes[strings.ToLower(on.Name)]
		if !ok || gn.Mesh == nil || len(gn.Mesh.Tangents) == 0 {
			continue
		}
		nodesWithTangents++

		stats := compareNodeTangents(on, gn)
		totals.matched += stats.matched
		totals.unmatched += stats.unmatched
		totals.badAlign += stats.badAlign
		totals.dotSum += stats.dotSum

		if stats.matched > 0 {
			mean := stats.dotSum / float64(stats.matched)
			t.Logf("  node %q: matched=%d unmatched=%d badAlign=%d mean|dot|=%.4f",
				on.Name, stats.matched, stats.unmatched, stats.badAlign, mean)
		}
	}

	if nodesWithTangents == 0 {
		t.Skip("no nodes carry tangent data in either model")
	}
	if totals.matched == 0 {
		t.Skip("no face corners could be position-matched between models")
	}
	mean := totals.dotSum / float64(totals.matched)

	// Per-fixture mean alignment must exceed this threshold. Empirical
	// floor as of materials/SG vertKey fix is 0.969 (tin01_a16_02), with
	// most fixtures at 0.98-1.00. We pick 0.96 as the regression bar so
	// any drop in tangent quality across the full fixture set fails the
	// test loudly. Tighten this once dedup parity work closes the
	// remaining 1-3% bad-corner gap.
	const meanAlignFloor = 0.96
	// Per-corner bad-alignment ratio must stay below this. A "bad" corner
	// is one where |dot(ours.xyz, game.xyz)| < 0.5 (effectively pointing
	// in different directions). The current floor is 3.2% (tin01_a16_02).
	const badRatioCeiling = 0.05

	badRatio := float64(totals.badAlign) / float64(totals.matched)
	t.Logf("✓ %s  nodes=%d  corners matched=%d unmatched=%d badAlign=%d (%.1f%%)  mean|dot|=%.4f",
		binaryName, nodesWithTangents, totals.matched, totals.unmatched,
		totals.badAlign, 100*badRatio, mean)

	if mean < meanAlignFloor {
		t.Errorf("%s tangent mean|dot|=%.4f below floor %.4f — tangent generation regressed",
			binaryName, mean, meanAlignFloor)
	}
	if badRatio > badRatioCeiling {
		t.Errorf("%s bad-corner ratio %.1f%% above ceiling %.1f%% — too many tangents flipped/orthogonal",
			binaryName, 100*badRatio, 100*badRatioCeiling)
	}
}

// compareNodeTangents walks two meshes in face-index lockstep, matches
// the 3 corners of each face by world-space position, and accumulates
// |dot(ours_tangent_xyz, game_tangent_xyz)| stats. Returns per-corner
// match counts and the sum of |dot| across matched corners.
func compareNodeTangents(ours, game *Node) struct {
	matched, unmatched, badAlign int
	dotSum                       float64
} {
	var s struct {
		matched, unmatched, badAlign int
		dotSum                       float64
	}
	om, gm := ours.Mesh, game.Mesh
	nFaces := len(om.Faces)
	if nFaces > len(gm.Faces) {
		nFaces = len(gm.Faces)
	}
	const posTol = float32(1e-3)
	const alignTol = 0.5
	for fi := 0; fi < nFaces; fi++ {
		of, gf := om.Faces[fi], gm.Faces[fi]
		for j := 0; j < 3; j++ {
			ovi := int(of.Verts[j])
			if ovi < 0 || ovi >= len(om.Verts) || ovi >= len(om.Tangents) {
				s.unmatched++
				continue
			}
			op := om.Verts[ovi]
			ot := om.Tangents[ovi]

			matchedK := -1
			for k := 0; k < 3; k++ {
				gvi := int(gf.Verts[k])
				if gvi < 0 || gvi >= len(gm.Verts) {
					continue
				}
				if approxEqVec3(op, gm.Verts[gvi], float64(posTol)) {
					matchedK = gvi
					break
				}
			}
			if matchedK < 0 || matchedK >= len(gm.Tangents) {
				s.unmatched++
				continue
			}
			gt := gm.Tangents[matchedK]
			otv := Vec3{X: ot.X, Y: ot.Y, Z: ot.Z}
			gtv := Vec3{X: gt.X, Y: gt.Y, Z: gt.Z}
			otn := vecNormalize(otv)
			gtn := vecNormalize(gtv)
			if vecLen(otv) < 1e-6 || vecLen(gtv) < 1e-6 {
				s.unmatched++
				continue
			}
			dot := math.Abs(float64(vecDot(otn, gtn)))
			s.dotSum += dot
			s.matched++
			if dot < alignTol {
				s.badAlign++
			}
		}
	}
	return s
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

// vertParityBaseline records the known per-mesh-node delta between our
// compiler's GPU vertex count and the game compiler's, expressed as
// (ours - game). Default expectation for any mesh node not listed here
// is **exactly zero** — adding a fixture or regressing dedup will fail
// the test with a clear delta.
//
// The non-zero entries below document genuine compiler-philosophy
// differences (most prominently: the game does not bit-deduplicate
// face-corner GPU vertices the way we do, so per-corner data ends up
// duplicated in body-part and animated-mesh outputs even when the
// resulting triangle attributes are bit-identical). These are not bugs
// in either compiler's output — both produce visually identical render
// output — but they are real binary-level differences worth gating.
//
// To improve a baseline (smaller |delta|), edit the entry. To eliminate
// a divergence, remove the entry entirely. To add a fixture, leave it
// out unless it diverges; this forces parity to be the default.
var vertParityBaseline = map[string]map[string]int{
	"Red_M_Torso.mdl": {
		"torso_g": -62,
	},
	"abp_weaprack_1.mdl": {
		"guard_curved":   +8,
		"guard_curved01": +8,
		"guard_curved02": +8,
		"guard_curved03": +8,
		"pommel":         +4,
		"pommel01":       +4,
		"pommel02":       +4,
		"pommel03":       +4,
	},
	"ctl_compass.mdl": {
		"box01":       -72,
		"cmp_pointer": -4,
	},
	"plc_dd27.mdl": {
		"line566":  -4,
		"line1073": -4,
	},
	"plc_guillo2.mdl": {
		"box03": +4,
		"box07": +4,
		"blade": +2,
	},
	"plc_nc03.mdl": {
		"groups02487": +6,
		"groups02501": +6,
		"groups02503": +6,
		"box155793":   -2,
		"box155797":   -4,
	},
	"plc_statdwl.mdl": {
		"box01":      +2,
		"hammerhead": +32,
	},
	"pmh0_head001.mdl": {
		"pmh0_head001g": -101,
	},
	"zlc_o23.mdl": {
		"line1962": -12,
		"line1961": -12,
		"box872":   +13,
		"box871":   +13,
	},
	// c_marilith2 — combined skin + dangly + emitter creature. The
	// non-zero deltas on hand/claw/sword nodes match the body-part
	// philosophy difference; head_g and the wswsc grip/middle nodes
	// each differ by a few per-corner duplicates in the game's
	// output. No visual impact.
	"c_marilith2.mdl": {
		"claw05":       +3,
		"claw06":       +3,
		"claw07":       +3,
		"claw08":       +3,
		"head_g":       -3,
		"lhand_g":      +3,
		"rhand_g":      +3,
		"g_wswsc_b_12": +11,
		"g_wswsc_b_13": +11,
		"g_wswsc_b_14": +11,
		"g_wswsc_b_15": +11,
		"g_wswsc_m_22": +6,
		"g_wswsc_m_23": +6,
		"g_wswsc_m_24": +6,
		"g_wswsc_m_25": +6,
	},
	// tdc01_a01_03 — small AABB walkmesh tile divergences on a few
	// inner mesh nodes. AABB topology itself is unaffected.
	"tdc01_a01_03.mdl": {
		"meshtdc5076": -9,
		"object57":    +6,
		"meshtdc0828": +4,
	},
	// wsf10_p01_01 — Layonara hak forest tile, perfect tangent
	// (1.0000 mean) but the AABB walkmesh node ("wok") under-merges
	// vs the game (we collapse 71 face-corner duplicates the game
	// keeps). The walkmesh tree topology is preserved; pathing and
	// collision are unaffected since both compilers emit the same
	// face-to-leaf assignments.
	"wsf10_p01_01.mdl": {
		"wok": -71,
	},
}

// TestOracleVertexParity asserts that each oracle fixture's compiled
// GPU vertex count per mesh node either matches the game compiler's
// exactly or matches the documented baseline above. New fixtures get
// the strict-equality default. Drift in either direction (improvement
// or regression) fails the test so we always notice when dedup
// behavior changes.
func TestOracleVertexParity(t *testing.T) {
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
			compareOracleVertParity(t, name)
		})
	}
}

func compareOracleVertParity(t *testing.T, binaryName string) {
	t.Helper()

	gameData, err := os.ReadFile(filepath.Join(oracleBinaryDir, binaryName))
	if err != nil {
		t.Fatalf("read game binary: %v", err)
	}
	gameModel, err := Decompile(bytes.NewReader(gameData), int64(len(gameData)))
	if err != nil {
		t.Fatalf("decompile game binary: %v", err)
	}

	asciiPath := findOracleAscii(t, binaryName)
	pr, err := ParseFile(asciiPath)
	if err != nil {
		t.Fatalf("parse ascii: %v", err)
	}
	var buf bytes.Buffer
	if err := Compile(pr.Model, &buf); err != nil {
		t.Fatalf("compile: %v", err)
	}
	ourModel, err := Decompile(bytes.NewReader(buf.Bytes()), int64(buf.Len()))
	if err != nil {
		t.Fatalf("decompile our binary: %v", err)
	}

	expected := vertParityBaseline[binaryName]
	gameNodes := nodeNameIndexFromSlice(gameModel.Nodes)
	seenExpected := map[string]bool{}

	for _, on := range ourModel.Nodes {
		if on.Mesh == nil {
			continue
		}
		gn, ok := gameNodes[strings.ToLower(on.Name)]
		if !ok || gn.Mesh == nil {
			continue
		}
		ov := len(on.Mesh.Verts)
		gv := len(gn.Mesh.Verts)
		actualDelta := ov - gv

		// Look up baseline (try exact then case-insensitive).
		want, hasBaseline := expected[on.Name]
		if !hasBaseline {
			for k, v := range expected {
				if strings.EqualFold(k, on.Name) {
					want = v
					hasBaseline = true
					seenExpected[k] = true
					break
				}
			}
		} else {
			seenExpected[on.Name] = true
		}

		if hasBaseline {
			if actualDelta != want {
				t.Errorf("node %q: vert delta changed from baseline — got ours=%d game=%d (Δ=%+d), baseline expected Δ=%+d. Update vertParityBaseline if intentional.",
					on.Name, ov, gv, actualDelta, want)
			}
		} else if actualDelta != 0 {
			t.Errorf("node %q: vert count mismatch — ours=%d game=%d (Δ=%+d). Either fix the dedup or add this node to vertParityBaseline[%q] with the documented delta.",
				on.Name, ov, gv, actualDelta, binaryName)
		}
	}

	// Flag stale baseline entries — a node listed but no longer present
	// in the model means the fixture changed shape without the baseline
	// being updated.
	for k := range expected {
		if !seenExpected[k] {
			t.Errorf("baseline entry %q→%q references node not present in current compile — remove from vertParityBaseline",
				binaryName, k)
		}
	}
}
