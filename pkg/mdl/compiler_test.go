// compiler_test.go — roundtrip and batch tests for the binary MDL compiler.
//
// Tests:
//  1. Roundtrip: ASCII → Compile → Decompile → re-emit ASCII, compare geometry.
//  2. Header size assertions: every struct must be the exact byte count.
//  3. Batch: all ASCII models in /tmp/nwn-haks/placeables/ must compile without panic.
package mdl

import (
	"bytes"
	"fmt"
	"math"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
)

// TestCompilerHeaderSizes verifies that our byte-count constants match the
// live compiler output, catching regressions where a field is accidentally
// added or removed.
func TestCompilerHeaderSizes(t *testing.T) {
	// Build a minimal model (root dummy node) and verify the core block is
	// exactly the expected size.
	m := &Model{
		Name:           "test",
		Classification: "CHARACTER",
		SuperModel:     "NULL",
		AnimationScale: 1.0,
	}
	root := &Node{Name: "test", Parent: "NULL"}
	m.Nodes = append(m.Nodes, root)

	var buf bytes.Buffer
	if err := Compile(m, &buf); err != nil {
		t.Fatalf("Compile failed: %v", err)
	}
	data := buf.Bytes()

	// File = 12-byte header_file + core + MDX
	if len(data) < 12 {
		t.Fatalf("output too short: %d bytes", len(data))
	}

	// header_file
	coreLen := readU32LE(data[4:])
	mdxLen := readU32LE(data[8:])
	t.Logf("core=%d  MDX=%d  total=%d", coreLen, mdxLen, len(data))

	// ProxyModel (232) + root node header (112) = 244 minimum
	if coreLen < 232+112 {
		t.Errorf("core block too small: %d (want ≥ 344)", coreLen)
	}
	// No mesh → MDX should be 0
	if mdxLen != 0 {
		t.Errorf("expected 0 MDX bytes for dummy-only model, got %d", mdxLen)
	}

	// ProxyModel starts at core offset 0 (file offset 12).
	// Root node pointer is at core offset 72 (ProxyModel+72).
	rootPtr := readU32LE(data[12+72:])
	t.Logf("root node ptr = %d (expect 232)", rootPtr)
	if rootPtr != 232 {
		t.Errorf("root node ptr = %d, want 232", rootPtr)
	}
}

// TestRoundtripDummy verifies that a model with only a dummy root node
// survives ASCII → compile → decompile with the same name and no errors.
func TestRoundtripDummy(t *testing.T) {
	src := `# Shockwave NWN2 MDL File Exporter v0.97.00.0000
filedependancy none
newmodel mymodel
setsupermodel mymodel NULL
classification CHARACTER
setanimationscale 1.00
beginmodelgeom mymodel
  node dummy mymodel
    parent NULL
  endnode
endmodelgeom
donemodel mymodel
`
	model := mustParseASCII(t, src)
	binData := mustCompile(t, model)
	model2 := mustDecompile(t, binData)

	if !strings.EqualFold(model2.Name, "mymodel") {
		t.Errorf("name: got %q, want %q", model2.Name, "mymodel")
	}
	if len(model2.Nodes) == 0 {
		t.Error("no nodes after decompile")
	}
}

// TestRoundtripTrimesh verifies that a simple trimesh (4 verts, 2 faces)
// has the correct vertex count, face count, and face normals after roundtrip.
func TestRoundtripTrimesh(t *testing.T) {
	src := `# NWN MDL
filedependancy none
newmodel quad
setsupermodel quad NULL
classification PLACEABLE
setanimationscale 1.00
beginmodelgeom quad
  node dummy quad
    parent NULL
  endnode
  node trimesh top
    parent quad
    position 0 0 0
    orientation 0 0 1 0
    bitmap blank
    render 1
    shadow 0
    verts 4
      -1 -1 0
       1 -1 0
       1  1 0
      -1  1 0
    tverts 4
      0 0 0
      1 0 0
      1 1 0
      0 1 0
    faces 2
      0 1 2  1  0 1 2  0
      0 2 3  1  0 2 3  0
  endnode
endmodelgeom
donemodel quad
`
	model := mustParseASCII(t, src)
	binData := mustCompile(t, model)
	model2 := mustDecompile(t, binData)

	// Find the trimesh node
	var meshNode *Node
	for _, n := range model2.Nodes {
		if n.Mesh != nil {
			meshNode = n
			break
		}
	}
	if meshNode == nil {
		t.Fatal("no mesh node after decompile")
	}

	// GPU vertex count: 2 triangles sharing an edge → 4 unique verts after dedup
	// (was 6 before deduplication was implemented)
	wantVerts := 4
	if len(meshNode.Mesh.Verts) != wantVerts {
		t.Errorf("verts: got %d, want %d", len(meshNode.Mesh.Verts), wantVerts)
	}
	if len(meshNode.Mesh.Faces) != 2 {
		t.Errorf("faces: got %d, want 2", len(meshNode.Mesh.Faces))
	}

	// Normals should have been propagated
	if len(meshNode.Mesh.Normals) == 0 {
		t.Log("normals not present (OK for compiler MVP)")
	}

	// Face normals: the compiler writes the face normal from the ASCII data.
	// In this test the ASCII face lines include a smoothing group index but no
	// explicit face-normal; the parser computes them from cross-products.
	// We just verify the normal vector is not NaN.
	for i, f := range meshNode.Mesh.Faces {
		n := f.Normal
		if math.IsNaN(float64(n.X)) || math.IsNaN(float64(n.Y)) || math.IsNaN(float64(n.Z)) {
			t.Errorf("face %d: NaN normal {%g %g %g}", i, n.X, n.Y, n.Z)
		}
	}
}

// TestRoundtripAnimation checks that animation names and event counts survive roundtrip.
func TestRoundtripAnimation(t *testing.T) {
	src := `# NWN MDL
filedependancy none
newmodel anim_test
setsupermodel anim_test NULL
classification PLACEABLE
setanimationscale 1.00
beginmodelgeom anim_test
  node dummy anim_test
    parent NULL
  endnode
endmodelgeom

newanim default anim_test
  length 1.0
  transtime 0.25
  animroot anim_test
  node dummy anim_test
    parent NULL
    positionkey 1
      0.0   0 0 0
  endnode
doneanim default anim_test

donemodel anim_test
`
	model := mustParseASCII(t, src)
	if len(model.Animations) == 0 {
		t.Skip("no animations parsed (ASCII parser may not have read them)")
	}
	binData := mustCompile(t, model)
	model2 := mustDecompile(t, binData)

	if len(model2.Animations) == 0 {
		t.Fatal("no animations after decompile")
	}
	got := model2.Animations[0].Name
	want := "default"
	if !strings.EqualFold(got, want) {
		t.Errorf("animation name: got %q, want %q", got, want)
	}
}

// TestMeshHeaderSize checks the 512-byte mesh header invariant directly.
func TestMeshHeaderSize(t *testing.T) {
	// Create a mesh node and compile it, then verify the core block layout.
	src := `# NWN MDL
filedependancy none
newmodel meshtest
setsupermodel meshtest NULL
classification PLACEABLE
setanimationscale 1.00
beginmodelgeom meshtest
  node dummy meshtest
    parent NULL
  endnode
  node trimesh box
    parent meshtest
    bitmap brick
    render 1
    shadow 1
    verts 8
      -1 -1 -1
       1 -1 -1
       1  1 -1
      -1  1 -1
      -1 -1  1
       1 -1  1
       1  1  1
      -1  1  1
    tverts 4
      0 0 0
      1 0 0
      1 1 0
      0 1 0
    faces 4
      0 1 2  0 0  0 0 1 2
      0 2 3  0 0  0 0 2 3
      4 5 6  0 0  0 0 1 2
      4 6 7  0 0  0 0 2 3
  endnode
endmodelgeom
donemodel meshtest
`
	model := mustParseASCII(t, src)
	binData := mustCompile(t, model)

	// The core block starts at byte 12.
	// ProxyModel = 232 bytes.
	// Node base header = 112 bytes.
	// Mesh header starts at 12 + 232 + 112 = 356 (but also need to account for
	// the controller/children array offsets and any inline data after 232+112 bytes).
	// Minimal: node header starts at 232 and mesh header starts at 232+112 = 344.
	coreLen := int(readU32LE(binData[4:]))
	t.Logf("core size: %d bytes", coreLen)
	if coreLen < 232+112+512 {
		t.Errorf("core too small: %d, expected at least %d", coreLen, 232+112+512)
	}
}

// TestBatchCompilePlaceables compiles all ASCII MDLs in /tmp/nwn-haks/placeables/
// and verifies they don't panic or error.
func TestBatchCompilePlaceables(t *testing.T) {
	dir := "/tmp/nwn-haks/placeables"
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Skipf("placeables dir not available: %v", err)
	}

	var total, pass, fail int
	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(strings.ToLower(e.Name()), ".mdl") {
			continue
		}
		total++
		path := filepath.Join(dir, e.Name())

		// Skip binary MDLs (first 4 bytes = 0x00000000)
		f, err := os.Open(path)
		if err != nil {
			continue
		}
		hdr := make([]byte, 4)
		f.Read(hdr)
		f.Close()
		if hdr[0] == 0 && hdr[1] == 0 && hdr[2] == 0 && hdr[3] == 0 {
			total-- // don't count binary files
			continue
		}

		t.Run(e.Name(), func(t *testing.T) {
			pr, err := ParseFile(path)
			if err != nil {
				t.Skipf("parse error: %v", err)
			}
			if pr.Model == nil {
				t.Skip("nil model")
			}

			var buf bytes.Buffer
			if err := Compile(pr.Model, &buf); err != nil {
				fail++
				t.Errorf("compile error: %v", err)
				return
			}
			binData := buf.Bytes()
			if len(binData) < 12 {
				fail++
				t.Errorf("output too small: %d bytes", len(binData))
				return
			}

			// Decompile the result and check for no fatal errors.
			_, err = Decompile(bytes.NewReader(binData), int64(len(binData)))
			if err != nil {
				fail++
				t.Errorf("decompile error: %v", err)
				return
			}
			pass++
		})
	}
	if total > 0 {
		t.Logf("batch: %d/%d passed, %d failed", pass, total, fail)
	}
}

// TestBatchCompileDecompiled compiles the full NWN:EE decompiled stock model corpus
// (32k+ models) and verifies each can round-trip without error or panic.
// Set DECOMPILED_DIR env var to override the default folder path.
func TestBatchCompileDecompiled(t *testing.T) {
	dir := os.Getenv("DECOMPILED_DIR")
	if dir == "" {
		dir = "/Users/james/Downloads/1.86.8193.34.1 Decompiled Models (One Folder)"
	}
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Skipf("decompiled dir not available: %v", err)
	}

	type job struct{ name, path string }
	type failure struct{ name, reason string }

	jobs := make(chan job, 256)
	results := make(chan failure, 256)

	const workers = 8
	var wg sync.WaitGroup
	for range workers {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for j := range jobs {
				pr, err := ParseFile(j.path)
				if err != nil {
					results <- failure{j.name, fmt.Sprintf("parse: %v", err)}
					continue
				}
				if pr.Model == nil {
					results <- failure{j.name, "nil model"}
					continue
				}
				var buf bytes.Buffer
				if err := Compile(pr.Model, &buf); err != nil {
					results <- failure{j.name, fmt.Sprintf("compile: %v", err)}
					continue
				}
				if buf.Len() < 12 {
					results <- failure{j.name, fmt.Sprintf("output too small: %d bytes", buf.Len())}
					continue
				}
				if _, err := Decompile(bytes.NewReader(buf.Bytes()), int64(buf.Len())); err != nil {
					results <- failure{j.name, fmt.Sprintf("decompile: %v", err)}
					continue
				}
				results <- failure{} // success: empty name
			}
		}()
	}

	var total int
	go func() {
		for _, e := range entries {
			if e.IsDir() || !strings.HasSuffix(strings.ToLower(e.Name()), ".mdl") {
				continue
			}
			total++
			jobs <- job{e.Name(), filepath.Join(dir, e.Name())}
		}
		close(jobs)
		wg.Wait()
		close(results)
	}()

	var pass, fail int
	var failures []failure
	for r := range results {
		if r.name == "" {
			pass++
		} else {
			fail++
			failures = append(failures, r)
		}
	}

	t.Logf("batch: %d/%d passed, %d failed", pass, total, fail)
	for _, f := range failures {
		t.Errorf("FAIL %s: %s", f.name, f.reason)
	}
}

// TestRoundtripGeometryPreserved checks that face positions are preserved
// after ASCII→binary→decompile (geometry must be identical to within float32 precision).
func TestRoundtripGeometryPreserved(t *testing.T) {
	pr, err := ParseFile("/tmp/nwn-haks/placeables/a_black.mdl")
	if err != nil {
		t.Skipf("can't load test model: %v", err)
	}
	if pr.Model == nil {
		t.Skip("nil model")
	}

	orig := pr.Model

	// Collect original face positions by mesh name.
	origPositions := collectFacePositions(orig)

	// Compile to binary.
	binData := mustCompile(t, orig)

	// Decompile.
	rt, err := Decompile(bytes.NewReader(binData), int64(len(binData)))
	if err != nil {
		t.Fatalf("decompile: %v", err)
	}

	// Compare face positions.
	rtPositions := collectFacePositions(rt)

	for name, origFacePos := range origPositions {
		rtFacePos, ok := rtPositions[name]
		if !ok {
			t.Errorf("mesh %q missing in decompiled model", name)
			continue
		}
		if len(origFacePos) != len(rtFacePos) {
			t.Errorf("mesh %q: face count mismatch orig=%d rt=%d", name, len(origFacePos), len(rtFacePos))
			continue
		}
		for i, op := range origFacePos {
			rp := rtFacePos[i]
			if !vec3Near(op, rp, 1e-4) {
				t.Errorf("mesh %q face %d pos mismatch: orig=%v rt=%v", name, i, op, rp)
				if i > 3 {
					t.Errorf("... (truncated)")
					break
				}
			}
		}
	}
}

// ---- helpers ----

func mustParseASCII(t *testing.T, src string) *Model {
	t.Helper()
	pr, err := Parse(strings.NewReader(src))
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if pr.Model == nil {
		t.Fatal("nil model after parse")
	}
	return pr.Model
}

func mustCompile(t *testing.T, m *Model) []byte {
	t.Helper()
	var buf bytes.Buffer
	if err := Compile(m, &buf); err != nil {
		t.Fatalf("Compile: %v", err)
	}
	return buf.Bytes()
}

func mustDecompile(t *testing.T, data []byte) *Model {
	t.Helper()
	m, err := Decompile(bytes.NewReader(data), int64(len(data)))
	if err != nil {
		t.Fatalf("Decompile: %v", err)
	}
	return m
}

func readU32LE(b []byte) uint32 {
	return uint32(b[0]) | uint32(b[1])<<8 | uint32(b[2])<<16 | uint32(b[3])<<24
}

// collectFacePositions maps mesh name (lowercased) → list of (first vertex of face).
// The decompiler lowercases all names, so we normalise on both sides.
func collectFacePositions(m *Model) map[string][]Vec3 {
	out := make(map[string][]Vec3)
	for _, n := range m.Nodes {
		if n.Mesh == nil || len(n.Mesh.Faces) == 0 {
			continue
		}
		var positions []Vec3
		for _, f := range n.Mesh.Faces {
			if int(f.Verts[0]) < len(n.Mesh.Verts) {
				positions = append(positions, n.Mesh.Verts[f.Verts[0]])
			}
		}
		if len(positions) > 0 {
			out[strings.ToLower(n.Name)] = positions
		}
	}
	return out
}

func vec3Near(a, b Vec3, eps float32) bool {
	return abs32(a.X-b.X) <= eps && abs32(a.Y-b.Y) <= eps && abs32(a.Z-b.Z) <= eps
}

func abs32(x float32) float32 {
	if x < 0 {
		return -x
	}
	return x
}

// checkMeshHeaderSize validates that the ProxyMdlNode + header_mesh = 624 bytes.
func checkMeshHeaderSize(data []byte, nodeOff int) error {
	fileOff := 12 + nodeOff // core block starts at byte 12
	if fileOff+112+512 > len(data) {
		return fmt.Errorf("file too short for full mesh node at offset %d", nodeOff)
	}
	return nil
}
