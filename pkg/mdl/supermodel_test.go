package mdl

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"
	"time"
)

func binaryNodeNumbers(t *testing.T, data []byte) map[string]int32 {
	t.Helper()
	m, err := Decompile(bytes.NewReader(data), int64(len(data)))
	if err != nil {
		t.Fatalf("decompile: %v", err)
	}
	out := map[string]int32{}
	for _, n := range m.Nodes {
		out[n.Name] = n.PartNumber
	}
	return out
}

func headerCountNodes(data []byte) uint32 {
	o := 12 + 8 + 64 + 4
	return uint32(data[o]) | uint32(data[o+1])<<8 | uint32(data[o+2])<<16 | uint32(data[o+3])<<24
}

// TestSupermodelNodeNumbersMatchEngine compiles c_marilith2 against its binary
// supermodel c_marilithe and requires every node number and the header's
// count_nodes to equal what the in-game compiler wrote. Without the supermodel
// we number 1..N and none of the supermodel-matched nodes line up.
func TestSupermodelNodeNumbersMatchEngine(t *testing.T) {
	root := filepath.Join("..", "..", "tests", "fixtures", "oracle")
	engine, err := os.ReadFile(filepath.Join(root, "game_binary", "c_marilith2.mdl"))
	if err != nil {
		t.Skip("oracle fixtures not available")
	}
	res, err := ParseFile(filepath.Join(root, "ascii", "c_marilith2.mdl"))
	if err != nil {
		t.Fatal(err)
	}

	var buf bytes.Buffer
	opts := CompileOptions{SupermodelDirs: []string{filepath.Join(root, "game_binary")}}
	if err := CompileWithOptions(res.Model, &buf, opts); err != nil {
		t.Fatal(err)
	}
	want, got := binaryNodeNumbers(t, engine), binaryNodeNumbers(t, buf.Bytes())
	bad := 0
	for name, n := range want {
		if got[name] != n {
			if bad < 5 {
				t.Errorf("node %s: number %d, engine wrote %d", name, got[name], n)
			}
			bad++
		}
	}
	if bad > 0 {
		t.Errorf("%d of %d node numbers differ from the engine's", bad, len(want))
	}
	if g, w := headerCountNodes(buf.Bytes()), headerCountNodes(engine); g != w {
		t.Errorf("count_nodes = %d, engine wrote %d", g, w)
	}

	// Without a supermodel to read, numbering stays sequential (the old behaviour).
	var plain bytes.Buffer
	if err := Compile(res.Model, &plain); err != nil {
		t.Fatal(err)
	}
	if headerCountNodes(plain.Bytes()) == headerCountNodes(engine) {
		t.Error("compile without a supermodel unexpectedly matched the engine's count_nodes")
	}
}

const superASCII = `newmodel sup
setsupermodel sup NULL
classification CHARACTER
setanimationscale 1.0
beginmodelgeom sup
  node dummy sup
    parent NULL
  endnode
  node dummy hips
    parent sup
  endnode
  node dummy spine
    parent hips
  endnode
endmodelgeom
donemodel sup
`

const childASCII = `newmodel kid
setsupermodel kid sup
classification CHARACTER
setanimationscale 1.0
beginmodelgeom kid
  node dummy kid
    parent NULL
  endnode
  node dummy hips
    parent kid
  endnode
  node dummy spine
    parent hips
  endnode
  node dummy extra
    parent spine
  endnode
  node dummy extra_child
    parent extra
  endnode
  node dummy moved
    parent kid
  endnode
endmodelgeom
donemodel kid
`

// TestSupermodelASCIINumbering pins the rule against an ASCII supermodel,
// numbered root 0 then tree order with count_nodes = its node count (3):
// matched nodes take the supermodel's number, an unmatched child of a matched
// node is -1, and an unmatched child of an unmatched node is
// (count+1) + its tree position.
func TestSupermodelASCIINumbering(t *testing.T) {
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "SUP.MDL"), []byte(superASCII), 0o644); err != nil { // case differs on purpose
		t.Fatal(err)
	}
	res, err := Parse(bytes.NewReader([]byte(childASCII)))
	if err != nil {
		t.Fatal(err)
	}

	var buf bytes.Buffer
	if err := CompileWithOptions(res.Model, &buf, CompileOptions{SupermodelDirs: []string{dir}}); err != nil {
		t.Fatal(err)
	}
	got := binaryNodeNumbers(t, buf.Bytes())
	// Tree order: kid(0) hips(1) spine(2) extra(3) extra_child(4) moved(5); base = 3+1.
	want := map[string]int32{
		"kid":         0,
		"hips":        1,  // in supermodel, same parent
		"spine":       2,  // in supermodel, same parent
		"extra":       -1, // not in supermodel, parent matched
		"extra_child": 4 + 4,
		"moved":       -1, // not in supermodel, parent is the root
	}
	for name, n := range want {
		if got[name] != n {
			t.Errorf("node %s = %d, want %d", name, got[name], n)
		}
	}
	// 6 nodes + 1 + supermodel count 3.
	if c := headerCountNodes(buf.Bytes()); c != 10 {
		t.Errorf("count_nodes = %d, want 10", c)
	}
}

// TestSupermodelNotFoundWarns: a supermodel that was asked for but is absent
// falls back to sequential numbering and says so.
func TestSupermodelNotFoundWarns(t *testing.T) {
	res, err := Parse(bytes.NewReader([]byte(childASCII)))
	if err != nil {
		t.Fatal(err)
	}
	var warned string
	var buf bytes.Buffer
	opts := CompileOptions{SupermodelDirs: []string{t.TempDir()}, Warn: func(m string) { warned = m }}
	if err := CompileWithOptions(res.Model, &buf, opts); err != nil {
		t.Fatal(err)
	}
	if warned == "" {
		t.Error("no warning for a missing supermodel")
	}
	if c := headerCountNodes(buf.Bytes()); c != 6 {
		t.Errorf("fallback count_nodes = %d, want 6 (sequential, no supermodel)", c)
	}
}

// A supermodel is parsed once per process and search path, and again if its
// file changes.
func TestSupermodelIsCached(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "cachesuper.mdl")
	write := func(extra string) {
		src := "newmodel cachesuper\nsetsupermodel cachesuper NULL\nclassification character\nbeginmodelgeom cachesuper\nnode dummy cachesuper\n  parent NULL\nendnode\n" + extra + "endmodelgeom cachesuper\ndonemodel cachesuper\n"
		if err := os.WriteFile(path, []byte(src), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	load := func() *supermodelInfo {
		ref := findModelResource("cachesuper", []string{dir})
		if ref == nil {
			t.Fatal("supermodel not found")
		}
		sm, err := loadSupermodel(ref, []string{dir}, map[string]bool{})
		if err != nil {
			t.Fatal(err)
		}
		return sm
	}

	write("")
	first := load()
	if load() != first {
		t.Fatal("second load of an unchanged supermodel was not served from the cache")
	}
	write("node dummy extra\n  parent cachesuper\nendnode\n")
	if err := os.Chtimes(path, time.Now().Add(time.Hour), time.Now().Add(time.Hour)); err != nil {
		t.Fatal(err)
	}
	if again := load(); again == first || len(again.nodes) != 2 {
		t.Fatalf("changed supermodel was not reloaded: %d nodes", len(again.nodes))
	}
}
