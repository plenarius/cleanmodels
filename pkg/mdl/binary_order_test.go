package mdl

import (
	"bytes"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
)

// recompile writes model as ASCII, parses it back and compiles it the way
// the engine fixtures are compiled.
func recompile(t *testing.T, model *Model, dirs []string) []byte {
	t.Helper()
	var text bytes.Buffer
	if err := Write(model, &text); err != nil {
		t.Fatal(err)
	}
	res, err := Parse(strings.NewReader(text.String()))
	if err != nil {
		t.Fatal(err)
	}
	var out bytes.Buffer
	if err := CompileWithOptions(res.Model, &out, CompileOptions{SupermodelDirs: dirs}); err != nil {
		t.Fatal(err)
	}
	return out.Bytes()
}

// storedTree lists a binary's geometry nodes as stored: name, parent and
// part number, depth first.
func storedTree(t *testing.T, data []byte) []string {
	t.Helper()
	m, _, err := decompileStored(bytes.NewReader(data), int64(len(data)), DecompileOptions{})
	if err != nil {
		t.Fatal(err)
	}
	var out []string
	for _, n := range m.Nodes {
		out = append(out, n.Parent+"/"+n.Name+"#"+strconv.Itoa(int(n.PartNumber)))
	}
	return out
}

func sameTree(t *testing.T, want, got []string, numbers bool) {
	t.Helper()
	if len(want) != len(got) {
		t.Fatalf("%d nodes, want %d", len(got), len(want))
	}
	for i := range want {
		w, g := want[i], got[i]
		if !numbers {
			w, g = w[:strings.LastIndex(w, "#")], g[:strings.LastIndex(g, "#")]
		}
		if w != g {
			t.Fatalf("stored node %d: got %s, want %s", i, g, w)
		}
	}
}

// decompileRenumbers are the engine goldens whose part numbers no declaration
// order the decompiler can choose gives; their tree still comes back.
var decompileRenumbers = map[string]string{
	"zcm_kidx":      "numbered against a supermodel from a declaration order the binary does not record",
	"zspec_numbers": "a node left out of the tree took a number (gaps)",
}

// TestDecompileOrderRebuildsEngineGoldens decompiles each of the game's own
// compiles in tests/fixtures/engine and compiles the ASCII again: the stored
// tree and the part numbers must come back. zcm_tsort24/50/90 sort more than
// 16 children (the inverse sort, or part-number order), zspec_nodeorder was
// declared out of tree order (part-number order).
func TestDecompileOrderRebuildsEngineGoldens(t *testing.T) {
	dir := filepath.Join("..", "..", "tests", "fixtures", "engine")
	probes, err := filepath.Glob(filepath.Join(dir, "*.ascii.mdl"))
	if err != nil || len(probes) == 0 {
		t.Fatalf("no probes in %s: %v", dir, err)
	}
	for _, probe := range probes {
		name := strings.TrimSuffix(filepath.Base(probe), ".ascii.mdl")
		t.Run(name, func(t *testing.T) {
			golden, err := os.ReadFile(filepath.Join(dir, name+".mdl"))
			if err != nil {
				t.Fatal(err)
			}
			again := recompile(t, mustDecompile(t, golden), []string{dir})
			_, renumbered := decompileRenumbers[name]
			sameTree(t, storedTree(t, golden), storedTree(t, again), !renumbered)
		})
	}
}

// TestDecompileOrderInverseSort is the inverse sort on its own: zcm_tsort50
// with a supermodel that can't be found, so part-number order is not tried.
// Declared in stored order, the 50 children would be sorted again and come
// back shuffled.
func TestDecompileOrderInverseSort(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "..", "tests", "fixtures", "engine", "zcm_tsort50.ascii.mdl"))
	if err != nil {
		t.Fatal(err)
	}
	text := strings.Replace(string(src), "setsupermodel zcm_tsort50 NULL", "setsupermodel zcm_tsort50 zz_missing", 1)
	res, err := Parse(strings.NewReader(text))
	if err != nil {
		t.Fatal(err)
	}
	var first bytes.Buffer
	if err := Compile(res.Model, &first); err != nil {
		t.Fatal(err)
	}
	m := mustDecompile(t, first.Bytes())
	if partNumberOrder(m) != nil {
		t.Fatal("part-number order tried with a supermodel")
	}
	again := recompile(t, m, nil)
	sameTree(t, storedTree(t, first.Bytes()), storedTree(t, again), false)
}

// TestDecompileOrderKeepsUnsorted: a stored order the sort can't give
// (a hint-1 mesh before a dummy) stays as stored.
func TestDecompileOrderKeepsUnsorted(t *testing.T) {
	mesh := func(name string, hint int32) *Node {
		md := NewMeshData()
		md.TransparencyHint = hint
		return &Node{Name: name, Parent: "root", Mesh: md, PartNumber: 0}
	}
	nodes := []*Node{{Name: "root", Parent: "NULL"}, mesh("a", 1), {Name: "b", Parent: "root"}}
	for i := 0; i < 20; i++ {
		nodes = append(nodes, mesh("m"+strconv.Itoa(i), 0))
	}
	m := &Model{Nodes: append([]*Node(nil), nodes...), SuperModel: "NULL"}
	declarationOrder(m, nil)
	for i := range nodes {
		if m.Nodes[i] != nodes[i] {
			t.Fatalf("node %d: %s, want %s (stored order)", i, m.Nodes[i].Name, nodes[i].Name)
		}
	}
}
