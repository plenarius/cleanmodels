package mdl

import (
	"os"
	"path/filepath"
	"testing"
)

// fan: three faces on the edge 0-1 (a multiple edge). quad: two faces sharing
// one edge (fine). sliver: two separate pairs on edge 0-1, plus a zero-area face
// (corners 0 and 5 at one position) that names both; it must not join them.
const shadowEdgesSrc = `newmodel se
setsupermodel se NULL
classification PLACEABLE
setanimationscale 1.0
beginmodelgeom se
  node dummy se
    parent NULL
  endnode
  node trimesh fan
    parent se
    bitmap blank
    verts 5
      0 0 0
      1 0 0
      0.5 1 0
      0.5 -1 0
      0.5 0 1
    tverts 1
      0 0 0
    faces 3
      0 1 2  1  0 0 0  0
      1 0 3  1  0 0 0  0
      0 1 4  1  0 0 0  0
  endnode
  node trimesh quad
    parent se
    bitmap blank
    verts 4
      0 0 0
      1 0 0
      1 1 0
      0 1 0
    tverts 1
      0 0 0
    faces 2
      0 1 2  1  0 0 0  0
      0 2 3  1  0 0 0  0
  endnode
  node trimesh noshadow
    parent se
    bitmap blank
    shadow 0
    verts 5
      0 0 0
      1 0 0
      0.5 1 0
      0.5 -1 0
      0.5 0 1
    tverts 1
      0 0 0
    faces 3
      0 1 2  1  0 0 0  0
      1 0 3  1  0 0 0  0
      0 1 4  1  0 0 0  0
  endnode
endmodelgeom
donemodel se
`

func compiledShadowEdges(t *testing.T) ([]byte, *Model) {
	t.Helper()
	data := mustCompile(t, mustParseASCII(t, shadowEdgesSrc))
	return data, mustDecompile(t, data)
}

func TestStoredAdjacencyIsRead(t *testing.T) {
	_, m := compiledShadowEdges(t)
	n := m.FindNode("quad")
	if n == nil || len(n.Mesh.StoredAdjacency) != 2 {
		t.Fatalf("quad: stored adjacency %v", n.Mesh.StoredAdjacency)
	}
	// Faces 0 and 1 share the edge 0-2 (face 0's edge 2, face 1's edge 0).
	want := [][3]int16{{-1, -1, 1}, {0, -1, -1}}
	for i := range want {
		if n.Mesh.StoredAdjacency[i] != want[i] {
			t.Errorf("face %d: got %v want %v", i, n.Mesh.StoredAdjacency[i], want[i])
		}
	}
	if n.Mesh.FacesOffset <= 0 {
		t.Errorf("FacesOffset = %d", n.Mesh.FacesOffset)
	}
	ascii := mustParseASCII(t, shadowEdgesSrc)
	if _, _, ok := StoredMultiEdges(ascii.FindNode("fan").Mesh); ok {
		t.Error("an ASCII model has no stored adjacency, so StoredMultiEdges must say !ok")
	}
}

func TestStoredMultiEdges(t *testing.T) {
	_, m := compiledShadowEdges(t)
	entries, edges, ok := StoredMultiEdges(m.FindNode("fan").Mesh)
	if !ok || edges != 1 || len(entries) != 3 {
		t.Fatalf("fan: ok=%v edges=%d entries=%v, want one edge and its 3 neighbour slots", ok, edges, entries)
	}
	for _, e := range entries {
		if e.Edge != 0 {
			t.Errorf("entry %v: the shared edge is each face's edge 0", e)
		}
	}
	if _, edges, ok := StoredMultiEdges(m.FindNode("quad").Mesh); !ok || edges != 0 {
		t.Errorf("quad: ok=%v edges=%d, want none", ok, edges)
	}
}

// A zero-area face names two faces that are each a proper pair across the same
// edge; it must not join them into one group of three.
func TestStoredMultiEdgesSkipsZeroArea(t *testing.T) {
	mesh := &MeshData{
		Verts: []Vec3{{0, 0, 0}, {1, 0, 0}, {0.5, 1, 0}, {0.5, -1, 0}, {0.5, 0, 1}, {0, 0, 0}},
		Faces: []Face{
			{Verts: [3]int32{0, 1, 2}}, // pair A
			{Verts: [3]int32{1, 0, 3}},
			{Verts: [3]int32{0, 1, 5}}, // zero area: 0 and 5 are one position
		},
		StoredAdjacency: [][3]int16{{1, -1, -1}, {0, -1, -1}, {0, 1, 1}},
	}
	if _, edges, ok := StoredMultiEdges(mesh); !ok || edges != 0 {
		t.Errorf("ok=%v edges=%d, want no multiple edge", ok, edges)
	}
}

func TestOpenMultiEdgesBinary(t *testing.T) {
	data, m := compiledShadowEdges(t)
	out, msgs, err := OpenMultiEdgesBinary(data, m)
	if err != nil {
		t.Fatal(err)
	}
	if len(msgs) != 1 {
		t.Errorf("messages %q, want one (fan; noshadow casts no shadow)", msgs)
	}
	if len(out) != len(data) {
		t.Fatalf("size changed: %d -> %d", len(data), len(out))
	}
	diff := 0
	for i := range data {
		if data[i] != out[i] {
			diff++
		}
	}
	if diff != 6 { // three 2-byte slots
		t.Errorf("%d bytes changed, want 6", diff)
	}
	p := mustDecompile(t, out)
	if _, edges, _ := StoredMultiEdges(p.FindNode("fan").Mesh); edges != 0 {
		t.Errorf("after opening: %d multiple edges left", edges)
	}
	for i, a := range p.FindNode("fan").Mesh.StoredAdjacency {
		if a[0] != -1 {
			t.Errorf("fan face %d edge 0: %d, want -1", i, a[0])
		}
	}
	if _, edges, _ := StoredMultiEdges(p.FindNode("noshadow").Mesh); edges != 1 {
		t.Errorf("noshadow casts no shadow and must be left alone; edges %d", edges)
	}
	ascii := mustParseASCII(t, shadowEdgesSrc)
	ascii.FileType = "ascii"
	if _, _, err := OpenMultiEdgesBinary(nil, ascii); err == nil {
		t.Error("an ASCII model must be refused")
	}
}

// The reader keeps what the game's binaries store.
func TestStoredAdjacencyMatchesGameBinaries(t *testing.T) {
	compared := 0
	for _, name := range adjacencyStrictFixtures {
		binPath := filepath.Join(oracleBinaryDir, name)
		raw, err := os.ReadFile(binPath)
		if err != nil {
			t.Skipf("no game_binary fixtures: %v", err)
		}
		game, err := DecompileFile(binPath)
		if err != nil {
			t.Fatalf("%s: %v", name, err)
		}
		for _, n := range game.Nodes {
			if n.Mesh == nil || len(n.Mesh.Faces) == 0 {
				continue
			}
			want, ok := findGameFaceAdjacency(raw, n.Mesh.Faces)
			if !ok {
				continue
			}
			compared++
			for i := range want {
				for k := 0; k < 3; k++ {
					if uint16(n.Mesh.StoredAdjacency[i][k]) != want[i][k] {
						t.Fatalf("%s %q face %d: stored %v, file %v", name, n.Name, i, n.Mesh.StoredAdjacency[i], want[i])
					}
				}
			}
		}
	}
	if compared == 0 {
		t.Skip("no nodes compared")
	}
}
