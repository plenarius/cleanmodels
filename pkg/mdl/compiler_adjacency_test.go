package mdl

import (
	"bytes"
	"encoding/binary"
	"os"
	"path/filepath"
	"testing"
)

func TestBuildFaceAdjacencyBasics(t *testing.T) {
	// Two triangles sharing the edge 1-2, plus one touching face 0 only at vertex 0.
	faces := []Face{
		{Verts: [3]int32{0, 1, 2}},
		{Verts: [3]int32{1, 3, 2}},
		{Verts: [3]int32{0, 4, 5}},
	}
	got := buildFaceAdjacency(faces, 3)
	want := [][3]uint16{
		{0xFFFF, 1, 0xFFFF}, // edge 1-2 is shared with face 1; 0-1 and 2-0 are free
		{0xFFFF, 0xFFFF, 0},
		{0xFFFF, 0xFFFF, 0xFFFF},
	}
	for i := range want {
		if got[i] != want[i] {
			t.Errorf("face %d: got %v want %v", i, got[i], want[i])
		}
	}
}

// The lowest-numbered neighbour wins when an edge has more than two faces.
func TestBuildFaceAdjacencyLowestNeighbourWins(t *testing.T) {
	faces := []Face{
		{Verts: [3]int32{0, 1, 2}},
		{Verts: [3]int32{1, 0, 3}},
		{Verts: [3]int32{0, 1, 4}},
	}
	got := buildFaceAdjacency(faces, 3)
	if got[2][0] != 0 || got[0][0] != 1 {
		t.Errorf("got %v", got)
	}
}

// After degenerate faces are removed the game still searches the first n
// source faces by position, so numbers refer to source numbering.
func TestBuildFaceAdjacencyOnlySearchesFirstN(t *testing.T) {
	faces := []Face{
		{Verts: [3]int32{0, 1, 2}},
		{Verts: [3]int32{9, 8, 7}},
		{Verts: [3]int32{1, 0, 3}}, // would be face 0's neighbour, but n=2 excludes it
	}
	got := buildFaceAdjacency(faces, 2)
	if len(got) != 2 || got[0][0] != 0xFFFF {
		t.Errorf("got %v", got)
	}
}

// findGameFaceAdjacency locates a node's face array inside a game binary and
// returns the three adjacency slots of every face. The decompiled faces carry
// the GPU indices the array stores at record offset 26.
func findGameFaceAdjacency(raw []byte, faces []Face) ([][3]uint16, bool) {
	if len(faces) == 0 {
		return nil, false
	}
	pat := make([]byte, 6)
	for i := 0; i < 3; i++ {
		binary.LittleEndian.PutUint16(pat[i*2:], uint16(faces[0].Verts[i]))
	}
	check := min(len(faces), 4)
	for off := 0; ; {
		p := bytes.Index(raw[off:], pat)
		if p < 0 {
			return nil, false
		}
		p += off
		base := p - 26
		ok := base >= 0
		for i := 0; ok && i < check; i++ {
			rec := base + 32*i
			if rec+32 > len(raw) {
				ok = false
				break
			}
			for c := 0; c < 3; c++ {
				if binary.LittleEndian.Uint16(raw[rec+26+2*c:]) != uint16(faces[i].Verts[c]) {
					ok = false
				}
			}
		}
		if ok && base+32*len(faces) <= len(raw) {
			out := make([][3]uint16, len(faces))
			for i := range faces {
				for c := 0; c < 3; c++ {
					out[i][c] = binary.LittleEndian.Uint16(raw[base+32*i+20+2*c:])
				}
			}
			return out, true
		}
		off = p + 1
	}
}

// Fixtures whose game binary was compiled from exactly this ASCII by the EE
// compiler, so every node's adjacency must match. The remaining fixtures are
// base-game assets built by the legacy tool (their adjacency was computed
// before faces were removed and holds out-of-range numbers) or ASCII/binary
// pairs from different revisions, so they cannot be compared.
var adjacencyStrictFixtures = []string{
	"tin01_d11_01.mdl", "wsf10_p01_01.mdl", "plc_dd27.mdl", "zlc_o23.mdl",
	"c_marilithe.mdl", "plc_statdwl.mdl", "plc_crysblu.mdl", "BUT_CHAT_BAR360.mdl",
	"INV_EQP_HELMET.mdl", "ctl_compass.mdl", "c_elemashm.mdl",
}

func TestOracleFaceAdjacency(t *testing.T) {
	compared := 0
	for _, name := range adjacencyStrictFixtures {
		binPath := filepath.Join(oracleBinaryDir, name)
		asciiPath := filepath.Join(oracleAsciiDir, name)
		raw, err := os.ReadFile(binPath)
		if err != nil {
			t.Skipf("no game_binary fixtures: %v", err)
		}
		game, err := DecompileFile(binPath)
		if err != nil {
			t.Fatalf("%s: decompile: %v", name, err)
		}
		res, err := ParseFile(asciiPath)
		if err != nil {
			t.Fatalf("%s: parse: %v", name, err)
		}
		for _, gn := range game.Nodes {
			sn := res.Model.FindNode(gn.Name)
			if gn.Mesh == nil || sn == nil || sn.Mesh == nil || len(gn.Mesh.Faces) == 0 {
				continue
			}
			want, ok := findGameFaceAdjacency(raw, gn.Mesh.Faces)
			if !ok {
				continue
			}
			got := buildFaceAdjacency(sn.Mesh.Faces, len(gn.Mesh.Faces))
			compared++
			for i := range want {
				if got[i] != want[i] {
					t.Errorf("%s node %q face %d: adjacency got %v, game %v", name, gn.Name, i, got[i], want[i])
					break
				}
			}
		}
	}
	if compared == 0 {
		t.Skip("no nodes compared")
	}
	t.Logf("compared %d nodes", compared)
}

// Compile the ASCII ourselves and check the adjacency we actually wrote
// against the game's, node by node.
func TestCompiledFaceAdjacencyMatchesGame(t *testing.T) {
	compared := 0
	for _, name := range adjacencyStrictFixtures {
		gameRaw, err := os.ReadFile(filepath.Join(oracleBinaryDir, name))
		if err != nil {
			t.Skipf("no game_binary fixtures: %v", err)
		}
		res, err := ParseFile(filepath.Join(oracleAsciiDir, name))
		if err != nil {
			t.Fatalf("%s: parse: %v", name, err)
		}
		ours := filepath.Join(t.TempDir(), name)
		if err := CompileFile(res.Model, ours); err != nil {
			t.Fatalf("%s: compile: %v", name, err)
		}
		ourRaw, _ := os.ReadFile(ours)
		ourModel, err := DecompileFile(ours)
		if err != nil {
			t.Fatalf("%s: decompile ours: %v", name, err)
		}
		game, _ := DecompileFile(filepath.Join(oracleBinaryDir, name))
		for _, on := range ourModel.Nodes {
			gn := game.FindNode(on.Name)
			if on.Mesh == nil || gn == nil || gn.Mesh == nil || len(on.Mesh.Faces) != len(gn.Mesh.Faces) || len(on.Mesh.Faces) == 0 {
				continue
			}
			want, ok1 := findGameFaceAdjacency(gameRaw, gn.Mesh.Faces)
			got, ok2 := findGameFaceAdjacency(ourRaw, on.Mesh.Faces)
			if !ok1 || !ok2 {
				continue
			}
			compared++
			for i := range want {
				if got[i] != want[i] {
					t.Errorf("%s node %q face %d: we wrote %v, game has %v", name, on.Name, i, got[i], want[i])
					break
				}
			}
		}
	}
	if compared == 0 {
		t.Skip("no nodes compared")
	}
	t.Logf("compared %d nodes", compared)
}

// object01 in tdc01_g02_01 loses three degenerate faces (19 -> 16). The game
// still computes adjacency from the first 16 source faces, which is what the
// first-n search above reproduces.
func TestOracleFaceAdjacencyAfterStripping(t *testing.T) {
	const name = "tdc01_g02_01.mdl"
	binPath := filepath.Join("..", "..", "tests", "fixtures", "oracle", "tangent_pairs", "game_binary", name)
	asciiPath := filepath.Join("..", "..", "tests", "fixtures", "oracle", "tangent_pairs", "ascii", name)
	raw, err := os.ReadFile(binPath)
	if err != nil {
		t.Skipf("fixture missing: %v", err)
	}
	game, err := DecompileFile(binPath)
	if err != nil {
		t.Fatal(err)
	}
	res, err := ParseFile(asciiPath)
	if err != nil {
		t.Fatal(err)
	}
	gn, sn := game.FindNode("object01"), res.Model.FindNode("object01")
	if gn == nil || sn == nil || len(sn.Mesh.Faces) != 19 || len(gn.Mesh.Faces) != 16 {
		t.Fatalf("unexpected fixture shape")
	}
	want, ok := findGameFaceAdjacency(raw, gn.Mesh.Faces)
	if !ok {
		t.Fatal("could not locate the face array")
	}
	got := buildFaceAdjacency(sn.Mesh.Faces, len(gn.Mesh.Faces))
	for i := range want {
		if got[i] != want[i] {
			t.Errorf("face %d: got %v, game %v", i, got[i], want[i])
		}
	}
}
