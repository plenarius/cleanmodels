package mdl

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"
)

// The faces take the index list's clones; a list triangle that is another
// triangle altogether leaves its face alone.
func TestApplyTangentClones(t *testing.T) {
	m := mergeQuad("tex", 1, 0)
	// Vertex 4 clones vertex 2 (same position and UV), as tangent generation
	// appends it; vertex 5 is somewhere else.
	m.Verts = append(m.Verts, m.Verts[2], Vec3{7, 7, 7})
	m.TVerts = append(m.TVerts, m.TVerts[2], Vec3{0.5, 0.5, 0})
	n := &Node{Name: "quad", Mesh: m}
	d := &decompiler{model: &Model{Nodes: []*Node{n}}}
	d.renderLists = map[*Node][]uint16{n: {0, 1, 2, 0, 4, 3}}
	d.applyTangentClones()
	if got := m.Faces[1].Verts; got != [3]int32{0, 4, 3} {
		t.Errorf("face 1 = %v, want the clone: [0 4 3]", got)
	}
	if got := m.Faces[1].UVs; got != [3]int32{0, 4, 3} {
		t.Errorf("face 1 UVs = %v, want [0 4 3]", got)
	}

	d.renderLists[n] = []uint16{0, 1, 2, 0, 5, 3}
	m.Faces[1].Verts, m.Faces[1].UVs = [3]int32{0, 2, 3}, [3]int32{0, 2, 3}
	d.applyTangentClones()
	if got := m.Faces[1].Verts; got != [3]int32{0, 2, 3} {
		t.Errorf("face 1 = %v, want it left as read: [0 2 3]", got)
	}
}

// TestDecompileDrawsTangentClones: the game's compile of zspec_tanmirror
// clones the seam vertices and draws them through the index list only
// (nwn-mdl-spec MDL-MESH-043, MDL-BIN-049). The decompiled faces must draw
// what the index list draws, handedness included, and use every vertex.
// Built from the face records instead, the clones went unused and the seam
// triangles took the other handedness.
func TestDecompileDrawsTangentClones(t *testing.T) {
	golden, err := os.ReadFile(filepath.Join("..", "..", "tests", "fixtures", "engine", "zspec_tanmirror.mdl"))
	if err != nil {
		t.Fatal(err)
	}
	// The game's view: the raw index list and the vertex streams as read.
	raw := &decompiler{
		r: bytes.NewReader(golden), fileSize: int64(len(golden)),
		model: &Model{AnimationScale: 1.0}, visitedPtrs: map[uint32]bool{}, maxAlloc: int64(len(golden)) * 16,
	}
	if raw.readFileHeader() != nil || raw.readModelHeader() != nil || raw.readNode(raw.rootNodePtr, "NULL") != nil {
		t.Fatal("cannot read the golden")
	}
	rn := raw.model.FindNode("strip")
	rl := raw.renderLists[rn]
	if rn == nil || len(rl) != 3*len(rn.Mesh.Faces) {
		t.Fatalf("golden has no index list for strip")
	}
	var want []float32
	differs := false
	for i, v := range rl {
		want = append(want, rn.Mesh.Tangents[v].W)
		if int32(v) != rn.Mesh.Faces[i/3].Verts[i%3] {
			differs = true
		}
	}
	if !differs {
		t.Fatal("the golden's index list equals its faces: the fixture no longer tests the clones")
	}

	// KeepVertexSplits: the default decompile writes these generated
	// tangents not at all (TestGeneratedTangentsRoundTrip).
	dm, err := DecompileWithOptions(bytes.NewReader(golden), int64(len(golden)), DecompileOptions{KeepVertexSplits: true})
	if err != nil {
		t.Fatal(err)
	}
	m := dm.FindNode("strip").Mesh
	used := make([]bool, len(m.Verts))
	for i, f := range m.Faces {
		for k := 0; k < 3; k++ {
			used[f.Verts[k]] = true
			if got := m.Tangents[f.Verts[k]].W; got != want[3*i+k] {
				t.Errorf("face %d corner %d: handedness %v, the game draws %v", i, k, got, want[3*i+k])
			}
		}
	}
	for v, u := range used {
		if !u {
			t.Errorf("vertex %d is used by no face", v)
		}
	}
}
