package mdl

import (
	"bytes"
	"math"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestRegression_AlphaZeroPreserved(t *testing.T) {
	src := `newmodel alphatest
setsupermodel alphatest NULL
classification CHARACTER
setanimationscale 1.00
beginmodelgeom alphatest
  node dummy alphatest
    parent NULL
  endnode
  node trimesh mymesh
    parent alphatest
    bitmap blank
    render 1
    alpha 0
    verts 3
      0 0 0
      1 0 0
      0 1 0
    tverts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2  0 0  0 0 1 2
  endnode
endmodelgeom
donemodel alphatest
`
	model := mustParseASCII(t, src)
	binData := mustCompile(t, model)
	model2 := mustDecompile(t, binData)

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
	if meshNode.Mesh.Alpha != 0 {
		t.Errorf("alpha: got %g, want 0", meshNode.Mesh.Alpha)
	}
}

func TestRegression_SnapFloatNegative(t *testing.T) {
	got := snapFloat(-1.007, 0.01)
	if math.Abs(float64(got)-(-1.01)) > 0.001 {
		t.Errorf("snapFloat(-1.007, 0.01) = %g, want ≈ -1.01", got)
	}

	got = snapFloat(1.007, 0.01)
	if math.Abs(float64(got)-1.01) > 0.001 {
		t.Errorf("snapFloat(1.007, 0.01) = %g, want ≈ 1.01", got)
	}

	got = snapFloat(0, 0.01)
	if got != 0 {
		t.Errorf("snapFloat(0, 0.01) = %g, want 0", got)
	}

	got = snapFloat(5.0, 0)
	if got != 5.0 {
		t.Errorf("snapFloat(5.0, 0) = %g, want 5.0 (grid=0 passthrough)", got)
	}
}

func TestRegression_AABBRestoreOnError(t *testing.T) {
	mesh := NewMeshData()
	mesh.Verts = []Vec3{{0, 0, 0}, {0, 0, 0}, {0, 0, 0}}
	mesh.Faces = []Face{
		{Verts: [3]int32{0, 1, 2}},
		{Verts: [3]int32{0, 2, 1}},
	}
	original := []AabbEntry{
		{BoundMin: Vec3{-1, -1, -1}, BoundMax: Vec3{1, 1, 1}, LeafFace: 0},
		{BoundMin: Vec3{-2, -2, -2}, BoundMax: Vec3{2, 2, 2}, LeafFace: 1},
	}
	node := &Node{
		Name: "walkmesh",
		Mesh: mesh,
		Aabb: &AabbData{Entries: append([]AabbEntry(nil), original...)},
	}

	err := RebuildAABB(node)
	if err == nil {
		t.Fatal("expected RebuildAABB to fail on degenerate colocated faces")
	}

	if len(node.Aabb.Entries) != len(original) {
		t.Fatalf("entries count after error: got %d, want %d", len(node.Aabb.Entries), len(original))
	}
	for i, e := range node.Aabb.Entries {
		if e != original[i] {
			t.Errorf("entry %d not restored: got %+v, want %+v", i, e, original[i])
		}
	}
}

func TestRegression_CompileFileAtomic(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "test.mdl")

	if err := os.WriteFile(path, []byte("original content"), 0644); err != nil {
		t.Fatal(err)
	}

	model := minimalModel()
	if err := CompileFile(model, path); err != nil {
		t.Fatalf("CompileFile: %v", err)
	}

	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	if string(data) == "original content" {
		t.Error("file was not overwritten")
	}
	if len(data) < 12 {
		t.Errorf("binary output too short: %d bytes", len(data))
	}

	tmpPath := path + ".tmp"
	if _, err := os.Stat(tmpPath); err == nil {
		t.Error(".tmp file was left behind")
	}
}

func TestRegression_WriteFileAtomic(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "test.mdl")

	if err := os.WriteFile(path, []byte("original content"), 0644); err != nil {
		t.Fatal(err)
	}

	model := minimalModel()
	if err := WriteFile(model, path); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}

	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	if string(data) == "original content" {
		t.Error("file was not overwritten")
	}
	if !strings.Contains(string(data), "newmodel") {
		t.Error("output doesn't contain expected ASCII MDL content")
	}

	tmpPath := path + ".tmp"
	if _, err := os.Stat(tmpPath); err == nil {
		t.Error(".tmp file was left behind")
	}
}

func TestRegression_SplitMultiEdgeTVerts(t *testing.T) {
	mesh := NewMeshData()
	mesh.Verts = []Vec3{
		{0, 0, 0},
		{1, 0, 0},
		{0, 1, 0},
		{1, 1, 0},
	}
	mesh.TVerts = []Vec3{
		{0, 0, 0},
		{1, 0, 0},
		{0, 1, 0},
		{1, 1, 0},
	}
	mesh.Faces = []Face{
		{Verts: [3]int32{0, 1, 2}, UVs: [3]int32{0, 1, 2}},
		{Verts: [3]int32{1, 3, 2}, UVs: [3]int32{1, 3, 2}},
		{Verts: [3]int32{2, 1, 3}, UVs: [3]int32{2, 1, 3}},
	}
	node := &Node{Name: "multi", Mesh: mesh}

	SplitMultipleEdges(node)

	for fi, f := range node.Mesh.Faces {
		for vi := 0; vi < 3; vi++ {
			if int(f.UVs[vi]) >= len(node.Mesh.TVerts) {
				t.Errorf("face %d uv[%d]=%d >= len(TVerts)=%d",
					fi, vi, f.UVs[vi], len(node.Mesh.TVerts))
			}
		}
	}
}

func TestRegression_TilefadeAnimOrphan(t *testing.T) {
	model := &Model{
		Name:           "tiletest",
		SuperModel:     "NULL",
		Classification: "TILE",
		AnimationScale: 1.0,
	}
	root := &Node{Name: "tiletest", Parent: "NULL"}
	meshNode := &Node{
		Name:   "mymesh",
		Parent: "tiletest",
		Mesh: &MeshData{
			Render: 1,
			Alpha:  1.0,
			Bitmap: "blank",
			Verts: []Vec3{
				{0, 0, 0}, {1, 0, 0}, {0, 1, 0},
				{0, 0, 3}, {1, 0, 3}, {0, 1, 3},
			},
			TVerts: []Vec3{
				{0, 0, 0}, {1, 0, 0}, {0, 1, 0},
				{0, 0, 0}, {1, 0, 0}, {0, 1, 0},
			},
			Faces: []Face{
				{Verts: [3]int32{0, 1, 2}, UVs: [3]int32{0, 1, 2}},
				{Verts: [3]int32{3, 4, 5}, UVs: [3]int32{3, 4, 5}},
				{Verts: [3]int32{0, 1, 3}, UVs: [3]int32{0, 1, 3}},
			},
			TileFade: 2,
		},
	}
	model.Nodes = []*Node{root, meshNode}
	model.Animations = []Animation{
		{
			Name:      "tiledefault",
			Length:    1.0,
			TransTime: 0.25,
			Root:      "tiletest",
			Nodes: []AnimNode{
				{Name: "tiletest", Parent: "NULL"},
				{Name: "mymesh", Parent: "tiletest"},
			},
		},
	}

	SliceTileFade(model, 1.5)

	nodeNames := make(map[string]bool)
	for _, n := range model.Nodes {
		nodeNames[strings.ToLower(n.Name)] = true
	}

	for _, anim := range model.Animations {
		for _, an := range anim.Nodes {
			if strings.EqualFold(an.Parent, "NULL") {
				continue
			}
			if !nodeNames[strings.ToLower(an.Parent)] {
				t.Errorf("animation node %q has parent %q not found in model nodes",
					an.Name, an.Parent)
			}
		}
	}
}

func TestRegression_CaseSensitiveFindNode(t *testing.T) {
	model := &Model{
		Name: "test",
		Nodes: []*Node{
			{Name: "MyNode", Parent: "NULL"},
		},
	}

	if n := model.FindNode("mynode"); n == nil {
		t.Error("FindNode(\"mynode\") returned nil for node named \"MyNode\"")
	}
	if n := model.FindNode("MYNODE"); n == nil {
		t.Error("FindNode(\"MYNODE\") returned nil for node named \"MyNode\"")
	}
	if n := model.FindNode("MyNode"); n == nil {
		t.Error("FindNode(\"MyNode\") returned nil for node named \"MyNode\"")
	}
}

func TestRegression_NaNFloat(t *testing.T) {
	src := `newmodel nantest
setsupermodel nantest NULL
classification CHARACTER
setanimationscale 1.00
beginmodelgeom nantest
  node dummy nantest
    parent NULL
  endnode
  node trimesh mymesh
    parent nantest
    bitmap blank
    render 1
    verts 3
      NaN 0 0
      1 0 0
      0 1 0
    tverts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2  0 0  0 0 1 2
  endnode
endmodelgeom
donemodel nantest
`
	pr, err := Parse(strings.NewReader(src))
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if pr.Model == nil {
		t.Fatal("nil model")
	}
	meshNode := pr.Model.FindNode("mymesh")
	if meshNode == nil || meshNode.Mesh == nil {
		t.Fatal("no mesh node")
	}

	for i, v := range meshNode.Mesh.Verts {
		if math.IsNaN(float64(v.X)) || math.IsNaN(float64(v.Y)) || math.IsNaN(float64(v.Z)) {
			t.Errorf("vert %d contains NaN: {%g %g %g}", i, v.X, v.Y, v.Z)
		}
	}
}

func TestRegression_EmitterCtrlIDComplete(t *testing.T) {
	for name := range emitterFloatFields {
		id := emitterCtrlID(name)
		if id == 0 {
			t.Errorf("emitterCtrlID(%q) = 0 (unmapped controller)", name)
		}
	}
	for name := range emitterColorFields {
		id := emitterCtrlID(name)
		if id == 0 {
			t.Errorf("emitterCtrlID(%q) = 0 (unmapped controller)", name)
		}
	}
}

func TestRegression_AlphaRoundtrip(t *testing.T) {
	for _, alpha := range []float32{0.0, 0.5, 1.0} {
		model := minimalModelWithMesh()
		model.Nodes[1].Mesh.Alpha = alpha

		binData := mustCompile(t, model)
		model2 := mustDecompile(t, binData)

		var meshNode *Node
		for _, n := range model2.Nodes {
			if n.Mesh != nil {
				meshNode = n
				break
			}
		}
		if meshNode == nil {
			t.Fatalf("alpha=%g: no mesh node after decompile", alpha)
		}
		if meshNode.Mesh.Alpha != alpha {
			t.Errorf("alpha=%g: got %g after roundtrip", alpha, meshNode.Mesh.Alpha)
		}
	}
}

func TestRegression_CompileDecompileNodeCount(t *testing.T) {
	model := minimalModelWithMesh()
	binData := mustCompile(t, model)
	model2 := mustDecompile(t, binData)

	if len(model2.Nodes) != len(model.Nodes) {
		t.Errorf("node count: got %d, want %d", len(model2.Nodes), len(model.Nodes))
	}
}

func TestRegression_WriteReadRoundtrip(t *testing.T) {
	model := minimalModelWithMesh()

	var buf bytes.Buffer
	if err := Write(model, &buf); err != nil {
		t.Fatalf("Write: %v", err)
	}
	ascii := buf.String()
	if !strings.Contains(ascii, "newmodel") {
		t.Error("ASCII output missing newmodel")
	}
	if !strings.Contains(ascii, "donemodel") {
		t.Error("ASCII output missing donemodel")
	}
}

// ---- helpers ----

func minimalModel() *Model {
	return &Model{
		Name:           "test",
		SuperModel:     "NULL",
		Classification: "CHARACTER",
		AnimationScale: 1.0,
		Nodes:          []*Node{{Name: "test", Parent: "NULL"}},
	}
}

func minimalModelWithMesh() *Model {
	m := minimalModel()
	mesh := NewMeshData()
	mesh.Bitmap = "blank"
	mesh.Verts = []Vec3{{0, 0, 0}, {1, 0, 0}, {0, 1, 0}}
	mesh.TVerts = []Vec3{{0, 0, 0}, {1, 0, 0}, {0, 1, 0}}
	mesh.Faces = []Face{
		{Verts: [3]int32{0, 1, 2}, UVs: [3]int32{0, 1, 2}},
	}
	meshNode := &Node{Name: "mymesh", Parent: "test", Mesh: mesh}
	m.Nodes = append(m.Nodes, meshNode)
	return m
}
