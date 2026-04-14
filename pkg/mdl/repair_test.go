package mdl

import (
	"math"
	"strings"
	"testing"
)

func testModel(nodes ...*Node) *Model {
	return &Model{
		Name:           "test",
		Classification: "CHARACTER",
		SuperModel:     "NULL",
		AnimationScale: 1.0,
		Nodes:          nodes,
	}
}

func testTileModel(nodes ...*Node) *Model {
	m := testModel(nodes...)
	m.Classification = "TILE"
	return m
}

func dummyNode(name, parent string) *Node {
	return &Node{Name: name, Parent: parent}
}

func trimeshNode(name, parent string, mesh *MeshData) *Node {
	return &Node{Name: name, Parent: parent, Mesh: mesh}
}

func quadMesh(bitmap string) *MeshData {
	m := NewMeshData()
	m.Verts = []Vec3{{0, 0, 0}, {1, 0, 0}, {1, 1, 0}, {0, 1, 0}}
	m.TVerts = []Vec3{{0, 0, 0}, {1, 0, 0}, {1, 1, 0}, {0, 1, 0}}
	m.Faces = []Face{
		{Verts: [3]int32{0, 1, 2}, UVs: [3]int32{0, 1, 2}},
		{Verts: [3]int32{0, 2, 3}, UVs: [3]int32{0, 2, 3}},
	}
	m.Bitmap = bitmap
	return m
}

func TestStripDegenerateFaces(t *testing.T) {
	mesh := NewMeshData()
	mesh.Verts = []Vec3{{0, 0, 0}, {1, 0, 0}, {1, 1, 0}, {0, 1, 0}}
	mesh.Faces = []Face{
		{Verts: [3]int32{0, 1, 2}},
		{Verts: [3]int32{0, 0, 1}},
		{Verts: [3]int32{0, 2, 3}},
	}
	model := testModel(trimeshNode("mesh1", "NULL", mesh))

	count := StripDegenerateFaces(model)
	if count != 1 {
		t.Fatalf("expected 1 degenerate face removed, got %d", count)
	}
	if len(mesh.Faces) != 2 {
		t.Fatalf("expected 2 faces remaining, got %d", len(mesh.Faces))
	}
}

func TestSplitMultipleEdges(t *testing.T) {
	mesh := NewMeshData()
	mesh.Verts = []Vec3{{0, 0, 0}, {1, 0, 0}, {0.5, 1, 0}, {0.5, -1, 0}}
	mesh.TVerts = []Vec3{{0, 0, 0}, {1, 0, 0}, {0.5, 1, 0}, {0.5, -1, 0}}
	mesh.Faces = []Face{
		{Verts: [3]int32{0, 1, 2}, UVs: [3]int32{0, 1, 2}},
		{Verts: [3]int32{0, 1, 3}, UVs: [3]int32{0, 1, 3}},
		{Verts: [3]int32{1, 0, 3}, UVs: [3]int32{1, 0, 3}},
	}
	node := trimeshNode("mesh1", "NULL", mesh)

	count := SplitMultipleEdges(node)
	if count == 0 {
		t.Fatal("expected at least 1 multi-edge split, got 0")
	}

	edgeMap := BuildEdgeFaceMap(mesh.Faces)
	for e, faces := range edgeMap {
		if len(faces) > 2 {
			t.Errorf("edge %v still shared by %d faces after split", e, len(faces))
		}
	}

	for fi, f := range mesh.Faces {
		for vi := 0; vi < 3; vi++ {
			if int(f.UVs[vi]) >= len(mesh.TVerts) {
				t.Errorf("face %d UV index %d = %d exceeds TVerts len %d", fi, vi, f.UVs[vi], len(mesh.TVerts))
			}
		}
	}
}

func TestForceRender(t *testing.T) {
	mesh := NewMeshData()
	mesh.Render = 0
	model := testModel(trimeshNode("mesh1", "NULL", mesh))

	count := ForceRender(model, "all")
	if count != 1 {
		t.Fatalf("expected 1 node changed, got %d", count)
	}
	if mesh.Render != 1 {
		t.Fatalf("expected Render=1, got %d", mesh.Render)
	}

	mesh.Render = 1
	count = ForceRender(model, "none")
	if count != 1 {
		t.Fatalf("expected 1 node changed for none mode, got %d", count)
	}
	if mesh.Render != 0 {
		t.Fatalf("expected Render=0, got %d", mesh.Render)
	}
}

func TestForceShadow(t *testing.T) {
	mesh := NewMeshData()
	mesh.Shadow = 0
	model := testModel(trimeshNode("mesh1", "NULL", mesh))

	count := ForceShadow(model, "all")
	if count != 1 {
		t.Fatalf("expected 1 node changed, got %d", count)
	}
	if mesh.Shadow != 1 {
		t.Fatalf("expected Shadow=1, got %d", mesh.Shadow)
	}

	mesh.Shadow = 1
	count = ForceShadow(model, "none")
	if count != 1 {
		t.Fatalf("expected 1 node changed for none mode, got %d", count)
	}
	if mesh.Shadow != 0 {
		t.Fatalf("expected Shadow=0, got %d", mesh.Shadow)
	}
}

func TestForceWhiteAmbientDiffuse(t *testing.T) {
	mesh := NewMeshData()
	mesh.Ambient = Vec3{0, 0, 0}
	mesh.Diffuse = Vec3{0.5, 0.5, 0.5}
	model := testModel(trimeshNode("mesh1", "NULL", mesh))

	count := ForceWhiteAmbientDiffuse(model)
	if count != 1 {
		t.Fatalf("expected 1 node changed, got %d", count)
	}
	white := Vec3{1, 1, 1}
	if mesh.Ambient != white {
		t.Fatalf("expected Ambient={1,1,1}, got %v", mesh.Ambient)
	}
	if mesh.Diffuse != white {
		t.Fatalf("expected Diffuse={1,1,1}, got %v", mesh.Diffuse)
	}
}

func TestCullInvisibleMeshes(t *testing.T) {
	visible := NewMeshData()
	visible.Render = 1

	invisible := NewMeshData()
	invisible.Render = 0
	invisible.Shadow = 0

	model := testModel(
		trimeshNode("vis", "root", visible),
		trimeshNode("invis", "root", invisible),
	)

	count := CullInvisibleMeshes(model)
	if count != 1 {
		t.Fatalf("expected 1 culled, got %d", count)
	}

	invisNode := model.FindNode("invis")
	if invisNode.Mesh != nil {
		t.Fatal("expected invisible node's Mesh to be nil after cull")
	}
	visNode := model.FindNode("vis")
	if visNode.Mesh == nil {
		t.Fatal("expected visible node's Mesh to still be present")
	}
}

func TestMergeByBitmap(t *testing.T) {
	mesh1 := quadMesh("shared_bitmap")
	mesh2 := quadMesh("shared_bitmap")

	model := testModel(
		dummyNode("root", "NULL"),
		trimeshNode("m1", "root", mesh1),
		trimeshNode("m2", "root", mesh2),
	)

	origVerts := len(mesh1.Verts) + len(mesh2.Verts)
	origFaces := len(mesh1.Faces) + len(mesh2.Faces)

	count := MergeByBitmap(model)
	if count != 1 {
		t.Fatalf("expected 1 merge group, got %d", count)
	}

	m2Node := model.FindNode("m2")
	if m2Node.Mesh != nil {
		t.Fatal("expected source node's mesh to be nil after merge")
	}

	if len(mesh1.Verts) != origVerts {
		t.Fatalf("expected %d combined verts, got %d", origVerts, len(mesh1.Verts))
	}
	if len(mesh1.Faces) != origFaces {
		t.Fatalf("expected %d combined faces, got %d", origFaces, len(mesh1.Faces))
	}
}

func TestSnapVertices(t *testing.T) {
	mesh := NewMeshData()
	mesh.Verts = []Vec3{{-1.007, 1.007, 0}}
	model := testModel(trimeshNode("mesh1", "NULL", mesh))

	SnapVertices(model, "decimal")

	v := mesh.Verts[0]
	if math.Abs(float64(v.X)-(-1.01)) > 1e-5 {
		t.Errorf("expected X ≈ -1.01, got %f", v.X)
	}
	if math.Abs(float64(v.Y)-1.01) > 1e-5 {
		t.Errorf("expected Y ≈ 1.01, got %f", v.Y)
	}
	if v.Z != 0 {
		t.Errorf("expected Z = 0, got %f", v.Z)
	}
}

func TestScaleModel(t *testing.T) {
	mesh := NewMeshData()
	mesh.Verts = []Vec3{{1, 0, 0}, {0, 1, 0}}
	node := trimeshNode("mesh1", "root", mesh)
	node.Position = Vec3{1, 2, 3}
	model := testModel(dummyNode("root", "NULL"), node)

	ScaleModel(model, 2.0)

	if node.Position != (Vec3{2, 4, 6}) {
		t.Fatalf("expected position {2,4,6}, got %v", node.Position)
	}
	if mesh.Verts[0] != (Vec3{2, 0, 0}) {
		t.Fatalf("expected vert[0] {2,0,0}, got %v", mesh.Verts[0])
	}
	if mesh.Verts[1] != (Vec3{0, 2, 0}) {
		t.Fatalf("expected vert[1] {0,2,0}, got %v", mesh.Verts[1])
	}
}

func TestScaleModelPerAxis(t *testing.T) {
	mesh := NewMeshData()
	mesh.Verts = []Vec3{{1, 0, 0}, {0, 1, 0}}
	node := trimeshNode("mesh1", "root", mesh)
	node.Position = Vec3{1, 2, 3}
	model := testModel(dummyNode("root", "NULL"), node)

	ScaleModelPerAxis(model, 2, 3, 4)

	if node.Position != (Vec3{2, 6, 12}) {
		t.Fatalf("expected position {2,6,12}, got %v", node.Position)
	}
	if mesh.Verts[0] != (Vec3{2, 0, 0}) {
		t.Fatalf("expected vert[0] {2,0,0}, got %v", mesh.Verts[0])
	}
	if mesh.Verts[1] != (Vec3{0, 3, 0}) {
		t.Fatalf("expected vert[1] {0,3,0}, got %v", mesh.Verts[1])
	}
}

func TestWrapRootInDummy(t *testing.T) {
	mesh := quadMesh("tex")
	model := testModel(trimeshNode("test", "NULL", mesh))

	msg := WrapRootInDummy(model)
	if msg == "" {
		t.Fatal("expected non-empty repair message")
	}

	root := model.Nodes[0]
	if root.NodeType() != "dummy" {
		t.Fatalf("expected first node to be dummy, got %s", root.NodeType())
	}
	if root.Name != "test" {
		t.Fatalf("expected wrapper named 'test', got %q", root.Name)
	}

	meshNode := model.Nodes[1]
	if meshNode.Mesh == nil {
		t.Fatal("expected second node to have mesh data")
	}
	if meshNode.Parent != "test" {
		t.Fatalf("expected mesh node parent='test', got %q", meshNode.Parent)
	}
}

func TestReparentFromRestrictedNodes(t *testing.T) {
	root := dummyNode("root", "NULL")
	light := &Node{Name: "mylight", Parent: "root", Light: &LightData{}}
	child := trimeshNode("childmesh", "mylight", quadMesh("tex"))

	model := testModel(root, light, child)

	msgs := ReparentFromRestrictedNodes(model)
	if len(msgs) == 0 {
		t.Fatal("expected at least one reparent message")
	}
	if child.Parent != "root" {
		t.Fatalf("expected child reparented to 'root', got %q", child.Parent)
	}
}

func TestFixAnimationLengths(t *testing.T) {
	model := testTileModel(dummyNode("root", "NULL"))
	model.Animations = []Animation{
		{Name: "day2night", Length: 0.001},
	}

	msgs := FixAnimationLengths(model)
	if len(msgs) == 0 {
		t.Fatal("expected at least one fix message")
	}
	if model.Animations[0].Length < TileAnimMinLength {
		t.Fatalf("expected length >= %g, got %g", TileAnimMinLength, model.Animations[0].Length)
	}
}

func TestFixAnimationLengthsNegative(t *testing.T) {
	model := testModel(dummyNode("root", "NULL"))
	model.Animations = []Animation{
		{Name: "attack", Length: -1.0},
	}

	msgs := FixAnimationLengths(model)
	if len(msgs) == 0 {
		t.Fatal("expected at least one fix message")
	}
	if model.Animations[0].Length != 0 {
		t.Fatalf("expected length = 0, got %g", model.Animations[0].Length)
	}
}

func TestRebuildAABB(t *testing.T) {
	mesh := NewMeshData()
	mesh.Verts = []Vec3{
		{0, 0, 0}, {4, 0, 0}, {4, 4, 0}, {0, 4, 0},
		{2, 2, 0}, {3, 0, 0},
	}
	mesh.Faces = []Face{
		{Verts: [3]int32{0, 1, 4}},
		{Verts: [3]int32{1, 2, 4}},
		{Verts: [3]int32{2, 3, 4}},
		{Verts: [3]int32{3, 0, 4}},
	}
	node := &Node{
		Name:   "walkmesh",
		Parent: "NULL",
		Mesh:   mesh,
		Aabb:   &AabbData{},
	}

	err := RebuildAABB(node)
	if err != nil {
		t.Fatalf("RebuildAABB error: %v", err)
	}
	if len(node.Aabb.Entries) == 0 {
		t.Fatal("expected non-empty AABB entries")
	}

	leafCount := 0
	for _, e := range node.Aabb.Entries {
		if e.LeafFace >= 0 {
			leafCount++
		}
	}
	if leafCount != len(mesh.Faces) {
		t.Fatalf("expected %d leaf entries (one per face), got %d", len(mesh.Faces), leafCount)
	}
}

func TestSliceTileFade(t *testing.T) {
	mesh := NewMeshData()
	mesh.Verts = []Vec3{
		{0, 0, -1}, {1, 0, -1}, {1, 1, -1}, {0, 1, -1},
		{0, 0, 1}, {1, 0, 1}, {1, 1, 1}, {0, 1, 1},
	}
	mesh.TVerts = []Vec3{
		{0, 0, 0}, {1, 0, 0}, {1, 1, 0}, {0, 1, 0},
		{0, 0, 0}, {1, 0, 0}, {1, 1, 0}, {0, 1, 0},
	}
	mesh.Faces = []Face{
		{Verts: [3]int32{0, 1, 4}, UVs: [3]int32{0, 1, 4}},
		{Verts: [3]int32{1, 5, 4}, UVs: [3]int32{1, 5, 4}},
		{Verts: [3]int32{1, 2, 5}, UVs: [3]int32{1, 2, 5}},
		{Verts: [3]int32{2, 6, 5}, UVs: [3]int32{2, 6, 5}},
	}
	mesh.TileFade = 2

	model := testTileModel(
		dummyNode("root", "NULL"),
		trimeshNode("wall", "root", mesh),
	)

	msgs := SliceTileFade(model, 0)
	if len(msgs) == 0 {
		t.Fatal("expected at least one slice message")
	}

	var tf0, tf1 *Node
	for _, n := range model.Nodes {
		if n.Mesh != nil && strings.Contains(n.Name, "__tf0") {
			tf0 = n
		}
		if n.Mesh != nil && strings.Contains(n.Name, "__tf1") {
			tf1 = n
		}
	}
	if tf0 == nil {
		t.Fatal("expected a __tf0 node")
	}
	if tf1 == nil {
		t.Fatal("expected a __tf1 node")
	}
	if len(tf0.Mesh.Faces) == 0 {
		t.Fatal("expected __tf0 to have faces")
	}
	if len(tf1.Mesh.Faces) == 0 {
		t.Fatal("expected __tf1 to have faces")
	}
	if tf0.Mesh.TileFade != 0 {
		t.Fatalf("expected __tf0 TileFade=0, got %d", tf0.Mesh.TileFade)
	}
	if tf1.Mesh.TileFade != 1 {
		t.Fatalf("expected __tf1 TileFade=1, got %d", tf1.Mesh.TileFade)
	}
}

func TestRepairPivots(t *testing.T) {
	mesh := NewMeshData()
	mesh.Verts = []Vec3{
		{-2, -2, 0}, {2, -2, 0}, {2, 2, 0}, {-2, 2, 0},
		{0, 0, 3},
	}
	mesh.Faces = []Face{
		{Verts: [3]int32{0, 1, 2}},
		{Verts: [3]int32{0, 2, 3}},
		{Verts: [3]int32{0, 1, 4}},
		{Verts: [3]int32{1, 2, 4}},
	}
	node := &Node{
		Name:     "aabb_node",
		Parent:   "NULL",
		Mesh:     mesh,
		Aabb:     &AabbData{},
		Position: Vec3{99, 99, 99},
	}
	model := testModel(node)

	msgs := RepairPivots(model)
	if len(msgs) == 0 {
		t.Fatal("expected at least one pivot message")
	}
	if node.Position == (Vec3{99, 99, 99}) {
		t.Fatal("expected Position to be updated from initial value")
	}
	if node.Position.Z < 0 {
		t.Fatalf("expected pivot Z >= 0, got %f", node.Position.Z)
	}
}
