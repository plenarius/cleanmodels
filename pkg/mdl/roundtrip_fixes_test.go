package mdl

import (
	"bytes"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

// Round trips that must give the first compile back byte for byte: compile
// an ASCII, decompile it, compile that. Each source below came out unstable
// in the round trip of Layonara's haks (MDL pipeline Phase 2).

// recompileStable fails unless compiling the decompile of first gives first.
func recompileStable(t *testing.T, first []byte) *Model {
	t.Helper()
	m := mustDecompile(t, first)
	var text bytes.Buffer
	if err := Write(m, &text); err != nil {
		t.Fatal(err)
	}
	again := mustCompile(t, mustParseASCII(t, text.String()))
	if !bytes.Equal(again, first) {
		t.Errorf("the compile of the decompile differs from the first compile (%d bytes, was %d)", len(again), len(first))
	}
	return m
}

// A normal-mapped quad folded along its diagonal, with a UV seam on the fold:
// the second face's UVs are turned, so the tangents generated on
// either side of the seam differ. Every vertex is first used in vertex order,
// so the compile's vertex list is "in order"; the decompile must still leave
// the tangents out, or the seam's positions can't be shared and the second
// compile links no faces across it.
const inOrderSeamSrc = `newmodel ios
setsupermodel ios NULL
classification PLACEABLE
setanimationscale 1.0
beginmodelgeom ios
  node dummy ios
    parent NULL
  endnode
  node trimesh fold
    parent ios
    bitmap blank
    renderhint NormalAndSpecMapped
    verts 4
      0 0 0
      1 0 0
      1 1 0.5
      0 1 0
    tverts 6
      0 0 0
      1 0 0
      1 1 0
      1 1 0
      0 1 0
      0 0 0
    faces 2
      0 1 2  1  0 1 2  0
      0 2 3  1  3 4 5  0
  endnode
endmodelgeom
donemodel ios
`

func TestRoundTripInOrderGeneratedTangents(t *testing.T) {
	first := mustCompile(t, mustParseASCII(t, inOrderSeamSrc))
	m := recompileStable(t, first)
	fold := m.FindNode("fold").Mesh
	if len(fold.Tangents) != 0 {
		t.Errorf("tangents written (%d): a compile generates the same ones", len(fold.Tangents))
	}
	if len(fold.Verts) != 4 {
		t.Errorf("%d positions, want the source's 4 (the seam shared)", len(fold.Verts))
	}
}

// A degenerate face before a strip with UV seams: the compile drops it and
// leaves the face records' neighbours in source-face numbering, so the
// decompile must renumber them to find the seams' links. The degenerate face
// uses only vertices an earlier face uses (one that is first to use a vertex
// moves it in the compile's order, the dead-face-vertices case). As in the game, only
// the first four source faces are searched for neighbours, so the last face
// is a triangle of its own.
const droppedFaceSrc = `newmodel dfs
setsupermodel dfs NULL
classification PLACEABLE
setanimationscale 1.0
beginmodelgeom dfs
  node dummy dfs
    parent NULL
  endnode
  node trimesh strip
    parent dfs
    bitmap blank
    verts 9
      0 0 0
      1 0 0
      1 1 0
      0 1 0
      2 0 0
      2 1 0
      5 5 0
      6 5 0
      5 6 0
    tverts 9
      0 0 0
      1 0 0
      1 1 0
      0.5 0 0
      0.5 0.5 0
      0 0.5 0
      0.2 0 0
      0.2 0.2 0
      0.4 0.2 0
    faces 6
      0 1 2  1  0 1 2  0
      0 0 1  1  0 0 1  0
      0 2 3  1  3 4 5  0
      1 4 5  1  6 7 8  0
      1 5 2  1  6 8 2  0
      6 7 8  1  0 1 2  0
  endnode
endmodelgeom
donemodel dfs
`

func TestRoundTripAfterDroppedFace(t *testing.T) {
	first := mustCompile(t, mustParseASCII(t, droppedFaceSrc))
	stored := func(data []byte) *MeshData {
		t.Helper()
		m, err := DecompileWithOptions(bytes.NewReader(data), int64(len(data)), DecompileOptions{KeepVertexSplits: true})
		if err != nil {
			t.Fatal(err)
		}
		return m.FindNode("strip").Mesh
	}
	if n := len(stored(first).Faces); n != 5 {
		t.Fatalf("%d binary faces, want 5 (the degenerate one dropped)", n)
	}
	m := mustDecompile(t, first)
	if n := len(m.FindNode("strip").Mesh.Verts); n != 9 {
		t.Errorf("%d positions, want the source's 9 (each seam shared)", n)
	}
	// The second compile has no face to drop, so its records are numbered as
	// its faces; the vertices and the faces are the first compile's.
	var text bytes.Buffer
	if err := Write(m, &text); err != nil {
		t.Fatal(err)
	}
	a, b := stored(first), stored(mustCompile(t, mustParseASCII(t, text.String())))
	if !slices.Equal(a.Verts, b.Verts) || !slices.Equal(a.TVerts, b.TVerts) || !slices.Equal(a.Normals, b.Normals) {
		t.Errorf("vertices differ: %v %v, was %v %v", b.Verts, b.TVerts, a.Verts, a.TVerts)
	}
	for i := range a.Faces {
		if i >= len(b.Faces) || a.Faces[i].Verts != b.Faces[i].Verts {
			t.Fatalf("faces differ at %d", i)
		}
	}
}

// storedNeighbours on records written in source numbering after source face 1
// was dropped: record 0 is binary face 0's, naming binary face 1 as source
// face 2; record 1 the dropped face's; record 2 binary face 1's. Binary face
// 2 (source face 3) has no record, and was not searched as a neighbour.
func TestStoredNeighboursRenumbers(t *testing.T) {
	m := &MeshData{
		Verts: []Vec3{{0, 0, 0}, {1, 0, 0}, {1, 1, 0}, {0, 1, 0}, {2, 1, 0}},
		Faces: []Face{
			{Verts: [3]int32{0, 1, 2}},
			{Verts: [3]int32{0, 2, 3}},
			{Verts: [3]int32{1, 4, 2}},
		},
		StoredAdjacency: [][3]int16{
			{-1, -1, 2}, // source face 0: 2-0 to source face 2
			{-1, 0, -1},
			{0, -1, -1}, // source face 2 (binary 1): 0-2 to source face 0
		},
	}
	want := [][3]int32{{-1, -1, 1}, {0, -1, -1}, {-1, -1, -1}}
	if got := storedNeighbours(m); !slices.Equal(got, want) {
		t.Errorf("neighbours %v, want %v", got, want)
	}
	// Neighbours that already name binary faces stay as they are.
	m.StoredAdjacency = [][3]int16{{-1, 2, 1}, {0, -1, -1}, {-1, -1, 0}}
	if got := storedNeighbours(m); !slices.Equal(got, [][3]int32{{-1, 2, 1}, {0, -1, -1}, {-1, -1, 0}}) {
		t.Errorf("binary-numbered neighbours changed: %v", got)
	}
}

// Two axis-angles that compile to the same quaternion (mst04_e05_04's, one
// ulp apart in the axis) give the same model box.
func TestModelBoundsFromStoredQuaternion(t *testing.T) {
	a := Vec4{X: -0.5202803, Y: 0.5914203, Z: -0.6160604, W: 2.1787696}
	b := Vec4{X: -0.5202803, Y: 0.5914203, Z: -0.6160603, W: 2.1787696}
	if !sameQuatBits(axisAngleToQuat(a), axisAngleToQuat(b)) {
		t.Fatal("the two axis-angles compile to different quaternions")
	}
	box := func(o Vec4) [2]Vec3 {
		md := NewMeshData()
		md.Verts = []Vec3{{-3.1, 2.7, 0.9}, {4.3, -1.9, 2.2}, {0.7, 5.3, -3.3}, {12.5, 7.25, 3.125}}
		m := &Model{Nodes: []*Node{
			{Name: "root", Parent: "NULL"},
			{Name: "mesh", Parent: "root", Orientation: o, Position: Vec3{X: 5, Y: 5}, Mesh: md},
		}}
		bmin, bmax, _ := modelBounds(m)
		return [2]Vec3{bmin, bmax}
	}
	if ba, bb := box(a), box(b); ba != bb {
		t.Errorf("box %v from one, %v from the other", ba, bb)
	}
}

// The writer and the compiler find a parent by name; a duplicate name that is
// no node's parent must not stop the inverse sort (zcm_tsort50 with two
// children named alike, and a supermodel that can't be found so part-number
// order is not tried).
func TestDecompileOrderDuplicateChildName(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "..", "tests", "fixtures", "engine", "zcm_tsort50.ascii.mdl"))
	if err != nil {
		t.Fatal(err)
	}
	text := strings.Replace(string(src), "setsupermodel zcm_tsort50 NULL", "setsupermodel zcm_tsort50 zz_missing", 1)
	text = strings.Replace(text, "node trimesh m03\n", "node trimesh m02\n", 1)
	first := mustCompile(t, mustParseASCII(t, text))
	again := recompile(t, mustDecompile(t, first), nil)
	sameTree(t, storedTree(t, first), storedTree(t, again), false)
}

// A duplicate name that is a parent keeps the stored order.
func TestDecompileOrderDuplicateParentName(t *testing.T) {
	mesh := func(name, parent string, hint int32) *Node {
		md := NewMeshData()
		md.TransparencyHint = hint
		return &Node{Name: name, Parent: parent, Mesh: md}
	}
	nodes := []*Node{{Name: "root", Parent: "NULL"}, {Name: "p", Parent: "root"}}
	for i := 0; i < 20; i++ {
		nodes = append(nodes, mesh("m", "p", 1))
	}
	nodes = append(nodes, &Node{Name: "p", Parent: "root"})
	m := &Model{Nodes: append([]*Node(nil), nodes...), SuperModel: "NULL"}
	declarationOrder(m)
	if !slices.Equal(m.Nodes, nodes) {
		t.Error("reordered although a duplicate name is a parent")
	}
}

// One parent whose stored children no declaration order gives (a hint-1 mesh
// before a dummy) keeps them; another parent's children still get the inverse
// sort.
func TestDecompileOrderPerParent(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "..", "tests", "fixtures", "engine", "zcm_tsort50.ascii.mdl"))
	if err != nil {
		t.Fatal(err)
	}
	text := strings.Replace(string(src), "setsupermodel zcm_tsort50 NULL", "setsupermodel zcm_tsort50 zz_missing", 1)
	first := mustCompile(t, mustParseASCII(t, text))
	m, err := decompileStored(bytes.NewReader(first), int64(len(first)), DecompileOptions{})
	if err != nil {
		t.Fatal(err)
	}
	// Under a new root: the sorted model, then a parent stored unsorted.
	top := &Node{Name: "top", Parent: "NULL"}
	m.Nodes[0].Parent = "top"
	tri := func(name string, hint int32) *Node {
		md := NewMeshData()
		md.TransparencyHint = hint
		md.Bitmap = "zcm_t0"
		md.Verts = []Vec3{{0, 0, 0}, {1, 0, 0}, {0, 1, 0}}
		md.TVerts = []Vec3{{0, 0, 0}, {1, 0, 0}, {0, 1, 0}}
		md.Faces = []Face{{Verts: [3]int32{0, 1, 2}, UVs: [3]int32{0, 1, 2}, SmoothGroup: 1}}
		return &Node{Name: name, Parent: "q", Mesh: md}
	}
	tail := []*Node{{Name: "q", Parent: "top"}, tri("qa", 1), {Name: "qb", Parent: "q"}}
	for i := 0; i < 20; i++ {
		tail = append(tail, tri("q"+string(rune('a'+i))+"x", 0))
	}
	stored := append(append([]*Node{top}, m.Nodes...), tail...)
	m.Nodes = append([]*Node(nil), stored...)
	declarationOrder(m)
	if slices.Equal(m.Nodes, stored) {
		t.Fatal("stored order kept: the unsorted parent dropped the inverse sort of the other")
	}
	// zcm_tsort50's children come back as stored; q's are sorted as a compile
	// of the stored order sorts them.
	parentOf, children := resolveGeomTree(m.Nodes)
	reorderChildren(m.Nodes[0], children)
	_, storedKids := resolveGeomTree(stored)
	if !slices.Equal(children[stored[1]], storedKids[stored[1]]) {
		t.Error("zcm_tsort50's children do not come back in stored order")
	}
	_, fromStored := resolveGeomTree(stored)
	reorderChildren(stored[0], fromStored)
	q := tail[0]
	if !slices.Equal(children[q], fromStored[q]) {
		t.Error("q's children are not what a compile of the stored order gives")
	}
	_ = parentOf
}

// A chunk emitter's texture field holding junk bytes (uninitialised memory
// an old compiler left) is not written as a name.
func TestDecompileEmitterJunkTexture(t *testing.T) {
	src := `newmodel ejt
setsupermodel ejt NULL
classification EFFECT
setanimationscale 1.0
beginmodelgeom ejt
  node dummy ejt
    parent NULL
  endnode
  node emitter chunks
    parent ejt
    update Explosion
    render Normal
    blend Normal
    texture zzjunkmarker
    chunkName fx_chunk
  endnode
endmodelgeom
donemodel ejt
`
	data := mustCompile(t, mustParseASCII(t, src))
	at := bytes.Index(data, []byte("zzjunkmarker"))
	if at < 0 {
		t.Fatal("texture name not in the binary")
	}
	copy(data[at:], []byte{0xcd, 0xcc, 'L', '>', 0, 0, 0, 0, 0, 0, 0, 0})
	m := mustDecompile(t, data)
	if tex := m.FindNode("chunks").Emitter.Texture; tex != "" {
		t.Errorf("texture %q, want none", tex)
	}
	var text bytes.Buffer
	if err := Write(m, &text); err != nil {
		t.Fatal(err)
	}
	if strings.Contains(text.String(), "texture ") {
		t.Error("a texture line written")
	}
	found := false
	for _, w := range m.Warnings {
		if strings.Contains(w.Message, `emitter "chunks": texture field holds bytes that are not text (cd cc 4c 3e)`) {
			found = true
		}
	}
	if !found {
		t.Errorf("no warning; warnings %v", m.Warnings)
	}
}

// A light's shadowradius and verticaldisplacement are written once.
func TestWriteLightControllersOnce(t *testing.T) {
	src := `newmodel lco
setsupermodel lco NULL
classification EFFECT
setanimationscale 1.0
beginmodelgeom lco
  node dummy lco
    parent NULL
  endnode
  node light lamp
    parent lco
    color 1 1 1
    radius 5
    multiplier 1
    shadowradius 7
    verticaldisplacement 2
  endnode
endmodelgeom
donemodel lco
`
	var text bytes.Buffer
	if err := Write(mustDecompile(t, mustCompile(t, mustParseASCII(t, src))), &text); err != nil {
		t.Fatal(err)
	}
	for _, kw := range []string{"shadowradius 7", "verticaldisplacement 2"} {
		if n := strings.Count(text.String(), kw); n != 1 {
			t.Errorf("%q written %d times", kw, n)
		}
	}
}
