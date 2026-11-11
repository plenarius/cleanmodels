package mdl

import (
	"bytes"
	"fmt"
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

// A duplicate name that is a parent gets no part-number order, but the
// inverse sort still applies, checked against the stored parents: a compile
// of the order taken rebuilds the stored tree.
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
	declarationOrder(m, nil)
	_, children := resolveGeomTree(m.Nodes)
	reorderChildren(m.Nodes[0], children)
	if !walksAs(m.Nodes, children, nodes) {
		t.Error("a compile of the order taken does not rebuild the stored tree")
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
	m, _, err := decompileStored(bytes.NewReader(first), int64(len(first)), DecompileOptions{})
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
	declarationOrder(m, nil)
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

// A restored donor takes its receiver's transparencyhint
// (copyDrawingState), but the stored tree was sorted by its own:
// declarationOrder plans with the stored one. Here part-number order gives
// the stored tree and numbers only with the donor's stored hint 0 (with 2 it
// would sort after trim).
func TestDecompileOrderStoredDonorHint(t *testing.T) {
	hinted := func(m *MeshData, hint int32) *MeshData {
		m.TransparencyHint = hint
		return m
	}
	root := &Node{Name: "root", Parent: "NULL"}
	b := &Node{Name: "b", Parent: "root", PartNumber: 3}
	lower := &Node{Name: "lower", Parent: "root", Mesh: hinted(mergeQuad("tex", 0, 5), 0), Position: Vec3{X: -3}, PartNumber: 1}
	trim := &Node{Name: "trim", Parent: "root", Mesh: hinted(mergeQuad("othertex", 1, 9), 1), PartNumber: 2}
	wall := &Node{Name: "wall", Parent: "root", Mesh: hinted(mergeQuad("tex", 1, 0), 2), PartNumber: 4}
	wall.Mesh.Verts = append(wall.Mesh.Verts, Vec3{2, 0, 0}, Vec3{3, 0, 0}, Vec3{3, 1, 0}, Vec3{2, 1, 0})
	wall.Mesh.TVerts = append(wall.Mesh.TVerts, Vec3{0, 0, 0}, Vec3{1, 0, 0}, Vec3{1, 1, 0}, Vec3{0, 1, 0})
	stored := []*Node{root, b, lower, trim, wall}
	d := &decompiler{model: &Model{Nodes: append([]*Node(nil), stored...), SuperModel: "NULL"}}
	d.renderLists = map[*Node][]uint16{wall: {0, 1, 2, 0, 2, 3, 4, 5, 6, 4, 6, 7}, trim: nil}
	d.unmergeRenderLists()
	if lower.Mesh.Render != 1 || lower.Mesh.TransparencyHint != 2 {
		t.Fatalf("lower: render %d, hint %d; want restored with wall's hint 2", lower.Mesh.Render, lower.Mesh.TransparencyHint)
	}
	if h, ok := d.storedHints[lower]; !ok || h != 0 || len(d.storedHints) != 1 {
		t.Fatalf("stored hints %v, want lower's 0 only", d.storedHints)
	}
	declarationOrder(d.model, d.storedHints)
	if want := []*Node{root, lower, trim, b, wall}; !slices.Equal(d.model.Nodes, want) {
		t.Errorf("order %s, want part-number order", nodeNames(d.model.Nodes))
	}
	if lower.Mesh.TransparencyHint != 2 {
		t.Error("the ASCII must keep the donor's new hint")
	}
}

func nodeNames(nodes []*Node) string {
	var names []string
	for _, n := range nodes {
		names = append(names, n.Name)
	}
	return strings.Join(names, " ")
}

// A name used twice by nodes that are no parent (two leaves named m) still
// lets part-number order through, and the writer keeps it: the original was
// declared out of tree order (b before a's child), and its numbers come back.
func TestDecompileOrderPartNumbersDuplicateLeaf(t *testing.T) {
	src := `newmodel pdl
setsupermodel pdl NULL
classification TILE
setanimationscale 1.0
beginmodelgeom pdl
  node dummy pdl
    parent NULL
  endnode
  node dummy a
    parent pdl
  endnode
  node dummy m
    parent pdl
  endnode
  node dummy m
    parent a
  endnode
endmodelgeom pdl
donemodel pdl
`
	first := mustCompile(t, mustParseASCII(t, src))
	again := recompile(t, mustDecompile(t, first), nil)
	sameTree(t, storedTree(t, first), storedTree(t, again), true)
}

// A child dummy named like the root and parent of more than 16 meshes, one
// with transparencyhint 1 (eld02_p18_01): the meshes are sorted by std::sort,
// which shuffles the hint-0 ones. The decompile declares them in the order
// the sort turns back into the stored one, and the writer finds each node's
// children by node (nearest preceding name), so the tree comes back.
func TestDecompileOrderChildNamedLikeRoot(t *testing.T) {
	var src strings.Builder
	src.WriteString(`newmodel dpr
setsupermodel dpr NULL
classification TILE
setanimationscale 1.0
beginmodelgeom dpr
  node dummy dpr
    parent NULL
  endnode
  node dummy dpr
    parent dpr
  endnode
`)
	for i := 0; i < 24; i++ {
		hint := 0
		if i == 3 {
			hint = 1
		}
		fmt.Fprintf(&src, `  node trimesh m%02d
    parent dpr
    transparencyhint %d
    bitmap blank
    verts 3
      0 0 0
      1 0 0
      0 1 %d
    faces 1
      0 1 2  1  0 0 0  0
  endnode
`, i, hint, i)
	}
	src.WriteString("endmodelgeom dpr\ndonemodel dpr\n")
	first := mustCompile(t, mustParseASCII(t, src.String()))
	again := recompile(t, mustDecompile(t, first), nil)
	sameTree(t, storedTree(t, first), storedTree(t, again), false)
}

// Two controllers of one id on an animation node (nwn-mdl-spec
// MDL-CTRL-036): the engine applies both in order, so the last one's keys are
// kept, not both merged into one list. The binary is made by turning an
// animation node's scale controller (36) into a second alpha controller
// (128).
func TestDecompileDuplicateAnimControllerKeepsLast(t *testing.T) {
	src := `newmodel dac
setsupermodel dac NULL
classification PLACEABLE
setanimationscale 1.0
beginmodelgeom dac
  node dummy dac
    parent NULL
  endnode
  node trimesh quad
    parent dac
    bitmap blank
    verts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2  1  0 0 0  0
  endnode
endmodelgeom dac
newanim fade dac
  length 1
  transtime 0
  animroot dac
  node dummy dac
    parent NULL
  endnode
  node trimesh quad
    parent dac
    scalekey 1
      0 0.5
    alphakey 1
      0 0.25
  endnode
doneanim fade dac
donemodel dac
`
	data := mustCompile(t, mustParseASCII(t, src))
	scale := []byte{36, 0, 0, 0, 1, 0}
	at := -1
	for i := 0; i+12 <= len(data); i++ {
		if bytes.Equal(data[i:i+6], scale) && data[i+10] == 1 {
			if at >= 0 {
				t.Fatal("two scale controller keys")
			}
			at = i
		}
	}
	if at < 0 {
		t.Fatal("no scale controller key")
	}
	data[at] = 128
	m := mustDecompile(t, data)
	an := m.Animations[0].Nodes[1]
	if want := []FloatKey{{Time: 0, Value: 0.25}}; !slices.Equal(an.AlphaKeys, want) || len(an.ScaleKeys) != 0 {
		t.Errorf("alpha keys %v, scale keys %v; want the last alpha controller's only", an.AlphaKeys, an.ScaleKeys)
	}
}

// A walkmesh with no tree gets one built by the compiler (buildAABBTree). Its
// planes must be the engine's (inner nodes from their children, leaves 0),
// as a compile of the decompile derives them from the tree it reads back, or
// the second compile differs (tin01_i01_01).
func TestRoundTripBuiltAABBTreePlanes(t *testing.T) {
	src := `newmodel bat
setsupermodel bat NULL
classification TILE
setanimationscale 1.0
beginmodelgeom bat
  node dummy bat
    parent NULL
  endnode
  node aabb walk
    parent bat
    bitmap blank
    verts 9
      0 0 0
      1 0 0
      2 0 0
      0 1 0
      1 1 0
      2 1 0
      0 3 0
      1 3 0
      2 3 0
    faces 8
      0 1 4  1  0 0 0  1
      0 4 3  1  0 0 0  1
      1 2 5  1  0 0 0  1
      1 5 4  1  0 0 0  1
      3 4 7  1  0 0 0  1
      3 7 6  1  0 0 0  1
      4 5 8  1  0 0 0  1
      4 8 7  1  0 0 0  1
  endnode
endmodelgeom bat
donemodel bat
`
	model := mustParseASCII(t, src)
	tree := buildAABBTree(model.FindNode("walk").Mesh)
	for i, e := range tree {
		if e.LeafFace >= 0 && e.Plane != 0 {
			t.Errorf("leaf %d: plane %d, want 0", i, e.Plane)
		}
		if e.LeafFace < 0 && e.Plane != aabbSplitPlane(tree[e.Left], tree[e.Right]) {
			t.Errorf("inner node %d: plane %d, want the engine's %d", i, e.Plane, aabbSplitPlane(tree[e.Left], tree[e.Right]))
		}
	}
	first := mustCompile(t, model)
	again := recompile(t, mustDecompile(t, first), nil)
	want, got := mustDecompile(t, first).FindNode("walk").Aabb.Entries, mustDecompile(t, again).FindNode("walk").Aabb.Entries
	if !slices.Equal(want, got) {
		t.Errorf("the compile of the decompile writes another tree:\n%v\nwas\n%v", got, want)
	}
}

// An animation node the geometry has no node for, stored with the mesh flag
// and alpha keys (tvd_udoor_09's 09_DWK_wg_closed): the writer keeps its
// type, so the keys are read back as mesh controllers, and the compiler gives
// it the mesh flag and an empty mesh header, so they are written.
func TestRoundTripAnimMeshKeysWithoutGeometry(t *testing.T) {
	src := `newmodel agn
setsupermodel agn NULL
classification DOOR
setanimationscale 1.0
beginmodelgeom agn
  node dummy agn
    parent NULL
  endnode
  node dummy grnd
    parent agn
  endnode
endmodelgeom agn
newanim closed agn
  length 1
  transtime 0
  animroot agn
  node dummy agn
    parent NULL
  endnode
  node dummy gone
    parent agn
  endnode
  node trimesh wg_closed
    parent gone
    alphakey 2
      0 0
      1 1
  endnode
doneanim closed agn
donemodel agn
`
	first := mustCompile(t, mustParseASCII(t, src))
	m := recompileStable(t, first)
	an := m.Animations[0].Nodes[2]
	if an.Name != "wg_closed" || an.Type != "trimesh" || len(an.AlphaKeys) != 2 {
		t.Fatalf("%s: type %q, %d alpha keys; want trimesh with 2", an.Name, an.Type, len(an.AlphaKeys))
	}
	var text bytes.Buffer
	if err := Write(m, &text); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(text.String(), "node trimesh wg_closed") {
		t.Error("the animation node is not written as a trimesh")
	}
	if !strings.Contains(text.String(), "node dummy gone") {
		t.Error("a node with no mesh flag is not written as a dummy")
	}
}

// A name used by two parents in different branches (q under p, and q under
// the root): the writer finds each node's children by node, so c2 is written
// after the root's q, not under p's (which a compile would then make its
// parent, the nearest q declared before).
func TestWriteDuplicateParentNameTree(t *testing.T) {
	src := `newmodel wdp
setsupermodel wdp NULL
classification PLACEABLE
setanimationscale 1.0
beginmodelgeom wdp
  node dummy wdp
    parent NULL
  endnode
  node dummy p
    parent wdp
  endnode
  node dummy q
    parent p
  endnode
  node dummy c1
    parent q
  endnode
  node dummy q
    parent wdp
  endnode
  node dummy c2
    parent q
  endnode
endmodelgeom wdp
donemodel wdp
`
	first := mustCompile(t, mustParseASCII(t, src))
	again := recompile(t, mustDecompile(t, first), nil)
	sameTree(t, storedTree(t, first), storedTree(t, again), true)
}

// Animation nodes the geometry lacks (nwn-mdl-spec MDL-NODE-044, the game's
// compile of zspec_animnogeo): each takes the type its type word names,
// whatever keys it has, and keeps only the keys that type reads. Our compile
// gives each node the game's type and controllers, the decompile of either
// writes the type word back, and that compiles to the same binary.
func TestAnimNodesWithoutGeometryFollowTypeWord(t *testing.T) {
	dir := filepath.Join("..", "..", "tests", "fixtures", "engine")
	golden, err := os.ReadFile(filepath.Join(dir, "zspec_animnogeo.mdl"))
	if err != nil {
		t.Fatal(err)
	}
	src, err := os.ReadFile(filepath.Join(dir, "zspec_animnogeo.ascii.mdl"))
	if err != nil {
		t.Fatal(err)
	}
	ours := mustCompile(t, mustParseASCII(t, string(src)))
	type want struct {
		typ   string
		ctrls []string
	}
	wants := map[string]want{
		"x": {"trimesh", []string{"position", "orientation", "alpha"}},
		"y": {"light", []string{"position", "color", "radius"}},
		"z": {"trimesh", []string{"scale", "selfillumcolor"}},
		"w": {"", []string{"position"}},
		"v": {"trimesh", []string{"position"}},
		"u": {"emitter", []string{"position", "birthrate"}},
		"t": {"", nil},
	}
	ctrls := func(an AnimNode) []string {
		var out []string
		if len(an.PositionKeys) > 0 {
			out = append(out, "position")
		}
		if len(an.OrientationKeys) > 0 {
			out = append(out, "orientation")
		}
		for _, e := range animControllerFloatList {
			if len(*e.Getter(&an)) > 0 {
				out = append(out, e.Name)
			}
		}
		for _, e := range animControllerColorList {
			if len(*e.Getter(&an)) > 0 {
				out = append(out, e.Name)
			}
		}
		slices.Sort(out)
		return out
	}
	for label, data := range map[string][]byte{"game": golden, "ours": ours} {
		m := mustDecompile(t, data)
		seen := 0
		for _, an := range m.Animations[0].Nodes {
			w, ok := wants[an.Name]
			if !ok {
				continue
			}
			seen++
			wc := slices.Clone(w.ctrls)
			slices.Sort(wc)
			if an.Type != w.typ || !slices.Equal(ctrls(an), wc) {
				t.Errorf("%s %s: type %q, controllers %v; want %q, %v", label, an.Name, an.Type, ctrls(an), w.typ, wc)
			}
		}
		if seen != len(wants) {
			t.Errorf("%s: %d of the %d nodes", label, seen, len(wants))
		}
	}
	var text bytes.Buffer
	if err := Write(mustDecompile(t, golden), &text); err != nil {
		t.Fatal(err)
	}
	for _, line := range []string{"node trimesh x", "node light y", "node trimesh v", "node emitter u", "node dummy t"} {
		if !strings.Contains(text.String(), line) {
			t.Errorf("the decompile of the game's compile has no %q", line)
		}
	}
	recompileStable(t, ours)
}
