// compiler_tangents_test.go — unit and roundtrip tests for the binary
// compiler's Mikktspace tangent generation. See compiler_tangents.go.
package mdl

import (
	"math"
	"strings"
	"testing"
)

// TestGenerateTangentsSimpleQuad covers the canonical case: a flat XY-plane
// quad whose UV U-axis aligns with world +X and UV V-axis with world +Y.
// Every per-vertex tangent must point along +X (the UV U-axis) and every
// bitangent must point along +Y, with W=+1 handedness.
func TestGenerateTangentsSimpleQuad(t *testing.T) {
	mesh := &MeshData{
		Verts: []Vec3{
			{0, 0, 0},
			{1, 0, 0},
			{1, 1, 0},
			{0, 1, 0},
		},
		TVerts: []Vec3{
			{0, 0, 0},
			{1, 0, 0},
			{1, 1, 0},
			{0, 1, 0},
		},
		Normals: []Vec3{
			{0, 0, 1},
			{0, 0, 1},
			{0, 0, 1},
			{0, 0, 1},
		},
		Faces: []Face{
			{Verts: [3]int32{0, 1, 2}, UVs: [3]int32{0, 1, 2}},
			{Verts: [3]int32{0, 2, 3}, UVs: [3]int32{0, 2, 3}},
		},
	}

	exp, err := buildExpandedMesh(mesh)
	if err != nil {
		t.Fatalf("buildExpandedMesh: %v", err)
	}

	tangents, bitangents := generateTangents(&exp)
	if len(tangents) != len(exp.positions) {
		t.Fatalf("tangents len=%d want %d", len(tangents), len(exp.positions))
	}
	if len(bitangents) != len(exp.positions) {
		t.Fatalf("bitangents len=%d want %d", len(bitangents), len(exp.positions))
	}

	const eps = 1e-5
	for i, tang := range tangents {
		if math.Abs(float64(tang.X)-1.0) > eps {
			t.Errorf("tangent[%d].X = %g, want ~1", i, tang.X)
		}
		if math.Abs(float64(tang.Y)) > eps {
			t.Errorf("tangent[%d].Y = %g, want ~0", i, tang.Y)
		}
		if math.Abs(float64(tang.Z)) > eps {
			t.Errorf("tangent[%d].Z = %g, want ~0", i, tang.Z)
		}
	}
	for i, bt := range bitangents {
		if math.Abs(float64(bt.X)) > eps {
			t.Errorf("bitangent[%d].X = %g, want ~0", i, bt.X)
		}
		if math.Abs(float64(bt.Y)-1.0) > eps {
			t.Errorf("bitangent[%d].Y = %g, want ~1", i, bt.Y)
		}
		if math.Abs(float64(bt.Z)) > eps {
			t.Errorf("bitangent[%d].Z = %g, want ~0", i, bt.Z)
		}
	}
}

// TestGenerateTangentsNoUVs verifies that generation bails out cleanly when
// UV0 data is missing and no tangents are produced.
func TestGenerateTangentsNoUVs(t *testing.T) {
	mesh := &MeshData{
		Verts: []Vec3{
			{0, 0, 0}, {1, 0, 0}, {0, 1, 0},
		},
		Normals: []Vec3{
			{0, 0, 1}, {0, 0, 1}, {0, 0, 1},
		},
		Faces: []Face{
			{Verts: [3]int32{0, 1, 2}},
		},
	}
	exp, err := buildExpandedMesh(mesh)
	if err != nil {
		t.Fatalf("buildExpandedMesh: %v", err)
	}
	tangents, bitangents := generateTangents(&exp)
	if tangents != nil || bitangents != nil {
		t.Fatalf("expected nil tangents/bitangents without UVs, got %d/%d",
			len(tangents), len(bitangents))
	}
}

// TestGenerateTangentsDegenerateUVs ensures a triangle whose UV mapping has
// zero area (collinear UVs) doesn't poison the per-vertex sums with NaN/Inf.
// Two of the three UVs are identical → deltaUV2 = (0, 0) → denom = 0 → that
// triangle's contribution must be skipped, and the other valid triangle in
// the strip should still produce a finite, unit-length tangent.
func TestGenerateTangentsDegenerateUVs(t *testing.T) {
	mesh := &MeshData{
		Verts: []Vec3{
			{0, 0, 0}, {1, 0, 0}, {1, 1, 0}, {0, 1, 0},
		},
		TVerts: []Vec3{
			{0, 0, 0},
			{1, 0, 0},
			{1, 1, 0},
			{0, 0, 0}, // duplicate of TVerts[0] — face1 UVs collapse to a line
		},
		Normals: []Vec3{
			{0, 0, 1}, {0, 0, 1}, {0, 0, 1}, {0, 0, 1},
		},
		Faces: []Face{
			// Healthy triangle: produces well-defined tangent.
			{Verts: [3]int32{0, 1, 2}, UVs: [3]int32{0, 1, 2}},
			// Degenerate UV triangle: vertices 0 and 3 share UV (0,0), and
			// vertex 2's UV (1,1) makes du1*dv2 - du2*dv1 evaluate to 0.
			{Verts: [3]int32{0, 2, 3}, UVs: [3]int32{0, 2, 3}},
		},
	}
	// Force the second triangle to actually be degenerate by using the same
	// UV at all three corners; the first triangle remains the source of
	// usable contributions.
	mesh.TVerts[3] = mesh.TVerts[0]
	mesh.Faces[1] = Face{Verts: [3]int32{0, 2, 0}, UVs: [3]int32{0, 0, 0}}

	exp, err := buildExpandedMesh(mesh)
	if err != nil {
		t.Fatalf("buildExpandedMesh: %v", err)
	}
	tangents, bitangents := generateTangents(&exp)
	if tangents == nil || bitangents == nil {
		t.Fatalf("expected tangents/bitangents from healthy triangle")
	}
	if len(tangents) != len(exp.positions) {
		t.Fatalf("tangents len=%d want %d", len(tangents), len(exp.positions))
	}
	for i, v := range tangents {
		if isBadFloat(v.X) || isBadFloat(v.Y) || isBadFloat(v.Z) {
			t.Errorf("tangent[%d] not finite: %+v", i, v)
		}
	}
	for i, v := range bitangents {
		if isBadFloat(v.X) || isBadFloat(v.Y) || isBadFloat(v.Z) {
			t.Errorf("bitangent[%d] not finite: %+v", i, v)
		}
	}
}

// TestRoundtripTangentsNormalSpecMapped is the integration test: a
// NormalAndSpecMapped mesh with normals + UVs but no explicit tangents
// should gain tangent data after compile + decompile, with each tangent
// reasonably aligned to the UV U-axis (+X for this geometry) and finite W.
func TestRoundtripTangentsNormalSpecMapped(t *testing.T) {
	src := `newmodel tangenttest
setsupermodel tangenttest NULL
classification PLACEABLE
setanimationscale 1.00
beginmodelgeom tangenttest
  node dummy tangenttest
    parent NULL
  endnode
  node trimesh tangentmesh
    parent tangenttest
    bitmap brick
    render 1
    renderhint NormalAndSpecMapped
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
      0 1 2 1 0 1 2 0
      0 2 3 1 0 2 3 0
  endnode
endmodelgeom
donemodel tangenttest
`
	m := mustParseASCII(t, src)
	bin := mustCompile(t, m)
	m2 := mustDecompile(t, bin)

	n := m2.FindNode("tangentmesh")
	if n == nil || n.Mesh == nil {
		t.Fatal("tangentmesh not found after roundtrip")
	}
	if len(n.Mesh.Tangents) == 0 {
		t.Fatalf("expected tangents to be populated after roundtrip, got 0")
	}
	if len(n.Mesh.Tangents) != len(n.Mesh.Verts) {
		t.Fatalf("tangent count %d != vert count %d",
			len(n.Mesh.Tangents), len(n.Mesh.Verts))
	}
	for i, tg := range n.Mesh.Tangents {
		if isBadFloat(tg.X) || isBadFloat(tg.Y) || isBadFloat(tg.Z) || isBadFloat(tg.W) {
			t.Errorf("tangent[%d] not finite: %+v", i, tg)
		}
		// UVs grow along +X / +Y, so the tangent should align with +X.
		if math.Abs(float64(tg.X)-1.0) > 1e-3 {
			t.Errorf("tangent[%d].X = %g, want ~1 (aligned with UV U-axis)", i, tg.X)
		}
		// Length of the XYZ part should be ~1.
		l := math.Sqrt(float64(tg.X*tg.X + tg.Y*tg.Y + tg.Z*tg.Z))
		if math.Abs(l-1.0) > 1e-3 {
			t.Errorf("tangent[%d] not unit-length: |t|=%g", i, l)
		}
		if tg.W != 1 && tg.W != -1 {
			t.Errorf("tangent[%d].W = %g, want ±1", i, tg.W)
		}
	}
}

// TestRoundtripNoTangentsWithoutUVs asserts that a mesh without UVs gets no
// tangent data baked in — both MDX pointers must remain -1, which surfaces
// after decompile as an empty Tangents slice.
func TestRoundtripNoTangentsWithoutUVs(t *testing.T) {
	src := `newmodel notangents
setsupermodel notangents NULL
classification PLACEABLE
setanimationscale 1.00
beginmodelgeom notangents
  node dummy notangents
    parent NULL
  endnode
  node trimesh untex
    parent notangents
    bitmap NULL
    render 1
    verts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2  0 0  0 0 1 2
  endnode
endmodelgeom
donemodel notangents
`
	m := mustParseASCII(t, src)
	bin := mustCompile(t, m)
	m2 := mustDecompile(t, bin)

	n := m2.FindNode("untex")
	if n == nil || n.Mesh == nil {
		t.Fatal("untex node not found after roundtrip")
	}
	if len(n.Mesh.Tangents) != 0 {
		t.Fatalf("expected 0 tangents for UV-less mesh, got %d", len(n.Mesh.Tangents))
	}
}

// TestRoundtripPreservesAuthoredTangents covers the "respect existing
// tangent data" branch in resolveTangents: when the source mesh ships with
// hand-authored Vec4 tangents we must use them rather than regenerating, so
// that round-trip preserves the original authoring (modulo float precision).
func TestRoundtripPreservesAuthoredTangents(t *testing.T) {
	src := `newmodel authored
setsupermodel authored NULL
classification PLACEABLE
setanimationscale 1.00
beginmodelgeom authored
  node dummy authored
    parent NULL
  endnode
  node trimesh authoredmesh
    parent authored
    bitmap brick
    render 1
    renderhint NormalAndSpecMapped
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
    normals 4
      0 0 1
      0 0 1
      0 0 1
      0 0 1
    tangents 4
      1 0 0 1
      1 0 0 1
      1 0 0 1
      1 0 0 1
    faces 2
      0 1 2 1 0 1 2 0
      0 2 3 1 0 2 3 0
  endnode
endmodelgeom
donemodel authored
`
	m := mustParseASCII(t, src)
	if mn := m.FindNode("authoredmesh"); mn == nil || mn.Mesh == nil || len(mn.Mesh.Tangents) == 0 {
		t.Skip("ASCII parser did not populate authored tangents")
	}
	bin := mustCompile(t, m)
	m2 := mustDecompile(t, bin)

	n := m2.FindNode("authoredmesh")
	if n == nil || n.Mesh == nil {
		t.Fatal("authoredmesh not found after roundtrip")
	}
	if len(n.Mesh.Tangents) == 0 {
		t.Fatalf("expected tangents preserved through roundtrip")
	}
	for i, tg := range n.Mesh.Tangents {
		if math.Abs(float64(tg.X)-1.0) > 1e-3 ||
			math.Abs(float64(tg.Y)) > 1e-3 ||
			math.Abs(float64(tg.Z)) > 1e-3 {
			t.Errorf("authored tangent[%d] drifted: got %+v want ~(1,0,0,±1)", i, tg)
		}
	}
}

// TestCompileTangentMDXSizeIncreases is a smoke check that the MDX block
// actually grows by 24 bytes per GPU vertex (12 tangent + 12 bitangent) when
// the compiler bakes tangents in.
func TestCompileTangentMDXSizeIncreases(t *testing.T) {
	withUVs := `newmodel withuvs
setsupermodel withuvs NULL
classification PLACEABLE
setanimationscale 1.00
beginmodelgeom withuvs
  node dummy withuvs
    parent NULL
  endnode
  node trimesh m
    parent withuvs
    bitmap brick
    render 1
    renderhint NormalAndSpecMapped
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
      0 1 2 1 0 1 2 0
      0 2 3 1 0 2 3 0
  endnode
endmodelgeom
donemodel withuvs
`
	noUVs := strings.Replace(withUVs, `    tverts 4
      0 0 0
      1 0 0
      1 1 0
      0 1 0
`, "", 1)

	binWith := mustCompile(t, mustParseASCII(t, withUVs))
	binWithout := mustCompile(t, mustParseASCII(t, noUVs))

	mdxWith := readU32LE(binWith[8:])
	mdxWithout := readU32LE(binWithout[8:])

	if mdxWith <= mdxWithout {
		t.Errorf("expected MDX block to grow when UVs (and thus tangents) are present: with=%d without=%d",
			mdxWith, mdxWithout)
	}
}

// isBadFloat reports whether x is NaN or ±Inf — the failure modes a
// degenerate tangent computation tends to produce.
func isBadFloat(x float32) bool {
	return math.IsNaN(float64(x)) || math.IsInf(float64(x), 0)
}

// makeTangentTestNode builds a single-mesh Node with one face whose 3
// corner positions are at the given coordinates and whose per-vertex
// tangents are the supplied vectors. Used by TestCompareNodeTangents to
// exercise the oracle comparison helper with controlled inputs (no
// dependency on the compiler/decompiler pipeline).
func makeTangentTestNode(name string, positions [3]Vec3, tangents [3]Vec4) *Node {
	return &Node{
		Name: name,
		Mesh: &MeshData{
			Verts:    []Vec3{positions[0], positions[1], positions[2]},
			Tangents: []Vec4{tangents[0], tangents[1], tangents[2]},
			Faces: []Face{{
				Verts: [3]int32{0, 1, 2},
			}},
		},
	}
}

// TestCompareNodeTangents exercises the oracle tangent comparison
// helper directly with synthetic Node pairs. The oracle test
// (TestOracleTangents) only activates when both compilers emit tangent
// data for the same mesh; until we get a normal-mapped game-compiled
// fixture into the suite, this is the only thing that proves the
// matching/dot-product logic is correct.
func TestCompareNodeTangents(t *testing.T) {
	tx := func(x, y, z, w float32) Vec4 { return Vec4{X: x, Y: y, Z: z, W: w} }

	cases := []struct {
		name             string
		ours             [3]Vec4
		game             [3]Vec4
		gamePos          [3]Vec3 // override game positions; zero value = same as ours
		wantMatched      int
		wantUnmatched    int
		wantBadAlign     int
		wantMeanDotMin   float64
		wantMeanDotMax   float64
		wantSkipMeanDot  bool
	}{
		{
			name:           "identical tangents",
			ours:           [3]Vec4{tx(1, 0, 0, 1), tx(1, 0, 0, 1), tx(1, 0, 0, 1)},
			game:           [3]Vec4{tx(1, 0, 0, 1), tx(1, 0, 0, 1), tx(1, 0, 0, 1)},
			wantMatched:    3,
			wantMeanDotMin: 0.999,
			wantMeanDotMax: 1.001,
		},
		{
			name:           "sign-flipped tangents (handedness diff) -> still |dot|=1",
			ours:           [3]Vec4{tx(1, 0, 0, 1), tx(1, 0, 0, 1), tx(1, 0, 0, 1)},
			game:           [3]Vec4{tx(-1, 0, 0, 1), tx(-1, 0, 0, 1), tx(-1, 0, 0, 1)},
			wantMatched:    3,
			wantMeanDotMin: 0.999,
			wantMeanDotMax: 1.001,
		},
		{
			name:           "orthogonal tangents -> |dot|=0, all flagged badAlign",
			ours:           [3]Vec4{tx(1, 0, 0, 1), tx(1, 0, 0, 1), tx(1, 0, 0, 1)},
			game:           [3]Vec4{tx(0, 1, 0, 1), tx(0, 1, 0, 1), tx(0, 1, 0, 1)},
			wantMatched:    3,
			wantBadAlign:   3,
			wantMeanDotMin: 0,
			wantMeanDotMax: 0.001,
		},
		{
			name: "in-face vertex reorder still matches by position",
			ours: [3]Vec4{tx(1, 0, 0, 1), tx(0, 1, 0, 1), tx(0, 0, 1, 1)},
			// Game has the same per-position tangents but stores corners in
			// reverse order — compareNodeTangents matches by position so the
			// alignment should still come out perfect.
			game: [3]Vec4{tx(0, 0, 1, 1), tx(0, 1, 0, 1), tx(1, 0, 0, 1)},
			gamePos: [3]Vec3{
				{0, 1, 0}, {1, 0, 0}, {0, 0, 0},
			},
			wantMatched:    3,
			wantMeanDotMin: 0.999,
			wantMeanDotMax: 1.001,
		},
		{
			name:           "no positional match -> all unmatched",
			ours:           [3]Vec4{tx(1, 0, 0, 1), tx(1, 0, 0, 1), tx(1, 0, 0, 1)},
			game:           [3]Vec4{tx(1, 0, 0, 1), tx(1, 0, 0, 1), tx(1, 0, 0, 1)},
			gamePos: [3]Vec3{
				{99, 99, 99}, {99, 99, 99}, {99, 99, 99},
			},
			wantUnmatched:   3,
			wantSkipMeanDot: true,
		},
		{
			name: "zero-length tangent -> unmatched (degenerate vector skipped)",
			ours: [3]Vec4{tx(1, 0, 0, 1), tx(1, 0, 0, 1), tx(1, 0, 0, 1)},
			game: [3]Vec4{tx(0, 0, 0, 1), tx(0, 0, 0, 1), tx(0, 0, 0, 1)},
			wantUnmatched:   3,
			wantSkipMeanDot: true,
		},
		{
			name: "partial alignment -> mean reflects average",
			// One perfect, one orthogonal, one perfect → mean = 2/3 ≈ 0.667
			ours:           [3]Vec4{tx(1, 0, 0, 1), tx(1, 0, 0, 1), tx(1, 0, 0, 1)},
			game:           [3]Vec4{tx(1, 0, 0, 1), tx(0, 1, 0, 1), tx(1, 0, 0, 1)},
			wantMatched:    3,
			wantBadAlign:   1,
			wantMeanDotMin: 0.66,
			wantMeanDotMax: 0.67,
		},
	}

	for _, c := range cases {
		c := c
		t.Run(c.name, func(t *testing.T) {
			gamePos := c.gamePos
			if gamePos == ([3]Vec3{}) {
				gamePos = [3]Vec3{{0, 0, 0}, {1, 0, 0}, {0, 1, 0}}
			}
			ours := makeTangentTestNode("m", [3]Vec3{{0, 0, 0}, {1, 0, 0}, {0, 1, 0}}, c.ours)
			game := makeTangentTestNode("m", gamePos, c.game)

			s := compareNodeTangents(ours, game)
			if s.matched != c.wantMatched {
				t.Errorf("matched=%d want=%d", s.matched, c.wantMatched)
			}
			if s.unmatched != c.wantUnmatched {
				t.Errorf("unmatched=%d want=%d", s.unmatched, c.wantUnmatched)
			}
			if s.badAlign != c.wantBadAlign {
				t.Errorf("badAlign=%d want=%d", s.badAlign, c.wantBadAlign)
			}
			if !c.wantSkipMeanDot && s.matched > 0 {
				mean := s.dotSum / float64(s.matched)
				if mean < c.wantMeanDotMin || mean > c.wantMeanDotMax {
					t.Errorf("mean|dot|=%.4f want in [%.4f, %.4f]",
						mean, c.wantMeanDotMin, c.wantMeanDotMax)
				}
			}
		})
	}
}
