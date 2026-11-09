package mdl

import (
	"bytes"
	"encoding/binary"
	"math"
	"strings"
	"testing"
)

// The quad of hintTangentModel generates tangent (1, 0, 0) with handedness +1
// at every vertex. Its stored tangents are (0.28, 0.96, 0) with handedness -1,
// so a kept one and a filled one are told apart.
const storedTangentRow = "0.28 0.96 0 -1"

// patchedTangentQuad compiles the quad with stored tangents and overwrites the
// tangent stream's vectors listed in patch (vertex index -> three raw floats).
// With texAnim0 it also sets texture animation slot 0, as a 1.69 water mesh
// with a bump-map stream has it.
func patchedTangentQuad(t *testing.T, hintLine string, patch map[int][3]float32, texAnim0 bool) []byte {
	t.Helper()
	bin := mustCompile(t, mustParseASCII(t, hintTangentModel(hintLine, storedTangentRow)))
	var row bytes.Buffer
	for _, f := range []float32{0.28, 0.96, 0} {
		binary.Write(&row, binary.LittleEndian, f)
	}
	stream := bytes.Repeat(row.Bytes(), 4)
	at := bytes.Index(bin, stream)
	if at < 0 || bytes.Index(bin[at+1:], stream) >= 0 {
		t.Fatalf("tangent stream not found exactly once")
	}
	for v, f := range patch {
		for k := 0; k < 3; k++ {
			binary.LittleEndian.PutUint32(bin[at+12*v+4*k:], math.Float32bits(f[k]))
		}
	}
	if texAnim0 {
		// The mesh header's slot 3 (p_mdx_tex_anim3) holds the stream's MDX
		// pointer; slot 0 is three words before it.
		ptr := make([]byte, 4)
		binary.LittleEndian.PutUint32(ptr, uint32(at-12-int(binary.LittleEndian.Uint32(bin[4:]))))
		p := bytes.Index(bin, ptr)
		if p < 12 || bytes.Index(bin[p+1:], ptr) >= 0 {
			t.Fatalf("tangent pointer not found exactly once")
		}
		copy(bin[p-12:], ptr)
	}
	return bin
}

// A tangent stream with a few NaN and zero vectors among unit ones is read as
// tangents (MDL-BIN-048); only the bad vectors are replaced, with the tangent
// the game generates for that vertex, and one warning says so.
func TestDecompileFillsBadTangents(t *testing.T) {
	nan := float32(math.NaN())
	bin := patchedTangentQuad(t, "", map[int][3]float32{
		1: {nan, nan, nan},
		2: {0, 0, 0},
	}, false)
	m := mustDecompile(t, bin)
	n := m.FindNode("quad")
	if n == nil || n.Mesh == nil {
		t.Fatal("quad not found")
	}
	stored := Vec4{X: 0.28, Y: 0.96, Z: 0, W: -1}
	want := []Vec4{stored, {X: 1, W: 1}, {X: 1, W: 1}, stored}
	if len(n.Mesh.Tangents) != len(want) {
		t.Fatalf("tangents: %v", n.Mesh.Tangents)
	}
	for i, w := range want {
		if n.Mesh.Tangents[i] != w {
			t.Errorf("tangent %d = %v, want %v", i, n.Mesh.Tangents[i], w)
		}
	}
	if len(m.Warnings) != 1 || m.Warnings[0].Node != "quad" ||
		!strings.HasPrefix(m.Warnings[0].Message, `mesh "quad": 2 of 4 stored tangents are unusable (1 NaN or infinite, 1 zero); filled with`) {
		t.Errorf("warnings: %+v", m.Warnings)
	}
}

// Which slot-3 streams are read as tangents. The 1.69 bump-map streams (all
// zeros in Layonara's tcn01 water01, tiny values in stock tcn01_a15_01
// plane452, one vector of length 1 by chance and zeros with texture animation
// slots 0, 1, 2 and 4 set in stock tcn01_l03_01 plane211) must stay refused.
func TestDecompileTangentStreamGateMixed(t *testing.T) {
	nan := float32(math.NaN())
	inf := float32(math.Inf(1))
	cases := []struct {
		name, hint string
		patch      map[int][3]float32
		texAnim0   bool
		keep       bool
		filled     int
	}{
		{"all unit", "", nil, false, true, 0},
		{"all unit, slot 0 set", "", nil, true, true, 0},
		{"unit with NaN and infinite", "", map[int][3]float32{0: {nan, 0, 0}, 3: {0, inf, 0}}, false, true, 2},
		{"unit with zero", "", map[int][3]float32{2: {0, 0, 0}}, false, true, 1},
		{"unit with zero, slot 0 set", "", map[int][3]float32{2: {0, 0, 0}}, true, false, 0},
		{"one of length 1 by chance, zeros, slot 0 set (tcn01_l03_01)", "", map[int][3]float32{0: {6.142437e-33, 3.07993e-36, 1.00106}, 1: {}, 2: {}, 3: {}}, true, false, 0},
		{"unit with one of another length", "", map[int][3]float32{2: {3.080194e-36, 3.080194e-36, 0}}, false, false, 0},
		{"all zero", "", map[int][3]float32{0: {}, 1: {}, 2: {}, 3: {}}, false, false, 0},
		{"all NaN", "", map[int][3]float32{0: {nan, nan, nan}, 1: {nan, nan, nan}, 2: {nan, nan, nan}, 3: {nan, nan, nan}}, false, false, 0},
		{"bump-map junk", "", map[int][3]float32{0: {3.080194e-36, 3.080194e-36, 0}, 1: {3.080194e-36, 3.080194e-36, 0}, 2: {}, 3: {}}, false, false, 0},
		{"normal-mapped hint, another length filled", "    renderhint NormalTangents", map[int][3]float32{2: {0.5, 0, 0}}, false, true, 1},
	}
	for _, c := range cases {
		m := mustDecompile(t, patchedTangentQuad(t, c.hint, c.patch, c.texAnim0))
		n := m.FindNode("quad")
		if n == nil || n.Mesh == nil {
			t.Fatalf("%s: quad not found", c.name)
		}
		if got := len(n.Mesh.Tangents) > 0; got != c.keep {
			t.Errorf("%s: tangents kept = %v, want %v", c.name, got, c.keep)
			continue
		}
		filled := 0
		for _, tg := range n.Mesh.Tangents {
			if tg == (Vec4{X: 1, W: 1}) {
				filled++
			}
		}
		if filled != c.filled {
			t.Errorf("%s: %d tangents filled, want %d: %v", c.name, filled, c.filled, n.Mesh.Tangents)
		}
		warned := false
		for _, w := range m.Warnings {
			warned = warned || strings.Contains(w.Message, "stored tangents are unusable")
		}
		if warned != (c.filled > 0) {
			t.Errorf("%s: warnings %+v", c.name, m.Warnings)
		}
	}
}

// A vertex the generation gives no tangent (no triangle reaches it) gets one
// perpendicular to its normal.
func TestFallbackTangent(t *testing.T) {
	for _, nv := range []Vec3{{Z: 1}, {X: 1}, {}, {X: 0.57735026, Y: 0.57735026, Z: 0.57735026}, {Y: -3}} {
		got := fallbackTangent(nv)
		tv := Vec3{X: got.X, Y: got.Y, Z: got.Z}
		if !isUnitTangent(tv) || got.W != 1 {
			t.Errorf("fallbackTangent(%v) = %v, not unit", nv, got)
		}
		l := math.Sqrt(float64(nv.X*nv.X + nv.Y*nv.Y + nv.Z*nv.Z))
		if l > 0 && math.Abs(float64(nv.X*tv.X+nv.Y*tv.Y+nv.Z*tv.Z))/l > 1e-5 {
			t.Errorf("fallbackTangent(%v) = %v, not perpendicular", nv, got)
		}
	}
}
