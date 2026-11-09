package mdl

import (
	"bytes"
	"math"
	"math/rand"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// plainAngleAxis is quaternionToAngleAxis without the exact-preimage search.
func plainAngleAxis(q Vec4) Vec4 {
	s := float32(math.Sqrt(float64(float32(q.X*q.X) + float32(q.Y*q.Y) + float32(q.Z*q.Z))))
	a := float32(2.0 * math.Atan2(float64(s), float64(q.W)))
	return Vec4{X: q.X / s, Y: q.Y / s, Z: q.Z / s, W: a}
}

func decompiledAngleAxis(q Vec4) Vec4 {
	aa := quaternionToAngleAxis([]float32{q.X, q.Y, q.Z, q.W})
	return Vec4{X: aa[0], Y: aa[1], Z: aa[2], W: aa[3]}
}

// TestQuaternionToAngleAxisExact: quaternions from tde01_p15_01 (which never
// settled), tde01_a03_01 (angle near pi) and plc_a09 that the plain
// conversion does not give back exactly; the decompile must.
func TestQuaternionToAngleAxisExact(t *testing.T) {
	for _, aa := range []Vec4{
		{X: -0.70644635, Y: 0.70644635, Z: 0.04320978, W: 0.17272002},
		{X: 0, Y: -0.79863966, Z: -0.6018097, W: 3.14159},
		{X: 0.70710677, Y: 0.70710677, Z: 0, W: 3.1415901},
		{X: 1, Y: 0, Z: 0, W: -0.012599919},
		// Past 2pi: written as the negated axis and 4pi minus the angle.
		{X: -0.90267134, Y: 0.43029666, Z: -0.0054048495, W: -6.5626225},
		{X: -0.8758026, Y: -0.002978856, Z: -0.48266035, W: -7.683061},
	} {
		q := axisAngleToQuat(aa)
		if sameQuatBits(axisAngleToQuat(plainAngleAxis(q)), q) {
			t.Errorf("%v: the plain conversion already gives back %v; not a test case", aa, q)
		}
		if got := axisAngleToQuat(decompiledAngleAxis(q)); !sameQuatBits(got, q) {
			t.Errorf("%v: compiling the decompiled axis-angle gives %v, want %v", aa, got, q)
		}
	}
}

// TestQuaternionToAngleAxisExactRandom: every quaternion the compiler writes
// (random axes; angles near 0 and pi, negative, and up to 4pi either way) is
// decompiled to an axis-angle that compiles back to the same bits.
func TestQuaternionToAngleAxisExactRandom(t *testing.T) {
	r := rand.New(rand.NewSource(1))
	plainMisses := 0
	for i := 0; i < 50000; i++ {
		x, y, z := r.Float64()*2-1, r.Float64()*2-1, r.Float64()*2-1
		l := math.Sqrt(x*x + y*y + z*z)
		aa := Vec4{X: float32(x / l), Y: float32(y / l), Z: float32(z / l)}
		switch i % 5 {
		case 0:
			aa.W = float32(r.Float64() * 2 * math.Pi)
		case 1:
			aa.W = float32(math.Pi - r.Float64()*1e-3)
		case 2:
			aa.W = float32(r.Float64() * 1e-2)
		case 3:
			aa.W = float32(r.Float64()*16 - 8)
		case 4:
			aa.W = float32((r.Float64()*2 - 1) * 4 * math.Pi)
		}
		q := axisAngleToQuat(aa)
		if float32(math.Sqrt(float64(float32(q.X*q.X)+float32(q.Y*q.Y)+float32(q.Z*q.Z)))) < 1e-6 {
			continue // decompiled as the identity
		}
		if !sameQuatBits(axisAngleToQuat(plainAngleAxis(q)), q) {
			plainMisses++
		}
		if got := axisAngleToQuat(decompiledAngleAxis(q)); !sameQuatBits(got, q) {
			t.Fatalf("axis-angle %v: quaternion %v decompiles to %v, which compiles to %v",
				aa, q, decompiledAngleAxis(q), got)
		}
	}
	if plainMisses == 0 {
		t.Error("the plain conversion gave every quaternion back; the test checks nothing")
	}
}

// TestExactAxisAngleKeepsUnreachable: a quaternion no nearby axis-angle
// reaches (not unit length) is written as the plain conversion gives it.
func TestExactAxisAngleKeepsUnreachable(t *testing.T) {
	q := Vec4{X: 0.3, Y: 0.3, Z: 0.3, W: 0.3}
	if got, want := decompiledAngleAxis(q), plainAngleAxis(q); !sameQuatBits(got, want) {
		t.Errorf("got %v, want the plain conversion %v", got, want)
	}
}

func TestF32Step(t *testing.T) {
	neg0 := float32(math.Copysign(0, -1))
	tiny := math.Float32frombits(1)
	for _, c := range []struct {
		v    float32
		k    int
		want uint32
	}{
		{1, 1, 0x3f800001},
		{1, -1, 0x3f7fffff},
		{-1, 1, 0xbf7fffff},
		{-1, -1, 0xbf800001},
		{neg0, 0, 0x80000000},
		{neg0, 1, 0x00000001},
		{0, -1, 0x80000001},
		{tiny, -1, 0},
		{-tiny, 1, 0},
		{tiny, -2, 0x80000001},
	} {
		if got := math.Float32bits(f32Step(c.v, c.k)); got != c.want {
			t.Errorf("f32Step(%v, %d) = %#08x, want %#08x", c.v, c.k, got, c.want)
		}
	}
}

// TestExactAxisAngleRoundTripStable: compiling zcm_quat, then decompiling
// and compiling twice, gives the same bytes every time.
func TestExactAxisAngleRoundTripStable(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "..", "tests", "fixtures", "quat", "zcm_quat.ascii.mdl"))
	if err != nil {
		t.Fatal(err)
	}
	b1 := mustCompile(t, mustParseASCII(t, string(src)))
	prev := b1
	for pass := 2; pass <= 3; pass++ {
		var ascii bytes.Buffer
		if err := Write(mustDecompile(t, prev), &ascii); err != nil {
			t.Fatal(err)
		}
		next := mustCompile(t, mustParseASCII(t, ascii.String()))
		if !bytes.Equal(next, b1) {
			t.Fatalf("pass %d binary differs from the first compile", pass)
		}
		if !strings.Contains(ascii.String(), "orientation -0.7064463 0.7064463 0.043209776 0.17272003") {
			t.Errorf("pass %d: d1's orientation is not the exact preimage:\n%s", pass, ascii.String())
		}
		prev = next
	}
	fixture, err := os.ReadFile(filepath.Join("..", "..", "tests", "fixtures", "quat", "zcm_quat.mdl"))
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(fixture, b1) {
		t.Error("tests/fixtures/quat/zcm_quat.mdl is not the compile of zcm_quat.ascii.mdl")
	}
}
