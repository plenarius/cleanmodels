package mdl

import "math"

// Products are wrapped so arm64 (Apple Silicon) cannot fuse them into FMAs: the
// results then match amd64 bit for bit. See compiler_normals.go.
func vecDot(a, b Vec3) float32 {
	return float32(a.X*b.X) + float32(a.Y*b.Y) + float32(a.Z*b.Z)
}

func vecCross(a, b Vec3) Vec3 {
	return Vec3{
		X: float32(a.Y*b.Z) - float32(a.Z*b.Y),
		Y: float32(a.Z*b.X) - float32(a.X*b.Z),
		Z: float32(a.X*b.Y) - float32(a.Y*b.X),
	}
}

func vecLen(v Vec3) float32 {
	return float32(math.Sqrt(float64(float32(v.X*v.X) + float32(v.Y*v.Y) + float32(v.Z*v.Z))))
}

// vecNormalize returns the unit-length vector, or the zero vector if the
// input length is below eps. Callers that need a non-zero fallback should
// check the result themselves.
func vecNormalize(v Vec3) Vec3 {
	l := vecLen(v)
	if l < 1e-8 {
		return Vec3{}
	}
	return Vec3{X: v.X / l, Y: v.Y / l, Z: v.Z / l}
}

func vecScale(v Vec3, s float32) Vec3 {
	return Vec3{X: v.X * s, Y: v.Y * s, Z: v.Z * s}
}

func vecSub(a, b Vec3) Vec3 {
	return Vec3{X: a.X - b.X, Y: a.Y - b.Y, Z: a.Z - b.Z}
}

func vecMin(a, b Vec3) Vec3 {
	return Vec3{
		X: minF(a.X, b.X),
		Y: minF(a.Y, b.Y),
		Z: minF(a.Z, b.Z),
	}
}

func vecMax(a, b Vec3) Vec3 {
	return Vec3{
		X: maxF(a.X, b.X),
		Y: maxF(a.Y, b.Y),
		Z: maxF(a.Z, b.Z),
	}
}

func vecLerp3(a, b Vec3, t float32) Vec3 {
	return Vec3{
		X: a.X + float32((b.X-a.X)*t),
		Y: a.Y + float32((b.Y-a.Y)*t),
		Z: a.Z + float32((b.Z-a.Z)*t),
	}
}

func vecLerp4(a, b Vec4, t float32) Vec4 {
	return Vec4{
		X: a.X + float32((b.X-a.X)*t),
		Y: a.Y + float32((b.Y-a.Y)*t),
		Z: a.Z + float32((b.Z-a.Z)*t),
		W: a.W + float32((b.W-a.W)*t),
	}
}

func minF(a, b float32) float32 {
	if a < b {
		return a
	}
	return b
}

func maxF(a, b float32) float32 {
	if a > b {
		return a
	}
	return b
}

// axisAngleToQuat converts an ASCII axis-angle to a quaternion as the game's
// compiler does (AddGenericControllerSingle 0x4486f0, Parse(AurGenericKey)
// 0x446ae0): the axis goes through engineNormalize, so a zero axis becomes +X
// and a non-unit one is scaled to unit length, then sincosf of angle*0.5f
// gives the float32 sine and cosine, and the axis is multiplied by the sine in
// float32. The sine and cosine are Go's, rounded to float32.
func axisAngleToQuat(aa Vec4) Vec4 {
	ax, ay, az := engineNormalize(aa.X, aa.Y, aa.Z)
	half := float64(aa.W * 0.5)
	s := float32(math.Sin(half))
	c := float32(math.Cos(half))
	return Vec4{X: ax * s, Y: ay * s, Z: az * s, W: c}
}

// Bounds of the squared length Vector::Normalize leaves alone: 0.99999 and
// 1.00001 as the float32 constants at 0x1306cc4 and 0x1306cc8.
var (
	normalizeLow  = math.Float32frombits(0x3f7fff58)
	normalizeHigh = math.Float32frombits(0x3f800054)
)

// engineNormalize is Vector::Normalize (0x5b3700): the squared length is
// summed in float32; a vector whose squared length is 1, or within
// [normalizeLow, normalizeHigh], is returned unchanged; a zero vector becomes
// (1, 0, 0); any other is multiplied by sqrt(1/length²), all in float32.
func engineNormalize(x, y, z float32) (float32, float32, float32) {
	l2 := float32(float32(x*x)+float32(y*y)) + float32(z*z)
	if l2 == 1 || (l2 >= normalizeLow && l2 <= normalizeHigh) {
		return x, y, z
	}
	if l2 == 0 {
		return 1, 0, 0
	}
	k := float32(math.Sqrt(float64(float32(1 / l2))))
	return x * k, y * k, z * k
}

func absF32(x float32) float32 {
	if x < 0 {
		return -x
	}
	return x
}

// CloneVertexWeight creates a deep copy of a VertexWeight.
func CloneVertexWeight(w VertexWeight) VertexWeight {
	return VertexWeight{
		Bones:   append([]string(nil), w.Bones...),
		Weights: append([]float32(nil), w.Weights...),
	}
}

// EdgeKey represents a canonical undirected edge between two vertices.
type EdgeKey struct{ V0, V1 int32 }

// MakeEdgeKey creates a canonical edge key with the smaller index first.
func MakeEdgeKey(a, b int32) EdgeKey {
	if a > b {
		a, b = b, a
	}
	return EdgeKey{a, b}
}

// BuildEdgeFaceMap builds a map from each edge to the list of face indices containing it.
func BuildEdgeFaceMap(faces []Face) map[EdgeKey][]int {
	m := make(map[EdgeKey][]int)
	for fi, f := range faces {
		for ei := 0; ei < 3; ei++ {
			e := MakeEdgeKey(f.Verts[ei], f.Verts[(ei+1)%3])
			m[e] = append(m[e], fi)
		}
	}
	return m
}
