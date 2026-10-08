package mdl

import "math"

func vecDot(a, b Vec3) float32 {
	return a.X*b.X + a.Y*b.Y + a.Z*b.Z
}

func vecCross(a, b Vec3) Vec3 {
	return Vec3{
		X: a.Y*b.Z - a.Z*b.Y,
		Y: a.Z*b.X - a.X*b.Z,
		Z: a.X*b.Y - a.Y*b.X,
	}
}

func vecLen(v Vec3) float32 {
	return float32(math.Sqrt(float64(v.X*v.X + v.Y*v.Y + v.Z*v.Z)))
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
		X: a.X + (b.X-a.X)*t,
		Y: a.Y + (b.Y-a.Y)*t,
		Z: a.Z + (b.Z-a.Z)*t,
	}
}

func vecLerp4(a, b Vec4, t float32) Vec4 {
	return Vec4{
		X: a.X + (b.X-a.X)*t,
		Y: a.Y + (b.Y-a.Y)*t,
		Z: a.Z + (b.Z-a.Z)*t,
		W: a.W + (b.W-a.W)*t,
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

func axisAngleToQuat(aa Vec4) Vec4 {
	angle := float64(aa.W)
	s := math.Sin(angle / 2)
	c := math.Cos(angle / 2)
	return Vec4{
		X: float32(float64(aa.X) * s),
		Y: float32(float64(aa.Y) * s),
		Z: float32(float64(aa.Z) * s),
		W: float32(c),
	}
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
