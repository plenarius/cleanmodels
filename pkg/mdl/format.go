package mdl

import (
	"strconv"
	"strings"
)

// fmtFloat formats a float32 in %g style, matching Prolog's format('~g').
//
// Equivalent to fmt.Sprintf("%g", v) but ~10× faster: no reflection and
// the AppendFloat path uses the same Ryu/Grisu shortest-roundtrip
// algorithm, so output is bit-identical for all finite float32 values.
func fmtFloat(v float32) string {
	return string(appendFloat(nil, v))
}

func fmtVec3(v Vec3) string {
	b := make([]byte, 0, 36)
	b = appendFloat(b, v.X)
	b = append(b, ' ')
	b = appendFloat(b, v.Y)
	b = append(b, ' ')
	b = appendFloat(b, v.Z)
	return string(b)
}

// appendFloat appends v in %g format to b. bitSize=32 mirrors fmt's
// default behaviour for float32 arguments — i.e. the shortest decimal
// that roundtrips back to the same float32, not float64.
func appendFloat(b []byte, v float32) []byte {
	return strconv.AppendFloat(b, float64(v), 'g', -1, 32)
}

// appendVec3 appends "X Y Z" in %g format with single-space separators.
func appendVec3(b []byte, v Vec3) []byte {
	b = appendFloat(b, v.X)
	b = append(b, ' ')
	b = appendFloat(b, v.Y)
	b = append(b, ' ')
	b = appendFloat(b, v.Z)
	return b
}

// indentBytes is a precomputed two-space indent string. The writer only
// uses indent levels 2, 3, 4, and 6 in practice; values past those use
// a dynamic build path that's still cheaper than fmt.Fprintf.
var indentBytes = []byte("                ") // 8 levels × 2 spaces

// appendIndent appends `level` two-space groups to b, falling back to
// dynamic length for the rare deep-indent case.
func appendIndent(b []byte, level int) []byte {
	if level <= 0 {
		return b
	}
	n := level * 2
	if n <= len(indentBytes) {
		return append(b, indentBytes[:n]...)
	}
	for i := 0; i < level; i++ {
		b = append(b, ' ', ' ')
	}
	return b
}

// camelCase converts an emitter enum string to its CamelCase form.
// Ref: output_models.pl camel/2
var camelMap = map[string]string{
	"fountain":                "Fountain",
	"single":                  "Single",
	"explosion":               "Explosion",
	"lightning":               "Lightning",
	"normal":                  "Normal",
	"linked":                  "Linked",
	"billboard_to_local_z":    "Billboard_to_Local_Z",
	"billboard_to_world_z":    "Billboard_to_World_Z",
	"aligned_to_world_z":      "Aligned_to_World_Z",
	"aligned_to_particle_dir": "Aligned_to_Particle_Dir",
	"motion_blur":             "Motion_Blur",
	"punch-through":           "Punch-Through",
	"lighten":                 "Lighten",
	"trail":                   "Trail",
	"bezier":                  "Bezier",
	"gravity":                 "Gravity",
	"dummy":                   "Dummy",
}

func camelCase(s string) string {
	lower := strings.ToLower(s)
	if cc, ok := camelMap[lower]; ok {
		return cc
	}
	return s
}
