package mdl

import (
	"fmt"
	"strings"
)

// fmtFloat formats a float32 in %g style, matching Prolog's format('~g').
func fmtFloat(v float32) string {
	return fmt.Sprintf("%g", v)
}

func fmtVec3(v Vec3) string {
	return fmt.Sprintf("%s %s %s", fmtFloat(v.X), fmtFloat(v.Y), fmtFloat(v.Z))
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
