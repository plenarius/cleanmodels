package checks

import (
	"fmt"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

const nwnResrefLimit = 16

func init() {
	Register("black_ambient", "parameters", false, "Detect zero ambient with non-zero diffuse", checkBlackAmbient)
	Register("black_diffuse", "parameters", false, "Detect zero diffuse color on mesh nodes", checkBlackDiffuse)
	Register("renderhint_consistency", "parameters", false, "Note a mesh with a renderhint and no materialname, or the reverse (info: both are valid; the hint then comes from the mesh or from its material)", checkRenderhintConsistency)
	Register("tangent_validation", "parameters", false, "Detect tangents of zero length, and a normal-mapped mesh of a binary model that stores no tangents", checkTangentValidation)
	Register("null_bitmaps", "parameters", true, "Set render=0 on meshes with missing bitmaps (not armour parts, which the game textures from their PLT)", checkNullBitmaps)
	Register("shadow_render_consistency", "parameters", false, "Detect shadow=1 with render=0 inconsistency", checkShadowRenderConsistency)
	Register("vertex_color_count", "parameters", true, "Fix vertex color array to match vertex count", checkVertexColorCount)
	Register("resref_length", "parameters", false, "Detect texture/material names exceeding 16-char resref limit", checkResrefLength)
	Register("plt_bitmap_match", "parameters", false, "Detect PLT bitmap names that don't match the model name", checkPltBitmapMatch)
}

func vec3IsZero(v mdl.Vec3) bool {
	return v.X == 0 && v.Y == 0 && v.Z == 0
}

func checkBlackAmbient(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil {
			continue
		}
		if vec3IsZero(n.Mesh.Ambient) && !vec3IsZero(n.Mesh.Diffuse) {
			out = append(out, mdl.CheckResult{
				Check:    "black_ambient",
				Node:     n.Name,
				Severity: mdl.SevInfo,
				Message: fmt.Sprintf(
					"%s: mesh node %q has zero ambient with non-zero diffuse",
					file, n.Name,
				),
				Fixed: false,
			})
		}
	}
	return out
}

func checkBlackDiffuse(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil {
			continue
		}
		if vec3IsZero(n.Mesh.Diffuse) {
			out = append(out, mdl.CheckResult{
				Check:    "black_diffuse",
				Node:     n.Name,
				Severity: mdl.SevInfo,
				Message: fmt.Sprintf(
					"%s: mesh node %q has zero diffuse",
					file, n.Name,
				),
				Fixed: false,
			})
		}
	}
	return out
}

// checkRenderhintConsistency notes a mesh that has a renderhint and no
// materialname, or a materialname and no renderhint. Both are valid EE
// material setups, so it is information only and changes nothing: a mesh's
// own renderhint is used as given (nwn-mdl-spec MDL-MESH-040), and a mesh with
// none takes the hint of its material (MDL-MESH-041). Clearing either would
// strip the normal mapping from models such as the tmd18c tiles.
func checkRenderhintConsistency(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil {
			continue
		}
		mh := n.Mesh
		var msg string
		switch {
		case mh.RenderHint != "" && mh.MaterialName == "":
			msg = fmt.Sprintf("%s: mesh node %q has renderhint %q and no materialname; the game uses the mesh's own hint",
				file, n.Name, mh.RenderHint)
		case mh.RenderHint == "" && mh.MaterialName != "":
			msg = fmt.Sprintf("%s: mesh node %q has materialname %q and no renderhint; the game takes the hint from the material",
				file, n.Name, mh.MaterialName)
		default:
			continue
		}
		out = append(out, mdl.CheckResult{
			Check:    "renderhint_consistency",
			Node:     n.Name,
			Severity: mdl.SevInfo,
			Message:  msg,
		})
	}
	return out
}

// checkTangentValidation reports tangents of zero length, which the game draws
// as white streaks on a normal-mapped surface; the fix drops them so the
// compiler computes them all from the geometry. It also reports a mesh of a
// binary model whose render hint is normal-mapped and which stores no
// tangents: the game reads a binary's tangents as stored (E31) and draws that
// mesh without them. An ASCII model needs none: the compiler generates them
// for hints 2 and 3 (MDL-MESH-042), so that finding is for binary input only.
func checkTangentValidation(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil {
			continue
		}
		if z := mdl.ZeroLengthTangents(n.Mesh); z > 0 {
			total := len(n.Mesh.Tangents)
			if fix {
				n.Mesh.Tangents = nil
			}
			out = append(out, mdl.CheckResult{
				Check:    "tangent_validation",
				Node:     n.Name,
				Severity: mdl.SevWarning,
				Fixed:    fix,
				Message: fmt.Sprintf(
					"%s: mesh node %q has %d of %d tangents of zero length (drawn as white streaks); %s",
					file, n.Name, z, total,
					fixVerb(fix, "dropped them so the compiler computes them", "dropping them lets the compiler compute them"),
				),
			})
			continue
		}
		if model.FileType != "binary" || n.Mesh.TangentsOmitted || len(n.Mesh.Tangents) != 0 {
			continue
		}
		if hint := n.Mesh.RenderHint; strings.EqualFold(hint, "NormalAndSpecMapped") || strings.EqualFold(hint, "NormalTangents") {
			out = append(out, mdl.CheckResult{
				Check:    "tangent_validation",
				Node:     n.Name,
				Severity: mdl.SevWarning,
				Message: fmt.Sprintf(
					"%s: mesh node %q uses RenderHint %s but the binary stores no tangents, so the game draws it without normal mapping; compiling the decompiled model generates them",
					file, n.Name, hint,
				),
			})
		}
	}
	return out
}

// armourPartName matches the models the game builds a creature's body from:
// p, gender, race, phenotype, then the slot and the part number
// (pmh0_chest001, pfb2_legl080).
var armourPartName = regexp.MustCompile(`^p[mf][a-z][0-9]+_(head|neck|chest|belt|pelvis|shol|shor|bicepl|bicepr|forel|forer|handl|handr|legl|legr|shinl|shinr|footl|footr|robe)[0-9]{3,}$`)

// isArmourPart reports whether a model, by its name or its file name, is an
// armour part, which the game textures from the part's PLT whatever bitmap its
// meshes name (nwn-mdl-spec E30).
func isArmourPart(model *mdl.Model, file string) bool {
	stem := strings.TrimSuffix(filepath.Base(file), filepath.Ext(file))
	return armourPartName.MatchString(strings.ToLower(model.Name)) ||
		armourPartName.MatchString(strings.ToLower(stem))
}

// checkNullBitmaps reports meshes drawn with no bitmap; the fix stops drawing
// them (render 0). Armour parts are left alone: the game replaces their
// texture with the part's PLT (E30), so a part with no bitmap is drawn
// textured, and render 0 would hide it.
func checkNullBitmaps(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil || isArmourPart(model, file) {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil {
			continue
		}
		nt := n.NodeType()
		if nt != "trimesh" && nt != "skin" {
			continue
		}
		if n.Mesh.Render == 1 && n.Mesh.Bitmap == "" {
			fixed := false
			if fix {
				n.Mesh.Render = 0
				fixed = true
			}
			out = append(out, mdl.CheckResult{
				Check:    "null_bitmaps",
				Node:     n.Name,
				Severity: mdl.SevWarning,
				Fixed:    fixed,
				Message: fmt.Sprintf(
					"%s: %s node %q has render=1 but no bitmap; %s",
					file, nt, n.Name, fixVerb(fix, "set render=0", "would set render=0"),
				),
			})
		}
	}
	return out
}

func checkShadowRenderConsistency(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil {
			continue
		}
		if n.Mesh.Shadow == 1 && n.Mesh.Render == 0 {
			out = append(out, mdl.CheckResult{
				Check:    "shadow_render_consistency",
				Node:     n.Name,
				Severity: mdl.SevInfo,
				Message: fmt.Sprintf(
					"%s: mesh node %q has shadow=1 but render=0",
					file, n.Name,
				),
				Fixed: false,
			})
		}
	}
	return out
}

func checkVertexColorCount(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil {
			continue
		}
		mh := n.Mesh
		if len(mh.Colors) == 0 {
			continue
		}
		if len(mh.Colors) != len(mh.Verts) {
			oldCount := len(mh.Colors)
			nv := len(mh.Verts)
			fixed := false
			if fix {
				if oldCount > nv {
					mh.Colors = mh.Colors[:nv]
				} else {
					for len(mh.Colors) < nv {
						mh.Colors = append(mh.Colors, mdl.Vec3{X: 1, Y: 1, Z: 1})
					}
				}
				fixed = true
			}
			out = append(out, mdl.CheckResult{
				Check:    "vertex_color_count",
				Node:     n.Name,
				Severity: mdl.SevError,
				Fixed:    fixed,
				Message: fmt.Sprintf(
					"%s: mesh node %q has %d vertex colors but %d vertices; %s",
					file, n.Name, oldCount, nv, fixVerb(fix, "adjusted", "would be adjusted"),
				),
			})
		}
	}
	return out
}

func checkResrefLength(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	type field struct {
		name string
		val  string
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil {
			continue
		}
		for _, f := range []field{
			{"bitmap", n.Mesh.Bitmap},
			{"texture1", n.Mesh.Texture1},
			{"texture2", n.Mesh.Texture2},
			{"materialname", n.Mesh.MaterialName},
		} {
			if len(f.val) > nwnResrefLimit {
				out = append(out, mdl.CheckResult{
					Check:    "resref_length",
					Node:     n.Name,
					Severity: mdl.SevWarning,
					Message: fmt.Sprintf(
						"%s: mesh node %q %s %q is %d chars (max %d)",
						file, n.Name, f.name, f.val, len(f.val), nwnResrefLimit,
					),
				})
			}
		}
	}
	return out
}

func checkPltBitmapMatch(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	if !strings.EqualFold(model.Classification, "CHARACTER") {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil || n.Mesh.Bitmap == "" {
			continue
		}
		bmp := strings.ToLower(n.Mesh.Bitmap)
		if !strings.Contains(bmp, "plt") {
			continue
		}
		expected := strings.ToLower(model.Name)
		if bmp != expected {
			out = append(out, mdl.CheckResult{
				Check:    "plt_bitmap_match",
				Node:     n.Name,
				Severity: mdl.SevWarning,
				Message: fmt.Sprintf(
					"%s: PLT bitmap %q does not match model name %q",
					file, n.Mesh.Bitmap, model.Name,
				),
			})
		}
	}
	return out
}
