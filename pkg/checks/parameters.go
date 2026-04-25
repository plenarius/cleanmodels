package checks

import (
	"fmt"
	"strings"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

const nwnResrefLimit = 16

func init() {
	Register("black_ambient", "parameters", false, "Detect zero ambient with non-zero diffuse", checkBlackAmbient)
	Register("black_diffuse", "parameters", false, "Detect zero diffuse color on mesh nodes", checkBlackDiffuse)
	Register("renderhint_consistency", "parameters", true, "Clear mismatched renderhint/materialname pairs", checkRenderhintConsistency)
	Register("tangent_validation", "parameters", false, "Detect missing tangents on normal-mapped meshes", checkTangentValidation)
	Register("null_bitmaps", "parameters", true, "Set render=0 on meshes with missing bitmaps", checkNullBitmaps)
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

func checkRenderhintConsistency(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil {
			continue
		}
		mh := n.Mesh
		hasHint := mh.RenderHint != ""
		hasMat := mh.MaterialName != ""
		if hasHint != hasMat {
			oldHint, oldMat := mh.RenderHint, mh.MaterialName
			fixed := false
			if fix {
				mh.RenderHint = ""
				mh.MaterialName = ""
				fixed = true
			}
			out = append(out, mdl.CheckResult{
				Check:    "renderhint_consistency",
				Node:     n.Name,
				Severity: mdl.SevWarning,
				Fixed:    fixed,
				Message: fmt.Sprintf(
					"%s: mesh node %q had renderhint %q and materialname %q (mismatched), cleared both",
					file, n.Name, oldHint, oldMat,
				),
			})
		}
	}
	return out
}

func checkTangentValidation(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil {
			continue
		}
		if n.Mesh.RenderHint != "NormalAndSpecMapped" {
			continue
		}
		if len(n.Mesh.Tangents) == 0 {
			out = append(out, mdl.CheckResult{
				Check:    "tangent_validation",
				Node:     n.Name,
				Severity: mdl.SevWarning,
				Message: fmt.Sprintf(
					"%s: mesh node %q uses RenderHint NormalAndSpecMapped but has no tangents",
					file, n.Name,
				),
				Fixed: false,
			})
		}
	}
	return out
}

func checkNullBitmaps(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil {
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
					"%s: %s node %q had render=1 but no bitmap, set render=0",
					file, nt, n.Name,
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
					"%s: mesh node %q had %d vertex colors but %d vertices, adjusted",
					file, n.Name, oldCount, nv,
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
