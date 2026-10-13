package checks

import (
	"fmt"
	"strings"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

func init() {
	Register("danglymesh_fields", "node_types", false, "Validate required fields on danglymesh nodes", checkDanglymeshFields)
	Register("skin_fields", "node_types", false, "Validate required fields on skin nodes", checkSkinFields)
	Register("emitter_fields", "node_types", false, "Validate required fields on emitter nodes", checkEmitterFields)
	Register("light_fields", "node_types", false, "Validate required fields on light nodes", checkLightFields)
	Register("aabb_fields", "node_types", false, "Validate required fields on AABB walkmesh nodes", checkAabbFields)
	Register("misplaced_tile_fields", "node_types", false, "Detect tile-only fields on non-TILE models", checkMisplacedTileFields)
	Register("misplaced_dangly_data", "node_types", false, "Detect danglymesh-only constraints on non-danglymesh nodes", checkMisplacedDanglyData)
}

func checkDanglymeshFields(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Dangly == nil {
			continue
		}
		if n.Mesh == nil || len(n.Mesh.Verts) == 0 {
			out = append(out, mdl.CheckResult{
				Check:    "danglymesh_fields",
				Node:     n.Name,
				Severity: mdl.SevError,
				Message:  fmt.Sprintf("%s: danglymesh node %q has no mesh geometry", file, n.Name),
			})
			continue
		}
		if n.Dangly.Displacement <= 0 {
			out = append(out, mdl.CheckResult{
				Check:    "danglymesh_fields",
				Node:     n.Name,
				Severity: mdl.SevWarning,
				Message:  fmt.Sprintf("%s: danglymesh node %q has displacement %g (expected > 0)", file, n.Name, n.Dangly.Displacement),
			})
		}
		if n.Dangly.Period <= 0 {
			out = append(out, mdl.CheckResult{
				Check:    "danglymesh_fields",
				Node:     n.Name,
				Severity: mdl.SevWarning,
				Message:  fmt.Sprintf("%s: danglymesh node %q has period %g (expected > 0)", file, n.Name, n.Dangly.Period),
			})
		}
		if len(n.Dangly.Constraints) == 0 {
			out = append(out, mdl.CheckResult{
				Check:    "danglymesh_fields",
				Node:     n.Name,
				Severity: mdl.SevError,
				Message:  fmt.Sprintf("%s: danglymesh node %q has no constraints array", file, n.Name),
			})
		}
	}
	return out
}

func checkSkinFields(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Skin == nil {
			continue
		}
		if n.Mesh == nil || len(n.Mesh.Verts) == 0 {
			out = append(out, mdl.CheckResult{
				Check:    "skin_fields",
				Node:     n.Name,
				Severity: mdl.SevError,
				Message:  fmt.Sprintf("%s: skin node %q has no mesh geometry", file, n.Name),
			})
			continue
		}
		if len(n.Skin.Weights) == 0 {
			out = append(out, mdl.CheckResult{
				Check:    "skin_fields",
				Node:     n.Name,
				Severity: mdl.SevError,
				Message:  fmt.Sprintf("%s: skin node %q has no bone weights", file, n.Name),
			})
		} else if len(n.Skin.Weights) != len(n.Mesh.Verts) {
			out = append(out, mdl.CheckResult{
				Check:    "skin_fields",
				Node:     n.Name,
				Severity: mdl.SevError,
				Message: fmt.Sprintf("%s: skin node %q has %d weights but %d vertices",
					file, n.Name, len(n.Skin.Weights), len(n.Mesh.Verts)),
			})
		}
	}
	return out
}

func checkEmitterFields(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Emitter == nil {
			continue
		}
		if n.Emitter.Texture == "" {
			out = append(out, mdl.CheckResult{
				Check:    "emitter_fields",
				Node:     n.Name,
				Severity: mdl.SevWarning,
				Message:  fmt.Sprintf("%s: emitter node %q has no texture", file, n.Name),
			})
		}
		if n.Emitter.Update == "" {
			out = append(out, mdl.CheckResult{
				Check:    "emitter_fields",
				Node:     n.Name,
				Severity: mdl.SevWarning,
				Message:  fmt.Sprintf("%s: emitter node %q has no Update mode", file, n.Name),
			})
		}
		if n.Emitter.Render == "" {
			out = append(out, mdl.CheckResult{
				Check:    "emitter_fields",
				Node:     n.Name,
				Severity: mdl.SevWarning,
				Message:  fmt.Sprintf("%s: emitter node %q has no Render mode", file, n.Name),
			})
		}
		if n.Emitter.Blend == "" {
			out = append(out, mdl.CheckResult{
				Check:    "emitter_fields",
				Node:     n.Name,
				Severity: mdl.SevWarning,
				Message:  fmt.Sprintf("%s: emitter node %q has no Blend mode", file, n.Name),
			})
		}
	}
	return out
}

func checkLightFields(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Light == nil {
			continue
		}
		if n.Light.Radius <= 0 {
			out = append(out, mdl.CheckResult{
				Check:    "light_fields",
				Node:     n.Name,
				Severity: mdl.SevWarning,
				Message:  fmt.Sprintf("%s: light node %q has radius %g (expected > 0)", file, n.Name, n.Light.Radius),
			})
		}
	}
	return out
}

func checkAabbFields(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Aabb == nil {
			continue
		}
		if n.Mesh == nil || len(n.Mesh.Faces) == 0 {
			out = append(out, mdl.CheckResult{
				Check:    "aabb_fields",
				Node:     n.Name,
				Severity: mdl.SevError,
				Message:  fmt.Sprintf("%s: AABB node %q has no mesh faces", file, n.Name),
			})
		}
		if len(n.Aabb.Entries) == 0 {
			out = append(out, mdl.CheckResult{
				Check:    "aabb_fields",
				Node:     n.Name,
				Severity: mdl.SevWarning,
				Message:  fmt.Sprintf("%s: AABB node %q has empty AABB tree (use --fix-aabb to rebuild)", file, n.Name),
			})
		}
	}
	return out
}

// checkMisplacedTileFields detects tile-specific fields (tilefade, rotatetexture)
// set on nodes in non-TILE models. These fields are ignored by the engine
// on non-tile models and likely indicate a copy-paste or tooling error.
func checkMisplacedTileFields(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	if strings.EqualFold(model.Classification, "TILE") {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil {
			continue
		}
		if n.Mesh.TileFade != 0 {
			out = append(out, mdl.CheckResult{
				Check:    "misplaced_tile_fields",
				Node:     n.Name,
				Severity: mdl.SevInfo,
				Message: fmt.Sprintf(
					"%s: non-TILE model has tilefade=%d on node %q (ignored by engine)",
					file, n.Mesh.TileFade, n.Name),
			})
		}
		if n.Mesh.RotateTexture != 0 {
			out = append(out, mdl.CheckResult{
				Check:    "misplaced_tile_fields",
				Node:     n.Name,
				Severity: mdl.SevInfo,
				Message: fmt.Sprintf(
					"%s: non-TILE model has rotatetexture=%d on node %q (ignored by engine)",
					file, n.Mesh.RotateTexture, n.Name),
			})
		}
	}
	return out
}

// checkMisplacedDanglyData detects danglymesh constraints on nodes that are
// not danglymesh type, which indicates data corruption or tooling errors.
func checkMisplacedDanglyData(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil {
			continue
		}
		// Skin data on non-skin nodes
		if n.Skin != nil && n.Dangly != nil {
			out = append(out, mdl.CheckResult{
				Check:    "misplaced_dangly_data",
				Node:     n.Name,
				Severity: mdl.SevWarning,
				Message: fmt.Sprintf(
					"%s: node %q has both skin and danglymesh data (conflicting node types)",
					file, n.Name),
			})
		}
	}
	return out
}
