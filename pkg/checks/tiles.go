package checks

import (
	"fmt"
	"strings"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

func init() {
	Register("tilefade_validation", "tiles", checkTilefadeValidation)
	Register("rotate_texture", "tiles", checkRotateTexture)
	Register("lightmapped_tile", "tiles", checkLightmappedTile)
}

func checkTilefadeValidation(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
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
				Check:    "tilefade_validation",
				Node:     n.Name,
				Severity: mdl.SevInfo,
				Message: fmt.Sprintf(
					"%s: non-TILE model has tilefade=%d on mesh node %q",
					file, n.Mesh.TileFade, n.Name,
				),
				Fixed: false,
			})
		}
	}
	return out
}

func checkRotateTexture(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
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
		if n.Mesh.RotateTexture != 0 {
			out = append(out, mdl.CheckResult{
				Check:    "rotate_texture",
				Node:     n.Name,
				Severity: mdl.SevInfo,
				Message: fmt.Sprintf(
					"%s: non-TILE model has RotateTexture=%d on mesh node %q",
					file, n.Mesh.RotateTexture, n.Name,
				),
				Fixed: false,
			})
		}
	}
	return out
}

func checkLightmappedTile(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	if !strings.EqualFold(model.Classification, "TILE") {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil {
			continue
		}
		if n.Mesh.LightMapped != 0 {
			out = append(out, mdl.CheckResult{
				Check:    "lightmapped_tile",
				Node:     n.Name,
				Severity: mdl.SevInfo,
				Message: fmt.Sprintf(
					"%s: TILE model mesh node %q has LightMapped=%d",
					file, n.Name, n.Mesh.LightMapped,
				),
				Fixed: false,
			})
		}
	}
	return out
}
