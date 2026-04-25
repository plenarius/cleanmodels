package checks

import (
	"fmt"
	"strings"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

func init() {
	Register("emitter_update_valid", "emitters", true, "Fix invalid emitter Update mode", checkEmitterUpdateValid)
	Register("emitter_render_valid", "emitters", true, "Fix invalid emitter Render mode", checkEmitterRenderValid)
	Register("emitter_blend_valid", "emitters", true, "Fix invalid emitter Blend mode", checkEmitterBlendValid)
	Register("emitter_negative_values", "emitters", false, "Detect negative emitter values (birthrate, mass, etc.)", checkEmitterNegativeValues)
	Register("emitter_percent_range", "emitters", false, "Detect emitter percent values outside 0-1 range", checkEmitterPercentRange)
	Register("emitter_size_consistency", "emitters", true, "Fix negative emitter size values", checkEmitterSizeConsistency)
	Register("emitter_missing_texture", "emitters", true, "Set default texture on emitters with missing texture", checkEmitterMissingTexture)
}

var validEmitterUpdates = []string{"Fountain", "Single", "Explosion", "Lightning"}

var validEmitterRenders = []string{
	"Normal",
	"Linked",
	"Billboard_to_Local_Z",
	"Billboard_to_World_Z",
	"Aligned_to_World_Z",
	"Aligned_to_Particle_Dir",
	"Motion_Blur",
}

var validEmitterBlends = []string{"Normal", "Punch-Through", "Lighten"}

func stringInListFold(s string, allowed []string) bool {
	for _, a := range allowed {
		if strings.EqualFold(s, a) {
			return true
		}
	}
	return false
}

func checkEmitterEnumField(checkName, fieldName string, getter func(*mdl.EmitterData) *string, allowed []string, defaultVal string) mdl.CheckFunc {
	return func(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
		if model == nil {
			return nil
		}
		var out []mdl.CheckResult
		for _, n := range model.Nodes {
			if n == nil || n.Emitter == nil {
				continue
			}
			ptr := getter(n.Emitter)
			val := *ptr
			if !stringInListFold(val, allowed) {
				fixed := false
				if fix {
					*ptr = defaultVal
					fixed = true
				}
				out = append(out, mdl.CheckResult{
					Check:    checkName,
					Node:     n.Name,
					Severity: mdl.SevWarning,
					Fixed:    fixed,
					Message: fmt.Sprintf(
						"%s: emitter node %q had invalid %s %q, set to %q",
						file, n.Name, fieldName, val, defaultVal,
					),
				})
			}
		}
		return out
	}
}

var checkEmitterUpdateValid = checkEmitterEnumField("emitter_update_valid", "Update",
	func(e *mdl.EmitterData) *string { return &e.Update }, validEmitterUpdates, "Fountain")

var checkEmitterRenderValid = checkEmitterEnumField("emitter_render_valid", "Render",
	func(e *mdl.EmitterData) *string { return &e.Render }, validEmitterRenders, "Normal")

var checkEmitterBlendValid = checkEmitterEnumField("emitter_blend_valid", "Blend",
	func(e *mdl.EmitterData) *string { return &e.Blend }, validEmitterBlends, "Normal")

var negativeValueFields = []struct {
	name string
	get  func(*mdl.EmitterData) float32
}{
	{"BirthRate", func(e *mdl.EmitterData) float32 { return e.BirthRate }},
	{"LifeExp", func(e *mdl.EmitterData) float32 { return e.LifeExp }},
	{"Mass", func(e *mdl.EmitterData) float32 { return e.Mass }},
	{"Velocity", func(e *mdl.EmitterData) float32 { return e.Velocity }},
}

func checkEmitterNegativeValues(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Emitter == nil {
			continue
		}
		for _, f := range negativeValueFields {
			if f.get(n.Emitter) < 0 {
				out = append(out, mdl.CheckResult{
					Check:    "emitter_negative_values",
					Node:     n.Name,
					Severity: mdl.SevInfo,
					Message:  fmt.Sprintf("%s: emitter node %q has negative %s %g", file, n.Name, f.name, f.get(n.Emitter)),
					Fixed:    false,
				})
			}
		}
	}
	return out
}

func inUnitInterval(v float32) bool {
	return v >= 0 && v <= 1
}

func clamp01(v float32) float32 {
	if v < 0 {
		return 0
	}
	if v > 1 {
		return 1
	}
	return v
}

var percentFields = []struct {
	name string
	ptr  func(*mdl.EmitterData) *float32
}{
	{"PercentStart", func(e *mdl.EmitterData) *float32 { return &e.PercentStart }},
	{"PercentMid", func(e *mdl.EmitterData) *float32 { return &e.PercentMid }},
	{"PercentEnd", func(e *mdl.EmitterData) *float32 { return &e.PercentEnd }},
}

func checkEmitterPercentRange(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Emitter == nil {
			continue
		}
		for _, f := range percentFields {
			p := f.ptr(n.Emitter)
			if !inUnitInterval(*p) {
				out = append(out, mdl.CheckResult{
					Check:    "emitter_percent_range",
					Node:     n.Name,
					Severity: mdl.SevInfo,
					Message:  fmt.Sprintf("%s: emitter node %q has %s %g (outside 0-1 range)", file, n.Name, f.name, *p),
				})
			}
		}
	}
	return out
}

var sizeFields = []struct {
	name string
	ptr  func(*mdl.EmitterData) *float32
}{
	{"SizeStart", func(e *mdl.EmitterData) *float32 { return &e.SizeStart }},
	{"SizeMid", func(e *mdl.EmitterData) *float32 { return &e.SizeMid }},
	{"SizeEnd", func(e *mdl.EmitterData) *float32 { return &e.SizeEnd }},
}

func checkEmitterSizeConsistency(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Emitter == nil {
			continue
		}
		for _, f := range sizeFields {
			p := f.ptr(n.Emitter)
			if *p < 0 {
				old := *p
				fixed := false
				if fix {
					*p = 0
					fixed = true
				}
				out = append(out, mdl.CheckResult{
					Check:    "emitter_size_consistency",
					Node:     n.Name,
					Severity: mdl.SevWarning,
					Fixed:    fixed,
					Message:  fmt.Sprintf("%s: emitter node %q had negative %s %g, set to 0", file, n.Name, f.name, old),
				})
			}
		}
	}
	return out
}

func checkEmitterMissingTexture(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Emitter == nil {
			continue
		}
		if n.Emitter.Texture == "" {
			fixed := false
			if fix {
				n.Emitter.Texture = "fxpa_default"
				fixed = true
			}
			out = append(out, mdl.CheckResult{
				Check:    "emitter_missing_texture",
				Node:     n.Name,
				Severity: mdl.SevWarning,
				Fixed:    fixed,
				Message: fmt.Sprintf(
					"%s: emitter node %q had empty texture, set to \"fxpa_default\"",
					file, n.Name,
				),
			})
		}
	}
	return out
}
