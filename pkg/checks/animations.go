package checks

import (
	"fmt"
	"math"
	"strings"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

func init() {
	Register("animation_length", "animations", checkAnimationLength)
	Register("animation_root", "animations", checkAnimationRoot)
	Register("missing_end_keys", "animations", checkMissingEndKeys)
	Register("animation_transtime", "animations", checkAnimationTransTime)
}

func keyTimeMatchesLength(t, length float32) bool {
	return math.Abs(float64(t-length)) < 1e-3
}

func checkAnimationLength(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	isTile := strings.EqualFold(model.Classification, "TILE")
	var out []mdl.CheckResult
	for _, a := range model.Animations {
		if a.Length < 0 {
			out = append(out, mdl.CheckResult{
				Check:    "animation_length",
				Node:     a.Name,
				Severity: mdl.SevWarning,
				Message: fmt.Sprintf(
					"%s: animation %q has negative length %g (use --fix-animations to clamp)",
					file, a.Name, a.Length,
				),
			})
		}
		if isTile && (strings.EqualFold(a.Name, "day2night") || strings.EqualFold(a.Name, "night2day")) && a.Length < mdl.TileAnimMinLength {
			out = append(out, mdl.CheckResult{
				Check:    "animation_length",
				Node:     a.Name,
				Severity: mdl.SevWarning,
				Message: fmt.Sprintf(
					"%s: TILE animation %q has length %g, needs at least single-frame minimum (use --fix-animations to set)",
					file, a.Name, a.Length,
				),
			})
		}
	}
	return out
}

func checkAnimationRoot(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}

	nodeNames := make(map[string]bool)
	for _, n := range model.Nodes {
		if n != nil {
			nodeNames[strings.ToLower(n.Name)] = true
		}
	}

	isTile := strings.EqualFold(model.Classification, "TILE")

	bestRoot := model.Name
	if !isTile && nodeNames["rootdummy"] {
		bestRoot = "rootdummy"
	}

	var out []mdl.CheckResult
	for i := range model.Animations {
		a := &model.Animations[i]
		if a.Root == "" {
			if fix {
				a.Root = bestRoot
			}
			out = append(out, mdl.CheckResult{
				Check:    "animation_root",
				Node:     a.Name,
				Severity: mdl.SevWarning,
				Fixed:    fix,
				Message: fmt.Sprintf(
					"%s: animation %q had empty animroot, set to %q",
					file, a.Name, bestRoot,
				),
			})
			continue
		}
		if !strings.EqualFold(a.Root, model.Name) && !nodeNames[strings.ToLower(a.Root)] {
			old := a.Root
			if fix {
				a.Root = bestRoot
			}
			out = append(out, mdl.CheckResult{
				Check:    "animation_root",
				Node:     a.Name,
				Severity: mdl.SevError,
				Fixed:    fix,
				Message: fmt.Sprintf(
					"%s: animation %q had animroot %q which does not exist, set to %q",
					file, a.Name, old, bestRoot,
				),
			})
		}
		if isTile && !strings.EqualFold(a.Root, model.Name) {
			old := a.Root
			if fix {
				a.Root = model.Name
			}
			out = append(out, mdl.CheckResult{
				Check:    "animation_root",
				Node:     a.Name,
				Severity: mdl.SevWarning,
				Fixed:    fix,
				Message: fmt.Sprintf(
					"%s: TILE animation %q had animroot %q, set to model name %q",
					file, a.Name, old, model.Name,
				),
			})
		}
	}
	return out
}

func checkMissingEndKeys(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, a := range model.Animations {
		if a.Length <= 0 {
			continue
		}
		for _, an := range a.Nodes {
			if len(an.PositionKeys) > 0 {
				if !positionKeysEndAt(an.PositionKeys, a.Length) {
					out = append(out, mdl.CheckResult{
						Check:    "missing_end_keys",
						Node:     an.Name,
						Severity: mdl.SevInfo,
						Message: fmt.Sprintf(
							"%s: animation %q node %q has position keys but none at time=%g (length)",
							file, a.Name, an.Name, a.Length,
						),
						Fixed: false,
					})
				}
			}
			if len(an.OrientationKeys) > 0 {
				if !orientationKeysEndAt(an.OrientationKeys, a.Length) {
					out = append(out, mdl.CheckResult{
						Check:    "missing_end_keys",
						Node:     an.Name,
						Severity: mdl.SevInfo,
						Message: fmt.Sprintf(
							"%s: animation %q node %q has orientation keys but none at time=%g (length)",
							file, a.Name, an.Name, a.Length,
						),
						Fixed: false,
					})
				}
			}
		}
	}
	return out
}

func positionKeysEndAt(keys []mdl.PositionKey, length float32) bool {
	for _, k := range keys {
		if keyTimeMatchesLength(k.Time, length) {
			return true
		}
	}
	return false
}

func orientationKeysEndAt(keys []mdl.OrientationKey, length float32) bool {
	for _, k := range keys {
		if keyTimeMatchesLength(k.Time, length) {
			return true
		}
	}
	return false
}

func checkAnimationTransTime(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for i := range model.Animations {
		a := &model.Animations[i]
		if a.TransTime < 0 {
			old := a.TransTime
			if fix {
				a.TransTime = 0
			}
			out = append(out, mdl.CheckResult{
				Check:    "animation_transtime",
				Node:     a.Name,
				Severity: mdl.SevWarning,
				Fixed:    fix,
				Message: fmt.Sprintf(
					"%s: animation %q had negative TransTime %g, set to 0",
					file, a.Name, old,
				),
			})
		}
	}
	return out
}
