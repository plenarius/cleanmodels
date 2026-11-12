package checks

import (
	"fmt"
	"math"
	"strings"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

func init() {
	Register("animation_length", "animations", false, "Detect negative or too-short animation lengths", checkAnimationLength)
	Register("animation_root", "animations", true, "Fix an empty or TILE animroot; warn about an animroot that matches no node of its animation (the whole animation plays)", checkAnimationRoot)
	Register("missing_end_keys", "animations", false, "Detect animations missing end-time keyframes", checkMissingEndKeys)
	Register("animation_transtime", "animations", true, "Clamp negative animation transition times to 0", checkAnimationTransTime)
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

// checkAnimationRoot reports animroots. The engine looks an animroot up among
// the animation's own nodes by exact name, case included, and prunes the
// animation to the path to that node and its subtree; when no node matches
// (a name that exists nowhere, another case, a node only the geometry has) it
// keeps the whole tree, so the animation plays on every node (nwn-mdl-spec
// MDL-NODE-045). Such an animroot is a warning and never fixed: whatever it
// were changed to, a compile would then prune. NULL (any case) and the model's
// own name prune nothing either way and are not reported. An empty animroot
// and a TILE animation's animroot that names one of its nodes are fixed.
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
					"%s: animation %q has empty animroot; %s %q",
					file, a.Name, fixVerb(fix, "set to", "would be set to"), bestRoot,
				),
			})
			continue
		}
		if !strings.EqualFold(a.Root, model.Name) && !strings.EqualFold(a.Root, "NULL") {
			if why := animRootMismatch(a, model.Nodes); why != "" {
				// Left as written: any other animroot would make a compile
				// prune the animation, which changes what plays.
				out = append(out, mdl.CheckResult{
					Check:    "animation_root",
					Node:     a.Name,
					Severity: mdl.SevWarning,
					Message: fmt.Sprintf(
						"%s: animation %q has animroot %q, which %s; the engine prunes nothing, so the whole animation plays (left as written)",
						file, a.Name, a.Root, why,
					),
				})
				continue
			}
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
					"%s: TILE animation %q has animroot %q; %s model name %q",
					file, a.Name, old, fixVerb(fix, "set to", "would be set to"), model.Name,
				),
			})
		}
	}
	return out
}

// animRootMismatch says why the animroot of a matches none of its nodes, or
// returns "" when a node has exactly that name (MDL-NODE-045).
func animRootMismatch(a *mdl.Animation, geometry []*mdl.Node) string {
	for _, n := range a.Nodes {
		if n.Name == a.Root {
			return ""
		}
	}
	for _, n := range a.Nodes {
		if strings.EqualFold(n.Name, a.Root) {
			return fmt.Sprintf("names node %q only in another case (the engine compares case)", n.Name)
		}
	}
	for _, n := range geometry {
		if n != nil && strings.EqualFold(n.Name, a.Root) {
			return fmt.Sprintf("names node %q, which the animation does not have", n.Name)
		}
	}
	return "matches no node"
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
					"%s: animation %q has negative TransTime %g; %s 0",
					file, a.Name, old, fixVerb(fix, "set to", "would be set to"),
				),
			})
		}
	}
	return out
}
