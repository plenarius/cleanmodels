package checks

import (
	"fmt"
	"math"
	"strings"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

func init() {
	Register("animation_length", "animations", false, "Detect negative or too-short animation lengths", checkAnimationLength)
	Register("animation_root", "animations", true, "Fix a TILE animroot that names a node; warn about an animroot that matches no node of its animation (the whole animation plays)", checkAnimationRoot)
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
// MDL-NODE-045). Such an animroot is never fixed: whatever it were changed
// to, a compile would then prune. A name that exists nowhere or only in the
// geometry is a warning; the name in another case is information, since the
// decompiler writes one on purpose to keep a stored tree whole (a binary
// another compiler wrote, wider than its animroot's cut). NULL (any case) and
// the model's own name prune nothing either way and are not reported. An
// empty animroot (none) prunes nothing too, and the compiler stores the
// model's name for it: information, left as it is. A TILE animation's
// animroot that names one of its nodes is fixed to the model's name.
func checkAnimationRoot(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}

	isTile := strings.EqualFold(model.Classification, "TILE")

	var out []mdl.CheckResult
	for i := range model.Animations {
		a := &model.Animations[i]
		if a.Root == "" {
			out = append(out, mdl.CheckResult{
				Check:    "animation_root",
				Node:     a.Name,
				Severity: mdl.SevInfo,
				Message: fmt.Sprintf(
					"%s: animation %q has no animroot; the engine prunes nothing, so the whole animation plays (left as it is; a compile stores the model name %q)",
					file, a.Name, model.Name,
				),
			})
			continue
		}
		if !strings.EqualFold(a.Root, model.Name) && !strings.EqualFold(a.Root, "NULL") {
			if why, sev := animRootMismatch(a, model.Nodes); why != "" {
				// Left as written: any other animroot would make a compile
				// prune the animation, which changes what plays.
				out = append(out, mdl.CheckResult{
					Check:    "animation_root",
					Node:     a.Name,
					Severity: sev,
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

// animRootMismatch says why the animroot of a matches none of its nodes, and
// how severe that is, or returns "" when a node has exactly that name
// (MDL-NODE-045). Only another case is information: the decompiler writes it
// on purpose (writtenAnimRoot).
func animRootMismatch(a *mdl.Animation, geometry []*mdl.Node) (string, mdl.Severity) {
	for _, n := range a.Nodes {
		if n.Name == a.Root {
			return "", 0
		}
	}
	for _, n := range a.Nodes {
		if strings.EqualFold(n.Name, a.Root) {
			return fmt.Sprintf("names node %q only in another case (the engine compares case)", n.Name), mdl.SevInfo
		}
	}
	for _, n := range geometry {
		if n != nil && strings.EqualFold(n.Name, a.Root) {
			return fmt.Sprintf("names node %q, which the animation does not have", n.Name), mdl.SevWarning
		}
	}
	return "matches no node", mdl.SevWarning
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
