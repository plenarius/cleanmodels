package checks

import (
	"fmt"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

func init() {
	Register("robe_torso_turn", "structural", true, "Detect robe nodes under torso_g with an orientation the game does not draw on a worn robe (fix: put it into their geometry)", checkRobeTorsoTurn)
}

// checkRobeTorsoTurn reports robe nodes whose parent is torso_g and whose
// orientation is not the identity: the game draws them as if it were
// (nwn-mdl-spec MDL-NODE-050). The fix puts the turn into their geometry.
func checkRobeTorsoTurn(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	turns := mdl.TorsoTurns(model)
	if len(turns) == 0 {
		return nil
	}
	fixed := map[*mdl.Node]bool{}
	if fix {
		mdl.BakeTorsoTurns(model)
		for _, t := range turns {
			fixed[t.Node] = t.Why == ""
		}
	}
	var out []mdl.CheckResult
	for _, t := range turns {
		msg := fmt.Sprintf("%s: node %q under torso_g is turned %.0f degrees; on a worn robe the game draws it unturned, so it and what hangs from it are out of place",
			file, t.Node.Name, t.Angle)
		if t.Why != "" {
			msg += " (cannot be fixed: " + t.Why + ")"
		} else {
			msg += "; repair --fix-robe-turns puts the turn into its geometry"
		}
		out = append(out, mdl.CheckResult{
			Check:    "robe_torso_turn",
			Node:     t.Node.Name,
			Severity: mdl.SevWarning,
			Fixed:    fixed[t.Node],
			Message:  msg,
		})
	}
	return out
}
