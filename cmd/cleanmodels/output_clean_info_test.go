package main

import (
	"bytes"
	"strings"
	"testing"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

// A file with nothing to warn about is "clean", but its info lines (a skin's
// summary, say) are still wanted when the user asks for verbose output.
func TestVerboseCleanFileStillShowsInfoLines(t *testing.T) {
	res := Result{
		File: "head.mdl",
		Checks: []mdl.CheckResult{{
			Check: "skin_summary", Node: "skin_head", Severity: mdl.SevInfo,
			Message: `head.mdl: skin "skin_head" @4: 714 vertices`,
		}},
	}
	var quiet, verbose bytes.Buffer
	newTermWriter(&quiet, false).printSingleResult("head.mdl", res, nil, nil, false, false)
	if quiet.Len() != 0 {
		t.Errorf("non-verbose output for a clean file should be empty, got %q", quiet.String())
	}
	newTermWriter(&verbose, false).printSingleResult("head.mdl", res, nil, nil, true, false)
	out := verbose.String()
	if !strings.Contains(out, "head.mdl clean") || !strings.Contains(out, `[INFO] skin_summary (skin_head): skin "skin_head" @4: 714 vertices`) {
		t.Errorf("verbose output lacks the clean line or the info line:\n%s", out)
	}
}
