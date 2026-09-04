package checks

import (
	"path/filepath"
	"testing"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

const fixtureRoot = "../../tests/fixtures"

func decompileFixture(t *testing.T, subdir, name string) *mdl.Model {
	t.Helper()
	path := filepath.Join(fixtureRoot, subdir, name)
	model, err := mdl.DecompileFile(path)
	if err != nil {
		t.Fatalf("DecompileFile(%s): %v", path, err)
	}
	return model
}

func findCheck(results []mdl.CheckResult, checkName string) *mdl.CheckResult {
	for i := range results {
		if results[i].Check == checkName {
			return &results[i]
		}
	}
	return nil
}

func findChecks(results []mdl.CheckResult, checkName string) []mdl.CheckResult {
	var out []mdl.CheckResult
	for _, r := range results {
		if r.Check == checkName {
			out = append(out, r)
		}
	}
	return out
}

func TestCleanModelHasNoWarnings(t *testing.T) {
	model := decompileFixture(t, "clean", "pmh0_head001.mdl")
	results := RunAll(model, "pmh0_head001.mdl", true)
	for _, r := range results {
		if r.Severity >= mdl.SevWarning {
			t.Errorf("clean model should have no warnings/errors, got: [%s] %s", r.Check, r.Message)
		}
	}
}

func TestAllCleanFixturesClean(t *testing.T) {
	cleanFiles := []string{
		"Fx_light_CLR.mdl",
		"boat1.mdl",
		"pmh0_head001.mdl",
		"ttw01_a00_01.mdl",
	}
	for _, name := range cleanFiles {
		t.Run(name, func(t *testing.T) {
			model := decompileFixture(t, "clean", name)
			results := RunAll(model, name, true)
			for _, r := range results {
				if r.Severity >= mdl.SevWarning {
					t.Errorf("clean fixture %s should have no warnings, got: [%s] %s", name, r.Check, r.Message)
				}
			}
		})
	}
}

func TestBoneLimitNotExceeded(t *testing.T) {
	model := decompileFixture(t, "clean", "c_blbear.mdl")
	results := RunAll(model, "c_blbear.mdl", false)
	checks := findChecks(results, "bone_limit")
	if len(checks) > 0 {
		t.Fatalf("c_blbear.mdl has 18 bones, should not exceed EE limit of 64: %s", checks[0].Message)
	}
}

func TestDuplicateNodeNames(t *testing.T) {
	model := decompileFixture(t, "broken", "ttw01_a04_02.mdl")
	results := RunAll(model, "ttw01_a04_02.mdl", true)
	checks := findChecks(results, "duplicate_node_names")
	if len(checks) == 0 {
		t.Fatal("expected duplicate_node_names check result")
	}
	for _, r := range checks {
		if !r.Fixed {
			t.Errorf("duplicate_node_names on node %q should be auto-fixed", r.Node)
		}
	}
}

func TestDuplicateNodeNamesMultiple(t *testing.T) {
	model := decompileFixture(t, "broken", "ttw01_a04_16.mdl")
	results := RunAll(model, "ttw01_a04_16.mdl", true)
	checks := findChecks(results, "duplicate_node_names")
	if len(checks) < 2 {
		t.Fatalf("expected at least 2 duplicate_node_names results, got %d", len(checks))
	}
	for _, r := range checks {
		if !r.Fixed {
			t.Errorf("duplicate_node_names on node %q should be auto-fixed", r.Node)
		}
	}
}

func TestAnimationRootEmpty(t *testing.T) {
	model := decompileFixture(t, "broken", "GUI_radialband.mdl")
	results := RunAll(model, "GUI_radialband.mdl", true)
	checks := findChecks(results, "animation_root")
	if len(checks) == 0 {
		t.Fatal("expected animation_root check result for empty animroot")
	}
	for _, r := range checks {
		if !r.Fixed {
			t.Errorf("animation_root should be auto-fixed, got: %s", r.Message)
		}
	}
}

func TestAnimationRootTile(t *testing.T) {
	model := decompileFixture(t, "broken", "ttw01_a02_01.mdl")
	results := RunAll(model, "ttw01_a02_01.mdl", true)
	checks := findChecks(results, "animation_root")
	if len(checks) < 2 {
		t.Fatalf("expected at least 2 animation_root results for tile model, got %d", len(checks))
	}
	for _, r := range checks {
		if !r.Fixed {
			t.Errorf("animation_root should be auto-fixed, got: %s", r.Message)
		}
	}
}
