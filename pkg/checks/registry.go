// Package checks implements model validation checks for NWN MDL files.
//
// Checks are organized by category: structural, geometry, parameters,
// tiles, animations, and emitters.
//
// Checks are registered via Register() and can be filtered by name.
// Each check is a function matching mdl.CheckFunc.
package checks

import (
	"github.com/plenarius/cleanmodels/pkg/mdl"
)

// CheckEntry holds a registered check with metadata.
type CheckEntry struct {
	Name        string        `json:"name"`
	Category    string        `json:"category"`
	Fixable     bool          `json:"fixable"`
	Description string        `json:"description"`
	Fn          mdl.CheckFunc `json:"-"`
}

var registry []CheckEntry

// Register adds a check to the global registry.
func Register(name, category string, fixable bool, description string, fn mdl.CheckFunc) {
	registry = append(registry, CheckEntry{
		Name:        name,
		Category:    category,
		Fixable:     fixable,
		Description: description,
		Fn:          fn,
	})
}

// ListAll returns metadata for all registered checks (no model required).
func ListAll() []CheckEntry {
	out := make([]CheckEntry, len(registry))
	copy(out, registry)
	return out
}

// RunAll runs all registered checks on a model.
func RunAll(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	var results []mdl.CheckResult
	for _, entry := range registry {
		results = append(results, entry.Fn(model, file, fix)...)
	}
	return results
}

// RunFiltered runs only checks whose names are in the include list.
// If include is empty, all checks run. Checks in exclude are skipped.
func RunFiltered(model *mdl.Model, file string, include, exclude map[string]bool, fix bool) []mdl.CheckResult {
	var results []mdl.CheckResult
	for _, entry := range registry {
		if len(include) > 0 && !include[entry.Name] {
			continue
		}
		if exclude[entry.Name] {
			continue
		}
		results = append(results, entry.Fn(model, file, fix)...)
	}
	return results
}
