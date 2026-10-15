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
	Name        string           `json:"name"`
	Category    string           `json:"category"`
	Fixable     bool             `json:"fixable"`
	Description string           `json:"description"`
	Fn          mdl.CheckFunc    `json:"-"`
	CtxFn       ContextCheckFunc `json:"-"` // set instead of Fn for checks that need Context
}

// Context is what some checks need beyond the model: where to look for the
// creature skeleton and capart.2da of a body part, in the order compile
// searches (the model's folder, --supermodel-dir and --resource-dir, the game
// install). Checks that need it report nothing when Locations is empty.
type Context struct {
	Locations []string
}

// ContextCheckFunc is a check that also receives a Context.
type ContextCheckFunc func(model *mdl.Model, file string, fix bool, ctx Context) []mdl.CheckResult

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

// RegisterWithContext adds a check that needs a Context to the registry.
func RegisterWithContext(name, category string, fixable bool, description string, fn ContextCheckFunc) {
	registry = append(registry, CheckEntry{
		Name:        name,
		Category:    category,
		Fixable:     fixable,
		Description: description,
		CtxFn:       fn,
	})
}

func (e CheckEntry) run(model *mdl.Model, file string, fix bool, ctx Context) []mdl.CheckResult {
	if e.CtxFn != nil {
		return e.CtxFn(model, file, fix, ctx)
	}
	return e.Fn(model, file, fix)
}

// ListAll returns metadata for all registered checks (no model required).
func ListAll() []CheckEntry {
	out := make([]CheckEntry, len(registry))
	copy(out, registry)
	return out
}

// RunAll runs all registered checks on a model, without a Context.
func RunAll(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	return RunFilteredContext(model, file, nil, nil, fix, Context{})
}

// RunFiltered runs only checks whose names are in the include list.
// If include is empty, all checks run. Checks in exclude are skipped.
func RunFiltered(model *mdl.Model, file string, include, exclude map[string]bool, fix bool) []mdl.CheckResult {
	return RunFilteredContext(model, file, include, exclude, fix, Context{})
}

// RunFilteredContext is RunFiltered with a Context for the checks that use one.
func RunFilteredContext(model *mdl.Model, file string, include, exclude map[string]bool, fix bool, ctx Context) []mdl.CheckResult {
	var results []mdl.CheckResult
	for _, entry := range registry {
		if len(include) > 0 && !include[entry.Name] {
			continue
		}
		if exclude[entry.Name] {
			continue
		}
		results = append(results, entry.run(model, file, fix, ctx)...)
	}
	return results
}
