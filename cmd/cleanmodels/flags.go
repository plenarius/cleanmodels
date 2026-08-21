package main

import (
	"flag"
	"fmt"
	"runtime"
	"strings"
)

type commonFlags struct {
	jsonOut   *bool
	jsonLines *bool
	verbose   *bool
	quiet     *bool
	workers   *int
	recursive *bool
	colorMode *string
}

func (cf *commonFlags) register(fs *flag.FlagSet) {
	cf.jsonOut = fs.Bool("json", false, "output results as JSON")
	cf.jsonLines = fs.Bool("json-lines", false, "stream NDJSON events (GUI integration)")
	cf.verbose = fs.Bool("verbose", false, "show all warnings and info")
	cf.quiet = fs.Bool("quiet", false, "suppress all output except errors")
	cf.workers = fs.Int("workers", runtime.NumCPU(), "parallel workers for batch mode")
	cf.recursive = fs.Bool("recursive", false, "process directories recursively")
	cf.colorMode = fs.String("color", "auto", "ANSI color: auto, always, never")

	fs.BoolVar(cf.jsonOut, "j", false, "alias for --json")
	fs.BoolVar(cf.verbose, "v", false, "alias for --verbose")
	fs.BoolVar(cf.quiet, "q", false, "alias for --quiet")
	fs.IntVar(cf.workers, "w", *cf.workers, "alias for --workers")
	fs.BoolVar(cf.recursive, "r", false, "alias for --recursive")
}

// validate checks values that aren't structurally enforced by flag.
// Call after fs.Parse and before apply.
func (cf *commonFlags) validate() error {
	return validateColorMode(*cf.colorMode)
}

func (cf *commonFlags) apply(o *procOpts) {
	o.jsonOut = *cf.jsonOut
	o.jsonLines = *cf.jsonLines
	o.verbose = *cf.verbose
	o.quiet = *cf.quiet
	o.workers = *cf.workers
	o.recursive = *cf.recursive
	o.colorMode = *cf.colorMode
}

// validateColorMode rejects values outside the documented set.
// Empty string is permitted and treated as "auto" by shouldColorize.
func validateColorMode(mode string) error {
	switch mode {
	case "", "auto", "always", "never":
		return nil
	}
	return fmt.Errorf("invalid --color value %q (want auto, always, or never)", mode)
}

// boolFlag matches the unexported interface the flag package itself uses
// (via flag.Value) to recognize flags that don't consume a following
// argument. Any *bool-backed flag.Value implements it.
type boolFlag interface {
	IsBoolFlag() bool
}

// parseArgs reorders args so that all flag tokens registered on fs precede
// positional arguments, then parses them.
//
// flag.FlagSet.Parse stops parsing at the first non-flag argument, so
// standard usage like "cleanmodels compile model.mdl --verbose" would
// otherwise leave "--verbose" as a leftover positional argument — which
// callers here treat as the output path, silently writing to a file named
// "--verbose". Reordering lets flags appear before or after positionals,
// matching how most CLI tools behave.
func parseArgs(fs *flag.FlagSet, args []string) error {
	return fs.Parse(reorderArgs(fs, args))
}

// reorderArgs moves every recognized flag token (and, where applicable, its
// value) to the front of args, preserving relative order within each group.
// A bare "--" and everything after it is passed through unchanged as
// positional, per convention.
func reorderArgs(fs *flag.FlagSet, args []string) []string {
	var flags, positionals []string

	for i := 0; i < len(args); i++ {
		arg := args[i]

		if arg == "--" {
			positionals = append(positionals, args[i+1:]...)
			break
		}
		if !strings.HasPrefix(arg, "-") || arg == "-" {
			positionals = append(positionals, arg)
			continue
		}

		flags = append(flags, arg)

		name := strings.TrimLeft(arg, "-")
		if eq := strings.IndexByte(name, '='); eq >= 0 {
			continue // value supplied inline, e.g. --workers=4
		}

		f := fs.Lookup(name)
		if f == nil {
			continue // unknown flag; let fs.Parse report it
		}
		if bf, ok := f.Value.(boolFlag); ok && bf.IsBoolFlag() {
			continue // bool flags never consume a following token
		}
		if i+1 < len(args) {
			i++
			flags = append(flags, args[i])
		}
	}

	return append(flags, positionals...)
}
