package main

import (
	"flag"
	"fmt"
	"runtime"
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
