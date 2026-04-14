package main

import (
	"flag"
	"runtime"
)

type commonFlags struct {
	jsonOut   *bool
	jsonLines *bool
	verbose   *bool
	quiet     *bool
	workers   *int
	recursive *bool
}

func (cf *commonFlags) register(fs *flag.FlagSet) {
	cf.jsonOut = fs.Bool("json", false, "output results as JSON")
	cf.jsonLines = fs.Bool("json-lines", false, "stream NDJSON events (GUI integration)")
	cf.verbose = fs.Bool("verbose", false, "show all warnings and info")
	cf.quiet = fs.Bool("quiet", false, "suppress all output except errors")
	cf.workers = fs.Int("workers", runtime.NumCPU(), "parallel workers for batch mode")
	cf.recursive = fs.Bool("recursive", false, "process directories recursively")
}

func (cf *commonFlags) apply(o *procOpts) {
	o.jsonOut = *cf.jsonOut
	o.jsonLines = *cf.jsonLines
	o.verbose = *cf.verbose
	o.quiet = *cf.quiet
	o.workers = *cf.workers
	o.recursive = *cf.recursive
}
