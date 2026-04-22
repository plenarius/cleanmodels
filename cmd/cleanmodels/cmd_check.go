package main

import (
	"errors"
	"flag"
	"fmt"
	"os"
)

func cmdCheck(args []string) int {
	fs := flag.NewFlagSet("cleanmodels check", flag.ContinueOnError)
	fs.SetOutput(os.Stderr)

	fix := fs.Bool("fix", false, "auto-fix safe issues (duplicate names, invalid parents)")
	includeStr := fs.String("include-checks", "", "comma-separated check names to run (exclusive)")
	excludeStr := fs.String("exclude-checks", "", "comma-separated check names to skip")
	dryRun := fs.Bool("dry-run", false, "report what would be fixed without writing output")

	fs.BoolVar(fix, "f", false, "alias for --fix")
	fs.BoolVar(dryRun, "n", false, "alias for --dry-run")

	var cf commonFlags
	cf.register(fs)

	fs.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: cleanmodels check [flags] <input> [output]\n\n")
		fmt.Fprintf(os.Stderr, "Run validation checks on MDL models.\n\n")
		fmt.Fprintf(os.Stderr, "Flags:\n")
		fs.PrintDefaults()
	}

	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return exitOK
		}
		return exitUsage
	}
	pos := fs.Args()
	if len(pos) < 1 {
		fs.Usage()
		return exitUsage
	}

	inputPath := pos[0]
	var outputPath string
	if len(pos) >= 2 {
		outputPath = pos[1]
	}

	opts := procOpts{
		check:   true,
		dryRun:  *dryRun,
		include: parseNameSet(*includeStr),
		exclude: parseNameSet(*excludeStr),
	}
	cf.apply(&opts)

	if *fix {
		setAllRepairs(&opts)
	}

	return dispatch(inputPath, outputPath, opts)
}
