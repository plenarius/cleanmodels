package main

import (
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"os"

	"github.com/plenarius/cleanmodels/pkg/checks"
)

func cmdCheck(args []string) int {
	fs := flag.NewFlagSet("cleanmodels check", flag.ContinueOnError)
	fs.SetOutput(os.Stderr)

	fix := fs.Bool("fix", false, "auto-fix safe issues (duplicate names, invalid parents)")
	includeStr := fs.String("include-checks", "", "comma-separated check names to run (exclusive)")
	excludeStr := fs.String("exclude-checks", "", "comma-separated check names to skip")
	dryRun := fs.Bool("dry-run", false, "report what would be fixed without writing output")
	listChecks := fs.Bool("list", false, "list all available checks as JSON and exit")

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

	if err := parseArgs(fs, args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return exitOK
		}
		return exitUsage
	}
	if err := cf.validate(); err != nil {
		fmt.Fprintf(os.Stderr, "cleanmodels: %v\n", err)
		return exitUsage
	}

	if *listChecks {
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		if err := enc.Encode(checks.ListAll()); err != nil {
			fmt.Fprintf(os.Stderr, "cleanmodels: %v\n", err)
			return exitErrors
		}
		return exitOK
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
