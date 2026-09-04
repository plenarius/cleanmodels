package main

import (
	"errors"
	"flag"
	"fmt"
	"os"
)

func cmdDecompile(args []string) int {
	fs := flag.NewFlagSet("cleanmodels decompile", flag.ContinueOnError)
	fs.SetOutput(os.Stderr)

	force := fs.Bool("force", false, "treat input as binary even if auto-detection fails")

	fs.BoolVar(force, "f", false, "alias for --force")

	var cf commonFlags
	cf.register(fs)

	fs.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: cleanmodels decompile [flags] <input> [output]\n\n")
		fmt.Fprintf(os.Stderr, "Decompile binary MDL to ASCII format.\n\n")
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
		decompileOnly: true,
		forceBin:      *force,
	}
	cf.apply(&opts)

	return dispatch(inputPath, outputPath, opts)
}
