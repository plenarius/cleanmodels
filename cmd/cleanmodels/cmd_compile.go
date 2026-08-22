package main

import (
	"errors"
	"flag"
	"fmt"
	"os"
)

func cmdCompile(args []string) int {
	fs := flag.NewFlagSet("cleanmodels compile", flag.ContinueOnError)
	fs.SetOutput(os.Stderr)

	var cf commonFlags
	cf.register(fs)

	fs.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: cleanmodels compile [flags] <input> [output]\n\n")
		fmt.Fprintf(os.Stderr, "Compile ASCII MDL to binary format.\n\n")
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
		compile: true,
	}
	cf.apply(&opts)

	return dispatch(inputPath, outputPath, opts)
}
