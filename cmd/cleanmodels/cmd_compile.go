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
	keepLost := registerKeepLost(fs)

	fs.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: cleanmodels compile [flags] <input>... [output]\n\n")
		fmt.Fprintf(os.Stderr, "Compile ASCII MDL to binary format. An output is required: to compile in place, give the input twice\n(cleanmodels compile a.mdl a.mdl).\n\nGive several inputs (files or directories) and the last argument is the output directory.\nAn existing output file is not overwritten unless --overwrite is given.\n\n")
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

	inputs, outputPath, splitErr := splitInputs(pos, true)
	if splitErr != nil {
		fmt.Fprintf(os.Stderr, "cleanmodels: %v\n", splitErr)
		return exitUsage
	}

	opts := procOpts{
		compile: true,
	}
	cf.apply(&opts)
	opts.force = *keepLost

	return dispatchInputs(inputs, outputPath, opts)
}
