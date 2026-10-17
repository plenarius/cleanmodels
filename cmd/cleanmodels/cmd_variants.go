package main

import (
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

// variantRaces are the races robes are made for, by the letter in the body
// prefix: halfling, dwarf, elf, gnome, human (also half-elf), half-orc.
const variantRaces = "adegho"

// variantResult is one variant's outcome, as printed by --json and --json-lines.
type variantResult struct {
	Source   string   `json:"source"`
	Target   string   `json:"target"`
	File     string   `json:"file,omitempty"`
	Warnings []string `json:"warnings,omitempty"`
	Error    string   `json:"error,omitempty"`
}

func cmdVariants(args []string) int {
	fs := flag.NewFlagSet("cleanmodels variants", flag.ContinueOnError)
	fs.SetOutput(os.Stderr)

	to := fs.String("to", "", "comma-separated bodies to make variants for (pmd0,pme2,...); default: every race in phenotypes 0 and 2, same gender as the source")
	compile := fs.Bool("compile", false, "write compiled (binary) models instead of ASCII")

	var cf commonFlags
	cf.register(fs)

	fs.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: cleanmodels variants [flags] <model.mdl> [output-dir]\n\n")
		fmt.Fprintf(os.Stderr, "Make a robe's variants for other races and phenotypes: pmh0_robe003.mdl gives\npmd0_robe003.mdl, pme0_robe003.mdl and so on. Each variant takes its bone positions\nfrom a stock robe of that body and is fitted using the race skeletons and stock body\nparts, all read from the game install (--game-dir) or --resource-dir. The output\ndirectory defaults to the robe's own. An existing file is not overwritten unless\n--overwrite is given.\n\n")
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
	if len(pos) < 1 || len(pos) > 2 {
		fs.Usage()
		return exitUsage
	}
	input := pos[0]
	outDir := filepath.Dir(input)
	if len(pos) == 2 {
		outDir = pos[1]
	}
	var o procOpts
	cf.apply(&o)

	src, err := loadVariantSource(input)
	if err != nil {
		fmt.Fprintf(os.Stderr, "cleanmodels: %v\n", wrapWithSuggestion(err, input))
		return exitErrors
	}
	srcPrefix := mdl.VariantPrefix(src.Name)
	if srcPrefix == "" || len(srcPrefix) != 4 {
		fmt.Fprintf(os.Stderr, "cleanmodels: %s: model %q is not named like a robe for a body (pmh0_robe003)\n", input, src.Name)
		return exitErrors
	}
	targets, err := variantTargets(*to, srcPrefix)
	if err != nil {
		fmt.Fprintf(os.Stderr, "cleanmodels: %v\n", err)
		return exitUsage
	}
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		fmt.Fprintf(os.Stderr, "cleanmodels: %v\n", err)
		return exitErrors
	}

	superDirs, resDirs := lookupDirs(input, o)
	suffix := src.Name[len(srcPrefix):]
	failed := 0
	var results []variantResult
	for _, target := range targets {
		res := variantResult{Source: input, Target: target}
		out, err := mdl.Variant(src, target, mdl.VariantOptions{
			Locs: superDirs,
			Warn: func(s string) { res.Warnings = append(res.Warnings, s) },
		})
		if err == nil {
			res.File = filepath.Join(outDir, target+suffix+".mdl")
			err = writeVariant(out, res.File, input, *compile, o.overwrite, superDirs, resDirs, &res)
		}
		if err != nil {
			res.Error = err.Error()
			res.File = ""
			failed++
		}
		results = append(results, res)
		if o.jsonLines {
			b, _ := json.Marshal(res)
			fmt.Println(string(b))
		} else if !o.jsonOut && !o.quiet {
			printVariantResult(res)
		} else if o.quiet && res.Error != "" {
			fmt.Fprintf(os.Stderr, "%s: %s\n", target, res.Error)
		}
	}
	if o.jsonOut {
		b, _ := json.MarshalIndent(results, "", "  ")
		fmt.Println(string(b))
	} else if !o.jsonLines && !o.quiet {
		fmt.Printf("\n%d of %d variants written\n", len(targets)-failed, len(targets))
	}
	if failed > 0 {
		return exitErrors
	}
	return exitOK
}

// variantTargets returns the bodies to make variants for: the --to list, or
// every race in phenotypes 0 and 2 of the source's gender, without the
// source's own body.
func variantTargets(list, srcPrefix string) ([]string, error) {
	var targets []string
	if list != "" {
		for _, t := range strings.Split(list, ",") {
			t = strings.ToLower(strings.TrimSpace(t))
			if len(t) != 4 || t[0] != 'p' || (t[1] != 'm' && t[1] != 'f') {
				return nil, fmt.Errorf("--to: %q is not a body prefix such as pmd0", t)
			}
			targets = append(targets, t)
		}
		return targets, nil
	}
	for _, race := range variantRaces {
		for _, pheno := range "02" {
			t := string([]rune{'p', rune(srcPrefix[1]), race, pheno})
			if t != srcPrefix {
				targets = append(targets, t)
			}
		}
	}
	return targets, nil
}

func loadVariantSource(path string) (*mdl.Model, error) {
	bin, err := isBinaryMDL(path)
	if err != nil {
		return nil, err
	}
	if bin {
		return mdl.DecompileFile(path)
	}
	res, err := mdl.ParseFile(path)
	if res == nil || res.Model == nil {
		if err == nil {
			err = fmt.Errorf("%s: no model", path)
		}
		return nil, err
	}
	return res.Model, nil
}

func writeVariant(m *mdl.Model, path, input string, compile, overwrite bool, superDirs, resDirs []string, res *variantResult) error {
	if same, _ := sameFile(path, input); same {
		return fmt.Errorf("%s is the input robe; give an output directory", path)
	}
	if _, err := os.Stat(path); err == nil && !overwrite {
		return fmt.Errorf("%s exists; add --overwrite to replace it", path)
	}
	if !compile {
		return mdl.WriteFile(m, path)
	}
	return mdl.CompileFileWithOptions(m, path, mdl.CompileOptions{
		SupermodelDirs: superDirs,
		ResourceDirs:   resDirs,
		Warn:           func(s string) { res.Warnings = append(res.Warnings, s) },
	})
}

func sameFile(a, b string) (bool, error) {
	sa, err := os.Stat(a)
	if err != nil {
		return false, err
	}
	sb, err := os.Stat(b)
	if err != nil {
		return false, err
	}
	return os.SameFile(sa, sb), nil
}

func printVariantResult(r variantResult) {
	if r.Error != "" {
		fmt.Printf("%s  failed: %s\n", r.Target, r.Error)
		return
	}
	fmt.Printf("%s  %s\n", r.Target, r.File)
	for _, w := range r.Warnings {
		fmt.Printf("    %s\n", w)
	}
}
