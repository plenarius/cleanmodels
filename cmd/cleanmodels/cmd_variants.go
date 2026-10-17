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

// variantRaces are the races variants are made for, by the letter in the body
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
		fmt.Fprintf(os.Stderr, "Make a robe's, cloak's or armour part's versions for other races and phenotypes:\npmh0_robe003.mdl gives pmd0_robe003.mdl, pme0_robe003.mdl and so on; pmh0_chest050.mdl\ngives pmd0_chest050.mdl and so on. Each version is fitted to that body using the race\nskeletons and stock base body parts (and, for robes and cloaks, a stock one of that\nbody), all read from the game install (--game-dir) or --resource-dir. The output directory\ndefaults to the model's own. An existing file is not overwritten unless --overwrite\nis given.\n\n")
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

	superDirs, resDirs := lookupDirs(input, o)
	src, err := loadVariantSource(input, superDirs)
	if err != nil {
		fmt.Fprintf(os.Stderr, "cleanmodels: %v\n", wrapWithSuggestion(err, input))
		return exitErrors
	}
	srcPrefix := mdl.VariantPrefix(src.Name)
	if srcPrefix == "" || len(srcPrefix) != 4 {
		fmt.Fprintf(os.Stderr, "cleanmodels: %s: model %q is not named for a body (pmh0_robe003, pmh0_chest050)\n", input, src.Name)
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

	suffix := src.Name[len(srcPrefix):]
	failed := 0
	var results []variantResult
	for i, target := range targets {
		name := target + suffix + ".mdl"
		if o.jsonLines {
			emitEvent(Event{Type: "start", File: name, Index: i + 1, Total: len(targets), Source: input, Target: target})
		}
		res := variantResult{Source: input, Target: target}
		out, err := mdl.Variant(src, target, mdl.VariantOptions{
			Locs: superDirs,
			Warn: func(s string) { res.Warnings = append(res.Warnings, s) },
		})
		if err == nil {
			res.File = filepath.Join(outDir, name)
			err = writeVariant(out, res.File, input, *compile, o.overwrite, superDirs, resDirs, &res)
		}
		if err != nil {
			res.Error = err.Error()
			res.File = ""
			failed++
		}
		results = append(results, res)
		if o.jsonLines {
			evt := Event{File: name, Index: i + 1, Total: len(targets), Source: input, Target: target}
			if res.Error != "" {
				evt.Type, evt.Message = "error", res.Error
			} else {
				r := Result{File: res.File}
				for _, w := range res.Warnings {
					r.Warnings = append(r.Warnings, mdl.DecompileWarning{Kind: mdl.WarnGeneral, Message: w})
				}
				evt.Type, evt.Output, evt.Result = "done", res.File, &r
				if abs, err := filepath.Abs(res.File); err == nil {
					evt.Output = abs
				}
			}
			emitEvent(evt)
		} else if !o.jsonOut && !o.quiet {
			printVariantResult(res)
		} else if o.quiet && res.Error != "" {
			fmt.Fprintf(os.Stderr, "%s: %s\n", target, res.Error)
		}
	}
	summary := fmt.Sprintf("%d of %d variants written", len(targets)-failed, len(targets))
	if o.jsonOut {
		b, _ := json.MarshalIndent(results, "", "  ")
		fmt.Println(string(b))
	} else if o.jsonLines {
		emitEvent(Event{Type: "summary", Total: len(targets), Message: summary})
	} else if !o.quiet {
		fmt.Printf("\n%s\n", summary)
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

// loadVariantSource reads the source model; a binary skinned body part is
// decompiled with the skeletons found in dirs, so its bones are named.
func loadVariantSource(path string, dirs []string) (*mdl.Model, error) {
	bin, err := isBinaryMDL(path)
	if err != nil {
		return nil, err
	}
	if bin {
		return mdl.DecompileFileWithOptions(path, mdl.DecompileOptions{SkeletonDirs: dirs})
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
