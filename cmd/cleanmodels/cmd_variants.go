package main

import (
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

// variantRaces are the stock races, by the letter in the body prefix:
// halfling, dwarf, elf, gnome, human (also half-elf), half-orc. They are the
// default targets when no bodies are found (no game install).
const variantRaces = "adegho"

// bodyPrefixRE matches a body prefix: p, gender, race letter, phenotype.
var bodyPrefixRE = regexp.MustCompile(`^p[mf][a-z][0-9]+$`)

// variantResult is one variant's outcome, as printed by --json and --json-lines.
type variantResult struct {
	Source string `json:"source"`
	Target string `json:"target"`
	File   string `json:"file,omitempty"`
	// LearnedFrom is how many existing versions of an armour part's slot its
	// fit was learned from; 0 for the measured fit.
	LearnedFrom int `json:"learnedFrom,omitempty"`
	// Action is what --dry-run would do: write, replace, or skip (exists).
	Action string `json:"action,omitempty"`
	// SameShape is set when the variant is that body's, renamed (mdl.Body).
	SameShape string   `json:"sameShape,omitempty"`
	Preview   string   `json:"preview,omitempty"`
	Body      string   `json:"body,omitempty"`
	Warnings  []string `json:"warnings,omitempty"`
	Error     string   `json:"error,omitempty"`
}

func cmdVariants(args []string) int {
	fs := flag.NewFlagSet("cleanmodels variants", flag.ContinueOnError)
	fs.SetOutput(os.Stderr)

	to := fs.String("to", "", "comma-separated bodies to make variants for (pmd0,pme2,...); default: every body found (game install, --resource-dir) of the source's gender that has models of the source's kind")
	listBodies := fs.Bool("list-bodies", false, "list the bodies found in the game install and --resource-dir, with their shape groups; given a model, the bodies it would get variants for (and its own)")
	compile := fs.Bool("compile", false, "write compiled (binary) models instead of ASCII")
	dryRun := fs.Bool("dry-run", false, "make nothing: list each variant's file, whether it would be written, replace one or be skipped (exists, no --overwrite), and how it would be fitted")
	width := fs.Float64("width", 1, "multiply the fit's width across every bone (1.05: 5% looser)")
	length := fs.Float64("length", 1, "multiply the fit's stretch along every bone")
	shapeFlag := fs.String("shape", "", "narrow or widen by body region after the fit: shoulders=0.9,chest=1,waist=0.85,hips=1,hem=1.1 (factors 0.5 to 2; sleeves stay on the arms)")
	previewDir := fs.String("preview-dir", "", "also write, per variant, static preview models of it in place on its body and of the bare body (for viewers such as cleanmodels-qt)")

	var cf commonFlags
	cf.register(fs)

	fs.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: cleanmodels variants [flags] <model.mdl> [output-dir]\n       cleanmodels variants --list-bodies [flags] [model.mdl]\n\n")
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
	for _, f := range []struct {
		name string
		v    float64
	}{{"width", *width}, {"length", *length}} {
		if f.v < 0.5 || f.v > 2 {
			fmt.Fprintf(os.Stderr, "cleanmodels: --%s %g: give a factor from 0.5 to 2\n", f.name, f.v)
			return exitUsage
		}
	}
	if *dryRun && *cf.jsonLines {
		fmt.Fprintf(os.Stderr, "cleanmodels: --dry-run lists what would be done; use it with --json or plain output, not --json-lines\n")
		return exitUsage
	}
	shape, err := mdl.ParseShape(*shapeFlag)
	if err != nil {
		fmt.Fprintf(os.Stderr, "cleanmodels: --shape %v\n", err)
		return exitUsage
	}
	pos := fs.Args()
	if *listBodies && len(pos) == 0 {
		var o procOpts
		cf.apply(&o)
		locs := append(append([]string{}, o.supermodelDirs...), o.resourceDirs...)
		if g := resolveGameInstall(o.gameDir); g.dir != "" {
			locs = append(locs, g.dir)
		}
		return printBodies(mdl.VariantBodies(locs), "", o)
	}
	if len(pos) < 1 || len(pos) > 2 || (*listBodies && len(pos) > 1) {
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
	kind := mdl.VariantKind(src.Name)
	if !bodyPrefixRE.MatchString(srcPrefix) || kind == "" {
		fmt.Fprintf(os.Stderr, "cleanmodels: %s: model %q is not a robe, cloak or armour part named for a body (pmh0_robe003, pmh0_cloak_001, pmh0_chest050)\n", input, src.Name)
		return exitErrors
	}
	bodies := mdl.VariantBodies(superDirs)
	targets, err := variantTargets(*to, srcPrefix, kind, bodies)
	if err != nil {
		fmt.Fprintf(os.Stderr, "cleanmodels: %v\n", err)
		return exitUsage
	}
	if *listBodies {
		var list []mdl.Body
		for _, b := range bodies {
			if b.Prefix == srcPrefix {
				list = append([]mdl.Body{b}, list...)
			} else if slices.Contains(targets, b.Prefix) {
				list = append(list, b)
			}
		}
		return printBodies(list, srcPrefix, o)
	}
	sameShape := map[string]string{}
	for _, b := range bodies {
		sameShape[b.Prefix] = b.SameShape
	}
	for _, d := range []string{outDir, *previewDir} {
		if d == "" || *dryRun {
			continue // a dry run makes nothing, folders included
		}
		if err := os.MkdirAll(d, 0o755); err != nil {
			fmt.Fprintf(os.Stderr, "cleanmodels: %v\n", err)
			return exitErrors
		}
	}
	if *previewDir != "" && o.jsonLines {
		// The source in place on its own body, to compare the variants with.
		evt := Event{Type: "source", File: filepath.Base(input), Source: input, Target: srcPrefix}
		var r variantResult
		writePreview(src, srcPrefix, src.Name, *previewDir, superDirs, &r)
		evt.Preview, evt.Body = r.Preview, r.Body
		if len(r.Warnings) > 0 {
			evt.Message = strings.Join(r.Warnings, "; ")
		}
		emitEvent(evt)
	}

	suffix := src.Name[len(srcPrefix):]
	failed := 0
	var results []variantResult
	// A body of the same shape as another (mdl.Body.SameShape) gets that
	// body's variant, renamed: fitted once per shape.
	type made struct {
		m        *mdl.Model
		warnings []string
		learned  int // pairs the fit was learned from (0: measured)
		err      error
	}
	fitted := map[string]made{}
	fit := func(body string) made {
		if f, ok := fitted[body]; ok {
			return f
		}
		var f made
		f.m, f.err = mdl.Variant(src, body, mdl.VariantOptions{
			Locs:      superDirs,
			OutputDir: outDir,
			Warn:      func(s string) { f.warnings = append(f.warnings, s) },
			Width:     *width,
			Length:    *length,
			Shape:     shape,
			Learned:   func(pairs int) { f.learned = pairs },
		})
		fitted[body] = f
		return f
	}
	for i, target := range targets {
		name := target + suffix + ".mdl"
		if o.jsonLines {
			emitEvent(Event{Type: "start", File: name, Index: i + 1, Total: len(targets), Source: input, Target: target})
		}
		res := variantResult{Source: input, Target: target}
		var out *mdl.Model
		var err error
		switch rep := sameShape[target]; {
		case rep == "" || *to != "" && !slices.Contains(targets, rep) && rep != srcPrefix:
			// Its own shape, or one asked for alone: fit it itself.
			f := fit(target)
			out, err, res.LearnedFrom = f.m, f.err, f.learned
			res.Warnings = append([]string(nil), f.warnings...)
		case rep == srcPrefix:
			res.SameShape = rep
			out = mdl.RenameBody(src, srcPrefix, target)
		default:
			res.SameShape = rep
			f := fit(rep)
			if f.err != nil {
				err = fmt.Errorf("same shape as %s, which failed: %v", rep, f.err)
			} else {
				out = mdl.RenameBody(f.m, rep, target)
				res.LearnedFrom = f.learned
			}
		}
		if err == nil && *dryRun {
			res.File = filepath.Join(outDir, name)
			res.Action = "write"
			if same, _ := sameFile(res.File, input); same {
				err = fmt.Errorf("%s is the input model; give an output directory", res.File)
			} else if _, statErr := os.Stat(res.File); statErr == nil {
				res.Action = "replace"
				if !o.overwrite {
					res.Action = "skip: it exists, add --overwrite to replace it"
				}
			}
		} else if err == nil {
			res.File = filepath.Join(outDir, name)
			err = writeVariant(out, res.File, input, *compile, o.overwrite, superDirs, resDirs, &res)
		}
		if err == nil && !*dryRun && *previewDir != "" && res.SameShape == "" {
			writePreview(out, target, target+suffix, *previewDir, superDirs, &res)
		}
		if err != nil {
			res.Error = err.Error()
			res.File = ""
			failed++
		}
		results = append(results, res)
		if o.jsonLines {
			evt := Event{File: name, Index: i + 1, Total: len(targets), Source: input, Target: target, SameShape: res.SameShape, LearnedFrom: res.LearnedFrom}
			if res.Error != "" {
				evt.Type, evt.Message = "error", res.Error
			} else {
				r := Result{File: res.File}
				for _, w := range res.Warnings {
					r.Warnings = append(r.Warnings, mdl.DecompileWarning{Kind: mdl.WarnGeneral, Message: w})
				}
				evt.Type, evt.Output, evt.Result = "done", res.File, &r
				evt.Preview, evt.Body = res.Preview, res.Body
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
	if *dryRun {
		summary = fmt.Sprintf("dry run: %d of %d variants would be made, nothing written", len(targets)-failed, len(targets))
	}
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
// every body found of the source's gender that has models of its kind (robe,
// cloak, chest, ...), without the source's own body. When none is found (no
// game install), the stock races in phenotypes 0 and 2.
func variantTargets(list, srcPrefix, kind string, bodies []mdl.Body) ([]string, error) {
	var targets []string
	if list != "" {
		for _, t := range strings.Split(list, ",") {
			t = strings.ToLower(strings.TrimSpace(t))
			if !bodyPrefixRE.MatchString(t) {
				return nil, fmt.Errorf("--to: %q is not a body prefix such as pmd0", t)
			}
			targets = append(targets, t)
		}
		return targets, nil
	}
	for _, b := range bodies {
		if b.Gender == srcPrefix[1:2] && b.Slots[kind] > 0 && b.Prefix != srcPrefix {
			targets = append(targets, b.Prefix)
		}
	}
	if len(targets) > 0 {
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

// printBodies prints bodies for --list-bodies: a JSON array (--json), one
// JSON object per line (--json-lines), or a table. source, if set, is the
// model's own body.
func printBodies(bodies []mdl.Body, source string, o procOpts) int {
	type entry struct {
		mdl.Body
		Source bool `json:"source,omitempty"`
	}
	list := make([]entry, len(bodies))
	for i, b := range bodies {
		list[i] = entry{b, b.Prefix == source}
	}
	switch {
	case o.jsonOut:
		b, _ := json.MarshalIndent(list, "", "  ")
		fmt.Println(string(b))
	case o.jsonLines:
		for _, e := range list {
			b, _ := json.Marshal(e)
			fmt.Println(string(b))
		}
	default:
		if len(list) == 0 {
			fmt.Println("no bodies found: pass --game-dir or --resource-dir")
			return exitOK
		}
		fmt.Printf("%-7s %-10s %-3s %-16s %6s  %s\n", "body", "race", "sex", "phenotype", "parts", "notes")
		for _, e := range list {
			var notes []string
			if e.Source {
				notes = append(notes, "source")
			}
			if e.SameShape != "" {
				notes = append(notes, "same shape as "+e.SameShape)
			}
			if !e.Skeleton {
				notes = append(notes, "no skeleton of its own: the game uses "+e.SkeletonFrom)
			}
			if e.PartsFrom != e.Prefix {
				notes = append(notes, "base parts from "+e.PartsFrom)
			}
			ph := strconv.Itoa(e.Phenotype)
			if e.PhenotypeName != "" {
				ph += " " + e.PhenotypeName
			}
			fmt.Printf("%-7s %-10s %-3s %-16s %6d  %s\n", e.Prefix, e.RaceName, e.Gender, ph, e.Parts, strings.Join(notes, "; "))
		}
	}
	return exitOK
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

// writePreview writes m's preview models (mdl.VariantPreview) for the body
// target to dir as <name>.fit.mdl and <name>.body.mdl, and records their
// absolute paths in res. A preview that cannot be made is a warning: the
// variant itself is written.
func writePreview(m *mdl.Model, target, name, dir string, locs []string, res *variantResult) {
	fit, body, notes, err := mdl.VariantPreview(m, target, locs, dir)
	if err != nil {
		res.Warnings = append(res.Warnings, "no preview: "+err.Error())
		return
	}
	res.Warnings = append(res.Warnings, notes...)
	abs := func(p string) string {
		if a, err := filepath.Abs(p); err == nil {
			return a
		}
		return p
	}
	fitPath := filepath.Join(dir, strings.ToLower(name)+".fit.mdl")
	bodyPath := filepath.Join(dir, strings.ToLower(name)+".body.mdl")
	if err := mdl.WriteFile(fit, fitPath); err != nil {
		res.Warnings = append(res.Warnings, "no preview: "+err.Error())
		return
	}
	if err := mdl.WriteFile(body, bodyPath); err != nil {
		res.Warnings = append(res.Warnings, "no preview body: "+err.Error())
		bodyPath = ""
	}
	res.Preview = abs(fitPath)
	if bodyPath != "" {
		res.Body = abs(bodyPath)
	}
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
	var how []string
	if r.LearnedFrom > 0 {
		how = append(how, fmt.Sprintf("learned from %d existing pairs", r.LearnedFrom))
	}
	if r.SameShape != "" {
		how = append(how, r.SameShape+"'s fit: same shape")
	}
	line := r.Target + "  " + r.File
	if r.Action != "" {
		line += "  would " + r.Action
	}
	if len(how) > 0 {
		line += "  (" + strings.Join(how, "; ") + ")"
	}
	fmt.Println(line)
	if r.SameShape != "" {
		return
	}
	for _, w := range r.Warnings {
		fmt.Printf("    %s\n", w)
	}
}
