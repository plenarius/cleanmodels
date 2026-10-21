package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"sync"

	"github.com/plenarius/cleanmodels/pkg/checks"
	"github.com/plenarius/cleanmodels/pkg/mdl"
)

// Result is the JSON payload for a single processed file.
type Result struct {
	File     string                 `json:"file"`
	Warnings []mdl.DecompileWarning `json:"warnings,omitempty"`
	// Notes are decompile notes at info level (mdl.Model.Notes): what was
	// dropped that is known and harmless. Not counted as warnings.
	Notes []mdl.DecompileWarning `json:"notes,omitempty"`
	// ParseWarnings are the ASCII parser's complaints ("line 352: ... set to 0").
	// They are in the text output; this puts them in JSON and JSON-lines too, so
	// the GUI and scripts see what the parser had to work around.
	ParseWarnings []string          `json:"parse_warnings,omitempty"`
	Checks        []mdl.CheckResult `json:"checks,omitempty"`
	Repairs       []string          `json:"repairs,omitempty"`
	Actions       []string          `json:"actions,omitempty"`
	Error         string            `json:"error,omitempty"`
}

// Event is a streaming JSON-lines event for real-time GUI consumption.
type Event struct {
	Type    string `json:"type"`
	File    string `json:"file,omitempty"`
	Index   int    `json:"index,omitempty"`
	Total   int    `json:"total,omitempty"`
	Message string `json:"message,omitempty"`
	Fixes   int    `json:"fixes,omitempty"`
	*Result `json:"result,omitempty"`

	// variants: the model the version is made from, the body it is for
	// (pmd0) and the file written.
	Source string `json:"source,omitempty"`
	Target string `json:"target,omitempty"`
	Output string `json:"output,omitempty"`
	// Preview and Body are a variant's preview models (variants --preview-dir).
	Preview string `json:"preview,omitempty"`
	Body    string `json:"body,omitempty"`
	// SameShape: the variant is this body's fit, renamed (variants).
	SameShape string `json:"sameShape,omitempty"`
	// LearnedFrom: an armour part's fit was learned from this many existing
	// versions of its slot (variants); absent for the measured fit.
	LearnedFrom int `json:"learnedFrom,omitempty"`
	// Adjusts lists the adjustments that change the variant (variants):
	// width, length and shape regions.
	Adjusts []string `json:"adjusts,omitempty"`
}

type pivotOpts struct {
	allowSplit bool
	belowZ0    string // "disallow", "allow", "slice"
	moveBad    string // "no", "top", "middle", "bottom"
	smoothing  string // "use", "protect", "ignore"
	minFaces   int
	splitFirst string // "convex", "concave"
}

type repairOpts struct {
	fixPivots, fixAABB, fixTilefade, stripDegenerate bool
	fixAnimations, reparentChildren, wrapRoot        bool
	fixTangents                                      bool
	splitMultiEdge                                   bool
	forceWhite, mergeByBitmap, cullInvisible         bool
	scale                                            float32
	scaleX, scaleY, scaleZ                           float32
	classification                                   string
	snapMode                                         string
	renderOverride, shadowOverride                   string

	pivot pivotOpts

	tvertSnap             string // "256", "512", "1024"
	placeableTransparency bool
	transparencyKey       string
	remapWalkmeshMaterial string // "FROM:TO"
	tilefadeUndo          bool
	standardizeTexture0   bool
	stripEEExtras         bool
}

type tileOpts struct {
	tilefadeZ    float32
	water        bool
	waterKey     string
	dynamicWater string // "yes", "no", "wavy"
	waveHeight   float64
	rotateWater  string // "0" or "1"
	retileWater  string // "1", "2", "3"
	foliage      string // "tilefade", "animate", "de-animate", "ignore"
	foliageKey   string
	splotch      string // "animate"
	splotchKey   string
	rotateGround string // "0" or "1"
	groundKey    string
	chamfer      string // "add", "delete"
	retileGround string // "1", "2", "3"
	raiseLower   string // "raise", "lower"
	raiseAmount  float64
}

type procOpts struct {
	check, forceBin, jsonOut, jsonLines, verbose, quiet, recursive bool
	decompileOnly, dryRun                                          bool
	compile                                                        bool
	overwrite                                                      bool // replace an existing output file
	force                                                          bool // write even when reading the model lost triangles
	workers                                                        int
	include, exclude                                               map[string]bool
	colorMode                                                      string // "auto", "always", "never"
	supermodelDirs                                                 []string
	resourceDirs                                                   []string
	gameDir                                                        string

	repairOpts
	tileOpts
}

const (
	exitOK     = 0
	exitErrors = 1
	exitUsage  = 2
)

func setAllRepairs(o *procOpts) {
	o.fixPivots = true
	o.fixAABB = true
	o.fixTilefade = true
	o.stripDegenerate = true
	o.fixAnimations = true
	o.reparentChildren = true
	o.wrapRoot = true
	o.splitMultiEdge = true
	o.fixTangents = true
}

var eventMu sync.Mutex

func emitEvent(evt Event) {
	eventMu.Lock()
	defer eventMu.Unlock()
	data, err := json.Marshal(evt)
	if err != nil {
		fmt.Fprintf(os.Stderr, "cleanmodels: json marshal event: %v\n", err)
		return
	}
	os.Stdout.Write(data)
	os.Stdout.Write([]byte{'\n'})
}

func processPanicMessage(r interface{}) string {
	if os.Getenv("CLEANMODELS_DEBUG") != "" {
		buf := make([]byte, 4096)
		n := runtime.Stack(buf, false)
		return fmt.Sprintf("panic: %v\n%s", r, buf[:n])
	}
	return fmt.Sprintf("panic: %v", r)
}

func parseNameSet(s string) map[string]bool {
	if strings.TrimSpace(s) == "" {
		return nil
	}
	parts := strings.Split(s, ",")
	m := make(map[string]bool)
	for _, p := range parts {
		name := strings.TrimSpace(p)
		if name != "" {
			m[name] = true
		}
	}
	return m
}

func isBinaryMDL(path string) (bool, error) {
	f, err := os.Open(path)
	if err != nil {
		return false, err
	}
	defer f.Close()

	var hdr [4]byte
	n, err := io.ReadFull(f, hdr[:])
	if err != nil && !errors.Is(err, io.ErrUnexpectedEOF) && !errors.Is(err, io.EOF) {
		return false, err
	}
	if n < 4 {
		return false, nil
	}
	return hdr[0] == 0 && hdr[1] == 0 && hdr[2] == 0 && hdr[3] == 0, nil
}

// dispatchInputs runs one input as before and several as a batch.
func dispatchInputs(inputs []string, outputPath string, opts procOpts) int {
	if err := requireCompileOutput(opts, outputPath); err != nil {
		fmt.Fprintf(os.Stderr, "cleanmodels: %v\n", err)
		return exitUsage
	}
	if opts.compile {
		reportGameInstall(os.Stderr, opts)
	}
	if len(inputs) == 1 {
		return dispatch(inputs[0], outputPath, opts)
	}
	tw, errTw := newWriters(opts)
	return runBatch(inputs, outputPath, opts, tw, errTw)
}

func newWriters(opts procOpts) (tw, errTw *termWriter) {
	useColor := !opts.jsonOut && !opts.jsonLines && !opts.quiet
	tw = newTermWriter(os.Stdout, useColor && shouldColorize(os.Stdout.Fd(), opts.colorMode))
	if isTerminal(int(os.Stdout.Fd())) {
		tw.cols = terminalWidth(int(os.Stdout.Fd()))
	}
	// errTw mirrors the colour decision but targets stderr. Diagnostic
	// summary lines route here so they never collide with --json data on
	// stdout, even when the user pipes only stdout to a file.
	errTw = newTermWriter(os.Stderr, useColor && shouldColorize(os.Stderr.Fd(), opts.colorMode))
	if isTerminal(int(os.Stderr.Fd())) {
		errTw.cols = terminalWidth(int(os.Stderr.Fd()))
	}
	return tw, errTw
}

func dispatch(inputPath, outputPath string, opts procOpts) int {
	tw, errTw := newWriters(opts)
	fi, err := os.Stat(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "cleanmodels: %v\n", wrapWithSuggestion(err, inputPath))
		return exitErrors
	}
	if fi.IsDir() {
		return runBatch([]string{inputPath}, outputPath, opts, tw, errTw)
	}
	outputPath, err = intoDirectory(inputPath, outputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "cleanmodels: %v\n", err)
		return exitErrors
	}
	if err := checkOverwrite(inputPath, outputPath, opts.overwrite); err != nil {
		fmt.Fprintf(os.Stderr, "cleanmodels: %v\n", err)
		return exitUsage
	}
	return runSingle(inputPath, outputPath, opts, tw, errTw)
}

// intoDirectory returns where a single input file is written when the output
// is a directory: an existing one, or a path ending in a separator (created),
// takes the input's file name, as a batch does. Any other output is the file
// itself.
func intoDirectory(inputPath, outputPath string) (string, error) {
	if outputPath == "" {
		return outputPath, nil
	}
	if strings.HasSuffix(outputPath, "/") || strings.HasSuffix(outputPath, string(os.PathSeparator)) {
		if err := os.MkdirAll(outputPath, 0o755); err != nil {
			return "", err
		}
		return filepath.Join(outputPath, filepath.Base(inputPath)), nil
	}
	if st, err := os.Stat(outputPath); err == nil && st.IsDir() {
		return filepath.Join(outputPath, filepath.Base(inputPath)), nil
	}
	return outputPath, nil
}

// addError records a failure in the result without stopping the run: the other
// repairs are kept and the file is still written, but the result carries the error
// (so the exit code is non-zero and JSON has it in "error").
func addError(res *Result, msg string) {
	if res.Error == "" {
		res.Error = msg
		return
	}
	res.Error += "; " + msg
}

func applyRepairs(model *mdl.Model, o procOpts, res *Result) {
	if o.classification != "" {
		old := model.Classification
		model.Classification = o.classification
		if old != o.classification {
			res.Repairs = append(res.Repairs, fmt.Sprintf("classification overridden from %q to %q", old, o.classification))
		}
	}

	sx, sy, sz := o.scaleX, o.scaleY, o.scaleZ
	if sx == 0 && sy == 0 && sz == 0 && o.scale != 0 {
		sx, sy, sz = o.scale, o.scale, o.scale
	}
	if sx != 0 || sy != 0 || sz != 0 {
		if sx == 0 {
			sx = 1
		}
		if sy == 0 {
			sy = 1
		}
		if sz == 0 {
			sz = 1
		}
		if sx == sy && sy == sz {
			mdl.ScaleModel(model, sx)
			res.Repairs = append(res.Repairs, fmt.Sprintf("scaled model by factor %g (animationscale now %g)", sx, model.AnimationScale))
		} else {
			mdl.ScaleModelPerAxis(model, sx, sy, sz)
			res.Repairs = append(res.Repairs, fmt.Sprintf("scaled model by factors x=%g y=%g z=%g", sx, sy, sz))
		}
		hasAABB := false
		for _, n := range model.Nodes {
			if n != nil && n.Aabb != nil {
				hasAABB = true
				break
			}
		}
		if hasAABB && !o.fixAABB {
			res.Repairs = append(res.Repairs, "model has AABB nodes; consider using --fix-aabb to rebuild after scaling")
		}
	}

	if o.snapMode != "" {
		mdl.SnapVertices(model, o.snapMode)
		res.Repairs = append(res.Repairs, fmt.Sprintf("snapped vertices to %s grid", o.snapMode))
	}

	if o.renderOverride != "" {
		n := mdl.ForceRender(model, o.renderOverride)
		if n > 0 {
			res.Repairs = append(res.Repairs, fmt.Sprintf("set render=%s on %d mesh nodes", o.renderOverride, n))
		}
	}
	if o.shadowOverride != "" {
		n := mdl.ForceShadow(model, o.shadowOverride)
		if n > 0 {
			res.Repairs = append(res.Repairs, fmt.Sprintf("set shadow=%s on %d mesh nodes", o.shadowOverride, n))
		}
	}

	if o.forceWhite {
		n := mdl.ForceWhiteAmbientDiffuse(model)
		if n > 0 {
			res.Repairs = append(res.Repairs, fmt.Sprintf("set ambient/diffuse to white on %d mesh nodes", n))
		}
	}

	if o.cullInvisible {
		n := mdl.CullInvisibleMeshes(model)
		if n > 0 {
			res.Repairs = append(res.Repairs, fmt.Sprintf("culled %d invisible meshes to dummy", n))
		}
	}

	if o.mergeByBitmap {
		n := mdl.MergeByBitmap(model)
		if n > 0 {
			res.Repairs = append(res.Repairs, fmt.Sprintf("merged %d mesh groups by bitmap", n))
		}
	}
	if o.fixPivots {
		popts := mdl.PivotOptions{
			AllowSplit: o.pivot.allowSplit,
			BelowZ0:    o.pivot.belowZ0,
			MoveBad:    o.pivot.moveBad,
			Smoothing:  o.pivot.smoothing,
			MinFaces:   o.pivot.minFaces,
			SplitFirst: o.pivot.splitFirst,
		}
		msgs := mdl.RepairPivots(model, popts)
		for _, m := range msgs {
			res.Repairs = append(res.Repairs, m)
		}
	}
	if o.fixAABB {
		for _, n := range model.Nodes {
			if n == nil || n.Aabb == nil || n.Mesh == nil || len(n.Mesh.Faces) == 0 {
				continue
			}
			if err := mdl.RebuildAABB(n); err != nil {
				// A repair that failed is an error, not a line in the list of repairs:
				// the tree is left as it was, and the run must not exit 0.
				addError(res, fmt.Sprintf("aabb rebuild %s: %v", n.Name, err))
			} else {
				res.Repairs = append(res.Repairs, fmt.Sprintf("aabb rebuilt for %s (%d entries)", n.Name, len(n.Aabb.Entries)))
			}
		}
	}
	if o.fixTilefade {
		msgs := mdl.SliceTileFade(model, o.tilefadeZ)
		for _, m := range msgs {
			res.Repairs = append(res.Repairs, m)
		}
	}
	if o.stripDegenerate {
		total := mdl.StripDegenerateFaces(model)
		if total > 0 {
			res.Repairs = append(res.Repairs, fmt.Sprintf("stripped %d degenerate (zero-area) faces", total))
		}
	}
	if o.fixAnimations {
		for _, m := range mdl.FixAnimationLengths(model) {
			res.Repairs = append(res.Repairs, m)
		}
	}
	if o.fixTangents {
		res.Repairs = append(res.Repairs, mdl.DropZeroLengthTangents(model)...)
	}
	if o.reparentChildren {
		for _, m := range mdl.ReparentFromRestrictedNodes(model) {
			res.Repairs = append(res.Repairs, m)
		}
	}
	if o.wrapRoot {
		if msg := mdl.WrapRootInDummy(model); msg != "" {
			res.Repairs = append(res.Repairs, msg)
		}
	}
	if o.splitMultiEdge {
		for _, n := range model.Nodes {
			if n == nil || !n.IsShadowCaster() {
				continue
			}
			fixed := mdl.SplitMultipleEdges(n)
			if fixed > 0 {
				res.Repairs = append(res.Repairs, fmt.Sprintf("split %d multiple edge(s) on shadow node %q to fix shadow tearing", fixed, n.Name))
			}
		}
	}

	if o.tilefadeUndo {
		for _, m := range mdl.UndoTileFade(model) {
			res.Repairs = append(res.Repairs, m)
		}
	}

	if o.tvertSnap != "" {
		grid := 0
		switch o.tvertSnap {
		case "256":
			grid = 256
		case "512":
			grid = 512
		case "1024":
			grid = 1024
		}
		if grid > 0 {
			n := mdl.SnapTVerts(model, grid)
			if n > 0 {
				res.Repairs = append(res.Repairs, fmt.Sprintf("snapped %d tverts to 1/%d grid", n, grid))
			}
		}
	}

	if o.remapWalkmeshMaterial != "" {
		parts := strings.SplitN(o.remapWalkmeshMaterial, ":", 2)
		if len(parts) == 2 {
			var from, to int
			if _, err := fmt.Sscanf(parts[0], "%d", &from); err == nil {
				if _, err := fmt.Sscanf(parts[1], "%d", &to); err == nil {
					for _, m := range mdl.RemapAABBMaterial(model, from, to) {
						res.Repairs = append(res.Repairs, m)
					}
				}
			}
		}
	}

	if o.placeableTransparency && o.transparencyKey != "" {
		for _, m := range mdl.PlaceableTransparency(model, o.transparencyKey) {
			res.Repairs = append(res.Repairs, m)
		}
	}

	if o.standardizeTexture0 {
		n := mdl.StandardizeTexture0(model)
		if n > 0 {
			res.Repairs = append(res.Repairs, fmt.Sprintf("standardized bitmap to texture0 on %d mesh nodes", n))
		}
	}

	if o.stripEEExtras {
		n := mdl.StripEEExtras(model)
		if n > 0 {
			res.Repairs = append(res.Repairs, fmt.Sprintf("stripped wirecolor/specular/shininess from %d nodes", n))
		}
	}

	// Tile operations
	if o.tileOpts.rotateWater != "" && o.tileOpts.waterKey != "" {
		val := int32(0)
		if o.tileOpts.rotateWater == "1" {
			val = 1
		}
		for _, m := range mdl.SetRotateTexture(model, o.tileOpts.waterKey, val) {
			res.Repairs = append(res.Repairs, m)
		}
	}

	if o.tileOpts.retileWater != "" && o.tileOpts.waterKey != "" {
		ts := 0
		switch o.tileOpts.retileWater {
		case "1":
			ts = 1
		case "2":
			ts = 2
		case "3":
			ts = 3
		}
		if ts > 0 {
			for _, m := range mdl.RetileUVs(model, o.tileOpts.waterKey, ts) {
				res.Repairs = append(res.Repairs, m)
			}
		}
	}

	if o.tileOpts.rotateGround != "" && o.tileOpts.groundKey != "" {
		val := int32(0)
		if o.tileOpts.rotateGround == "1" {
			val = 1
		}
		for _, m := range mdl.SetRotateTexture(model, o.tileOpts.groundKey, val) {
			res.Repairs = append(res.Repairs, m)
		}
	}

	if o.tileOpts.retileGround != "" && o.tileOpts.groundKey != "" {
		ts := 0
		switch o.tileOpts.retileGround {
		case "1":
			ts = 1
		case "2":
			ts = 2
		case "3":
			ts = 3
		}
		if ts > 0 {
			for _, m := range mdl.RetileUVs(model, o.tileOpts.groundKey, ts) {
				res.Repairs = append(res.Repairs, m)
			}
		}
	}

	if o.tileOpts.foliage == "animate" && o.tileOpts.foliageKey != "" {
		for _, m := range mdl.ReparentToModela(model, o.tileOpts.foliageKey, "foliage") {
			res.Repairs = append(res.Repairs, m)
		}
	}

	if o.tileOpts.splotch == "animate" && o.tileOpts.splotchKey != "" {
		for _, m := range mdl.ReparentToModela(model, o.tileOpts.splotchKey, "splotch") {
			res.Repairs = append(res.Repairs, m)
		}
	}

	if o.tileOpts.raiseLower != "" && o.tileOpts.raiseAmount != 0 {
		for _, m := range mdl.RaiseLowerTile(model, o.tileOpts.raiseLower, float32(o.tileOpts.raiseAmount)) {
			res.Repairs = append(res.Repairs, m)
		}
	}

	switch o.tileOpts.chamfer {
	case "add":
		for _, m := range mdl.AddChamfers(model) {
			res.Repairs = append(res.Repairs, m)
		}
	case "delete":
		for _, m := range mdl.DeleteChamfers(model) {
			res.Repairs = append(res.Repairs, m)
		}
	case "":
	default:
		res.Repairs = append(res.Repairs, fmt.Sprintf("ignoring unknown chamfer mode %q (expected add|delete)", o.tileOpts.chamfer))
	}

	if o.tileOpts.water {
		switch o.tileOpts.dynamicWater {
		case "no":
			for _, m := range mdl.ConvertWateryToTrimesh(model, o.tileOpts.waterKey) {
				res.Repairs = append(res.Repairs, m)
			}
		case "wavy":
			for _, m := range mdl.ApplyWavyWater(model, mdl.WavyWaterOptions{
				WaterKey:   o.tileOpts.waterKey,
				WaveHeight: o.tileOpts.waveHeight,
			}) {
				res.Repairs = append(res.Repairs, m)
			}
		case "yes", "":
		default:
			res.Repairs = append(res.Repairs, fmt.Sprintf("ignoring unknown dynamic-water mode %q (expected yes|no|wavy)", o.tileOpts.dynamicWater))
		}
	}
}

// runChecks runs the selected checks. Checks that resolve a body part's
// skeleton search where compile would (lookupDirs).
func runChecks(model *mdl.Model, path string, o procOpts, include, exclude map[string]bool, fix bool) []mdl.CheckResult {
	locs, _ := lookupDirs(path, o)
	return checks.RunFilteredContext(model, path, include, exclude, fix, checks.Context{Locations: locs})
}

func severityString(s mdl.Severity) string {
	switch s {
	case mdl.SevInfo:
		return "INFO"
	case mdl.SevWarning:
		return "WARNING"
	case mdl.SevError:
		return "ERROR"
	case mdl.SevFatal:
		return "FATAL"
	default:
		return fmt.Sprintf("SEVERITY(%d)", int(s))
	}
}

func countCheckBySeverity(results []mdl.CheckResult, want mdl.Severity) int {
	n := 0
	for _, r := range results {
		if r.Severity == want {
			n++
		}
	}
	return n
}

func countCheckErrors(results []mdl.CheckResult) int {
	return countCheckBySeverity(results, mdl.SevError) + countCheckBySeverity(results, mdl.SevFatal)
}

func tally(res Result, parseErrCount int) (warnings, errors int) {
	warnings = len(res.Warnings) + parseErrCount + countCheckBySeverity(res.Checks, mdl.SevWarning)
	errors = countCheckErrors(res.Checks)
	if res.Error != "" {
		errors++
	}
	return warnings, errors
}

func countFixes(res Result) int {
	fixes := 0
	for _, c := range res.Checks {
		if c.Fixed {
			fixes++
		}
	}
	fixes += len(res.Repairs)
	return fixes
}

// lookupDirs returns where to look for supermodels and skeletons, and for
// materials, when processing path: its own directory, the flags' directories,
// then the game install if one is found.
func lookupDirs(path string, o procOpts) (superDirs, resDirs []string) {
	superDirs = append(append([]string{filepath.Dir(path)}, o.supermodelDirs...), o.resourceDirs...)
	resDirs = append([]string{filepath.Dir(path)}, o.resourceDirs...)
	if g := resolveGameInstall(o.gameDir); g.dir != "" {
		superDirs, resDirs = append(superDirs, g.dir), append(resDirs, g.dir)
	}
	return superDirs, resDirs
}

// processOne loads an MDL, optionally writes output, runs checks, returns a Result.
func processOne(path, outputPath string, o procOpts) (res Result, model *mdl.Model, pErrs []mdl.ParseError, retErr error) {
	res = Result{File: path}
	defer func() {
		if r := recover(); r != nil {
			res.Error = processPanicMessage(r)
			retErr = fmt.Errorf("panic processing %s: %v", path, r)
		}
	}()

	binMode := o.forceBin
	if !o.forceBin {
		b, err := isBinaryMDL(path)
		if err != nil {
			res.Error = wrapWithSuggestion(err, path).Error()
			return res, nil, nil, err
		}
		binMode = b
	}

	var parseErrs []mdl.ParseError
	var err error

	if o.forceBin || binMode {
		superDirs, _ := lookupDirs(path, o)
		model, err = mdl.DecompileFileWithOptions(path, mdl.DecompileOptions{SkeletonDirs: superDirs})
		if err != nil {
			res.Error = wrapWithSuggestion(err, path).Error()
			return res, nil, nil, err
		}
	} else {
		var pr *mdl.ParseResult
		pr, err = mdl.ParseFile(path)
		if err != nil {
			res.Error = wrapWithSuggestion(err, path).Error()
			return res, nil, nil, err
		}
		model = pr.Model
		parseErrs = pr.Errors
		for _, pe := range parseErrs {
			res.ParseWarnings = append(res.ParseWarnings, pe.Error())
		}
		if model != nil {
			model.FileType = "ascii"
		}
	}

	if model != nil {
		res.Warnings = model.Warnings
		res.Notes = model.Notes
	}

	if !o.decompileOnly && model != nil {
		applyRepairs(model, o, &res)
	}

	if o.check && !o.decompileOnly && model != nil {
		fix := !o.dryRun
		res.Checks = runChecks(model, path, o, o.include, o.exclude, fix)
	}

	if model != nil && !o.dryRun && !o.decompileOnly && (o.compile || outputPath != "") {
		if err := refuseLostTriangles(model, o.force); err != nil {
			res.Error = err.Error()
			return res, model, parseErrs, err
		}
	}

	if model != nil && !o.dryRun {
		switch {
		case o.compile:
			outPath := outputPath
			if outPath == "" {
				outPath = path
			}
			superDirs, resDirs := lookupDirs(path, o)
			copts := mdl.CompileOptions{
				SupermodelDirs: superDirs,
				ResourceDirs:   resDirs,
				Warn: func(msg string) {
					res.Warnings = append(res.Warnings, mdl.DecompileWarning{Kind: mdl.WarnGeneral, Message: msg})
				},
			}
			if err := mdl.CompileFileWithOptions(model, outPath, copts); err != nil {
				res.Error = err.Error()
				return res, model, parseErrs, err
			}
			res.Actions = append(res.Actions, fmt.Sprintf("compiled to binary: %s", outPath))
		case outputPath != "":
			// Explicit output target: write the (re-serialized) ASCII
			// model there. Honored for any mode that reaches here —
			// binary input decompiled to a file, or ASCII input
			// cleaned/repaired to a new file — since asking for an
			// output target is a strong "yes, write the model" signal.
			if err := mdl.WriteFile(model, outputPath); err != nil {
				res.Error = err.Error()
				return res, model, parseErrs, err
			}
		case o.decompileOnly && !o.jsonOut:
			// `decompile` with no output path: stream ASCII to stdout so
			// the tool behaves as a filter in a pipe. This fires for
			// ASCII input too (pass-through re-serialize), not just
			// binary — previously ASCII input produced no stdout output
			// at all, which silently broke `decompile a.mdl | ...`. The
			// caller routes the human summary to stderr so it can never
			// corrupt the model written here.
			if err := mdl.Write(model, os.Stdout); err != nil {
				res.Error = err.Error()
				return res, model, parseErrs, err
			}
		}
		// Any other single-file case (check/repair on a binary or ASCII
		// model with no output target) intentionally writes nothing: the
		// model stays put and only the report is emitted. Streaming the
		// whole decompiled model to the terminal ahead of check results
		// would bury them.
	}

	return res, model, parseErrs, nil
}

// refuseLostTriangles stops a repair or compile from writing a binary whose
// render lists drew triangles that are in no mesh's faces (see
// pkg/mdl/binary_merged.go): ASCII cannot hold them, so the output would
// lose them, as Layonara's tin01 tiles lost their lower walls. force writes
// it anyway; the warning still says what is lost.
func refuseLostTriangles(model *mdl.Model, force bool) error {
	if force {
		return nil
	}
	lost, nodes := 0, []string{}
	for _, m := range model.RenderMerges {
		if m.Lost > 0 {
			lost += m.Lost
			nodes = append(nodes, m.Receiver)
		}
	}
	if lost == 0 {
		return nil
	}
	return fmt.Errorf("not written: %d triangles the binary draws (render lists of %s) are in no mesh's faces, so the output would lose them; pass --force to write it anyway",
		lost, strings.Join(nodes, ", "))
}

// runSingle processes a single file with the given options and output configuration.
//
// Stream routing follows the same rule as runBatch: in plain mode all human
// output (detail + summary) goes to stdout; in --json mode the human summary
// moves to stderr (errTw) so stdout stays machine-clean.
func runSingle(inputPath, outputPath string, o procOpts, tw, errTw *termWriter) int {
	if o.jsonLines {
		emitEvent(Event{Type: "start", File: filepath.Base(inputPath), Index: 1, Total: 1})
	}

	res, model, parseErrs, err := processOne(inputPath, outputPath, o)
	if err != nil && res.Error == "" {
		res.Error = err.Error()
	}

	exit := exitOK
	if res.Error != "" {
		exit = exitErrors
	} else if countCheckErrors(res.Checks) > 0 {
		exit = exitErrors
	}

	fixes := countFixes(res)

	// When `decompile` streams the model to stdout (no output path, not
	// JSON), stdout is a pure data channel — every human-readable line
	// must go to stderr so the piped model stays byte-clean. Same rule as
	// --json mode, which keeps the JSON payload alone on stdout.
	stdoutIsData := o.decompileOnly && !o.jsonOut && outputPath == ""
	humanTw := tw
	if stdoutIsData {
		humanTw = errTw
	}

	if o.jsonLines {
		if res.Error != "" {
			emitEvent(Event{Type: "error", File: filepath.Base(inputPath), Message: res.Error})
		} else {
			emitEvent(Event{Type: "done", File: filepath.Base(inputPath), Fixes: fixes, Result: &res})
		}
	} else if o.jsonOut {
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		if err := enc.Encode(res); err != nil {
			fmt.Fprintf(os.Stderr, "cleanmodels: json encode: %v\n", err)
			return exitErrors
		}
	} else {
		humanTw.printSingleResult(inputPath, res, model, parseErrs, o.verbose, o.quiet)
		if res.Error != "" && o.quiet {
			fmt.Fprintf(os.Stderr, "cleanmodels: %s: %s\n", inputPath, res.Error)
		}
	}

	switch {
	case o.quiet, o.jsonLines:
		// Quiet emits nothing; json-lines already emitted a per-file event.
	case o.jsonOut, stdoutIsData:
		// Stdout carries machine/data output (JSON object or decompiled
		// model); route the human counts to stderr to keep it clean.
		w, e := tally(res, len(parseErrs))
		fmt.Fprintf(errTw.w, "\n%d %s, %d %s, %d %s\n",
			1, "file", w, pluralize(w, "warning", "warnings"), e, pluralize(e, "error", "errors"))
	default:
		// Plain mode: detail went to stdout, summary follows it.
		w, e := tally(res, len(parseErrs))
		fmt.Fprintf(tw.w, "\n%d %s, %d %s, %d %s\n",
			1, "file", w, pluralize(w, "warning", "warnings"), e, pluralize(e, "error", "errors"))
	}

	return exit
}
