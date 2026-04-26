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
	Checks   []mdl.CheckResult      `json:"checks,omitempty"`
	Repairs  []string               `json:"repairs,omitempty"`
	Actions  []string               `json:"actions,omitempty"`
	Error    string                 `json:"error,omitempty"`
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
	splitMultiEdge                                   bool
	forceWhite, mergeByBitmap, cullInvisible         bool
	scale                                            float32
	scaleX, scaleY, scaleZ                           float32
	classification                                   string
	snapMode                                         string
	renderOverride, shadowOverride                   string

	pivot pivotOpts

	tvertSnap              string // "256", "512", "1024"
	placeableTransparency  bool
	transparencyKey        string
	remapWalkmeshMaterial  string // "FROM:TO"
	tilefadeUndo           bool
	standardizeTexture0    bool
	stripEEExtras          bool
}

type tileOpts struct {
	tilefadeZ     float32
	water         bool
	waterKey      string
	dynamicWater  string // "yes", "no", "wavy"
	waveHeight    float64
	rotateWater   string // "0" or "1"
	retileWater   string // "1", "2", "3"
	foliage       string // "tilefade", "animate", "de-animate", "ignore"
	foliageKey    string
	splotch       string // "animate"
	splotchKey    string
	rotateGround  string // "0" or "1"
	groundKey     string
	chamfer       string // "add", "delete"
	retileGround  string // "1", "2", "3"
	raiseLower    string // "raise", "lower"
	raiseAmount   float64
}

type procOpts struct {
	check, forceBin, jsonOut, jsonLines, verbose, quiet, recursive bool
	decompileOnly, dryRun                                          bool
	compile                                                        bool
	workers                                                        int
	include, exclude                                               map[string]bool

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
	if err != nil && !errors.Is(err, io.ErrUnexpectedEOF) {
		return false, err
	}
	if n < 4 {
		return false, nil
	}
	return hdr[0] == 0 && hdr[1] == 0 && hdr[2] == 0 && hdr[3] == 0, nil
}

func dispatch(inputPath, outputPath string, opts procOpts) int {
	useColor := !opts.jsonOut && !opts.jsonLines && !opts.quiet
	tw := newTermWriter(os.Stdout, useColor && shouldColorize(os.Stdout.Fd()))
	fi, err := os.Stat(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "cleanmodels: %v\n", wrapWithSuggestion(err, inputPath))
		return exitErrors
	}
	if fi.IsDir() {
		return runBatch(inputPath, outputPath, opts, tw)
	}
	return runSingle(inputPath, outputPath, opts, tw)
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
				res.Repairs = append(res.Repairs, fmt.Sprintf("aabb rebuild %s: %v", n.Name, err))
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

func runChecks(model *mdl.Model, path string, include, exclude map[string]bool, fix bool) []mdl.CheckResult {
	if len(include) == 0 && len(exclude) == 0 {
		return checks.RunAll(model, path, fix)
	}
	return checks.RunFiltered(model, path, include, exclude, fix)
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
		model, err = mdl.DecompileFile(path)
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
		if model != nil {
			model.FileType = "ascii"
		}
	}

	if model != nil {
		res.Warnings = model.Warnings
	}

	if !o.decompileOnly && model != nil {
		applyRepairs(model, o, &res)
	}

	if o.check && !o.decompileOnly && model != nil {
		fix := !o.dryRun
		res.Checks = runChecks(model, path, o.include, o.exclude, fix)
	}

	if model != nil && !o.dryRun {
		switch {
		case o.compile:
			outPath := outputPath
			if outPath == "" {
				outPath = path
			}
			if err := mdl.CompileFile(model, outPath); err != nil {
				res.Error = err.Error()
				return res, model, parseErrs, err
			}
			res.Actions = append(res.Actions, fmt.Sprintf("compiled to binary: %s", outPath))
		case binMode || o.forceBin:
			if outputPath != "" {
				if err := mdl.WriteFile(model, outputPath); err != nil {
					res.Error = err.Error()
					return res, model, parseErrs, err
				}
			} else if !o.jsonOut {
				if err := mdl.Write(model, os.Stdout); err != nil {
					res.Error = err.Error()
					return res, model, parseErrs, err
				}
			}
		default:
			if outputPath != "" {
				if err := mdl.WriteFile(model, outputPath); err != nil {
					res.Error = err.Error()
					return res, model, parseErrs, err
				}
			}
		}
	}

	return res, model, parseErrs, nil
}

// runSingle processes a single file with the given options and output configuration.
func runSingle(inputPath, outputPath string, o procOpts, tw *termWriter) int {
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
		tw.printSingleResult(inputPath, res, model, parseErrs, o.verbose, o.quiet)
		if res.Error != "" && o.quiet {
			fmt.Fprintf(os.Stderr, "cleanmodels: %s: %s\n", inputPath, res.Error)
		}
	}

	if !o.quiet && !o.jsonOut && !o.jsonLines {
		w, e := tally(res, len(parseErrs))
		fmt.Fprintf(os.Stderr, "\n%d %s, %d %s, %d %s\n",
			1, "file", w, pluralize(w, "warning", "warnings"), e, pluralize(e, "error", "errors"))
	}

	return exit
}
