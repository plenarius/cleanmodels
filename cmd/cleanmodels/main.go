// Package main implements the cleanmodels CLI.
// cleanmodels validates, repairs, compiles, and decompiles NWN MDL model files.
package main

import (
	"errors"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"

	"github.com/plenarius/cleanmodels/pkg/buildinfo"
)

var version = "dev"

var subcommands = map[string]func([]string) int{
	"check":     cmdCheck,
	"repair":    cmdRepair,
	"compile":   cmdCompile,
	"decompile": cmdDecompile,
	"report":    cmdReport,
}

func main() {
	code := run(os.Args[1:])
	runProfileCleanups()
	os.Exit(code)
}

func run(args []string) int {
	if len(args) == 0 {
		printHelp()
		return exitUsage
	}

	first := args[0]

	if first == "help" || first == "--help" || first == "-h" {
		if len(args) >= 2 {
			if cmd, ok := subcommands[args[1]]; ok {
				return cmd([]string{"--help"})
			}
		}
		printHelp()
		return exitOK
	}

	if first == "version" || first == "--version" {
		fmt.Println("cleanmodels " + buildinfo.Version(version))
		return exitOK
	}

	if cmd, ok := subcommands[first]; ok {
		return cmd(args[1:])
	}

	// Legacy mode: first arg is not a known subcommand.
	// This preserves backwards compatibility with the Qt GUI and existing scripts.
	return runLegacy(args)
}

func printHelp() {
	fmt.Fprintf(os.Stderr, `cleanmodels -- validate, repair, and compile NWN MDL models

Commands:
  check       Run validation checks on models
  repair      Apply repairs and transformations
  compile     Compile ASCII MDL to binary
  decompile   Decompile binary MDL to ASCII
  report      Submit a bug report with model files

Run 'cleanmodels <command> --help' for command-specific flags.

Common flags (all commands):
  --json          Output results as JSON
  --json-lines    Stream NDJSON events (for GUI integration)
  --quiet         Suppress all output except errors
  --verbose       Show all warnings and info
  --workers N     Parallel workers for batch mode (default: CPU count)
  --recursive     Process directories recursively
  --color MODE    ANSI color: auto, always, never (default: auto)

Legacy mode:
  cleanmodels [flags] <input> [output]
  When the first argument is a file or flag (not a command name),
  legacy flag parsing is used for backwards compatibility.
`)
}

// runLegacy implements the original single-FlagSet interface for backwards compatibility.
func runLegacy(args []string) int {
	fs := flag.NewFlagSet("cleanmodels", flag.ContinueOnError)
	fs.SetOutput(os.Stderr)

	checkFlag := fs.Bool("check", false, "run validation checks")
	decompileOnly := fs.Bool("decompile-only", false, "decompile binary to ASCII without checks or repairs")
	decompileFlag := fs.Bool("decompile", false, "force binary decompilation mode")
	jsonOut := fs.Bool("json", false, "output results as JSON to stdout")
	jsonLines := fs.Bool("json-lines", false, "stream NDJSON events to stdout")
	verbose := fs.Bool("verbose", false, "show all warnings and info")
	quiet := fs.Bool("quiet", false, "suppress all output except errors")
	workers := fs.Int("workers", runtime.NumCPU(), "parallel workers for batch mode")
	colorMode := fs.String("color", "auto", "ANSI color: auto, always, never")
	excludeStr := fs.String("exclude-checks", "", "comma-separated check names to skip")
	includeStr := fs.String("include-checks", "", "comma-separated check names to run")
	recursive := fs.Bool("recursive", false, "process directories recursively")
	fixPivots := fs.Bool("fix-pivots", false, "repair walkmesh pivot points")
	fixAABB := fs.Bool("fix-aabb", false, "rebuild AABB trees")
	fixTilefade := fs.Bool("fix-tilefade", false, "slice tile geometry for tilefade")
	tilefadeZ := fs.Float64("tilefade-z", 5.0, "Z height for tilefade slicing")
	stripDegen := fs.Bool("strip-degenerate", false, "remove zero-area faces")
	fixAnims := fs.Bool("fix-animations", false, "clamp animation lengths")
	reparentChildren := fs.Bool("reparent-children", false, "reparent children of AABB/light nodes")
	wrapRoot := fs.Bool("wrap-root", false, "wrap non-dummy root in dummy parent")
	splitMultiEdge := fs.Bool("split-multiedge", false, "split faces at multiple edges")
	scaleFactor := fs.Float64("scale", 0, "scale all vertex positions")
	scaleX := fs.Float64("scale-x", 0, "scale X axis")
	scaleY := fs.Float64("scale-y", 0, "scale Y axis")
	scaleZ := fs.Float64("scale-z", 0, "scale Z axis")
	classification := fs.String("classification", "", "override classification")
	snapMode := fs.String("snap", "", "vertex snapping: binary, decimal, fine")
	renderOverride := fs.String("render", "", "force render: all, none")
	shadowOverride := fs.String("shadow", "", "force shadow: all, none")
	forceWhite := fs.Bool("force-white", false, "set ambient/diffuse to 1,1,1")
	mergeByBitmap := fs.Bool("merge-by-bitmap", false, "merge sibling trimeshes")
	cullInvisible := fs.Bool("cull-invisible", false, "convert invisible meshes to dummy")
	dryRun := fs.Bool("dry-run", false, "report without writing output")
	fixAll := fs.Bool("fix", false, "enable all repairs")
	compileFlag := fs.Bool("compile", false, "compile ASCII to binary")

	// Pivot sub-options
	pivotAllowSplit := fs.Bool("pivot-allow-split", true, "allow mesh splitting when pivot search fails")
	pivotBelowZ0 := fs.String("pivot-below-z0", "disallow", "Z>=0 constraint: disallow, allow, slice")
	pivotMoveBad := fs.String("pivot-move-bad", "no", "fallback placement: no, top, middle, bottom")
	pivotSmoothing := fs.String("pivot-smoothing", "use", "smoothing group handling: use, protect, ignore")
	pivotMinFaces := fs.Int("pivot-min-faces", 4, "minimum faces per split side")
	pivotSplitFirst := fs.String("pivot-split-first", "concave", "split priority: convex, concave")

	// Tile operations
	water := fs.Bool("water", false, "enable water processing")
	waterKey := fs.String("water-key", "", "water bitmap substring key")
	dynamicWater := fs.String("dynamic-water", "", "water animation mode: yes, no, wavy")
	waveHeight := fs.Float64("wave-height", 0, "wave animation height")
	rotateWater := fs.String("rotate-water", "", "set rotatetexture on water nodes: 0 or 1")
	retileWater := fs.String("retile-water", "", "retile water UVs: 1, 2, 3")
	foliage := fs.String("foliage", "", "foliage mode: tilefade, animate, de-animate, ignore")
	foliageKey := fs.String("foliage-key", "", "foliage bitmap substring key")
	splotch := fs.String("splotch", "", "splotch mode: animate")
	splotchKey := fs.String("splotch-key", "", "splotch bitmap substring key")
	rotateGround := fs.String("rotate-ground", "", "set rotatetexture on ground: 0 or 1")
	groundKey := fs.String("ground-key", "", "ground bitmap substring key")
	chamfer := fs.String("chamfer", "", "chamfer mode: add, delete")
	retileGround := fs.String("retile-ground", "", "retile ground UVs: 1, 2, 3")
	raiseLower := fs.String("raise-lower", "", "raise or lower tile: raise, lower")
	raiseAmount := fs.Float64("raise-amount", 0, "raise/lower amount")
	tilefadeUndo := fs.Bool("tilefade-undo", false, "undo tilefade slicing")

	// Mesh extras
	tvertSnap := fs.String("tvert-snap", "", "UV snap grid: 256, 512, 1024")
	placeableTransparency := fs.Bool("placeable-transparency", false, "enable placeable transparency processing")
	transparencyKey := fs.String("transparency-key", "", "transparency bitmap substring key")
	remapWalkmeshMaterial := fs.String("remap-walkmesh-material", "", "AABB material remap: FROM:TO")

	fs.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: %s [flags] <input_path> [output_path]\n\n", filepath.Base(os.Args[0]))
		fmt.Fprintf(os.Stderr, "Legacy mode. Consider using subcommands: check, repair, compile, decompile\n\n")
		fs.PrintDefaults()
	}

	if err := parseArgs(fs, args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return exitOK
		}
		return exitUsage
	}
	if err := validateColorMode(*colorMode); err != nil {
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

	if *workers < 1 {
		*workers = 1
	}

	if *decompileOnly && (*checkFlag || *fixAll || *fixPivots || *fixAABB || *fixTilefade || *stripDegen || *fixAnims || *reparentChildren || *wrapRoot || *splitMultiEdge || *scaleFactor != 0) {
		fmt.Fprintf(os.Stderr, "cleanmodels: --decompile-only cannot be combined with --check or repair flags\n")
		return exitUsage
	}

	if *dryRun {
		*checkFlag = true
	}

	opts := procOpts{
		check:         *checkFlag,
		decompileOnly: *decompileOnly,
		forceBin:      *decompileFlag,
		jsonOut:       *jsonOut,
		jsonLines:     *jsonLines,
		verbose:       *verbose,
		quiet:         *quiet,
		include:       parseNameSet(*includeStr),
		exclude:       parseNameSet(*excludeStr),
		workers:       *workers,
		recursive:     *recursive,
		dryRun:        *dryRun,
		compile:       *compileFlag,
		colorMode:     *colorMode,
		repairOpts: repairOpts{
			fixPivots:        *fixPivots || *fixAll,
			fixAABB:          *fixAABB || *fixAll,
			fixTilefade:      *fixTilefade || *fixAll,
			stripDegenerate:  *stripDegen || *fixAll,
			fixAnimations:    *fixAnims || *fixAll,
			reparentChildren: *reparentChildren || *fixAll,
			wrapRoot:         *wrapRoot || *fixAll,
			splitMultiEdge:   *splitMultiEdge || *fixAll,
			scale:            float32(*scaleFactor),
			scaleX:           float32(*scaleX),
			scaleY:           float32(*scaleY),
			scaleZ:           float32(*scaleZ),
			classification:   strings.ToUpper(*classification),
			snapMode:         *snapMode,
			renderOverride:   *renderOverride,
			shadowOverride:   *shadowOverride,
			forceWhite:       *forceWhite,
			mergeByBitmap:    *mergeByBitmap,
			cullInvisible:    *cullInvisible,
			pivot: pivotOpts{
				allowSplit: *pivotAllowSplit,
				belowZ0:    *pivotBelowZ0,
				moveBad:    *pivotMoveBad,
				smoothing:  *pivotSmoothing,
				minFaces:   *pivotMinFaces,
				splitFirst: *pivotSplitFirst,
			},
			tvertSnap:             *tvertSnap,
			placeableTransparency: *placeableTransparency,
			transparencyKey:       *transparencyKey,
			remapWalkmeshMaterial: *remapWalkmeshMaterial,
			tilefadeUndo:          *tilefadeUndo,
		},
		tileOpts: tileOpts{
			tilefadeZ:    float32(*tilefadeZ),
			water:        *water,
			waterKey:     *waterKey,
			dynamicWater: *dynamicWater,
			waveHeight:   *waveHeight,
			rotateWater:  *rotateWater,
			retileWater:  *retileWater,
			foliage:      *foliage,
			foliageKey:   *foliageKey,
			splotch:      *splotch,
			splotchKey:   *splotchKey,
			rotateGround: *rotateGround,
			groundKey:    *groundKey,
			chamfer:      *chamfer,
			retileGround: *retileGround,
			raiseLower:   *raiseLower,
			raiseAmount:  *raiseAmount,
		},
	}

	return dispatch(inputPath, outputPath, opts)
}
