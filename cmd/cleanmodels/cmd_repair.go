package main

import (
	"errors"
	"flag"
	"fmt"
	"os"
	"strings"
)

func cmdRepair(args []string) int {
	fs := flag.NewFlagSet("cleanmodels repair", flag.ContinueOnError)
	fs.SetOutput(os.Stderr)

	// Core repairs
	fixAll := fs.Bool("all", false, "enable all repairs")
	fixPivots := fs.Bool("fix-pivots", false, "repair walkmesh pivot points")
	fixAABB := fs.Bool("fix-aabb", false, "rebuild AABB trees from walkmesh geometry")
	fixTilefade := fs.Bool("fix-tilefade", false, "slice tile geometry for tilefade")
	tilefadeZ := fs.Float64("tilefade-z", 5.0, "Z height for tilefade slicing")
	stripDegen := fs.Bool("strip-degenerate", false, "remove zero-area faces")
	fixAnims := fs.Bool("fix-animations", false, "clamp negative/too-short animation lengths")
	reparentChildren := fs.Bool("reparent-children", false, "reparent children of AABB/light nodes")
	wrapRoot := fs.Bool("wrap-root", false, "wrap non-dummy root nodes in a dummy parent")
	splitMultiEdge := fs.Bool("split-multiedge", false, "split faces at multiple edges")
	check := fs.Bool("check", false, "run validation checks after repair")

	// Transforms
	scaleFactor := fs.Float64("scale", 0, "scale all vertex positions")
	scaleX := fs.Float64("scale-x", 0, "scale X axis")
	scaleY := fs.Float64("scale-y", 0, "scale Y axis")
	scaleZ := fs.Float64("scale-z", 0, "scale Z axis")
	classification := fs.String("classification", "", "override classification (CHARACTER, DOOR, EFFECT, ITEM, TILE)")
	snapMode := fs.String("snap", "", "vertex snapping: binary, decimal, fine")
	renderOverride := fs.String("render", "", "force render flag: all, none")
	shadowOverride := fs.String("shadow", "", "force shadow flag: all, none")
	forceWhite := fs.Bool("force-white", false, "set ambient/diffuse to 1,1,1")
	mergeByBitmap := fs.Bool("merge-by-bitmap", false, "merge sibling trimeshes with same bitmap")
	cullInvisible := fs.Bool("cull-invisible", false, "convert invisible meshes to dummy")

	dryRun := fs.Bool("dry-run", false, "report what would be fixed without writing")
	includeStr := fs.String("include-checks", "", "comma-separated check names to run")
	excludeStr := fs.String("exclude-checks", "", "comma-separated check names to skip")

	var cf commonFlags
	cf.register(fs)

	fs.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: cleanmodels repair [flags] <input> [output]\n\n")
		fmt.Fprintf(os.Stderr, "Apply repairs and transformations to MDL models.\n\n")
		fmt.Fprintf(os.Stderr, "Flags:\n")
		fs.PrintDefaults()
	}

	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return exitOK
		}
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
		check:   *check,
		dryRun:  *dryRun,
		include: parseNameSet(*includeStr),
		exclude: parseNameSet(*excludeStr),
		repairOpts: repairOpts{
			scale:          float32(*scaleFactor),
			scaleX:         float32(*scaleX),
			scaleY:         float32(*scaleY),
			scaleZ:         float32(*scaleZ),
			classification: strings.ToUpper(*classification),
			snapMode:       *snapMode,
			renderOverride: *renderOverride,
			shadowOverride: *shadowOverride,
			forceWhite:     *forceWhite,
			mergeByBitmap:  *mergeByBitmap,
			cullInvisible:  *cullInvisible,
		},
		tileOpts: tileOpts{
			tilefadeZ: float32(*tilefadeZ),
		},
	}
	cf.apply(&opts)

	if *fixAll {
		setAllRepairs(&opts)
	} else {
		opts.fixPivots = *fixPivots
		opts.fixAABB = *fixAABB
		opts.fixTilefade = *fixTilefade
		opts.stripDegenerate = *stripDegen
		opts.fixAnimations = *fixAnims
		opts.reparentChildren = *reparentChildren
		opts.wrapRoot = *wrapRoot
		opts.splitMultiEdge = *splitMultiEdge
	}

	return dispatch(inputPath, outputPath, opts)
}
