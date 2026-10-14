package main

import (
	"errors"
	"fmt"
	"os"
)

// splitInputs turns the positional arguments into the inputs and the optional
// output.
//
//   - takesOutput false (check without --fix): every argument is an input and
//     nothing is written, so `cleanmodels check *.mdl` checks every file.
//   - one argument: one input, no output.
//   - two arguments: <input> <output>, as before. The output may be a file
//     (single input) or a directory.
//   - three or more: all but the last are inputs and the last is an output
//     directory, like cp and mv. A file there is an error: reading it as the
//     output would silently overwrite it.
func splitInputs(pos []string, takesOutput bool) (inputs []string, output string, err error) {
	if len(pos) == 0 {
		return nil, "", errors.New("no input given")
	}
	if !takesOutput || len(pos) == 1 {
		return pos, "", nil
	}
	if len(pos) == 2 {
		return pos[:1], pos[1], nil
	}
	last := pos[len(pos)-1]
	if fi, statErr := os.Stat(last); statErr == nil && !fi.IsDir() {
		return nil, "", &cliError{
			err:        fmt.Errorf("%d inputs need an output directory as the last argument, but %q is a file", len(pos)-1, last),
			suggestion: "pass an output directory last (a file there would be overwritten), or run the files one at a time",
		}
	}
	return pos[:len(pos)-1], last, nil
}

// noOutputNote is printed after a repair that was given no output. The run
// reports what it repaired, which reads as done, but with no output nothing is
// written; saying so is cheaper than a user finding their models unchanged.
const noOutputNote = "\nnote: no output was given, so nothing was written. Name an output file or folder, or give the\n      input twice to repair in place (cleanmodels repair --all a.mdl a.mdl).\n"

// noOutputNoteLegacy is the same note for the flat-flag mode.
const noOutputNoteLegacy = "\nnote: no output was given, so nothing was written. Name an output file or folder, or give the\n      input twice to repair in place (cleanmodels --fix a.mdl a.mdl).\n"

// wantsRepairs reports whether the options ask for anything that changes a model,
// so a run with no output can say it wrote nothing. Check-only runs do not.
func (o procOpts) wantsRepairs() bool {
	r, t := o.repairOpts, o.tileOpts
	return r.fixPivots || r.fixAABB || r.fixTilefade || r.stripDegenerate || r.fixAnimations ||
		r.reparentChildren || r.wrapRoot || r.splitMultiEdge || r.forceWhite || r.mergeByBitmap ||
		r.cullInvisible || r.placeableTransparency || r.tilefadeUndo || r.standardizeTexture0 ||
		r.stripEEExtras || r.scale != 0 || r.scaleX != 0 || r.scaleY != 0 || r.scaleZ != 0 ||
		r.classification != "" || r.snapMode != "" || r.renderOverride != "" || r.shadowOverride != "" ||
		r.tvertSnap != "" || r.remapWalkmeshMaterial != "" ||
		t.water || t.rotateWater != "" || t.retileWater != "" || t.foliage != "" || t.splotch != "" ||
		t.rotateGround != "" || t.chamfer != "" || t.retileGround != "" || t.raiseLower != ""
}

// requireCompileOutput refuses a compile with no output. It used to write the
// binary over the ASCII source (and, given a folder, every model in it), so
// `cleanmodels compile model.mdl` silently destroyed the file it was given.
// Compiling in place is still possible, spelled out: name the input again.
func requireCompileOutput(o procOpts, output string) error {
	if !o.compile || output != "" || o.dryRun {
		return nil
	}
	return &cliError{
		err:        errors.New("compile needs an output, and would otherwise overwrite its input"),
		suggestion: "name an output, or the input twice to compile in place (compile a.mdl a.mdl)",
	}
}

// checkOverwrite refuses to replace an existing file with the result for a
// different input unless --overwrite was given. A model fed in as the second
// argument by mistake (`repair a.mdl b.mdl`, or a shell glob) would otherwise be
// replaced without a word. Writing the result over the input itself, or into an
// output directory, is unaffected.
func checkOverwrite(input, output string, overwrite bool) error {
	if output == "" || overwrite {
		return nil
	}
	out, err := os.Stat(output)
	if err != nil || !out.Mode().IsRegular() {
		return nil
	}
	if in, err := os.Stat(input); err == nil && os.SameFile(in, out) {
		return nil
	}
	return &cliError{
		err:        fmt.Errorf("refusing to overwrite %q with the result for %q", output, input),
		suggestion: "pass --overwrite to replace it, or give an output directory",
	}
}
