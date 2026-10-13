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
