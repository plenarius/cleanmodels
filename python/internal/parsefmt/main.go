// Command parsefmt is the Go side of python/parity_parse.py.
//
//	parsefmt files   paths on stdin: ParseFile then Write, as `cleanmodels decompile` does
//	parsefmt numbers hex-encoded tokens on stdin: mdl.ParseNumber and ParseInteger (the parser's)
//	parsefmt fold    pairs of hex-encoded strings on stdin: strings.EqualFold
//	parsefmt strings hex-encoded lines on stdin: the strings/strconv calls the parser uses
//	parsefmt runes   unicode.ToLower/ToUpper of every rune they change
//	parsefmt print   every rune strconv.IsPrint accepts
package main

import (
	"bufio"
	"bytes"
	"encoding/hex"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
	"unicode"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

func main() {
	in := bufio.NewScanner(os.Stdin)
	in.Buffer(make([]byte, 0, 1<<20), 1<<26)
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	switch os.Args[1] {
	case "files":
		for in.Scan() {
			files(out, in.Text())
		}
	case "numbers":
		for in.Scan() {
			s := unhex(in.Text())
			f, ferr := mdl.ParseNumber(s)
			i, ierr := mdl.ParseInteger(s)
			fs, is := "ERR", "ERR"
			if ferr == nil {
				fs = strconv.FormatUint(uint64(math.Float32bits(f)), 10)
			}
			if ierr == nil {
				is = strconv.FormatInt(int64(i), 10)
			}
			fmt.Fprintln(out, fs, is)
		}
	case "fold":
		for in.Scan() {
			f := strings.SplitN(in.Text(), " ", 2)
			fmt.Fprintln(out, strings.EqualFold(unhex(f[0]), unhex(f[1])))
		}
	case "strings":
		for in.Scan() {
			s := unhex(in.Text())
			fields := strings.Fields(s)
			fmt.Fprintln(out, hex.EncodeToString([]byte(strings.TrimSpace(s))),
				hex.EncodeToString([]byte(strings.Join(fields, "\x00"))), len(fields),
				hex.EncodeToString([]byte(strings.ToLower(s))),
				hex.EncodeToString([]byte(strings.ToUpper(s))),
				hex.EncodeToString([]byte(strconv.Quote(s))),
				strings.EqualFold(s, "endlist"), strings.EqualFold(s, "animlength"),
				hex.EncodeToString([]byte(strings.TrimRight(s, "\r"))))
		}
	case "runes":
		for r := rune(0x80); r <= unicode.MaxRune; r++ {
			if l, u := unicode.ToLower(r), unicode.ToUpper(r); l != r || u != r {
				fmt.Fprintln(out, r, l, u)
			}
		}
	case "print":
		for r := rune(0); r <= unicode.MaxRune; r++ {
			if strconv.IsPrint(r) {
				fmt.Fprintln(out, r)
			}
		}
	}
}

func unhex(s string) string {
	b, err := hex.DecodeString(s)
	if err != nil {
		panic(err)
	}
	return string(b)
}

// files writes "<status> <nwarn>\n", then each warning and the output as
// "<len>\n<bytes>". Status is "ok" or the error.
func files(out *bufio.Writer, path string) {
	pr, err := mdl.ParseFile(path)
	var text bytes.Buffer
	status := "ok"
	if err != nil {
		status = "refused: " + err.Error()
	} else {
		pr.Model.FileType = "ascii"
		if werr := mdl.Write(pr.Model, &text); werr != nil {
			status = "write: " + werr.Error()
		}
	}
	var warns []mdl.ParseError
	if pr != nil {
		warns = pr.Errors
	}
	fmt.Fprintf(out, "%d %d\n%s", len(status), len(warns), status)
	for _, w := range warns {
		s := w.Error()
		fmt.Fprintf(out, "%d\n%s", len(s), s)
	}
	fmt.Fprintf(out, "%d\n", text.Len())
	out.Write(text.Bytes())
	out.Flush()
}
