// Command parsefmt is the Go side of python/parity_parse.py.
//
//	parsefmt files   paths on stdin: ParseFile then Write, as `cleanmodels decompile` does
//	parsefmt numbers hex-encoded tokens on stdin: parser.go's parseFloat and parseInt
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
			f, ferr := parseFloat(s)
			i, ierr := parseInt(s)
			fs, is := "ERR", "ERR"
			if ferr == nil {
				fs = strconv.FormatUint(uint64(math.Float32bits(f)), 10)
			}
			if ierr == nil {
				is = strconv.FormatInt(int64(i), 10)
			}
			fmt.Fprintln(out, fs, is)
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

// parseFloat and parseInt are copies of pkg/mdl/parser.go's.

func parseFloat(s string) (float32, error) {
	v, err := strconv.ParseFloat(s, 32)
	if err != nil {
		return 0, err
	}
	if math.IsNaN(v) || math.IsInf(v, 0) {
		return 0, fmt.Errorf("invalid float value: %s", s)
	}
	return float32(v), nil
}

func parseInt(s string) (int32, error) {
	if v, err := strconv.ParseInt(s, 10, 32); err == nil {
		return int32(v), nil
	}
	if v, err := strconv.ParseFloat(s, 64); err == nil {
		return int32(math.Round(v)), nil
	}
	return 0, fmt.Errorf("not a number: %s", s)
}
