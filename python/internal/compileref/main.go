// Command compileref compiles models the way `cleanmodels compile` does and
// reports each result as one JSON line, for python/parity_compile.py.
//
// Paths are read from stdin, one per line. A binary model is decompiled first
// (mdl.DecompileFileWithOptions), an ASCII one parsed (mdl.ParseFile); then
// mdl.CompileWithOptions runs with the model's folder, then -game, as the
// supermodel and resource locations, as the CLI's lookupDirs gives them.
// With -o DIR the compiled bytes are also written to DIR/<line number>.mdl.
package main

import (
	"bufio"
	"bytes"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strconv"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

type result struct {
	Path     string   `json:"path"`
	Error    string   `json:"error,omitempty"`
	Warnings []string `json:"warnings"`
	Size     int      `json:"size"`
	SHA256   string   `json:"sha256,omitempty"`
}

func compileOne(path, game string) (res result, out []byte) {
	res = result{Path: path, Warnings: []string{}}
	defer func() {
		if r := recover(); r != nil {
			res.Error = fmt.Sprintf("panic: %v", r)
			out = nil
		}
	}()
	data, err := os.ReadFile(path)
	if err != nil {
		res.Error = err.Error()
		return res, nil
	}
	dirs := []string{filepath.Dir(path)}
	if game != "" {
		dirs = append(dirs, game)
	}
	var model *mdl.Model
	if len(data) >= 4 && data[0] == 0 && data[1] == 0 && data[2] == 0 && data[3] == 0 {
		model, err = mdl.DecompileFileWithOptions(path, mdl.DecompileOptions{SkeletonDirs: dirs})
	} else {
		var pr *mdl.ParseResult
		pr, err = mdl.ParseFile(path)
		if pr != nil {
			model = pr.Model
		}
		if model != nil {
			model.FileType = "ascii"
		}
	}
	if err != nil || model == nil {
		res.Error = fmt.Sprintf("read: %v", err)
		return res, nil
	}
	var buf bytes.Buffer
	opts := mdl.CompileOptions{
		SupermodelDirs: dirs,
		ResourceDirs:   dirs,
		Warn:           func(msg string) { res.Warnings = append(res.Warnings, msg) },
	}
	if err := mdl.CompileWithOptions(model, &buf, opts); err != nil {
		res.Error = "compile: " + err.Error()
		return res, nil
	}
	sum := sha256.Sum256(buf.Bytes())
	res.Size = buf.Len()
	res.SHA256 = hex.EncodeToString(sum[:])
	return res, buf.Bytes()
}

func main() {
	game := flag.String("game", "", "game install directory")
	outDir := flag.String("o", "", "also write each compiled model to DIR/<n>.mdl")
	flag.Parse()
	in := bufio.NewScanner(os.Stdin)
	in.Buffer(make([]byte, 1<<20), 1<<20)
	w := bufio.NewWriter(os.Stdout)
	defer w.Flush()
	enc := json.NewEncoder(w)
	for i := 0; in.Scan(); i++ {
		res, out := compileOne(in.Text(), *game)
		if *outDir != "" && out != nil {
			if err := os.WriteFile(filepath.Join(*outDir, strconv.Itoa(i)+".mdl"), out, 0o644); err != nil {
				fmt.Fprintln(os.Stderr, err)
				os.Exit(1)
			}
		}
		if err := enc.Encode(res); err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
	}
}
