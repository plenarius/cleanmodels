// Package main exposes the cleanmodels MDL pipeline to JavaScript via
// WebAssembly. The build target is GOOS=js GOARCH=wasm; this file is
// intentionally excluded from every other target by the build tag below.
//
// JS surface (attached to the global `cleanmodels` object):
//
//	cleanmodels.version()
//	  → string. Build version, "dev" for unreleased binaries.
//
//	cleanmodels.decompile(bytes: Uint8Array)
//	  → { ok: true,  ascii: string }                — success
//	  → { ok: false, error: string }                — failure
//	  Binary MDL → ASCII MDL. Primary use case for in-browser viewers
//	  that already understand ASCII MDL.
//
//	cleanmodels.compile(ascii: string)
//	  → { ok: true,  binary: Uint8Array, warnings: ParseError[] }
//	  → { ok: false, error: string }
//	  ASCII MDL → binary MDL.
//
//	cleanmodels.parse(ascii: string)
//	  → { ok: true,  warnings: ParseError[] }
//	  → { ok: false, error: string }
//	  Validate ASCII without producing output. Cheap linter.
//
// Where ParseError = { line: number, message: string }.
//
// All entrypoints are synchronous and run on the JS main thread.
// Decompile of a typical tile MDL is well under 10 ms in the browser;
// callers that load very large meshes should consider running this
// inside a Web Worker to keep the page responsive.

//go:build js && wasm

package main

import (
	"bytes"
	"fmt"
	"syscall/js"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

// version is overridden at link time by the release build:
//
//	go build -ldflags="-X main.version=v1.2.3" ./cmd/cleanmodels-wasm
var version = "dev"

func main() {
	api := map[string]any{
		"version":   js.FuncOf(jsVersion),
		"decompile": js.FuncOf(jsDecompile),
		"compile":   js.FuncOf(jsCompile),
		"parse":     js.FuncOf(jsParse),
	}
	js.Global().Set("cleanmodels", js.ValueOf(api))

	// syscall/js exposes Go callables only while the runtime is alive,
	// so we block forever. JS unloads the module by dropping the page.
	select {}
}

func jsVersion(this js.Value, args []js.Value) any {
	return version
}

// jsDecompile takes a Uint8Array of binary MDL bytes and returns the
// equivalent ASCII MDL string.
func jsDecompile(this js.Value, args []js.Value) (result any) {
	defer func() {
		if r := recover(); r != nil {
			result = errResult(fmt.Sprintf("decompile: panic: %v", r))
		}
	}()

	if len(args) < 1 {
		return errResult("decompile: expected 1 argument (Uint8Array)")
	}
	src := args[0]
	if src.Type() != js.TypeObject {
		return errResult("decompile: argument must be a Uint8Array")
	}

	n := src.Get("length").Int()
	data := make([]byte, n)
	js.CopyBytesToGo(data, src)

	model, err := mdl.Decompile(bytes.NewReader(data), int64(n))
	if err != nil {
		return errResult(fmt.Sprintf("decompile: %v", err))
	}

	var out bytes.Buffer
	if err := mdl.Write(model, &out); err != nil {
		return errResult(fmt.Sprintf("write ascii: %v", err))
	}

	return map[string]any{
		"ok":    true,
		"ascii": out.String(),
	}
}

// jsCompile takes an ASCII MDL string and returns the binary form as a
// Uint8Array, plus any non-fatal parse warnings.
func jsCompile(this js.Value, args []js.Value) (result any) {
	defer func() {
		if r := recover(); r != nil {
			result = errResult(fmt.Sprintf("compile: panic: %v", r))
		}
	}()

	if len(args) < 1 {
		return errResult("compile: expected 1 argument (string)")
	}
	src := args[0]
	if src.Type() != js.TypeString {
		return errResult("compile: argument must be a string")
	}
	asciiBytes := []byte(src.String())

	parsed, err := mdl.Parse(bytes.NewReader(asciiBytes))
	if err != nil {
		return errResult(fmt.Sprintf("parse ascii: %v", err))
	}
	if parsed == nil || parsed.Model == nil {
		return errResult("parse ascii: produced no model")
	}

	var out bytes.Buffer
	if err := mdl.Compile(parsed.Model, &out); err != nil {
		return errResult(fmt.Sprintf("compile: %v", err))
	}

	dst := js.Global().Get("Uint8Array").New(out.Len())
	js.CopyBytesToJS(dst, out.Bytes())

	return map[string]any{
		"ok":       true,
		"binary":   dst,
		"warnings": parseErrorsToJS(parsed.Errors),
	}
}

// jsParse parses an ASCII MDL string and returns any diagnostics
// without producing binary output. Useful for in-browser linting.
func jsParse(this js.Value, args []js.Value) (result any) {
	defer func() {
		if r := recover(); r != nil {
			result = errResult(fmt.Sprintf("parse: panic: %v", r))
		}
	}()

	if len(args) < 1 {
		return errResult("parse: expected 1 argument (string)")
	}
	src := args[0]
	if src.Type() != js.TypeString {
		return errResult("parse: argument must be a string")
	}

	parsed, err := mdl.Parse(bytes.NewReader([]byte(src.String())))
	if err != nil {
		return errResult(fmt.Sprintf("parse: %v", err))
	}

	warnings := []mdl.ParseError{}
	if parsed != nil {
		warnings = parsed.Errors
	}
	return map[string]any{
		"ok":       true,
		"warnings": parseErrorsToJS(warnings),
	}
}

func parseErrorsToJS(errs []mdl.ParseError) []any {
	out := make([]any, 0, len(errs))
	for _, e := range errs {
		out = append(out, map[string]any{
			"line":    e.Line,
			"message": e.Message,
		})
	}
	return out
}

func errResult(msg string) any {
	return map[string]any{
		"ok":    false,
		"error": msg,
	}
}
