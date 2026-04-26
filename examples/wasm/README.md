# cleanmodels-wasm demo

A self-contained drag-and-drop demo of the cleanmodels MDL pipeline
running entirely in the browser via WebAssembly.

- Binary `.mdl` → decompiled ASCII MDL string
- ASCII `.mdl` → parse diagnostics (linter mode)
- No server, no installation; runs offline once cached.

## Build and serve

From the repo root:

```bash
GOOS=js GOARCH=wasm go build -o examples/wasm/cleanmodels.wasm ./cmd/cleanmodels-wasm
cp "$(go env GOROOT)/lib/wasm/wasm_exec.js" examples/wasm/

cd examples/wasm
python3 -m http.server 8000
# open http://localhost:8000/
```

`file://` URLs do not work — browsers block `WebAssembly.instantiateStreaming`
without an HTTP origin. Any static server is fine (`python3 -m http.server`,
`npx serve`, `caddy file-server`, etc.).

## JS surface

The wasm module installs a single `cleanmodels` global. All entrypoints
are synchronous and return either `{ ok: true, ... }` on success or
`{ ok: false, error: string }` on failure.

```js
cleanmodels.version()
  // → string

cleanmodels.decompile(bytes /* Uint8Array */)
  // → { ok: true,  ascii:  string }
  // → { ok: false, error:  string }

cleanmodels.compile(ascii /* string */)
  // → { ok: true,  binary: Uint8Array,
  //                warnings: { line: number, message: string }[] }
  // → { ok: false, error: string }

cleanmodels.parse(ascii /* string */)
  // → { ok: true,  warnings: { line: number, message: string }[] }
  // → { ok: false, error: string }
```

## Embedding in another page

Two static files: `cleanmodels.wasm` and `wasm_exec.js`. Both ship as
release assets on the [cleanmodels releases page][releases]. Pin a
specific version in your build pipeline and check the SHA.

```html
<script src="wasm_exec.js"></script>
<script>
  const go = new Go();
  WebAssembly.instantiateStreaming(fetch("cleanmodels.wasm"), go.importObject)
    .then(r => go.run(r.instance));
</script>
```

After `go.run` returns, the `cleanmodels` global is ready to call.

For viewers that already understand ASCII MDL, the integration is one
branch in the file-drop handler:

```js
function handleDrop(file) {
  file.arrayBuffer().then(buf => {
    const bytes = new Uint8Array(buf);
    const looksBinary = bytes.length >= 4 &&
      bytes[0] === 0 && bytes[1] === 0 && bytes[2] === 0 && bytes[3] === 0;
    const ascii = looksBinary
      ? cleanmodels.decompile(bytes).ascii
      : new TextDecoder().decode(bytes);
    yourExistingAsciiParser(ascii);
  });
}
```

## Performance notes

- Binary MDLs are typically <1 MB; decompile finishes in single-digit
  milliseconds in the browser.
- The wasm runtime initialization happens once; subsequent calls reuse
  the same module instance.
- All entrypoints run on the JS main thread. If you need to keep a
  page interactive while processing very large meshes, run the wasm
  module inside a Web Worker.

## Artifact size

- `cleanmodels.wasm`: ~3.9 MB raw, ~1.0 MB gzipped (Go 1.x toolchain)
- `wasm_exec.js`: ~17 KB

A future build with [TinyGo][tinygo] could shrink this further at the
cost of a stricter stdlib subset.

[releases]: https://github.com/plenarius/cleanmodels/releases
[tinygo]: https://tinygo.org/
