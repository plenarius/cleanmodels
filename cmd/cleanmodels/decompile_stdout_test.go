package main

import (
	"bytes"
	"io"
	"os"
	"strings"
	"testing"
)

// captureStdio swaps os.Stdout and os.Stderr for pipes, runs fn, and returns
// whatever was written to each. Used to assert the CLI's stream-routing
// contract end-to-end, since processOne writes the decompiled model straight
// to os.Stdout.
func captureStdio(t *testing.T, fn func()) (stdout, stderr string) {
	t.Helper()
	origOut, origErr := os.Stdout, os.Stderr
	outR, outW, err := os.Pipe()
	if err != nil {
		t.Fatalf("pipe: %v", err)
	}
	errR, errW, err := os.Pipe()
	if err != nil {
		t.Fatalf("pipe: %v", err)
	}
	os.Stdout, os.Stderr = outW, errW

	outCh, errCh := make(chan string, 1), make(chan string, 1)
	go func() { var b bytes.Buffer; io.Copy(&b, outR); outCh <- b.String() }()
	go func() { var b bytes.Buffer; io.Copy(&b, errR); errCh <- b.String() }()

	func() {
		defer func() {
			outW.Close()
			errW.Close()
			os.Stdout, os.Stderr = origOut, origErr
		}()
		fn()
	}()

	return <-outCh, <-errCh
}

// TestDecompileASCIIPassThrough is a regression test for the report that
// `decompile <ascii.mdl>` emitted nothing on stdout (only the "1 file"
// summary), breaking pipelines. Both ASCII and binary inputs must stream the
// re-serialized model to stdout, and the human summary must land on stderr so
// the piped model stays byte-clean.
func TestDecompileASCIIPassThrough(t *testing.T) {
	const asciiFixture = "../../tests/fixtures/oracle/ascii/plc_dummy_only.mdl"

	opts := procOpts{decompileOnly: true, colorMode: "never"}
	stdout, stderr := captureStdio(t, func() {
		if code := dispatch(asciiFixture, "", opts); code != exitOK {
			t.Errorf("dispatch returned exit %d, want %d", code, exitOK)
		}
	})

	// The model itself must be on stdout.
	for _, want := range []string{"newmodel plc_dummy_only", "donemodel plc_dummy_only"} {
		if !strings.Contains(stdout, want) {
			t.Errorf("stdout missing %q; ASCII input was not passed through.\nstdout:\n%s", want, stdout)
		}
	}
	// The summary must NOT be on stdout — it would corrupt the piped model.
	if strings.Contains(stdout, "1 file") {
		t.Errorf("summary leaked onto stdout, corrupting the model:\n%s", stdout)
	}
	// ...and it must be on stderr.
	if !strings.Contains(stderr, "1 file") {
		t.Errorf("summary missing from stderr; got:\n%s", stderr)
	}
}
