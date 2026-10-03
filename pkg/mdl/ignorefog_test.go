package mdl

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestIgnoreFogMatchesEngine pins the model header's fog byte against the
// in-game compiler. The byte means "affected by fog", the negation of ASCII
// ignorefog: the engine compiled the plc_a01 sample with ignorefog 0 -> 01,
// ignorefog 1 -> 00, and no ignorefog line -> 01.
func TestIgnoreFogMatchesEngine(t *testing.T) {
	const fogByteOffset = 127 // 12-byte file prefix + header_model offset 115
	src, err := os.ReadFile(filepath.Join("..", "..", "tests", "fixtures", "bezier", "plc_a01.ascii.mdl"))
	if err != nil {
		t.Fatal(err)
	}
	for _, c := range []struct {
		name, line string
		wantByte   byte
		wantIgnore int32
	}{
		{"absent", "", 1, 0},
		{"ignorefog 0", "ignorefog 0\n", 1, 0},
		{"ignorefog 1", "ignorefog 1\n", 0, 1},
	} {
		text := strings.Replace(string(src), "setanimationscale 1.0", c.line+"setanimationscale 1.0", 1)
		m := mustParseASCII(t, text)
		var buf bytes.Buffer
		if err := Compile(m, &buf); err != nil {
			t.Fatalf("%s: %v", c.name, err)
		}
		if got := buf.Bytes()[fogByteOffset]; got != c.wantByte {
			t.Errorf("%s: fog byte = %02x, want %02x", c.name, got, c.wantByte)
		}
		back, err := Decompile(bytes.NewReader(buf.Bytes()), int64(buf.Len()))
		if err != nil {
			t.Fatalf("%s: %v", c.name, err)
		}
		if back.IgnoreFog != c.wantIgnore {
			t.Errorf("%s: decompiled IgnoreFog = %d, want %d", c.name, back.IgnoreFog, c.wantIgnore)
		}
	}

	engine, err := DecompileFile(filepath.Join("..", "..", "tests", "fixtures", "bezier", "plc_a01.mdl"))
	if err != nil {
		t.Fatal(err)
	}
	if engine.IgnoreFog != 0 {
		t.Errorf("engine-compiled plc_a01 decompiled with IgnoreFog = %d, want 0", engine.IgnoreFog)
	}
}
