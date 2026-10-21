package mdl

import (
	"bytes"
	"strings"
	"testing"
)

// A mesh with a zero-length tangent: compile keeps the tangents (as the
// game does) with a warning; DropZeroLengthTangents removes them, and the
// compile then computes them, none of zero length.
func TestZeroLengthTangents(t *testing.T) {
	src := alignModel(`    renderhint NormalAndSpecMapped
    verts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2 1 0 1 2 0
    tverts 3
      0 0 0
      1 0 0
      0 1 0
    tangents 3
      1 0 0 1
      0 0 0 1
      1 0 0 1`)
	m := mustParseASCII(t, src)
	if z := ZeroLengthTangents(m.FindNode("m").Mesh); z != 1 {
		t.Fatalf("zero-length tangents %d, want 1", z)
	}
	var warns []string
	if err := CompileWithOptions(m, &bytes.Buffer{}, CompileOptions{Warn: func(s string) { warns = append(warns, s) }}); err != nil {
		t.Fatal(err)
	}
	if len(warns) != 1 || !strings.Contains(warns[0], `mesh "m" has 1 of 3 tangents of zero length`) {
		t.Errorf("compile warnings: %q", warns)
	}
	msgs := DropZeroLengthTangents(m)
	if len(msgs) != 1 || m.FindNode("m").Mesh.Tangents != nil {
		t.Fatalf("drop: %q, tangents %v", msgs, m.FindNode("m").Mesh.Tangents)
	}
	var buf bytes.Buffer
	if err := Compile(m, &buf); err != nil {
		t.Fatal(err)
	}
	got := mustDecompile(t, buf.Bytes()).FindNode("m").Mesh
	if len(got.Tangents) == 0 || ZeroLengthTangents(got) != 0 {
		t.Errorf("computed tangents %v", got.Tangents)
	}
}
