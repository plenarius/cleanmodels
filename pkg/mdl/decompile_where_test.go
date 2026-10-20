package mdl

import (
	"bytes"
	"encoding/binary"
	"math"
	"strings"
	"testing"
)

// A damaged list entry names the node and the list, not only the line.
func TestListWarningNamesNodeAndList(t *testing.T) {
	_, errs := parseAlign(t, `    verts 3
      0 0 0
      1 0 0
      0 1 0
    tverts 2
      0.1 0.1 0
      0.0809014 0/934729 0`)
	if !hasError(errs, `node "m", tverts: "0/934729" is read as 0`) {
		t.Errorf("warning does not name the node and list: %+v", errs)
	}
}

// NaN floats in a binary are reported once per list of a node, with the
// node, the list and the count, not once per float with neither.
func TestNaNWarningNamesNodeAndList(t *testing.T) {
	m := mustParseASCII(t, alignModel(`    verts 3
      1234.5 0 0
      2345.5 0 0
      0 1 0
    faces 1
      0 1 2 1 0 1 2 0
    tverts 3
      0 0 0
      1 0 0
      0 1 0`))
	data := mustCompile(t, m)
	n := 0
	for _, v := range []float32{1234.5, 2345.5} {
		var pat [4]byte
		binary.LittleEndian.PutUint32(pat[:], math.Float32bits(v))
		for i := bytes.Index(data, pat[:]); i >= 0; i = bytes.Index(data, pat[:]) {
			binary.LittleEndian.PutUint32(data[i:], 0x7FC00000)
			n++
		}
	}
	if n == 0 {
		t.Fatal("vertex floats not found in the compiled model")
	}
	dm := mustDecompile(t, data)
	var nan []DecompileWarning
	for _, w := range dm.Warnings {
		if strings.Contains(w.Message, "NaN/Inf") {
			nan = append(nan, w)
		}
	}
	if len(nan) != 1 {
		t.Fatalf("want one NaN warning, got %+v", dm.Warnings)
	}
	if nan[0].Node != "m" || !strings.HasPrefix(nan[0].Message, "2 NaN/Inf floats in verts replaced with 0") {
		t.Errorf("NaN warning = %+v, want node m and 2 floats in verts", nan[0])
	}
}

// Light controller 144 (older EE compilers, nwn-mdl-spec E51) is a note, not
// an unknown-controller warning, and says where its key is.
func TestLightController144IsANote(t *testing.T) {
	m := mustParseASCII(t, `newmodel lt
setsupermodel lt NULL
classification TILE
setanimationscale 1.00
beginmodelgeom lt
  node dummy lt
    parent NULL
  endnode
  node light lamp
    parent lt
    multiplier 1.0
  endnode
endmodelgeom
donemodel lt
`)
	data := mustCompile(t, m)
	// The multiplier key: type 140, one row, one column.
	key := []byte{140, 0, 0, 0, 1, 0}
	at := bytes.Index(data, key)
	if at < 0 || bytes.Index(data[at+1:], key) >= 0 {
		t.Fatal("multiplier key not found exactly once")
	}
	data[at] = 144
	dm := mustDecompile(t, data)
	for _, w := range dm.Warnings {
		if strings.Contains(w.Message, "144") {
			t.Errorf("144 still warned: %+v", w)
		}
	}
	if len(dm.Notes) != 1 || dm.Notes[0].Node != "lamp" || dm.Notes[0].Offset != int64(at) ||
		!strings.HasPrefix(dm.Notes[0].Message, "light controller 144 dropped") {
		t.Errorf("notes = %+v, want one for lamp at offset %d", dm.Notes, at)
	}
}

// A lightmapped line is not read by the game (nwn-mdl-spec MDL-BIN-051), so
// it changes nothing in the compiled model, and nothing is decompiled back.
func TestLightmappedIgnored(t *testing.T) {
	body := `    verts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2 1 0 1 2 0
    tverts 3
      0 0 0
      1 0 0
      0 1 0`
	plain := mustCompile(t, mustParseASCII(t, alignModel(body)))
	with := mustCompile(t, mustParseASCII(t, alignModel("    lightmapped 253\n"+body)))
	if !bytes.Equal(plain, with) {
		t.Error("lightmapped 253 changed the compiled model")
	}
	var buf bytes.Buffer
	if err := Write(mustDecompile(t, with), &buf); err != nil {
		t.Fatal(err)
	}
	if strings.Contains(buf.String(), "lightmapped") {
		t.Error("decompiled ASCII has a lightmapped line")
	}
}
