package mdl

import (
	"bytes"
	"encoding/binary"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"
)

// binaryControllerSets returns, per geometry node name, the sorted controller
// type IDs in a compiled binary. It walks the node tree straight off the file
// layout (12-byte prefix, node header: children at +72, controller keys at +84,
// 12 bytes per key) so it does not depend on the decompiler under test.
func binaryControllerSets(t *testing.T, data []byte) map[string][]uint32 {
	t.Helper()
	u32 := func(o int) uint32 {
		if o < 0 || o+4 > len(data) {
			t.Fatalf("read at %d outside %d-byte file", o, len(data))
		}
		return binary.LittleEndian.Uint32(data[o:])
	}
	out := map[string][]uint32{}
	seen := map[uint32]bool{}
	var walk func(ptr uint32)
	walk = func(ptr uint32) {
		if ptr == 0 || seen[ptr] {
			return
		}
		seen[ptr] = true
		o := 12 + int(ptr)
		// The engine leaves uninitialised bytes after the name's NUL.
		name, _, _ := strings.Cut(string(data[o+32:o+64]), "\x00")
		name = strings.ToLower(name)
		var ids []uint32
		keysPtr, keysCount := int(u32(o+84)), int(u32(o+88))
		for i := 0; i < keysCount; i++ {
			ids = append(ids, u32(12+keysPtr+12*i))
		}
		sort.Slice(ids, func(i, j int) bool { return ids[i] < ids[j] })
		out[name] = ids
		childPtr, childCount := int(u32(o+72)), int(u32(o+76))
		for i := 0; i < childCount; i++ {
			walk(u32(12 + childPtr + 4*i))
		}
	}
	walk(u32(12 + 8 + 64))
	return out
}

// TestOracleControllerSets compiles each oracle ASCII model and requires every
// node to carry exactly the controller IDs the in-game compiler wrote for it.
// The engine writes a controller for each property present in the ASCII,
// whatever its value (see declared.go); this pins that across all oracle models.
func TestOracleControllerSets(t *testing.T) {
	root := filepath.Join("..", "..", "tests", "fixtures", "oracle")
	asciis, _ := filepath.Glob(filepath.Join(root, "ascii", "*.mdl"))
	pairs := [][2]string{}
	for _, a := range asciis {
		pairs = append(pairs, [2]string{a, filepath.Join(root, "game_binary", filepath.Base(a))})
	}
	pairs = append(pairs, [2]string{
		filepath.Join("..", "..", "tests", "fixtures", "bezier", "plc_a01.ascii.mdl"),
		filepath.Join("..", "..", "tests", "fixtures", "bezier", "plc_a01.mdl"),
	})

	compared := 0
	for _, p := range pairs {
		engineBin, err := os.ReadFile(p[1])
		if err != nil {
			continue // compile-only fixture with no game binary
		}
		res, err := ParseFile(p[0])
		if err != nil {
			t.Errorf("%s: %v", p[0], err)
			continue
		}
		var buf bytes.Buffer
		if err := Compile(res.Model, &buf); err != nil {
			t.Errorf("%s: compile: %v", p[0], err)
			continue
		}
		want := binaryControllerSets(t, engineBin)
		got := binaryControllerSets(t, buf.Bytes())
		compared++
		for name, ids := range want {
			if g := got[name]; !equalU32(g, ids) {
				t.Errorf("%s node %s: controller IDs\n got  %v\n want %v", filepath.Base(p[0]), name, g, ids)
			}
		}
	}
	if compared == 0 {
		t.Skip("no oracle pairs found")
	}
}

func equalU32(a, b []uint32) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// TestOracleControllerSetsRoundTrip decompiles each engine binary to ASCII,
// writes and re-parses it, and recompiles it: every node must keep the same
// controller IDs. This is what the writer's "declared" output exists for — a
// controller present at its default value must survive as an ASCII line.
func TestOracleControllerSetsRoundTrip(t *testing.T) {
	bins, _ := filepath.Glob(filepath.Join("..", "..", "tests", "fixtures", "oracle", "game_binary", "*.mdl"))
	if len(bins) == 0 {
		t.Skip("no oracle binaries found")
	}
	for _, path := range bins {
		orig, err := os.ReadFile(path)
		if err != nil {
			t.Fatal(err)
		}
		model, err := Decompile(bytes.NewReader(orig), int64(len(orig)))
		if err != nil {
			t.Errorf("%s: decompile: %v", filepath.Base(path), err)
			continue
		}
		var ascii bytes.Buffer
		if err := Write(model, &ascii); err != nil {
			t.Errorf("%s: write: %v", filepath.Base(path), err)
			continue
		}
		res, err := Parse(&ascii)
		if err != nil {
			t.Errorf("%s: reparse: %v", filepath.Base(path), err)
			continue
		}
		var recompiled bytes.Buffer
		if err := Compile(res.Model, &recompiled); err != nil {
			t.Errorf("%s: recompile: %v", filepath.Base(path), err)
			continue
		}
		want := binaryControllerSets(t, orig)
		got := binaryControllerSets(t, recompiled.Bytes())
		for name, ids := range want {
			if g := got[name]; !equalU32(g, ids) {
				t.Errorf("%s node %s: controller IDs\n got  %v\n want %v", filepath.Base(path), name, g, ids)
			}
		}
	}
}
