package mdl

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// animPartNumbers reads, straight from a compiled binary, the part number of
// every animation node, as "anim/node=number" in tree order. The decompiler
// does not keep these numbers, so this walks the raw layout: the animation
// array at core offset 120, each animation's root node pointer at 72, and in
// each node its number at 28, its name at 32 and its children at 72.
func animPartNumbers(t *testing.T, data []byte) []string {
	t.Helper()
	core := data[12:]
	u32 := func(off uint32) uint32 {
		if int(off)+4 > len(core) {
			t.Fatalf("offset %d out of range", off)
		}
		return binary.LittleEndian.Uint32(core[off:])
	}
	str := func(off uint32, n int) string {
		b := core[off : int(off)+n]
		if i := bytes.IndexByte(b, 0); i >= 0 {
			b = b[:i]
		}
		return string(b)
	}
	var out []string
	animPtr, animCount := u32(120), u32(124)
	for i := uint32(0); i < animCount; i++ {
		anim := u32(animPtr + 4*i)
		name := str(anim+8, 64)
		var walk func(node uint32, depth int)
		walk = func(node uint32, depth int) {
			if depth > 1000 {
				t.Fatal("animation tree too deep")
			}
			out = append(out, fmt.Sprintf("%s/%s=%d", name, str(node+32, 32), int32(u32(node+28))))
			kids, n := u32(node+72), u32(node+76)
			for k := uint32(0); k < n; k++ {
				walk(u32(kids+4*k), depth+1)
			}
		}
		if root := u32(anim + 72); root != 0 {
			walk(root, 0)
		}
	}
	return out
}

// TestAnimNodeNumbersMatchEngine checks the part number of every animation
// node in every probe of tests/fixtures/engine against the game's own compile
// (nwn-mdl-spec MDL-NODE-040). zspec_animnum isolates the nodes the geometry
// lacks: found by path, -1 under a matched parent, otherwise the declaration
// index in the animation. They used to be written as 0, the root's number,
// which binds their keys to the root.
func TestAnimNodeNumbersMatchEngine(t *testing.T) {
	dir := filepath.Join("..", "..", "tests", "fixtures", "engine")
	probes, err := filepath.Glob(filepath.Join(dir, "*.ascii.mdl"))
	if err != nil || len(probes) == 0 {
		t.Fatalf("no probes in %s: %v", dir, err)
	}
	withAnims := 0
	for _, probe := range probes {
		name := strings.TrimSuffix(filepath.Base(probe), ".ascii.mdl")
		golden, err := os.ReadFile(filepath.Join(dir, name+".mdl"))
		if err != nil {
			t.Fatal(err)
		}
		want := animPartNumbers(t, golden)
		if len(want) == 0 {
			continue
		}
		withAnims++
		t.Run(name, func(t *testing.T) {
			res, err := ParseFile(probe)
			if err != nil {
				t.Fatal(err)
			}
			var ours bytes.Buffer
			if err := CompileWithOptions(res.Model, &ours, CompileOptions{SupermodelDirs: []string{dir}}); err != nil {
				t.Fatal(err)
			}
			got := animPartNumbers(t, ours.Bytes())
			if strings.Join(got, "\n") != strings.Join(want, "\n") {
				t.Errorf("animation node numbers differ\nengine: %v\nours:   %v", want, got)
			}
		})
	}
	if withAnims == 0 {
		t.Fatal("no probe with animations")
	}
}
