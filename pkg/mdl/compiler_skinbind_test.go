package mdl

import (
	"bytes"
	"encoding/binary"
	"math"
	"os"
	"path/filepath"
	"testing"
)

// skinBindLists returns, per skin node name, the qbone_ref_inv and
// tbone_ref_inv lists of a compiled binary, read straight off the layout
// (node header 112 bytes, mesh header 512, then the skin header).
func skinBindLists(t *testing.T, data []byte) map[string][2][][]float32 {
	t.Helper()
	u32 := func(o int) int { return int(binary.LittleEndian.Uint32(data[o:])) }
	f32 := func(o int) float32 { return math.Float32frombits(binary.LittleEndian.Uint32(data[o:])) }
	out := map[string][2][][]float32{}
	seen := map[int]bool{}
	var walk func(ptr int)
	walk = func(ptr int) {
		if ptr == 0 || seen[ptr] {
			return
		}
		seen[ptr] = true
		o := 12 + ptr
		name, _, _ := bytes.Cut(data[o+32:o+64], []byte{0})
		if u32(o+108)&0x40 != 0 {
			b := o + 112 + 512
			var lists [2][][]float32
			for li, width := range []int{4, 3} {
				ptr, n := u32(b+28+12*li), u32(b+32+12*li)
				for i := 0; i < n; i++ {
					row := make([]float32, width)
					for k := range row {
						row[k] = f32(12 + ptr + 4*(i*width+k))
					}
					lists[li] = append(lists[li], row)
				}
			}
			out[string(bytes.ToLower(name))] = lists
		}
		for i := 0; i < u32(o+76); i++ {
			walk(u32(12 + u32(o+72) + 4*i))
		}
	}
	walk(u32(12 + 72))
	return out
}

// TestSkinBindListsMatchEngine compiles c_marilith2 and requires every skin
// node's inverse-bind rotations and translations to equal the in-game
// compiler's, entry for entry. We used to write these lists empty, and the
// engine reads them while rendering the skin (ExecuteSkinGPU): a model with a
// skin node crashed the client with an access violation as soon as it was
// drawn.
func TestSkinBindListsMatchEngine(t *testing.T) {
	root := filepath.Join("..", "..", "tests", "fixtures", "oracle")
	engine, err := os.ReadFile(filepath.Join(root, "game_binary", "c_marilith2.mdl"))
	if err != nil {
		t.Skip("oracle fixtures not available")
	}
	res, err := ParseFile(filepath.Join(root, "ascii", "c_marilith2.mdl"))
	if err != nil {
		t.Fatal(err)
	}
	var buf bytes.Buffer
	opts := CompileOptions{SupermodelDirs: []string{filepath.Join(root, "game_binary")}}
	if err := CompileWithOptions(res.Model, &buf, opts); err != nil {
		t.Fatal(err)
	}

	want, got := skinBindLists(t, engine), skinBindLists(t, buf.Bytes())
	if len(want) == 0 {
		t.Fatal("fixture has no skin nodes")
	}
	const eps = 2e-3
	for name, w := range want {
		g, ok := got[name]
		if !ok {
			t.Errorf("skin %s missing from our output", name)
			continue
		}
		for li, label := range []string{"qbone_ref_inv", "tbone_ref_inv"} {
			if len(g[li]) != len(w[li]) || len(w[li]) == 0 {
				t.Errorf("skin %s %s: %d entries, engine wrote %d", name, label, len(g[li]), len(w[li]))
				continue
			}
			bad := 0
			for i := range w[li] {
				same, negated := true, true
				for k := range w[li][i] {
					d := float64(w[li][i][k])
					if math.Abs(d-float64(g[li][i][k])) > eps {
						same = false
					}
					if math.Abs(d+float64(g[li][i][k])) > eps {
						negated = false
					}
				}
				// q and -q are the same rotation; translations must match as is.
				if !same && !(li == 0 && negated) {
					bad++
				}
			}
			if bad > 0 {
				t.Errorf("skin %s %s: %d of %d entries differ from the engine's", name, label, bad, len(w[li]))
			}
		}
	}
}
