package mdl

import (
	"bytes"
	"encoding/binary"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// meshLists is one mesh as a binary stores it: its 32-byte face records, raw,
// and its index list (what the game draws, nwn-mdl-spec MDL-BIN-049).
type meshLists struct {
	faces []byte
	index []uint16
}

// binaryMeshLists walks bin's geometry tree and returns each mesh node's face
// records and index list, by node name.
func binaryMeshLists(t *testing.T, bin []byte) map[string]meshLists {
	t.Helper()
	const core = 12
	u32 := func(o int) int {
		if o < 0 || o+4 > len(bin) {
			t.Fatalf("read past the end at %d", o)
		}
		return int(binary.LittleEndian.Uint32(bin[o:]))
	}
	mdx := core + u32(4)
	out := map[string]meshLists{}
	var walk func(ptr, depth int)
	walk = func(ptr, depth int) {
		if depth > 64 {
			t.Fatal("node tree too deep")
		}
		n := core + ptr
		name := string(bytes.TrimRight(bin[n+32:n+64], "\x00"))
		if u32(n+108)&0x20 != 0 {
			m := n + 112
			faceOff, faceCount := u32(m+8), u32(m+12)
			ml := meshLists{faces: bin[core+faceOff : core+faceOff+32*faceCount]}
			if u32(m+404+4) > 0 && u32(m+416+4) > 0 {
				count := u32(core + u32(m+404))
				start := mdx + u32(core+u32(m+416))
				for i := 0; i < count; i++ {
					ml.index = append(ml.index, binary.LittleEndian.Uint16(bin[start+2*i:]))
				}
			}
			out[name] = ml
		}
		kids, nk := u32(n+72), u32(n+76)
		for i := 0; i < nk; i++ {
			walk(u32(core+kids+4*i), depth+1)
		}
	}
	walk(u32(core+72), 0)
	return out
}

// faceRecordVerts is face record k's three vertex indices.
func faceRecordVerts(faces []byte, k int) [3]uint16 {
	r := faces[32*k+26:]
	return [3]uint16{binary.LittleEndian.Uint16(r), binary.LittleEndian.Uint16(r[2:]), binary.LittleEndian.Uint16(r[4:])}
}

// TestFaceRecordsMatchEngine compiles each probe in tests/fixtures/engine and
// compares every mesh's face records and index list with the game's compile
// of it, byte for byte. Tangent generation clones a vertex reached by
// triangles of both handedness and rewrites only the index list: the face
// records keep the original vertex (nwn-mdl-spec MDL-MESH-043, E53; probes
// zspec_tanmirror and zspec_tanmerge).
func TestFaceRecordsMatchEngine(t *testing.T) {
	dir := filepath.Join("..", "..", "tests", "fixtures", "engine")
	probes, err := filepath.Glob(filepath.Join(dir, "*.ascii.mdl"))
	if err != nil || len(probes) == 0 {
		t.Fatalf("no probes in %s: %v", dir, err)
	}
	meshes := 0
	for _, probe := range probes {
		name := strings.TrimSuffix(filepath.Base(probe), ".ascii.mdl")
		t.Run(name, func(t *testing.T) {
			res, err := ParseFile(probe)
			if err != nil {
				t.Fatal(err)
			}
			var ours bytes.Buffer
			if err := CompileWithOptions(res.Model, &ours, CompileOptions{SupermodelDirs: []string{dir}}); err != nil {
				t.Fatal(err)
			}
			golden, err := os.ReadFile(filepath.Join(dir, name+".mdl"))
			if err != nil {
				t.Fatal(err)
			}
			want, got := binaryMeshLists(t, golden), binaryMeshLists(t, ours.Bytes())
			for node, w := range want {
				meshes++
				g, ok := got[node]
				if !ok {
					t.Errorf("mesh %q: missing from our compile", node)
					continue
				}
				if len(g.faces) != len(w.faces) {
					t.Errorf("mesh %q: %d face records, engine %d", node, len(g.faces)/32, len(w.faces)/32)
					continue
				}
				for k := 0; k < len(w.faces)/32; k++ {
					if !bytes.Equal(g.faces[32*k:32*k+32], w.faces[32*k:32*k+32]) {
						t.Errorf("mesh %q face %d: record % x, engine % x (vertices %v, engine %v)", node, k,
							g.faces[32*k:32*k+32], w.faces[32*k:32*k+32], faceRecordVerts(g.faces, k), faceRecordVerts(w.faces, k))
					}
				}
				if !equalU16(g.index, w.index) {
					t.Errorf("mesh %q: index list %v, engine %v", node, g.index, w.index)
				}
			}
		})
	}
	if meshes == 0 {
		t.Fatal("no meshes compared")
	}
}

func equalU16(a, b []uint16) bool {
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
