package mdl

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestOracleAabbTreeMatchesGame compiles each oracle ASCII source that has a
// walkmesh and checks that its whole AABB tree, planes included, is the one
// the game compiled from the same source. The game keeps the authored tree and
// only derives each internal node's plane; see aabbSplitPlane.
func TestOracleAabbTreeMatchesGame(t *testing.T) {
	pairs := [][2]string{{oracleAsciiDir, oracleBinaryDir}, {tangentOracleAsciiDir, tangentOracleBinaryDir}}
	checked := 0
	for _, dirs := range pairs {
		bins, err := os.ReadDir(dirs[1])
		if err != nil {
			continue
		}
		for _, b := range bins {
			if b.IsDir() {
				continue
			}
			asciiPath := findAsciiIn(dirs[0], b.Name())
			if asciiPath == "" {
				continue
			}
			pr, err := ParseFile(asciiPath)
			if err != nil {
				continue
			}
			game, err := DecompileFile(filepath.Join(dirs[1], b.Name()))
			if err != nil {
				continue
			}
			var buf bytes.Buffer
			if err := Compile(pr.Model, &buf); err != nil {
				t.Fatalf("%s: compile: %v", b.Name(), err)
			}
			ours, err := Decompile(bytes.NewReader(buf.Bytes()), int64(buf.Len()))
			if err != nil {
				t.Fatalf("%s: decompile our binary: %v", b.Name(), err)
			}
			for _, gn := range game.Nodes {
				if gn == nil || gn.Aabb == nil {
					continue
				}
				for _, on := range ours.Nodes {
					if on == nil || on.Aabb == nil || !strings.EqualFold(on.Name, gn.Name) {
						continue
					}
					checked++
					want, got := gn.Aabb.Entries, on.Aabb.Entries
					if len(want) != len(got) {
						t.Errorf("%s %s: %d entries, game has %d", b.Name(), gn.Name, len(got), len(want))
						continue
					}
					for i := range want {
						if want[i] != got[i] {
							t.Errorf("%s %s entry %d: got %+v, game has %+v", b.Name(), gn.Name, i, got[i], want[i])
							break
						}
					}
				}
			}
		}
	}
	if checked == 0 {
		t.Skip("no walkmesh oracle fixtures")
	}
	t.Logf("compared %d walkmesh trees with the game's", checked)
}

func findAsciiIn(dir, binaryName string) string {
	base := strings.ToLower(strings.TrimSuffix(binaryName, filepath.Ext(binaryName)))
	ents, err := os.ReadDir(dir)
	if err != nil {
		return ""
	}
	for _, e := range ents {
		if strings.ToLower(strings.TrimSuffix(e.Name(), filepath.Ext(e.Name()))) == base {
			return filepath.Join(dir, e.Name())
		}
	}
	return ""
}
