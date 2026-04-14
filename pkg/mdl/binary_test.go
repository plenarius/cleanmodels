package mdl

import (
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestDecompileFileHeader(t *testing.T) {
	fixtures := findFixtures(t)
	if len(fixtures) == 0 {
		t.Skip("no test fixtures available")
	}

	for _, path := range fixtures[:min(5, len(fixtures))] {
		t.Run(filepath.Base(path), func(t *testing.T) {
			model, err := DecompileFile(path)
			if err != nil {
				t.Fatalf("DecompileFile(%s): %v", path, err)
			}
			if model.Name == "" {
				t.Error("model name is empty")
			}
			if model.Classification == "" {
				t.Error("classification is empty")
			}
		})
	}
}

func TestDecompileAllFixtures(t *testing.T) {
	fixtures := findFixtures(t)
	if len(fixtures) == 0 {
		t.Skip("no test fixtures available")
	}

	var failures int
	for _, path := range fixtures {
		t.Run(filepath.Base(path), func(t *testing.T) {
			model, err := DecompileFile(path)
			if err != nil {
				failures++
				t.Errorf("DecompileFile error: %v", err)
				return
			}
			if model.Name == "" {
				t.Error("empty model name")
			}
			if len(model.Nodes) == 0 {
				t.Error("no nodes decompiled")
			}
		})
	}
}

func TestDecompileNodeTypes(t *testing.T) {
	fixtures := findFixtures(t)
	if len(fixtures) == 0 {
		t.Skip("no test fixtures available")
	}

	nodeTypeCounts := make(map[string]int)
	for _, path := range fixtures {
		model, err := DecompileFile(path)
		if err != nil {
			continue
		}
		for _, node := range model.Nodes {
			nodeTypeCounts[node.NodeType()]++
		}
	}

	t.Logf("Node type distribution across %d fixtures:", len(fixtures))
	for typ, count := range nodeTypeCounts {
		t.Logf("  %s: %d", typ, count)
	}

	expected := []string{"dummy", "trimesh", "skin", "danglymesh", "emitter", "light", "aabb"}
	for _, typ := range expected {
		if nodeTypeCounts[typ] == 0 {
			t.Errorf("expected at least one %q node across all fixtures", typ)
		}
	}
}

func findFixtures(t *testing.T, subdirs ...string) []string {
	t.Helper()
	fixtureRoot := filepath.Join("..", "..", "tests", "fixtures")

	var roots []string
	if len(subdirs) > 0 {
		for _, s := range subdirs {
			roots = append(roots, filepath.Join(fixtureRoot, s))
		}
	} else {
		roots = []string{fixtureRoot}
	}

	var result []string
	for _, root := range roots {
		filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
			if err != nil {
				return nil
			}
			if d.IsDir() {
				return nil
			}
			if !strings.EqualFold(filepath.Ext(path), ".mdl") {
				return nil
			}
			info, err := os.Stat(path)
			if err == nil && info.Size() > 12 {
				// Skip ASCII files (binary files start with 4 zero bytes)
				f, ferr := os.Open(path)
				if ferr != nil {
					return nil
				}
				hdr := make([]byte, 4)
				f.Read(hdr)
				f.Close()
				if hdr[0] != 0 || hdr[1] != 0 || hdr[2] != 0 || hdr[3] != 0 {
					return nil
				}
				result = append(result, path)
			}
			return nil
		})
	}
	return result
}

