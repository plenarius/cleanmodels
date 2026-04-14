package mdl

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestRoundtripFixtures decompiles binary EE fixtures, writes to ASCII,
// parses the ASCII back, and verifies structural equality.
func TestRoundtripFixtures(t *testing.T) {
	fixtures := findFixtures(t)
	if len(fixtures) == 0 {
		t.Skip("no test fixtures available")
	}

	for _, path := range fixtures {
		t.Run(filepath.Base(path), func(t *testing.T) {
			// Decompile binary
			model, err := DecompileFile(path)
			if err != nil {
				t.Fatalf("decompile: %v", err)
			}

			// Write to ASCII
			var buf bytes.Buffer
			if err := Write(model, &buf); err != nil {
				t.Fatalf("write: %v", err)
			}

			// Parse ASCII back
			result, err := Parse(strings.NewReader(buf.String()))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}

			reparsed := result.Model

			// Compare structural equality
			if model.Name != reparsed.Name {
				t.Errorf("name: got %q, want %q", reparsed.Name, model.Name)
			}
			if model.Classification != reparsed.Classification {
				t.Errorf("classification: got %q, want %q", reparsed.Classification, model.Classification)
			}
			if model.SuperModel != reparsed.SuperModel {
				t.Errorf("supermodel: got %q, want %q", reparsed.SuperModel, model.SuperModel)
			}

			if len(reparsed.Nodes) != len(model.Nodes) {
				t.Errorf("node count: got %d, want %d", len(reparsed.Nodes), len(model.Nodes))
				return
			}

			for i, origNode := range model.Nodes {
				rNode := reparsed.Nodes[i]
				if origNode.Name != rNode.Name {
					t.Errorf("node[%d] name: got %q, want %q", i, rNode.Name, origNode.Name)
				}
				if origNode.NodeType() != rNode.NodeType() {
					t.Errorf("node[%d] type: got %q, want %q", i, rNode.NodeType(), origNode.NodeType())
				}

				// Verify mesh data roundtrip
				if origNode.Mesh != nil && rNode.Mesh != nil {
					if len(origNode.Mesh.Verts) != len(rNode.Mesh.Verts) {
						t.Errorf("node[%d] %s verts: got %d, want %d", i, origNode.Name, len(rNode.Mesh.Verts), len(origNode.Mesh.Verts))
					}
					if len(origNode.Mesh.Faces) != len(rNode.Mesh.Faces) {
						t.Errorf("node[%d] %s faces: got %d, want %d", i, origNode.Name, len(rNode.Mesh.Faces), len(origNode.Mesh.Faces))
					}
				}
			}

			if len(reparsed.Animations) != len(model.Animations) {
				t.Errorf("animation count: got %d, want %d", len(reparsed.Animations), len(model.Animations))
			}
		})
	}
}

func TestASCIIRoundtripFixtures(t *testing.T) {
	fixtureDir := filepath.Join("..", "..", "tests", "fixtures", "ascii")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Skip("no ASCII fixtures:", err)
	}
	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(strings.ToLower(e.Name()), ".mdl") {
			continue
		}
		path := filepath.Join(fixtureDir, e.Name())
		t.Run(e.Name(), func(t *testing.T) {
			f, err := os.Open(path)
			if err != nil {
				t.Fatal(err)
			}
			defer f.Close()

			result, err := Parse(f)
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			model := result.Model
			if model.Name == "" {
				t.Fatal("empty model name")
			}

			var buf bytes.Buffer
			if err := Write(model, &buf); err != nil {
				t.Fatalf("write: %v", err)
			}

			result2, err := Parse(strings.NewReader(buf.String()))
			if err != nil {
				t.Fatalf("reparse: %v", err)
			}
			m2 := result2.Model

			if m2.Name != model.Name {
				t.Errorf("name: got %q, want %q", m2.Name, model.Name)
			}
			if len(m2.Nodes) != len(model.Nodes) {
				t.Errorf("node count: got %d, want %d", len(m2.Nodes), len(model.Nodes))
			}
			if len(m2.Animations) != len(model.Animations) {
				t.Errorf("anim count: got %d, want %d", len(m2.Animations), len(model.Animations))
			}
			for i, n := range model.Nodes {
				if i >= len(m2.Nodes) {
					break
				}
				if n.NodeType() != m2.Nodes[i].NodeType() {
					t.Errorf("node[%d] type: got %q, want %q", i, m2.Nodes[i].NodeType(), n.NodeType())
				}
			}
		})
	}
}

func TestWriteAndParse(t *testing.T) {
	model := &Model{
		Name:           "test",
		SuperModel:     "NULL",
		Classification: "CHARACTER",
		AnimationScale: 1.0,
		Nodes: []*Node{
			{
				Name:   "test",
				Parent: "NULL",
				Scale:  1.0,
				Orientation: Vec4{W: 1.0},
			},
			{
				Name:   "mesh01",
				Parent: "test",
				Scale:  1.0,
				Orientation: Vec4{W: 1.0},
				Mesh: &MeshData{
					Render: 1,
					Alpha:  1.0,
					Diffuse: Vec3{X: 0.8, Y: 0.8, Z: 0.8},
					Ambient: Vec3{X: 0.2, Y: 0.2, Z: 0.2},
					Bitmap:  "test_texture",
					Verts: []Vec3{
						{X: 0, Y: 0, Z: 0},
						{X: 1, Y: 0, Z: 0},
						{X: 0, Y: 1, Z: 0},
					},
					Faces: []Face{
						{Verts: [3]int32{0, 1, 2}, SmoothGroup: 1, UVs: [3]int32{0, 1, 2}},
					},
				},
			},
		},
	}

	var buf bytes.Buffer
	if err := Write(model, &buf); err != nil {
		t.Fatal(err)
	}

	result, err := Parse(strings.NewReader(buf.String()))
	if err != nil {
		t.Fatal(err)
	}

	reparsed := result.Model
	if reparsed.Name != "test" {
		t.Errorf("name = %q, want test", reparsed.Name)
	}
	if len(reparsed.Nodes) != 2 {
		t.Fatalf("nodes = %d, want 2", len(reparsed.Nodes))
	}
	if reparsed.Nodes[1].Mesh == nil {
		t.Fatal("reparsed mesh is nil")
	}
	if len(reparsed.Nodes[1].Mesh.Verts) != 3 {
		t.Errorf("verts = %d, want 3", len(reparsed.Nodes[1].Mesh.Verts))
	}
}
