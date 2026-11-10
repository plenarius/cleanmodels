package mdl

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// tanMirrorThenQuad is the nwn-mdl-spec probe zspec_tanmirror (a UV mirror
// seam: the seam vertices get tangent clones, MDL-MESH-043) with one more
// quad after it. The clones go to the end of the vertex list, after the last
// quad's vertices, but are used before them: an ASCII that writes the
// tangents numbers each clone where it is first used instead.
const tanMirrorThenQuad = `newmodel tanmirror2
setsupermodel tanmirror2 NULL
classification character
beginmodelgeom tanmirror2
node dummy tanmirror2
  parent NULL
endnode
node trimesh strip
  parent tanmirror2
  bitmap tanmirror2_t0
  %HINT%
  verts 10
    -1 -1 0
     0 -1 0
     1 -1 0
    -1  1 0
     0  1 0
     1  1 0
     3 -1 0
     4 -1 0
     3  1 0
     4  1 0
  tverts 10
    0 0 0
    1 0 0
    0 0 0
    0 1 0
    1 1 0
    0 1 0
    0 0 0
    1 0 0
    0 1 0
    1 1 0
  faces 6
    0 1 4 1 0 1 4 0
    0 4 3 1 0 4 3 0
    1 2 5 1 1 2 5 0
    1 5 4 1 1 5 4 0
    6 7 9 1 6 7 9 0
    6 9 8 1 6 9 8 0
endnode
endmodelgeom tanmirror2
donemodel tanmirror2
`

// TestGeneratedTangentsRoundTrip: a compile that generates tangents with
// clones, decompiled and compiled again, gives the same bytes. Before
// omitGeneratedTangents the decompile wrote the tangents, and the second
// compile moved the clones to their first use.
func TestGeneratedTangentsRoundTrip(t *testing.T) {
	for _, tc := range []struct {
		name string
		hint string // the mesh's own renderhint line
		mtr  bool   // a NormalAndSpecMapped material next to it instead
	}{
		{"render hint", "renderhint NormalAndSpecMapped", false},
		{"material", "", true},
	} {
		t.Run(tc.name, func(t *testing.T) {
			var dirs []string
			if tc.mtr {
				dir := t.TempDir()
				if err := os.WriteFile(filepath.Join(dir, "tanmirror2_t0.mtr"), []byte("renderhint NormalAndSpecMapped\n"), 0o644); err != nil {
					t.Fatal(err)
				}
				dirs = []string{dir}
			}
			compile := func(m *Model) []byte {
				t.Helper()
				var buf bytes.Buffer
				if err := CompileWithOptions(m, &buf, CompileOptions{ResourceDirs: dirs}); err != nil {
					t.Fatal(err)
				}
				return buf.Bytes()
			}
			decompile := func(data []byte, opts DecompileOptions) *Model {
				t.Helper()
				m, err := DecompileWithOptions(bytes.NewReader(data), int64(len(data)), opts)
				if err != nil {
					t.Fatal(err)
				}
				return m
			}

			b1 := compile(mustParseASCII(t, strings.Replace(tanMirrorThenQuad, "%HINT%", tc.hint, 1)))
			raw := decompile(b1, DecompileOptions{KeepVertexSplits: true}).FindNode("strip").Mesh
			if len(raw.Verts) != 12 || len(raw.Tangents) != 12 {
				t.Fatalf("first compile: %d vertices, %d tangents; want 12 and 12 (10 and two clones)", len(raw.Verts), len(raw.Tangents))
			}

			a2 := decompile(b1, DecompileOptions{MaterialDirs: dirs})
			m := a2.FindNode("strip").Mesh
			if len(m.Tangents) != 0 {
				t.Errorf("decompile wrote %d tangents; want none, the compile generates them", len(m.Tangents))
			}
			if len(a2.Notes) != 1 || !strings.Contains(a2.Notes[0].Message, "2 cloned vertices") {
				t.Errorf("notes %v; want one about the 2 clones", a2.Notes)
			}
			if b2 := compile(a2); !bytes.Equal(b1, b2) {
				t.Errorf("decompile then compile changed the first compile's bytes")
			}

			// Its ASCII, written and read back, still gives the same bytes.
			var text bytes.Buffer
			if err := Write(a2, &text); err != nil {
				t.Fatal(err)
			}
			if b3 := compile(mustParseASCII(t, text.String())); !bytes.Equal(b1, b3) {
				t.Errorf("the decompiled ASCII compiles to other bytes")
			}

			if tc.mtr {
				// Without its material the mesh is not known to be
				// normal-mapped: the tangents stay.
				if m := decompile(b1, DecompileOptions{}).FindNode("strip").Mesh; len(m.Tangents) == 0 {
					t.Errorf("decompiled without the material: tangents left out")
				}
			}
		})
	}
}

// The game's own compile of zspec_tanmirror: the decompiled mesh has no
// tangents, and compiles back to the game's vertices, tangents and index
// list.
func TestGeneratedTangentsGameGolden(t *testing.T) {
	golden, err := os.ReadFile(filepath.Join("..", "..", "tests", "fixtures", "engine", "zspec_tanmirror.mdl"))
	if err != nil {
		t.Fatal(err)
	}
	m := mustDecompile(t, golden)
	if n := len(m.FindNode("strip").Mesh.Tangents); n != 0 {
		t.Fatalf("decompile wrote %d tangents; want none", n)
	}
	b1 := mustCompile(t, m)
	keep := DecompileOptions{KeepVertexSplits: true}
	want, err := DecompileWithOptions(bytes.NewReader(golden), int64(len(golden)), keep)
	if err != nil {
		t.Fatal(err)
	}
	got, err := DecompileWithOptions(bytes.NewReader(b1), int64(len(b1)), keep)
	if err != nil {
		t.Fatal(err)
	}
	w, g := want.FindNode("strip").Mesh, got.FindNode("strip").Mesh
	if len(w.Verts) != len(g.Verts) || len(w.Tangents) != len(g.Tangents) || len(w.Faces) != len(g.Faces) {
		t.Fatalf("compiled: %d vertices, %d tangents, %d faces; the game has %d, %d, %d",
			len(g.Verts), len(g.Tangents), len(g.Faces), len(w.Verts), len(w.Tangents), len(w.Faces))
	}
	for i := range w.Verts {
		if w.Verts[i] != g.Verts[i] || w.TVerts[i] != g.TVerts[i] || w.Normals[i] != g.Normals[i] || w.Tangents[i] != g.Tangents[i] {
			t.Errorf("vertex %d: compiled %v %v %v %v, the game %v %v %v %v", i,
				g.Verts[i], g.TVerts[i], g.Normals[i], g.Tangents[i], w.Verts[i], w.TVerts[i], w.Normals[i], w.Tangents[i])
		}
	}
	for i := range w.Faces {
		if w.Faces[i].Verts != g.Faces[i].Verts {
			t.Errorf("face %d draws %v, the game %v", i, g.Faces[i].Verts, w.Faces[i].Verts)
		}
	}
}
