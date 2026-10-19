package mdl

import (
	"strings"
	"testing"
)

// renderHintValue is SharedMaterial::ExtractRenderHint: case-insensitive,
// None 1, NormalAndSpecMapped 2, NormalTangents 3, anything else 0.
func TestRenderHintValue(t *testing.T) {
	for hint, want := range map[string]uint32{
		"NormalAndSpecMapped": 2, "normalandspecmapped": 2,
		"NormalTangents": 3, "NORMALTANGENTS": 3,
		"None": 1, "none": 1,
		"": 0, "Foo": 0,
	} {
		if got := renderHintValue(hint); got != want {
			t.Errorf("renderHintValue(%q) = %d, want %d", hint, got, want)
		}
	}
}

// hintTangentModel is a quad with authored tangents; the render hint line and
// the tangent rows are filled in by the test.
func hintTangentModel(hintLine, tangentRow string) string {
	return `newmodel hinted
setsupermodel hinted NULL
classification PLACEABLE
setanimationscale 1.00
beginmodelgeom hinted
  node dummy hinted
    parent NULL
  endnode
  node trimesh quad
    parent hinted
    bitmap brick
    render 1
` + hintLine + `
    verts 4
      -1 -1 0
       1 -1 0
       1  1 0
      -1  1 0
    tverts 4
      0 0 0
      1 0 0
      1 1 0
      0 1 0
    normals 4
      0 0 1
      0 0 1
      0 0 1
      0 0 1
    tangents 4
` + strings.Repeat("      "+tangentRow+"\n", 4) + `    faces 2
      0 1 2 1 0 1 2 0
      0 2 3 1 0 2 3 0
  endnode
endmodelgeom
donemodel hinted
`
}

// Every render hint survives compile and decompile; NormalTangents used to
// compile to 0 and None to 0, and 3 used to decompile as None.
func TestRenderHintRoundTrip(t *testing.T) {
	for hint, want := range map[string]string{
		"NormalAndSpecMapped": "NormalAndSpecMapped",
		"NormalTangents":      "NormalTangents",
		"normaltangents":      "NormalTangents",
		"None":                "None",
		"":                    "",
	} {
		line := ""
		if hint != "" {
			line = "    renderhint " + hint
		}
		m := mustDecompile(t, mustCompile(t, mustParseASCII(t, hintTangentModel(line, "1 0 0 1"))))
		n := m.FindNode("quad")
		if n == nil || n.Mesh == nil {
			t.Fatalf("%q: quad not found after round trip", hint)
		}
		if n.Mesh.RenderHint != want {
			t.Errorf("renderhint %q decompiled as %q, want %q", hint, n.Mesh.RenderHint, want)
		}
	}
}

// The tangent pointer of a 1.69 model holds water's bump-map stream. Without
// a normal-mapped render hint the stream is kept only if it is unit vectors,
// as the engine's compiler writes for meshes whose hint is in their material.
func TestDecompileTangentStreamGate(t *testing.T) {
	cases := []struct {
		name, hint, row string
		keep            bool
	}{
		{"no hint, bump-map junk", "", "3.080194e-36 3.080194e-36 0 1", false},
		{"no hint, unit tangents", "", "1 0 0 1", true},
		{"None, bump-map junk", "    renderhint None", "0 0 0 1", false},
		{"normal-mapped hint keeps whatever is there", "    renderhint NormalTangents", "3.080194e-36 3.080194e-36 0 1", true},
	}
	for _, c := range cases {
		m := mustDecompile(t, mustCompile(t, mustParseASCII(t, hintTangentModel(c.hint, c.row))))
		n := m.FindNode("quad")
		if n == nil || n.Mesh == nil {
			t.Fatalf("%s: quad not found after round trip", c.name)
		}
		if got := len(n.Mesh.Tangents) > 0; got != c.keep {
			t.Errorf("%s: tangents kept = %v, want %v", c.name, got, c.keep)
		}
	}
}

// Every key of an emitter is seen before any is named, so nwnmdlcomp's 464
// (alphamid) is not read as BioWare's percentstart when it comes before the
// 480-range key that tells the compilers apart. 468 with three columns is
// nwnmdlcomp's colormid on its own.
func TestEmitterCompilerDetectedFromAllKeys(t *testing.T) {
	cases := []struct {
		name string
		keys []binControllerKey
		want []string
	}{
		{"nwnmdlcomp, 464 first", []binControllerKey{{Type: 464, ColumnCount: 1}, {Type: 480, ColumnCount: 1}},
			[]string{"alphamid", "percentstart"}},
		{"nwnmdlcomp colormid alone", []binControllerKey{{Type: 468, ColumnCount: 3}}, []string{"colormid"}},
		{"BioWare sizemid alone", []binControllerKey{{Type: 468, ColumnCount: 1}}, []string{"sizemid"}},
		{"ambiguous 464 stays BioWare", []binControllerKey{{Type: 464, ColumnCount: 1}}, []string{"percentstart"}},
	}
	for _, c := range cases {
		d := &decompiler{model: &Model{}, compiler: CompilerUnknown}
		var got []string
		for _, r := range d.resolveControllerDefs(c.keys, 5, "emitter") {
			got = append(got, r.def.Name)
		}
		if strings.Join(got, ",") != strings.Join(c.want, ",") {
			t.Errorf("%s: got %v, want %v", c.name, got, c.want)
		}
	}
}
