package mdl

import (
	"bytes"
	"strings"
	"testing"
)

// splitAnimModel is the shape of Emonom's HD forge (plc_j01): NWMax wrote
// each animation's model name on the line after "newanim <name>".
func splitAnimModel(header string) string {
	return `newmodel forge
setsupermodel forge NULL
classification CHARACTER
beginmodelgeom forge
  node dummy forge
    parent NULL
  endnode
  node trimesh body
    parent forge
    bitmap forge_d
    verts 3
      0 0 0
      1 0 0
      0 1 0
    tverts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2 1 0 1 2 0
  endnode
endmodelgeom forge
` + header + `
  length 1
  transtime 0.25
  animroot forge
  node dummy forge
    parent NULL
  endnode
  node trimesh body
    parent forge
    positionkey
      0 0 0 0
    endlist
  endnode
doneanim default forge
donemodel forge
`
}

// The game reads only the animation's name; the model name may be missing
// (nwn-mdl-spec MDL-ASCII-060). Read as an error, every animation's nodes
// became a second geometry tree and the last root, with no meshes, was
// compiled: the forge was empty in the game (layonara/nwn-module#999).
func TestParseNewAnimWithoutModelName(t *testing.T) {
	compiled := func(header string) ([]byte, *Model) {
		t.Helper()
		r, err := Parse(strings.NewReader(splitAnimModel(header)))
		if err != nil {
			t.Fatal(err)
		}
		for _, e := range r.Errors {
			t.Errorf("%q: parse error: %s", header, e.Message)
		}
		var buf bytes.Buffer
		if err := Compile(r.Model, &buf); err != nil {
			t.Fatal(err)
		}
		return buf.Bytes(), r.Model
	}
	want, _ := compiled("newanim default forge")
	for _, header := range []string{"newanim default", "newanim default\n forge"} {
		got, m := compiled(header)
		if len(m.Nodes) != 2 {
			t.Errorf("%q: %d geometry nodes, want 2 (animation nodes read as geometry)", header, len(m.Nodes))
		}
		if len(m.Animations) != 1 || m.Animations[0].Name != "default" || len(m.Animations[0].Nodes) != 2 {
			t.Errorf("%q: animations = %+v, want one \"default\" with 2 nodes", header, m.Animations)
		}
		if !bytes.Equal(got, want) {
			t.Errorf("%q: compiles differently from \"newanim default forge\"", header)
		}
	}

	r, err := Parse(strings.NewReader(splitAnimModel("newanim")))
	if err != nil {
		t.Fatal(err)
	}
	if len(r.Errors) == 0 || !strings.Contains(r.Errors[0].Message, "requires an animation name") {
		t.Errorf("newanim without a name: errors = %v", r.Errors)
	}
}
