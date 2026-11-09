package mdl

import (
	"bytes"
	"strings"
	"testing"
)

// Node names of 32 and more characters: one that becomes another node's name
// when cut (Layonara's heads), two that cut to the same, one that stays
// apart, a 31-character one that is not cut, and an animation-only one.
const cutNamesSrc = `newmodel namecut
setsupermodel namecut NULL
classification character
setanimationscale 1
beginmodelgeom namecut
node dummy namecut
  parent NULL
endnode
node dummy abcdefghijklmnopqrstuvwxyz012345
  parent namecut
endnode
node dummy ABCDEFGHIJKLMNOPQRSTUVWXYZ01234
  parent namecut
endnode
node dummy twin_twin_twin_twin_twin_twin_1_a
  parent namecut
endnode
node dummy twin_twin_twin_twin_twin_twin_1_b
  parent namecut
endnode
node dummy a_long_name_that_stays_apart_xyz
  parent namecut
endnode
node dummy thirty_one_characters_exactly__
  parent namecut
endnode
endmodelgeom namecut
newanim walk namecut
  length 1
  transtime 0.25
  animroot namecut
  node dummy namecut
    parent NULL
  endnode
  node dummy abcdefghijklmnopqrstuvwxyz012345
    parent namecut
  endnode
  node dummy an_animation_node_with_a_long_name
    parent namecut
  endnode
doneanim walk namecut
donemodel namecut
`

// compile warns for every node name it cuts to 31 characters, and says so
// when the cut name is another node's.
func TestCompileWarnsCutNodeNames(t *testing.T) {
	var warns []string
	var buf bytes.Buffer
	opts := CompileOptions{Warn: func(s string) { warns = append(warns, s) }}
	if err := CompileWithOptions(mustParseASCII(t, cutNamesSrc), &buf, opts); err != nil {
		t.Fatal(err)
	}
	want := []string{
		`node "abcdefghijklmnopqrstuvwxyz012345": its name has 32 characters and is cut to 31, as the game's compiler does: "abcdefghijklmnopqrstuvwxyz01234", which is the name of node "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234", so the game cannot tell the two apart`,
		`node "twin_twin_twin_twin_twin_twin_1_a": its name has 33 characters and is cut to 31, as the game's compiler does: "twin_twin_twin_twin_twin_twin_1", which node "twin_twin_twin_twin_twin_twin_1_b" is also cut to, so the game cannot tell the two apart`,
		`node "twin_twin_twin_twin_twin_twin_1_b": its name has 33 characters and is cut to 31, as the game's compiler does: "twin_twin_twin_twin_twin_twin_1", which node "twin_twin_twin_twin_twin_twin_1_a" is also cut to, so the game cannot tell the two apart`,
		`node "a_long_name_that_stays_apart_xyz": its name has 32 characters and is cut to 31, as the game's compiler does: "a_long_name_that_stays_apart_xy"`,
		`animation "walk": node "an_animation_node_with_a_long_name": its name has 34 characters and is cut to 31, as the game's compiler does: "an_animation_node_with_a_long_n"`,
	}
	var got []string
	for _, w := range warns {
		if strings.Contains(w, "is cut to 31") {
			got = append(got, w)
		}
	}
	if strings.Join(got, "\n") != strings.Join(want, "\n") {
		t.Errorf("warnings:\n%s\nwant:\n%s", strings.Join(got, "\n"), strings.Join(want, "\n"))
	}
	// The binary holds the cut names.
	if !bytes.Contains(buf.Bytes(), []byte("abcdefghijklmnopqrstuvwxyz01234\x00")) ||
		bytes.Contains(buf.Bytes(), []byte("abcdefghijklmnopqrstuvwxyz012345")) {
		t.Error("the binary does not hold the name cut to 31 characters")
	}
}
