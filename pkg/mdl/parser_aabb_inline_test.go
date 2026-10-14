package mdl

import (
	"strings"
	"testing"
)

const inlineAabbModel = `newmodel t
setsupermodel t NULL
classification tile
beginmodelgeom t
  node dummy t
    parent NULL
  endnode
  node aabb w
    parent t
    verts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2 1 0 1 2 1
    aabb -5.00 -5.00 0.00   5.00 5.00 1.00   -1
      -5 -5 0 0 5 1 0
      0 -5 0 5 5 1 -1
        0 -5 0 5 0 1 0
        0 0 0 5 5 1 0
  endnode
endmodelgeom t
donemodel t
`

func TestParseInlineAabbTree(t *testing.T) {
	r, err := Parse(strings.NewReader(inlineAabbModel))
	if err != nil {
		t.Fatal(err)
	}
	var got []AabbEntry
	for _, n := range r.Model.Nodes {
		if n != nil && n.Aabb != nil {
			got = n.Aabb.Entries
		}
	}
	if len(got) != 5 {
		t.Fatalf("read %d aabb entries, want 5", len(got))
	}
	if got[0].LeafFace != -1 || got[0].BoundMin.X != -5 || got[2].LeafFace != -1 || got[4].LeafFace != 0 {
		t.Errorf("entries not read in order: %+v", got)
	}
}

func TestWriteAabbInlineRoundTrips(t *testing.T) {
	r, err := Parse(strings.NewReader(inlineAabbModel))
	if err != nil {
		t.Fatal(err)
	}
	var sb strings.Builder
	if err := Write(r.Model, &sb); err != nil {
		t.Fatal(err)
	}
	out := sb.String()
	if strings.Contains(out, "aabb 5") {
		t.Errorf("writer emitted a counted aabb, the game does not read that:\n%s", out)
	}
	if !strings.Contains(out, "aabb -5 -5 0 5 5 1 -1\n") {
		t.Errorf("root entry is not on the keyword line:\n%s", out)
	}
	r2, err := Parse(strings.NewReader(out))
	if err != nil {
		t.Fatal(err)
	}
	for _, n := range r2.Model.Nodes {
		if n != nil && n.Aabb != nil && len(n.Aabb.Entries) != 5 {
			t.Errorf("round trip kept %d entries, want 5", len(n.Aabb.Entries))
		}
	}
}
