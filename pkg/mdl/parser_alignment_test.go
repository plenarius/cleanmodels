package mdl

import (
	"fmt"
	"strings"
	"testing"
)

// model wraps a trimesh body in a minimal valid model.
func alignModel(body string) string {
	return `newmodel align
setsupermodel align NULL
classification PLACEABLE
setanimationscale 1.00
beginmodelgeom align
  node dummy align
    parent NULL
  endnode
  node trimesh m
    parent align
    bitmap NULL
    render 1
` + body + `
  endnode
endmodelgeom
donemodel align
`
}

func parseAlign(t *testing.T, body string) (*MeshData, []ParseError) {
	t.Helper()
	pr, err := Parse(strings.NewReader(alignModel(body)))
	if err != nil {
		t.Fatal(err)
	}
	n := pr.Model.FindNode("m")
	if n == nil || n.Mesh == nil {
		t.Fatal("trimesh m not parsed")
	}
	return n.Mesh, pr.Errors
}

func hasError(errs []ParseError, substr string) bool {
	for _, e := range errs {
		if strings.Contains(e.Message, substr) {
			return true
		}
	}
	return false
}

// The real case from NWNCQ's tsw01_o07_01: a tvert line "0.0809014 0/934729 0"
// used to be dropped silently, which slid every later tvert down one slot so
// that faces pointing at the last one were "out of range".
func TestListKeepsEntryWithTypoNumber(t *testing.T) {
	mesh, errs := parseAlign(t, `    verts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2 1 0 1 3 0
    tverts 4
      0.1 0.1 0
      0.0809014 0/934729 0
      0.5 0.5 0
      0.9 0.9 0`)
	if len(mesh.TVerts) != 4 {
		t.Fatalf("parsed %d tverts, declared 4", len(mesh.TVerts))
	}
	if mesh.TVerts[1].X != 0.0809014 || mesh.TVerts[1].Y != 0 {
		t.Errorf("tvert 1 = %+v, want the good X kept and the bad Y zeroed", mesh.TVerts[1])
	}
	if mesh.TVerts[3].X != 0.9 || mesh.TVerts[2].X != 0.5 {
		t.Errorf("later tverts slid out of place: %+v", mesh.TVerts)
	}
	if !hasError(errs, `"0/934729" is read as 0, as the game reads it`) {
		t.Errorf("no warning naming the token: %+v", errs)
	}
	for _, e := range errs {
		if strings.Contains(e.Message, "0/934729") && e.Line == 0 {
			t.Errorf("warning has no line number: %+v", e)
		}
	}
}

// Values outside float32 range, and nan/inf, used to vanish the same way.
func TestListKeepsEntryWithOutOfRangeAndNonFinite(t *testing.T) {
	mesh, errs := parseAlign(t, `    verts 1
      0 0 0
    tverts 4
      1e39 0.5 0
      0.5 -1e39 0
      nan 0.25 0
      0.75 inf 0`)
	if len(mesh.TVerts) != 4 {
		t.Fatalf("parsed %d tverts, declared 4: %+v", len(mesh.TVerts), mesh.TVerts)
	}
	want := []Vec3{{0, 0.5, 0}, {0.5, 0, 0}, {0, 0.25, 0}, {0.75, 0, 0}}
	for i, w := range want {
		if mesh.TVerts[i] != w {
			t.Errorf("tvert %d = %+v, want %+v", i, mesh.TVerts[i], w)
		}
	}
	if len(errs) != 4 {
		t.Errorf("want one warning per bad value, got %d: %+v", len(errs), errs)
	}
}

// A line with too few values is kept, zero-filled, and reported.
func TestListKeepsShortEntry(t *testing.T) {
	mesh, errs := parseAlign(t, `    verts 3
      0 0 0
      1 0
      0 1 0`)
	if len(mesh.Verts) != 3 {
		t.Fatalf("parsed %d verts, declared 3", len(mesh.Verts))
	}
	if mesh.Verts[1] != (Vec3{X: 1}) || mesh.Verts[2] != (Vec3{Y: 1}) {
		t.Errorf("verts = %+v", mesh.Verts)
	}
	if !hasError(errs, "entry has 2 value(s), expected 3") {
		t.Errorf("no short-entry warning: %+v", errs)
	}
}

// A well-formed list produces no warnings at all.
func TestListCleanProducesNoWarnings(t *testing.T) {
	_, errs := parseAlign(t, `    verts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2 1 0 1 2 0
    tverts 3
      0 0 0
      1 0 0
      0 1 0
    normals 3
      0 0 1
      0 0 1
      0 0 1`)
	if len(errs) != 0 {
		t.Fatalf("clean model produced warnings: %+v", errs)
	}
}

// A badly damaged list reports the first few and then one summary.
func TestListWarningsAreCapped(t *testing.T) {
	var b strings.Builder
	b.WriteString("    verts 1\n      0 0 0\n    tverts 20\n")
	for i := 0; i < 20; i++ {
		b.WriteString("      x 0.5 0\n")
	}
	mesh, errs := parseAlign(t, strings.TrimRight(b.String(), "\n"))
	if len(mesh.TVerts) != 20 {
		t.Fatalf("parsed %d tverts, declared 20", len(mesh.TVerts))
	}
	if len(errs) != maxListWarnings+1 {
		t.Fatalf("want %d warnings plus a summary, got %d: %+v", maxListWarnings, len(errs), errs)
	}
	if !hasError(errs, fmt.Sprintf("%d more damaged value(s)", 20-maxListWarnings)) {
		t.Errorf("no summary line: %+v", errs)
	}
}

// Faces are positional too: a short or non-numeric line keeps its slot.
func TestFaceListKeepsDamagedEntries(t *testing.T) {
	mesh, errs := parseAlign(t, `    verts 3
      0 0 0
      1 0 0
      0 1 0
    faces 3
      0 1 2 1 0 1 2 0
      0 1 2 1 0 1
      0 1 q 1 0 1 2 0`)
	if len(mesh.Faces) != 3 {
		t.Fatalf("parsed %d faces, declared 3", len(mesh.Faces))
	}
	if mesh.Faces[2].Verts != [3]int32{0, 1, 0} {
		t.Errorf("face 2 = %+v, want the bad index zeroed", mesh.Faces[2])
	}
	if !hasError(errs, "entry has 6 value(s), expected 8") || !hasError(errs, `"q" is not a usable integer`) {
		t.Errorf("missing warnings: %+v", errs)
	}
}

// An AABB entry is a tree node, so a damaged line must keep its place.
func TestAabbListKeepsDamagedEntry(t *testing.T) {
	src := `newmodel aabbt
setsupermodel aabbt NULL
classification TILE
setanimationscale 1.00
beginmodelgeom aabbt
  node dummy aabbt
    parent NULL
  endnode
  node aabb w
    parent aabbt
    verts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2 1 0 1 2 0
    aabb 3
      0 0 0 1 1 0 -1
      0 0 0 1 1
      0 0 0 1 1 0 0
  endnode
endmodelgeom
donemodel aabbt
`
	pr, err := Parse(strings.NewReader(src))
	if err != nil {
		t.Fatal(err)
	}
	n := pr.Model.FindNode("w")
	if n == nil || n.Aabb == nil {
		t.Fatal("aabb node not parsed")
	}
	if len(n.Aabb.Entries) != 3 {
		t.Fatalf("parsed %d aabb entries, declared 3", len(n.Aabb.Entries))
	}
	if n.Aabb.Entries[2].LeafFace != 0 || n.Aabb.Entries[2].BoundMax.X != 1 {
		t.Errorf("the entry after the damaged one slid: %+v", n.Aabb.Entries)
	}
	if !hasError(pr.Errors, "entry has 5 value(s), expected 7") {
		t.Errorf("no warning: %+v", pr.Errors)
	}
}
