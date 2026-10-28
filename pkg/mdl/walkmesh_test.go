package mdl

import (
	"bytes"
	"strings"
	"testing"
)

func TestWriteWalkmesh(t *testing.T) {
	aabb := &Node{Name: "wok", Parent: "tile", Aabb: &AabbData{Entries: []AabbEntry{{LeafFace: 0}}}, Mesh: &MeshData{}}
	m := &Model{Name: "tile", Nodes: []*Node{{Name: "tile", Parent: "NULL"}, aabb}}
	var buf bytes.Buffer
	ok, err := WriteWalkmesh(m, &buf)
	if err != nil || !ok {
		t.Fatalf("WriteWalkmesh = %v, %v", ok, err)
	}
	got := buf.String()
	for _, want := range []string{"#MAXWALKMESH  ASCII\n", "beginwalkmeshgeom tile\n", "node aabb wok\n", "endwalkmeshgeom tile\n"} {
		if !strings.Contains(got, want) {
			t.Errorf("walkmesh lacks %q:\n%s", want, got)
		}
	}
	if strings.Contains(got, "newmodel") {
		t.Errorf("walkmesh holds the model header:\n%s", got)
	}
	m.Nodes = m.Nodes[:1]
	buf.Reset()
	if ok, _ := WriteWalkmesh(m, &buf); ok || buf.Len() != 0 {
		t.Errorf("a model without an aabb node wrote %q", buf.String())
	}
}
