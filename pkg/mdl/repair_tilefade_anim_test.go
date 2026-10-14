package mdl

import (
	"strings"
	"testing"
)

// A tile whose mesh "slab" crosses the tilefade plane (z=5) and is animated: it has
// alpha and position keys in two animations, and a child "knob" animated under it.
const tilefadeAnimSrc = `newmodel tf
setsupermodel tf NULL
classification TILE
setanimationscale 1.00
beginmodelgeom tf
  node dummy tf
    parent NULL
  endnode
  node trimesh slab
    parent tf
    verts 3
      0 0 0
      1 0 0
      0 0 10
    faces 1
      0 1 2 1 0 1 2 0
  endnode
  node dummy knob
    parent slab
  endnode
endmodelgeom
newanim loop1 tf
  length 1.0
  transtime 0.0
  animroot tf
  node trimesh slab
    parent tf
    alphakey 2
      0.0 1.0
      1.0 0.5
    positionkey 2
      0.0 0 0 0
      1.0 0 0 1
  endnode
  node dummy knob
    parent slab
    positionkey 2
      0.0 0 0 0
      1.0 0 0 2
  endnode
doneanim loop1 tf
newanim loop2 tf
  length 2.0
  transtime 0.0
  animroot tf
  node trimesh slab
    parent tf
    alphakey 2
      0.0 0.0
      2.0 1.0
  endnode
doneanim loop2 tf
donemodel tf
`

func geometryNames(m *Model) map[string]bool {
	names := map[string]bool{}
	for _, n := range m.Nodes {
		if n != nil {
			names[strings.ToLower(n.Name)] = true
		}
	}
	return names
}

// The reported bug: after the slice the animations still named "slab", which no
// longer exists, so its keys animated nothing.
func TestTilefadeSliceKeepsEveryAnimationNodeOnARealNode(t *testing.T) {
	m := mustParseASCII(t, tilefadeAnimSrc)
	msgs := SliceTileFade(m, 5)
	if len(msgs) == 0 || !strings.Contains(strings.Join(msgs, "\n"), "slab__tf0") {
		t.Fatalf("the mesh was not sliced: %v", msgs)
	}
	geo := geometryNames(m)
	if !geo["slab__tf0"] || !geo["slab__tf1"] || geo["slab"] {
		t.Fatalf("geometry after the slice: %v", geo)
	}
	for _, a := range m.Animations {
		for _, an := range a.Nodes {
			if !geo[strings.ToLower(an.Name)] {
				t.Errorf("animation %q has a node %q with no geometry node", a.Name, an.Name)
			}
		}
	}
}

// Both halves were one mesh with one transform, so both animate with the same keys.
func TestTilefadeSliceGivesBothHalvesTheSameKeys(t *testing.T) {
	m := mustParseASCII(t, tilefadeAnimSrc)
	before := m.Animations[0].Nodes[0] // slab in loop1
	SliceTileFade(m, 5)

	find := func(a *Animation, name string) *AnimNode {
		for i := range a.Nodes {
			if a.Nodes[i].Name == name {
				return &a.Nodes[i]
			}
		}
		return nil
	}
	for ai := range m.Animations {
		a := &m.Animations[ai]
		n0, n1 := find(a, "slab__tf0"), find(a, "slab__tf1")
		if n0 == nil || n1 == nil {
			t.Fatalf("animation %q: halves %v %v", a.Name, n0, n1)
		}
		if len(n0.AlphaKeys) == 0 || len(n0.AlphaKeys) != len(n1.AlphaKeys) {
			t.Errorf("%q alpha keys: %d vs %d", a.Name, len(n0.AlphaKeys), len(n1.AlphaKeys))
		}
		for i := range n0.AlphaKeys {
			if n0.AlphaKeys[i] != n1.AlphaKeys[i] {
				t.Errorf("%q alpha key %d differs: %+v vs %+v", a.Name, i, n0.AlphaKeys[i], n1.AlphaKeys[i])
			}
		}
	}
	loop1 := &m.Animations[0]
	n0, n1 := find(loop1, "slab__tf0"), find(loop1, "slab__tf1")
	if len(n0.PositionKeys) != len(before.PositionKeys) || n0.PositionKeys[1].Value.Z != 1 || n1.PositionKeys[1].Value.Z != 1 {
		t.Errorf("position keys not carried to both halves: %+v / %+v", n0.PositionKeys, n1.PositionKeys)
	}
	// Copies, not shared slices.
	n0.AlphaKeys[0].Value = 99
	if n1.AlphaKeys[0].Value == 99 {
		t.Error("the two halves share their key slices")
	}
	// An animation node of the child (knob) is untouched, and its parent follows the first half.
	if k := find(loop1, "knob"); k == nil || k.Parent != "slab__tf0" || len(k.PositionKeys) != 2 {
		t.Errorf("knob: %+v", k)
	}
}

// A node that is not split keeps its animation nodes as they are.
func TestTilefadeSliceLeavesUnsplitAnimationsAlone(t *testing.T) {
	m := mustParseASCII(t, tilefadeAnimSrc)
	SliceTileFade(m, 50) // nothing crosses z=50
	if len(m.Animations[0].Nodes) != 2 || m.Animations[0].Nodes[0].Name != "slab" {
		t.Errorf("animation nodes changed without a split: %+v", m.Animations[0].Nodes)
	}
}
