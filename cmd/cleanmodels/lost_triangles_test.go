package main

import (
	"strings"
	"testing"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

func TestRefuseLostTriangles(t *testing.T) {
	put := &mdl.Model{RenderMerges: []mdl.RenderMerge{{Receiver: "wall", Donors: []string{"lower"}}}}
	if err := refuseLostTriangles(put, false); err != nil {
		t.Errorf("merge fully put back refused: %v", err)
	}
	lost := &mdl.Model{RenderMerges: []mdl.RenderMerge{{Receiver: "wall", Lost: 3}}}
	err := refuseLostTriangles(lost, false)
	if err == nil || !strings.Contains(err.Error(), "3 triangles") || !strings.Contains(err.Error(), "--force") {
		t.Errorf("lost triangles not refused: %v", err)
	}
	if err := refuseLostTriangles(lost, true); err != nil {
		t.Errorf("--force refused: %v", err)
	}
}

// Every subcommand registers its flags without a clash (decompile has a
// --force of its own; a duplicate name panics when the flags are set up).
func TestSubcommandFlagsRegister(t *testing.T) {
	for name, cmd := range subcommands {
		func() {
			defer func() {
				if r := recover(); r != nil {
					t.Errorf("%s: %v", name, r)
				}
			}()
			cmd([]string{"--help"})
		}()
	}
}

func TestRefuseDroppedRoots(t *testing.T) {
	mesh := func(name, parent string) *mdl.Node {
		return &mdl.Node{Name: name, Parent: parent, Mesh: &mdl.MeshData{}}
	}
	dummy := func(name, parent string) *mdl.Node { return &mdl.Node{Name: name, Parent: parent} }

	one := &mdl.Model{Nodes: []*mdl.Node{dummy("forge", "NULL"), mesh("body", "forge")}}
	if err := refuseDroppedRoots(one, false); err != nil {
		t.Errorf("one root refused: %v", err)
	}
	// An earlier root holding only dummies loses nothing that draws.
	dummies := &mdl.Model{Nodes: []*mdl.Node{dummy("forge", "NULL"), dummy("hand", "forge"),
		dummy("forge", "NULL"), mesh("body", "forge")}}
	if err := refuseDroppedRoots(dummies, false); err != nil {
		t.Errorf("earlier root of dummies refused: %v", err)
	}
	// plc_j01: the geometry, then an animation's nodes read as a second tree.
	split := &mdl.Model{Nodes: []*mdl.Node{dummy("forge", "NULL"), mesh("body", "forge"), mesh("bellows", "body"),
		dummy("forge", "NULL"), dummy("hand", "forge")}}
	err := refuseDroppedRoots(split, false)
	if err == nil || !strings.Contains(err.Error(), "2 mesh(es) (body, bellows)") || !strings.Contains(err.Error(), "--force") {
		t.Errorf("meshes under an earlier root not refused: %v", err)
	}
	if err := refuseDroppedRoots(split, true); err != nil {
		t.Errorf("--force refused: %v", err)
	}
	// A mesh whose parent no node is named, one with no parent line, and a
	// dummy in no tree (which draws nothing, so it is not refused).
	orphans := &mdl.Model{Nodes: []*mdl.Node{dummy("forge", "NULL"), mesh("body", "forge"),
		mesh("lost", "nosuchnode"), mesh("stray", ""), dummy("hook", "nosuchnode")}}
	err = refuseDroppedRoots(orphans, false)
	if err == nil || !strings.Contains(err.Error(), "2 mesh(es) (lost, stray) are not connected to the root") || strings.Contains(err.Error(), "hook") {
		t.Errorf("meshes in no tree not refused: %v", err)
	}
	if err := refuseDroppedRoots(orphans, true); err != nil {
		t.Errorf("--force refused: %v", err)
	}
}
