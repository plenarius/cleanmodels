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
