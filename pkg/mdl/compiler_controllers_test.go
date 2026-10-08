package mdl

import "testing"

// TestEncodeGeomNodeControllersAlwaysWritesTransformForNonRoot pins the fix
// for issue #12: a non-root node with a default transform (position 0,0,0,
// orientation axis 0,0,0 angle 0 — both exactly the ASCII default) used to
// get zero position/orientation controllers, because encodeGeomNodeControllers
// skipped writing them whenever the value was the default one.
//
// BioWare's own compiler never does this: every non-root node in every stock
// model we inspected (helm_010, plc_o04, vdr_magearmor2, ...) carries both a
// position and an orientation controller, even when the value is identity —
// only the model's root node has neither. The engine appears to rely on
// these controllers to initialize each node's runtime transform; a non-root
// node compiled with neither ends up with an uninitialized/degenerate
// transform that propagates to every descendant. That is exactly what
// vdr_magearmor.mdl hit: "Dummy01" has a fully default transform, so it
// silently lost both controllers, and its whole subtree ("shield",
// "shield01".."shield07", "Cylinder02") never rendered in-game despite the
// ASCII loading and rendering fine directly.
func TestEncodeGeomNodeControllersAlwaysWritesTransformForNonRoot(t *testing.T) {
	c := &compiler{}

	// A non-root node with an entirely default transform must still get
	// exactly a position and an orientation controller.
	child := &Node{
		Name:        "Dummy01",
		Parent:      "vdr_magearmor",
		Position:    Vec3{},
		Orientation: Vec4{}, // axis 0,0,0, angle 0 — fully degenerate axis-angle
	}
	keys, dataArr := c.encodeGeomNodeControllers(child)

	var gotPos, gotOri bool
	for _, k := range keys {
		switch k.Type {
		case 8:
			gotPos = true
			got := dataArr[k.DataStart : k.DataStart+3]
			want := []float32{0, 0, 0}
			for i := range want {
				if got[i] != want[i] {
					t.Errorf("position[%d] = %v, want %v", i, got[i], want[i])
				}
			}
		case 20:
			gotOri = true
			got := dataArr[k.DataStart : k.DataStart+4]
			// Identity quaternion, regardless of the degenerate source axis.
			want := []float32{0, 0, 0, 1}
			for i := range want {
				if got[i] != want[i] {
					t.Errorf("orientation[%d] = %v, want %v", i, got[i], want[i])
				}
			}
		}
	}
	if !gotPos {
		t.Error("non-root node with default position: no position (ID=8) controller written")
	}
	if !gotOri {
		t.Error("non-root node with default orientation: no orientation (ID=20) controller written")
	}

	// The model root (Parent NULL) must still omit both — matching every
	// stock file inspected, which never writes a transform controller for
	// the root node.
	root := &Node{
		Name:        "vdr_magearmor",
		Parent:      "NULL",
		Position:    Vec3{},
		Orientation: Vec4{},
	}
	rootKeys, _ := c.encodeGeomNodeControllers(root)
	for _, k := range rootKeys {
		if k.Type == 8 || k.Type == 20 {
			t.Errorf("root node got controller type %d; root must have neither position nor orientation", k.Type)
		}
	}
}

// TestControllerDataStartIsAdjacentToTimeStart pins the real fix for issue
// #12: every controller must satisfy DataStart == TimeStart + ValueCount —
// i.e. a controller's own data values are packed immediately after its own
// time values in the shared float array, not grouped separately from every
// other controller's time/data.
//
// We previously packed ALL controllers' times first, then ALL controllers'
// data after (a global split). That kept our own decompiler self-consistent
// (it computed the same offsets it wrote), which is exactly why round-tripping
// through cleanmodels' own compile+decompile never caught this: the bug is
// only visible when checked against an independently-written reader.
//
// Checked byte-for-byte against the retail vdr_magearmor2.mdl: every single
// controller in every node (position, orientation, alpha, emitter fields,
// ...) satisfies this adjacency with zero exceptions. Our old layout violated
// it for every controller in every node. The real game engine appears to
// derive each controller's data offset as TimeStart+ValueCount rather than
// trusting a separately stored DataStart, so the old layout pointed alpha
// (and every other multi-controller node's) data reads at unrelated floats
// belonging to a different controller — explaining why "shield"/"Cylinder02"
// (and, per further testing, "Cylinder01") never faded in despite the
// geometry, node tree, and mesh headers all being individually correct.
func TestControllerDataStartIsAdjacentToTimeStart(t *testing.T) {
	c := &compiler{}
	an := &AnimNode{
		Name: "shield",
		PositionKeys: []PositionKey{
			{Time: 0, Value: Vec3{X: 1, Y: 2, Z: 3}},
			{Time: 1, Value: Vec3{X: 4, Y: 5, Z: 6}},
		},
		AlphaKeys: []FloatKey{
			{Time: 0, Value: 0},
			{Time: 0.5, Value: 0.5},
			{Time: 1, Value: 1},
		},
	}
	keys, dataArr := c.encodeAnimNodeControllers(an, 0x21) // mesh content bit set
	if len(keys) == 0 {
		t.Fatal("expected controllers for position and alpha, got none")
	}
	for _, k := range keys {
		want := k.TimeStart + k.ValueCount
		if k.DataStart != want {
			t.Errorf("type=%d: DataStart=%d, want TimeStart(%d)+ValueCount(%d)=%d",
				k.Type, k.DataStart, k.TimeStart, k.ValueCount, want)
		}
		// And the data actually readable at DataStart must be this
		// controller's own values, not another controller's.
		gotFirst := dataArr[k.DataStart]
		switch k.Type {
		case 8: // position
			if gotFirst != 1 {
				t.Errorf("position data at DataStart = %v, want first position value 1", gotFirst)
			}
		case 128: // alpha
			if gotFirst != 0 {
				t.Errorf("alpha data at DataStart = %v, want first alpha value 0", gotFirst)
			}
		}
	}
}
