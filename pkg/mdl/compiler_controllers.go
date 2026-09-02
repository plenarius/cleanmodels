// compiler_controllers.go — binControllerKey encoding for the binary MDL compiler.
//
// Every controller's own time values are packed immediately followed by its
// own data values, in one shared float array — DataStart == TimeStart +
// ValueCount for every controller, always. This matches BioWare's own
// compiler exactly (verified byte-for-byte against retail binaries, e.g.
// vdr_magearmor2.mdl: every controller in every node satisfies this
// adjacency, with zero exceptions). We previously packed all controllers'
// times first and all controllers' data after (a global split rather than
// per-controller), which kept TimeStart/DataStart self-consistent for our
// own reader but broke this adjacency — the likely reason alpha-animated
// trimesh nodes (e.g. vdr_magearmor.mdl's "shield"/"Cylinder02", issue #12)
// silently failed to fade in: the engine appears to derive each controller's
// data offset from TimeStart+ValueCount rather than trusting a
// separately-stored DataStart, so our data lookups landed on unrelated
// floats belonging to a different controller entirely.
//
// Ref: binary.go readControllers / readControllerKeys / readControllerRows
package mdl

import (
	"fmt"
	"sort"
)

// binCtrlKey is the 12-byte binary controller key record.
// Matches binary.go binControllerKey exactly.
type binCtrlKey struct {
	Type        uint32
	ValueCount  uint16 // number of keyframe rows
	TimeStart   uint16 // index into shared time array
	DataStart   uint16 // index into shared data array
	ColumnCount byte   // number of floats per row
}

// encodeGeomNodeControllers builds controller keys + a shared data array for
// a geometry node. Geometry nodes store only a single static value (1
// keyframe at t=0) per controller.
//
// Returns: (keys, nil, data). The single float array passed to the binary is
// `data` (timeArr is always nil here — see the package doc comment for why
// each controller's time is packed immediately before its own data, rather
// than in a separate time array).
//
// Ref: binary.go readControllers → d.readControllerRows() for geometry nodes
func (c *compiler) encodeGeomNodeControllers(n *Node) (keys []binCtrlKey, timeArr, dataArr []float32) {
	// Helper: add one static controller. Its time (1 value) is packed
	// immediately followed by its data, so DataStart == TimeStart + 1 always.
	add := func(typeID uint32, cols byte, vals []float32) {
		if len(vals) == 0 {
			return
		}
		// Guard the full growth this call makes (1 time value + len(vals)
		// data values), not just the time slot — a call that only checked
		// the time slot could pass here and still push dataArr past what a
		// uint16 TimeStart/DataStart can address.
		if len(dataArr)+1+len(vals) >= 65535 {
			if c.err == nil {
				c.err = fmt.Errorf("geometry controller data exceeds uint16 index limit")
			}
			return
		}
		timeStart := uint16(len(dataArr))
		dataArr = append(dataArr, 0.0) // static: one keyframe at t=0
		dataStart := uint16(len(dataArr))
		dataArr = append(dataArr, vals...)
		keys = append(keys, binCtrlKey{
			Type:        typeID,
			ValueCount:  1,
			TimeStart:   timeStart,
			DataStart:   dataStart,
			ColumnCount: cols,
		})
	}

	// Universal controllers (all node types)
	//
	// position (ID=8, 3 cols) and orientation (ID=20, 4 cols) are written
	// unconditionally for every non-root node, even when the value is the
	// default (0,0,0 / identity) — matching BioWare's own compiler. Every
	// stock model we've inspected (helm_010, plc_o04, vdr_magearmor2, ...)
	// writes both controllers on every node except the model root, which
	// alone has neither.
	//
	// The engine appears to rely on these controllers to initialize each
	// node's runtime transform; a non-root node compiled with neither ends
	// up with an uninitialized/degenerate transform. That transform then
	// propagates to every descendant, so an entire subtree can silently fail
	// to render with no crash and no warning — exactly the vdr_magearmor.mdl
	// "shield"/"Cylinder02" case (issue #12): their parent, Dummy01, has
	// position 0,0,0 and orientation axis 0,0,0 angle 0 — both exactly
	// default — so the old "skip if default" logic emitted zero controllers
	// for Dummy01, and its whole subtree vanished in-game despite the ASCII
	// rendering fine when loaded directly.
	//
	// Skipping is still correct for the root node itself (Parent NULL):
	// every stock file we checked omits both there too.
	if !isRootParent(n.Parent) {
		pos := n.Position
		add(8, 3, []float32{pos.X, pos.Y, pos.Z})

		// ASCII axis-angle → binary quaternion xyzw. axisAngleToQuat handles
		// a zero axis correctly: sin(angle/2) scales X/Y/Z to 0 regardless of
		// axis when angle is also 0, yielding the identity quaternion (0,0,0,1).
		q := axisAngleToQuat(n.Orientation)
		add(20, 4, []float32{q.X, q.Y, q.Z, q.W})
	}
	// scale (ID=36, 1 col) — emit for any non-default value (default=1.0)
	if n.Scale != 1 {
		add(36, 1, []float32{n.Scale})
	}

	// Mesh controllers (bit 5 of nodeTypeFlag set)
	if n.Mesh != nil {
		// selfillumcolor (ID=100, 3 cols)
		sic := n.Mesh.SelfIllumColor
		if sic.X != 0 || sic.Y != 0 || sic.Z != 0 {
			add(100, 3, []float32{sic.X, sic.Y, sic.Z})
		}
		// alpha (ID=128, 1 col) — only skip the default value (1.0)
		if n.Mesh.Alpha != 1 {
			add(128, 1, []float32{n.Mesh.Alpha})
		}
	}

	// Light controllers
	if n.Light != nil {
		// color (ID=76 for nodeFlag=3, 3 cols)
		col := n.Light.Color
		if col.X != 0 || col.Y != 0 || col.Z != 0 {
			add(76, 3, []float32{col.X, col.Y, col.Z})
		}
		// radius (ID=88, 1 col)
		if n.Light.Radius != 0 {
			add(88, 1, []float32{n.Light.Radius})
		}
		// multiplier (ID=140, 1 col)
		if n.Light.Multiplier != 0 {
			add(140, 1, []float32{n.Light.Multiplier})
		}
		// shadowradius (ID=96, 1 col)
		if n.Light.ShadowRadius != 0 {
			add(96, 1, []float32{n.Light.ShadowRadius})
		}
		// verticaldisplacement (ID=100 for nodeFlag=3, 1 col)
		if n.Light.VerticalDisplacement != 0 {
			add(100, 1, []float32{n.Light.VerticalDisplacement})
		}
	}

	// Emitter controllers — static values stored as controllers (BioWare format)
	if n.Emitter != nil {
		em := n.Emitter
		// Use BioWare controller IDs from controllers.go
		addEmitterFloatCtrl := func(name string, val float32) {
			if val == 0 {
				return
			}
			typeID := emitterCtrlID(name)
			if typeID == 0 {
				return
			}
			add(typeID, 1, []float32{val})
		}
		addEmitterColorCtrl := func(name string, v Vec3) {
			if v.X == 0 && v.Y == 0 && v.Z == 0 {
				return
			}
			typeID := emitterCtrlID(name)
			if typeID == 0 {
				return
			}
			add(typeID, 3, []float32{v.X, v.Y, v.Z})
		}
		floatNames := make([]string, 0, len(emitterFloatFields))
		for name := range emitterFloatFields {
			floatNames = append(floatNames, name)
		}
		sort.Strings(floatNames)
		for _, name := range floatNames {
			addEmitterFloatCtrl(name, *emitterFloatFields[name](em))
		}
		colorNames := make([]string, 0, len(emitterColorFields))
		for name := range emitterColorFields {
			colorNames = append(colorNames, name)
		}
		sort.Strings(colorNames)
		for _, name := range colorNames {
			addEmitterColorCtrl(name, *emitterColorFields[name](em))
		}
	}

	return
}

// encodeAnimNodeControllers builds controller keys + a shared data array for
// an animation node. Animation nodes can have multi-frame keyframe arrays.
//
// Every controller packs its own time values immediately followed by its own
// data values into the single shared `dataArr` (timeArr is always nil — see
// the package doc comment).
func (c *compiler) encodeAnimNodeControllers(an *AnimNode, nodeFlag uint32) (keys []binCtrlKey, timeArr, dataArr []float32) {
	checkOverflow := func(n int) bool {
		if len(dataArr)+n > 65535 {
			if c.err == nil {
				c.err = fmt.Errorf("animation controller data exceeds uint16 index limit (%d entries)", len(dataArr)+n)
			}
			return true
		}
		return false
	}
	addFloat := func(typeID uint32, keyframes []FloatKey) {
		n := len(keyframes)
		if n == 0 || checkOverflow(2*n) {
			return
		}
		timeStart := uint16(len(dataArr))
		for _, kf := range keyframes {
			dataArr = append(dataArr, kf.Time)
		}
		dataStart := uint16(len(dataArr))
		for _, kf := range keyframes {
			dataArr = append(dataArr, kf.Value)
		}
		keys = append(keys, binCtrlKey{
			Type:        typeID,
			ValueCount:  uint16(n),
			TimeStart:   timeStart,
			DataStart:   dataStart,
			ColumnCount: 1,
		})
	}
	addColor := func(typeID uint32, keyframes []ColorKey) {
		n := len(keyframes)
		if n == 0 || checkOverflow(n+3*n) {
			return
		}
		timeStart := uint16(len(dataArr))
		for _, kf := range keyframes {
			dataArr = append(dataArr, kf.Time)
		}
		dataStart := uint16(len(dataArr))
		for _, kf := range keyframes {
			dataArr = append(dataArr, kf.Value.X, kf.Value.Y, kf.Value.Z)
		}
		keys = append(keys, binCtrlKey{
			Type:        typeID,
			ValueCount:  uint16(n),
			TimeStart:   timeStart,
			DataStart:   dataStart,
			ColumnCount: 3,
		})
	}
	addVec3 := func(typeID uint32, keyframes []PositionKey) {
		n := len(keyframes)
		if n == 0 || checkOverflow(n+3*n) {
			return
		}
		timeStart := uint16(len(dataArr))
		for _, kf := range keyframes {
			dataArr = append(dataArr, kf.Time)
		}
		dataStart := uint16(len(dataArr))
		for _, kf := range keyframes {
			dataArr = append(dataArr, kf.Value.X, kf.Value.Y, kf.Value.Z)
		}
		keys = append(keys, binCtrlKey{
			Type:        typeID,
			ValueCount:  uint16(n),
			TimeStart:   timeStart,
			DataStart:   dataStart,
			ColumnCount: 3,
		})
	}
	addOrientation := func(keyframes []OrientationKey) {
		n := len(keyframes)
		if n == 0 || checkOverflow(n+4*n) {
			return
		}
		timeStart := uint16(len(dataArr))
		for _, kf := range keyframes {
			dataArr = append(dataArr, kf.Time)
		}
		dataStart := uint16(len(dataArr))
		for _, kf := range keyframes {
			// ASCII stores axis-angle; convert to quaternion for binary.
			q := axisAngleToQuat(kf.Value)
			dataArr = append(dataArr, q.X, q.Y, q.Z, q.W)
		}
		keys = append(keys, binCtrlKey{
			Type:        20,
			ValueCount:  uint16(n),
			TimeStart:   timeStart,
			DataStart:   dataStart,
			ColumnCount: 4,
		})
	}
	addDetonate := func(keyframes []FloatKey) {
		n := len(keyframes)
		if n == 0 || checkOverflow(n) {
			return
		}
		timeStart := uint16(len(dataArr))
		for _, kf := range keyframes {
			dataArr = append(dataArr, kf.Time)
		}
		// detonate: ColumnCount=0, no data floats — DataStart == TimeStart+ValueCount,
		// same adjacency convention as every other controller.
		keys = append(keys, binCtrlKey{
			Type:        228, // ID 228 from nodeControllers map
			ValueCount:  uint16(n),
			TimeStart:   timeStart,
			DataStart:   uint16(len(dataArr)),
			ColumnCount: 0,
		})
	}

	// Universal
	addVec3(8, an.PositionKeys)
	addOrientation(an.OrientationKeys)
	addFloat(36, an.ScaleKeys)

	// Mesh controllers
	if nodeFlag&0x20 != 0 {
		addColor(100, an.SelfIllumColorKeys)
		addFloat(128, an.AlphaKeys)
	}

	// Light controllers
	if nodeFlag == 3 {
		addColor(76, an.ColorKeys)
		addFloat(88, an.RadiusKeys)
		addFloat(140, an.MultiplierKeys)
		addFloat(96, an.ShadowRadiusKeys)
		addFloat(100, an.VerticalDisplacementKeys)
	}

	// Emitter controllers (BioWare IDs)
	if nodeFlag == 5 {
		addFloat(84, an.AlphaStartKeys)
		addFloat(448, an.AlphaMidKeys) // BioWare ID
		addFloat(80, an.AlphaEndKeys)
		addFloat(88, an.BirthRateKeys)
		addFloat(204, an.BlurLengthKeys)
		addFloat(92, an.BounceCoKeys)
		addColor(108, an.ColorStartKeys)
		addColor(452, an.ColorMidKeys) // BioWare ID
		addColor(96, an.ColorEndKeys)
		addFloat(120, an.CombineTimeKeys)
		addDetonate(an.DetonateKeys)
		addFloat(124, an.DragKeys)
		addFloat(128, an.FPSKeys)
		addFloat(136, an.FrameStartKeys)
		addFloat(132, an.FrameEndKeys)
		addFloat(140, an.GravKeys)
		addFloat(144, an.LifeExpKeys)
		addFloat(208, an.LightningDelayKeys)
		addFloat(212, an.LightningRadiusKeys)
		addFloat(216, an.LightningScaleKeys)
		if len(an.LightningSubDivKeys) > 0 {
			addFloat(220, an.LightningSubDivKeys)
		}
		addFloat(148, an.MassKeys)
		addFloat(152, an.P2PBezier2Keys)
		addFloat(156, an.P2PBezier3Keys)
		addFloat(160, an.ParticleRotKeys)
		addFloat(464, an.PercentStartKeys) // BioWare ID
		addFloat(465, an.PercentMidKeys)   // BioWare ID
		addFloat(466, an.PercentEndKeys)   // BioWare ID
		addFloat(164, an.RandVelKeys)
		addFloat(168, an.SizeStartKeys)
		addFloat(468, an.SizeMidKeys) // BioWare ID
		addFloat(172, an.SizeEndKeys)
		addFloat(176, an.SizeStartYKeys)
		addFloat(472, an.SizeMidYKeys) // BioWare ID
		addFloat(180, an.SizeEndYKeys)
		addFloat(184, an.SpreadKeys)
		addFloat(188, an.ThresholdKeys)
		addFloat(192, an.VelocityKeys)
		addFloat(196, an.XSizeKeys)
		addFloat(200, an.YSizeKeys)
	}

	return
}

var emitterNameToID map[string]uint32

func init() {
	emitterNameToID = make(map[string]uint32)
	for k, def := range nodeControllers {
		if k.NodeFlag == 5 {
			emitterNameToID[def.Name] = k.TypeID
		}
	}
	for id, def := range biowareEmitterControllers {
		emitterNameToID[def.Name] = id
	}
}

func emitterCtrlID(name string) uint32 {
	return emitterNameToID[name]
}
