// compiler_controllers.go — binControllerKey encoding for the binary MDL compiler.
//
// For geometry nodes: each active field becomes exactly one static controller
// (one keyframe at t=0).  The time array and data array are packed contiguously.
//
// For animation nodes: multi-frame keyframe arrays are packed into shared time
// and data arrays; each controller references its slice via TimeStart/DataStart.
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

// encodeGeomNodeControllers builds controller keys + time/data arrays for a geometry node.
// Geometry nodes store only a single static value (1 keyframe at t=0) per controller.
//
// Returns: (keys, timeArray, dataArray).
// The shared float array passed to the binary is: timeArray ++ dataArray.
// TimeStart and DataStart are indices into the merged array where the decompiler reads.
//
// Ref: binary.go readControllers → d.readControllerRows() for geometry nodes
func (c *compiler) encodeGeomNodeControllers(n *Node) (keys []binCtrlKey, timeArr, dataArr []float32) {
	// Helper: add one static controller.
	add := func(typeID uint32, cols byte, vals []float32) {
		if len(vals) == 0 {
			return
		}
		if len(timeArr)+len(dataArr) >= 65535 {
			if c.err == nil {
				c.err = fmt.Errorf("geometry controller data exceeds uint16 index limit")
			}
			return
		}
		timeStart := uint16(len(timeArr))
		dataStart := uint16(len(dataArr))
		timeArr = append(timeArr, 0.0) // static: one keyframe at t=0
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
	// position (ID=8, 3 cols)
	pos := n.Position
	if pos.X != 0 || pos.Y != 0 || pos.Z != 0 {
		add(8, 3, []float32{pos.X, pos.Y, pos.Z})
	}
	// orientation (ID=20, 4 cols) — ASCII axis-angle → binary quaternion xyzw
	// A zero-length axis (e.g. "0 0 0 1") is a degenerate axis-angle that
	// represents identity rotation regardless of the angle value.
	ori := n.Orientation
	axisZero := ori.X == 0 && ori.Y == 0 && ori.Z == 0
	if !axisZero && ori.W != 0 {
		q := axisAngleToQuat(ori)
		if q.X != 0 || q.Y != 0 || q.Z != 0 || q.W != 1 {
			add(20, 4, []float32{q.X, q.Y, q.Z, q.W})
		}
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

	// DataStart values are relative to the data sub-array; writeCtrlBlock
	// writes [timeArr..., dataArr...] so the reader indexes from the start
	// of the combined array. Offset all DataStart by len(timeArr).
	for i := range keys {
		adjusted := int(keys[i].DataStart) + len(timeArr)
		if adjusted > 65535 {
			if c.err == nil {
				c.err = fmt.Errorf("geometry controller data index overflow: offset %d exceeds uint16 limit", adjusted)
			}
			return nil, nil, nil
		}
		keys[i].DataStart = uint16(adjusted)
	}
	return
}

// encodeAnimNodeControllers builds controller keys + time/data arrays for an animation node.
// Animation nodes can have multi-frame keyframe arrays.
func (c *compiler) encodeAnimNodeControllers(an *AnimNode, nodeFlag uint32) (keys []binCtrlKey, timeArr, dataArr []float32) {
	checkOverflow := func() bool {
		if len(timeArr) > 65535 || len(dataArr) > 65535 || len(timeArr)+len(dataArr) > 65535 {
			if c.err == nil {
				c.err = fmt.Errorf("animation controller data exceeds uint16 index limit (%d time, %d data entries)", len(timeArr), len(dataArr))
			}
			return true
		}
		return false
	}
	addFloat := func(typeID uint32, keyframes []FloatKey) {
		if len(keyframes) == 0 || checkOverflow() {
			return
		}
		timeStart := uint16(len(timeArr))
		dataStart := uint16(len(dataArr))
		for _, kf := range keyframes {
			timeArr = append(timeArr, kf.Time)
			dataArr = append(dataArr, kf.Value)
		}
		keys = append(keys, binCtrlKey{
			Type:        typeID,
			ValueCount:  uint16(len(keyframes)),
			TimeStart:   timeStart,
			DataStart:   dataStart,
			ColumnCount: 1,
		})
	}
	addColor := func(typeID uint32, keyframes []ColorKey) {
		if len(keyframes) == 0 || checkOverflow() {
			return
		}
		timeStart := uint16(len(timeArr))
		dataStart := uint16(len(dataArr))
		for _, kf := range keyframes {
			timeArr = append(timeArr, kf.Time)
			dataArr = append(dataArr, kf.Value.X, kf.Value.Y, kf.Value.Z)
		}
		keys = append(keys, binCtrlKey{
			Type:        typeID,
			ValueCount:  uint16(len(keyframes)),
			TimeStart:   timeStart,
			DataStart:   dataStart,
			ColumnCount: 3,
		})
	}
	addVec3 := func(typeID uint32, keyframes []PositionKey) {
		if len(keyframes) == 0 || checkOverflow() {
			return
		}
		timeStart := uint16(len(timeArr))
		dataStart := uint16(len(dataArr))
		for _, kf := range keyframes {
			timeArr = append(timeArr, kf.Time)
			dataArr = append(dataArr, kf.Value.X, kf.Value.Y, kf.Value.Z)
		}
		keys = append(keys, binCtrlKey{
			Type:        typeID,
			ValueCount:  uint16(len(keyframes)),
			TimeStart:   timeStart,
			DataStart:   dataStart,
			ColumnCount: 3,
		})
	}
	addOrientation := func(keyframes []OrientationKey) {
		if len(keyframes) == 0 || checkOverflow() {
			return
		}
		timeStart := uint16(len(timeArr))
		dataStart := uint16(len(dataArr))
		for _, kf := range keyframes {
			timeArr = append(timeArr, kf.Time)
			// ASCII stores axis-angle; convert to quaternion for binary.
			q := axisAngleToQuat(kf.Value)
			dataArr = append(dataArr, q.X, q.Y, q.Z, q.W)
		}
		keys = append(keys, binCtrlKey{
			Type:        20,
			ValueCount:  uint16(len(keyframes)),
			TimeStart:   timeStart,
			DataStart:   dataStart,
			ColumnCount: 4,
		})
	}
	addDetonate := func(keyframes []FloatKey) {
		if len(keyframes) == 0 || checkOverflow() {
			return
		}
		timeStart := uint16(len(timeArr))
		for _, kf := range keyframes {
			timeArr = append(timeArr, kf.Time)
		}
		// detonate: ColumnCount=0, no data floats.
		keys = append(keys, binCtrlKey{
			Type:        228, // ID 228 from nodeControllers map
			ValueCount:  uint16(len(keyframes)),
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

	for i := range keys {
		adjusted := int(keys[i].DataStart) + len(timeArr)
		if adjusted > 65535 {
			if c.err == nil {
				c.err = fmt.Errorf("animation controller data index overflow: offset %d exceeds uint16 limit", adjusted)
			}
			return nil, nil, nil
		}
		keys[i].DataStart = uint16(adjusted)
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
