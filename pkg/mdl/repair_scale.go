package mdl

import (
	"strings"
)

func scaleModelCore(model *Model, sx, sy, sz float32) {
	for _, n := range model.Nodes {
		if n == nil {
			continue
		}
		if n.Parent != "" && !strings.EqualFold(n.Parent, "NULL") {
			n.Position = Vec3{X: n.Position.X * sx, Y: n.Position.Y * sy, Z: n.Position.Z * sz}
		}
		if n.Mesh != nil {
			for i := range n.Mesh.Verts {
				v := &n.Mesh.Verts[i]
				v.X *= sx
				v.Y *= sy
				v.Z *= sz
			}
		}
		if n.AnimMesh != nil {
			for i := range n.AnimMesh.AnimVerts {
				v := &n.AnimMesh.AnimVerts[i]
				v.X *= sx
				v.Y *= sy
				v.Z *= sz
			}
		}
		if n.Light != nil {
			avg := (sx + sy + sz) / 3
			n.Light.Radius *= avg
			n.Light.ShadowRadius *= avg
			n.Light.VerticalDisplacement *= sz
			n.Light.FlareRadius *= avg
		}
		if n.Emitter != nil {
			avg := (sx + sy + sz) / 3
			n.Emitter.BlastRadius *= avg
			n.Emitter.BlastLength *= avg
			n.Emitter.DeadSpace *= avg
		}
		if n.Aabb != nil {
			for i := range n.Aabb.Entries {
				e := &n.Aabb.Entries[i]
				e.BoundMin = Vec3{X: e.BoundMin.X * sx, Y: e.BoundMin.Y * sy, Z: e.BoundMin.Z * sz}
				e.BoundMax = Vec3{X: e.BoundMax.X * sx, Y: e.BoundMax.Y * sy, Z: e.BoundMax.Z * sz}
				for axis := 0; axis < 3; axis++ {
					minP, maxP := e.BoundMin.Index(axis), e.BoundMax.Index(axis)
					if minP > maxP {
						e.BoundMin.SetIndex(axis, maxP)
						e.BoundMax.SetIndex(axis, minP)
					}
				}
			}
		}
	}

	for ai := range model.Animations {
		for ni := range model.Animations[ai].Nodes {
			an := &model.Animations[ai].Nodes[ni]
			for ki := range an.PositionKeys {
				v := &an.PositionKeys[ki].Value
				v.X *= sx
				v.Y *= sy
				v.Z *= sz
			}
			if an.Mesh != nil {
				for i := range an.Mesh.Verts {
					v := &an.Mesh.Verts[i]
					v.X *= sx
					v.Y *= sy
					v.Z *= sz
				}
			}
			if an.AnimMesh != nil {
				for i := range an.AnimMesh.AnimVerts {
					v := &an.AnimMesh.AnimVerts[i]
					v.X *= sx
					v.Y *= sy
					v.Z *= sz
				}
			}
		}
	}

	avg := (sx + sy + sz) / 3
	model.AnimationScale *= avg
}

// ScaleModelPerAxis scales each axis independently. AnimationScale is set
// to the arithmetic mean of the three factors.
func ScaleModelPerAxis(model *Model, sx, sy, sz float32) {
	scaleModelCore(model, sx, sy, sz)
}

// ScaleModel multiplies all vertex positions and animation position keys
// by the given factor, and adjusts AnimationScale accordingly.
// AABB bounding boxes are also scaled; however, for best results use
// --fix-aabb to rebuild them from the scaled geometry.
func ScaleModel(model *Model, factor float32) {
	scaleModelCore(model, factor, factor, factor)
}
