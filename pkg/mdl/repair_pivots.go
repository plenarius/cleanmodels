package mdl

import (
	"fmt"
	"math"
)

// Half-space constraint for pivot search: either n·p >= d or n·p <= d.
// Ref: fix_pivots.pl constraint accumulation for tile bounds and boundary faces.
type pivotHalfSpace struct {
	n  Vec3
	d  float32
	ge bool // true => n·p >= d; false => n·p <= d
}

const (
	pivotTileHalf     = float32(5)    // walkmesh XY in [-5, 5]
	pivotSnapGrid     = float32(0.01) // top-centre snap (f_find_pivot grid snap)
	pivotTol          = float32(2e-4)
	pivotZBand        = float32(0.002) // vertices within this of max Z count as "top"
	pivotBoundaryBand = float32(0.06)  // tile-edge band for boundary faces
	pivotPlaneNZEps   = float32(1e-5)
)

// RepairPivots finds and sets valid pivot points for AABB (walkmesh) nodes.
// A valid pivot must satisfy half-space constraints derived from tile boundaries
// and face normals. For each AABB node, the function tries (in order):
//  1. Top-centre of the geometry (highest Z, centred in XY)
//  2. Centroid of all face centroids
//  3. Iterative bisection within the constraint bounding box
//
// Ref: fix_pivots.pl f_find_pivot/5
func RepairPivots(model *Model) []string {
	if model == nil {
		return nil
	}
	var out []string
	for _, node := range model.Nodes {
		if node == nil || node.Aabb == nil {
			continue
		}
		if node.Mesh == nil || len(node.Mesh.Verts) == 0 || len(node.Mesh.Faces) == 0 {
			out = append(out, fmt.Sprintf("node %q: skipped (no walkmesh geometry)", node.Name))
			continue
		}
		mesh := node.Mesh
		cons := buildPivotConstraints(mesh)
		boxMin, boxMax := pivotSearchBox(mesh)

		var pivot Vec3
		var method string

		top := pivotTopCentre(mesh)
		if pivotSatisfies(top, cons) {
			pivot, method = top, "top-centre"
		} else {
			fidx := make([]int32, len(mesh.Faces))
			for i := range fidx {
				fidx[i] = int32(i)
			}
			cent := centroidAverage(mesh, fidx)
			if pivotSatisfies(cent, cons) {
				pivot, method = cent, "face-centroid average"
			} else if p, ok := pivotByBoxBisection(boxMin, boxMax, cons); ok {
				pivot, method = p, "bounding-box bisection"
			} else if p, ok := pivotRelaxInBox(boxMin, boxMax, cons); ok {
				pivot, method = p, "constraint relaxation"
			} else {
				// Last resort: clamp top-centre into the search box (may violate face constraints)
				pivot = clampToBox(top, boxMin, boxMax)
				method = "fallback (clamped top-centre)"
				out = append(out, fmt.Sprintf(
					"node %q: warning — no point satisfied all constraints; %s",
					node.Name, method))
				node.Position = pivot
				continue
			}
		}

		node.Position = pivot
		out = append(out, fmt.Sprintf(
			"node %q: pivot set to [%.4f, %.4f, %.4f] via %s",
			node.Name, pivot.X, pivot.Y, pivot.Z, method))
	}
	return out
}

// buildPivotConstraints collects tile slab planes, z>=0, and inward half-spaces
// from boundary walkmesh faces (vertices near |x|=5 or |y|=5).
// Ref: fix_pivots.pl pivot constraint setup.
func buildPivotConstraints(mesh *MeshData) []pivotHalfSpace {
	var hs []pivotHalfSpace
	// Tile planes: interior is [-tileHalf, tileHalf] on X and Y.
	// x >= -pivotTileHalf  =>  (1,0,0)·p >= -pivotTileHalf
	hs = append(hs, pivotHalfSpace{n: Vec3{X: 1}, d: -pivotTileHalf, ge: true})
	// x <= pivotTileHalf  =>  (-1,0,0)·p >= -pivotTileHalf
	hs = append(hs, pivotHalfSpace{n: Vec3{X: -1}, d: -pivotTileHalf, ge: true})
	hs = append(hs, pivotHalfSpace{n: Vec3{Y: 1}, d: -pivotTileHalf, ge: true})
	hs = append(hs, pivotHalfSpace{n: Vec3{Y: -1}, d: -pivotTileHalf, ge: true})
	// Optional floor: pivot at or above z = 0 (tile walkmesh convention).
	hs = append(hs, pivotHalfSpace{n: Vec3{Z: 1}, d: 0, ge: true})

	for fi := range mesh.Faces {
		if !isBoundaryWalkmeshFace(mesh, int32(fi)) {
			continue
		}
		f := mesh.Faces[fi]
		v0, ok := pivotVert(mesh, f.Verts[0])
		if !ok {
			continue
		}
		fmin, fmax, err := faceBounds(mesh, int32(fi))
		if err != nil {
			continue
		}
		n := outwardBoundaryNormal(mesh, int32(fi), fmin, fmax)
		if vecLen(n) < pivotPlaneNZEps {
			continue
		}
		// Outward n: interior is n·p <= n·v0  (pivot must see interior from adjacent tiles).
		d := vecDot(n, v0)
		hs = append(hs, pivotHalfSpace{n: n, d: d, ge: false})
	}
	return hs
}

func pivotVert(mesh *MeshData, idx int32) (Vec3, bool) {
	if idx < 0 || int(idx) >= len(mesh.Verts) {
		return Vec3{}, false
	}
	return mesh.Verts[idx], true
}

func isBoundaryWalkmeshFace(mesh *MeshData, fi int32) bool {
	f := mesh.Faces[fi]
	b := pivotBoundaryBand
	for _, vi := range f.Verts {
		v, ok := pivotVert(mesh, vi)
		if !ok {
			continue
		}
		if v.X >= pivotTileHalf-b || v.X <= -pivotTileHalf+b {
			return true
		}
		if v.Y >= pivotTileHalf-b || v.Y <= -pivotTileHalf+b {
			return true
		}
	}
	return false
}

// outwardBoundaryNormal aligns the face normal with the exterior of the tile
// slab on whichever side the face sits, so n·p <= n·v0 describes the walkable side.
func outwardBoundaryNormal(mesh *MeshData, fi int32, fmin, fmax Vec3) Vec3 {
	f := mesh.Faces[fi]
	n := vecNormalize(f.Normal)
	if vecLen(n) < pivotPlaneNZEps {
		n = pivotFaceCrossNormal(mesh, fi)
	}
	if vecLen(n) < pivotPlaneNZEps {
		return Vec3{}
	}
	cx := (fmin.X + fmax.X) * 0.5
	cy := (fmin.Y + fmax.Y) * 0.5
	b := pivotBoundaryBand
	// +X wall
	if fmax.X >= pivotTileHalf-b && cx >= 0 {
		if n.X < 0 {
			n = vecScale(n, -1)
		}
	}
	// -X wall
	if fmin.X <= -pivotTileHalf+b && cx <= 0 {
		if n.X > 0 {
			n = vecScale(n, -1)
		}
	}
	// +Y wall
	if fmax.Y >= pivotTileHalf-b && cy >= 0 {
		if n.Y < 0 {
			n = vecScale(n, -1)
		}
	}
	// -Y wall
	if fmin.Y <= -pivotTileHalf+b && cy <= 0 {
		if n.Y > 0 {
			n = vecScale(n, -1)
		}
	}
	return vecNormalize(n)
}

func pivotFaceCrossNormal(mesh *MeshData, fi int32) Vec3 {
	f := mesh.Faces[fi]
	p0, ok0 := pivotVert(mesh, f.Verts[0])
	p1, ok1 := pivotVert(mesh, f.Verts[1])
	p2, ok2 := pivotVert(mesh, f.Verts[2])
	if !ok0 || !ok1 || !ok2 {
		return Vec3{}
	}
	e1 := Vec3{X: p1.X - p0.X, Y: p1.Y - p0.Y, Z: p1.Z - p0.Z}
	e2 := Vec3{X: p2.X - p0.X, Y: p2.Y - p0.Y, Z: p2.Z - p0.Z}
	c := vecCross(e1, e2)
	if vecLen(c) > pivotPlaneNZEps {
		return vecNormalize(c)
	}
	return vecNormalize(f.Normal)
}

func pivotSearchBox(mesh *MeshData) (minB, maxB Vec3) {
	if len(mesh.Verts) == 0 {
		return Vec3{X: -pivotTileHalf, Y: -pivotTileHalf}, Vec3{X: pivotTileHalf, Y: pivotTileHalf, Z: 32}
	}
	minB = mesh.Verts[0]
	maxB = mesh.Verts[0]
	for i := 1; i < len(mesh.Verts); i++ {
		minB = vecMin(minB, mesh.Verts[i])
		maxB = vecMax(maxB, mesh.Verts[i])
	}
	minB.X = maxF(minB.X, -pivotTileHalf)
	minB.Y = maxF(minB.Y, -pivotTileHalf)
	maxB.X = minF(maxB.X, pivotTileHalf)
	maxB.Y = minF(maxB.Y, pivotTileHalf)
	if minB.Z < 0 {
		minB.Z = 0
	}
	return minB, maxB
}

// pivotTopCentre is the Prolog "top centre" candidate: mean XY of highest-Z vertices,
// Z at roof, XY snapped to grid. Ref: fix_pivots.pl f_find_pivot top-centre try.
func pivotTopCentre(mesh *MeshData) Vec3 {
	if len(mesh.Verts) == 0 {
		return Vec3{}
	}
	zmax := mesh.Verts[0].Z
	for i := 1; i < len(mesh.Verts); i++ {
		zmax = maxF(zmax, mesh.Verts[i].Z)
	}
	var sx, sy float32
	var n int
	for i := range mesh.Verts {
		v := mesh.Verts[i]
		if v.Z >= zmax-pivotZBand {
			sx += v.X
			sy += v.Y
			n++
		}
	}
	if n == 0 {
		return snapPivotGridXY(Vec3{Z: zmax}, pivotSnapGrid)
	}
	out := Vec3{
		X: sx / float32(n),
		Y: sy / float32(n),
		Z: zmax,
	}
	// Prolog snaps XY to the tile grid; Z stays at roof height so roof half-spaces stay valid.
	return snapPivotGridXY(out, pivotSnapGrid)
}

func snapPivotGridXY(v Vec3, cell float32) Vec3 {
	if cell <= 0 {
		return v
	}
	c := float64(cell)
	return Vec3{
		X: float32(math.Round(float64(v.X)/c) * c),
		Y: float32(math.Round(float64(v.Y)/c) * c),
		Z: v.Z,
	}
}

func pivotSatisfies(p Vec3, hs []pivotHalfSpace) bool {
	for i := range hs {
		c := &hs[i]
		v := vecDot(c.n, p)
		if c.ge {
			if v < c.d-pivotTol {
				return false
			}
		} else {
			if v > c.d+pivotTol {
				return false
			}
		}
	}
	return true
}

// pivotByBoxBisection refines an XY sub-box; at each sample it solves the feasible
// Z interval implied by linear constraints (fix_pivots.pl bisection search in the slab).
func pivotByBoxBisection(minB, maxB Vec3, hs []pivotHalfSpace) (Vec3, bool) {
	const maxDepth = 14
	const minSpan = float32(0.02)
	return pivotBisectXY(minB.X, maxB.X, minB.Y, maxB.Y, minB.Z, maxB.Z, hs, 0, maxDepth, minSpan)
}

func pivotBisectXY(xmin, xmax, ymin, ymax, zmin, zmax float32, hs []pivotHalfSpace, depth, maxDepth int, minSpan float32) (Vec3, bool) {
	xc := (xmin + xmax) * 0.5
	yc := (ymin + ymax) * 0.5
	if z, ok := pivotSolveZ(xc, yc, zmin, zmax, hs); ok {
		return Vec3{X: xc, Y: yc, Z: z}, true
	}
	if depth >= maxDepth {
		return Vec3{}, false
	}
	if xmax-xmin < minSpan && ymax-ymin < minSpan {
		return Vec3{}, false
	}
	xmid := (xmin + xmax) * 0.5
	ymid := (ymin + ymax) * 0.5
	quads := [][4]float32{
		{xmin, xmid, ymin, ymid},
		{xmid, xmax, ymin, ymid},
		{xmin, xmid, ymid, ymax},
		{xmid, xmax, ymid, ymax},
	}
	for _, q := range quads {
		if q[1]-q[0] < minSpan*0.5 && q[3]-q[2] < minSpan*0.5 {
			continue
		}
		if p, ok := pivotBisectXY(q[0], q[1], q[2], q[3], zmin, zmax, hs, depth+1, maxDepth, minSpan); ok {
			return p, true
		}
	}
	return Vec3{}, false
}

// pivotSolveZ returns a feasible z at (x0,y0) intersecting [zmin,zmax], if any.
func pivotSolveZ(x0, y0, zmin, zmax float32, hs []pivotHalfSpace) (float32, bool) {
	lo, hi := zmin, zmax
	for i := range hs {
		c := &hs[i]
		rhs := c.d - c.n.X*x0 - c.n.Y*y0
		nz := c.n.Z
		if absF32(nz) < pivotPlaneNZEps {
			// Constraint independent of z: check (nx*x0+ny*y0) vs d.
			lhs := c.n.X*x0 + c.n.Y*y0
			if c.ge {
				if lhs < c.d-pivotTol {
					return 0, false
				}
			} else {
				if lhs > c.d+pivotTol {
					return 0, false
				}
			}
			continue
		}
		if c.ge {
			// nz*z >= rhs
			if nz > 0 {
				t := rhs / nz
				lo = maxF(lo, t)
			} else {
				t := rhs / nz
				hi = minF(hi, t)
			}
		} else {
			// nz*z <= rhs
			if nz > 0 {
				t := rhs / nz
				hi = minF(hi, t)
			} else {
				t := rhs / nz
				lo = maxF(lo, t)
			}
		}
		if lo > hi+pivotTol {
			return 0, false
		}
	}
	if lo > hi+pivotTol {
		return 0, false
	}
	return (lo + hi) * 0.5, true
}

// pivotRelaxInBox projects toward feasible half-spaces from the box centre when
// bisection finds no exact Z interval at sampled XY (degenerate or tight feasible set).
// Ref: fix_pivots.pl f_find_pivot fallback iterations.
func pivotRelaxInBox(minB, maxB Vec3, hs []pivotHalfSpace) (Vec3, bool) {
	p := Vec3{
		X: (minB.X + maxB.X) * 0.5,
		Y: (minB.Y + maxB.Y) * 0.5,
		Z: (minB.Z + maxB.Z) * 0.5,
	}
	for iter := 0; iter < 256; iter++ {
		p = clampToBox(p, minB, maxB)
		if pivotSatisfies(p, hs) {
			return p, true
		}
		moved := false
		for i := range hs {
			c := &hs[i]
			v := vecDot(c.n, p)
			dn := vecDot(c.n, c.n)
			if dn < pivotPlaneNZEps*pivotPlaneNZEps {
				continue
			}
			if c.ge && v < c.d {
				t := (c.d - v) / dn
				p.X += c.n.X * t
				p.Y += c.n.Y * t
				p.Z += c.n.Z * t
				moved = true
			} else if !c.ge && v > c.d {
				t := (c.d - v) / dn
				p.X += c.n.X * t
				p.Y += c.n.Y * t
				p.Z += c.n.Z * t
				moved = true
			}
		}
		if !moved {
			break
		}
	}
	p = clampToBox(p, minB, maxB)
	if pivotSatisfies(p, hs) {
		return p, true
	}
	return Vec3{}, false
}

func clampToBox(p, minB, maxB Vec3) Vec3 {
	return Vec3{
		X: minF(maxF(p.X, minB.X), maxB.X),
		Y: minF(maxF(p.Y, minB.Y), maxB.Y),
		Z: minF(maxF(p.Z, minB.Z), maxB.Z),
	}
}

