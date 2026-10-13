package mdl

import "math"

// skinBindPatch records where a skin header's bind-pose lists are patched in.
type skinBindPatch struct {
	boneIndexPtr, boneIndexSize int
	lists                       [3][3]int // qbone_ref_inv, tbone_ref_inv, boneconstantindices: ptr, count, alloc
	boneTree                    []int16   // bone index -> tree position
}

// writeSkinBindData writes a skin node's bind-pose data, one entry per node of
// the model in tree order:
//
//   - boneindexarray: the bone index that tree position maps to, or -1;
//   - qbone_ref_inv / tbone_ref_inv: the rotation (w, x, y, z) and translation
//     taking the skin node's space into that node's space,
//     conj(Rnode) * Rskin and conj(Rnode) * (Pskin - Pnode) in world terms;
//   - boneconstantindices: one int per node. The engine writes uninitialised
//     memory here and sets it at load; we write zeros.
//
// The engine reads the two bind lists while rendering (ExecuteSkinGPU). Leaving
// them empty, as we used to, makes it read past the end of an empty list: an
// access violation as soon as the skin is drawn. Layout and maths were checked
// against the in-game compiler's output for five skins (wemic pmw0, centaur
// pmh9); every entry of both lists matched.
func (c *compiler) writeSkinBindData(n *Node) {
	order := make([]*Node, len(c.treeIndex))
	for node, idx := range c.treeIndex {
		if int(idx) < len(order) {
			order[idx] = node
		}
	}
	count := len(order)
	if count == 0 {
		return
	}
	if c.worldRot == nil {
		c.computeWorldTransforms()
	}

	boneOf := make([]int16, count)
	for i := range boneOf {
		boneOf[i] = -1
	}
	for bone, tree := range c.skinBind.boneTree {
		if tree >= 0 && int(tree) < count {
			boneOf[tree] = int16(bone)
		}
	}
	c.core.patchU32(c.skinBind.boneIndexPtr, uint32(c.core.len()))
	c.core.patchU32(c.skinBind.boneIndexSize, uint32(count))
	for _, b := range boneOf {
		c.core.u16le(uint16(b))
	}
	if count%2 == 1 {
		c.core.u16le(0) // keep the following lists 4-byte aligned
	}

	startList := func(i int) {
		c.core.patchU32(c.skinBind.lists[i][0], uint32(c.core.len()))
		c.core.patchU32(c.skinBind.lists[i][1], uint32(count))
		c.core.patchU32(c.skinBind.lists[i][2], uint32(count))
	}
	skinPos, skinRot := c.worldPos[n], c.worldRot[n]

	startList(0)
	for _, node := range order {
		q := quatMul(quatConj(c.worldRot[node]), skinRot)
		c.core.f32le(float32(q[3]))
		c.core.f32le(float32(q[0]))
		c.core.f32le(float32(q[1]))
		c.core.f32le(float32(q[2]))
	}
	startList(1)
	for _, node := range order {
		p := c.worldPos[node]
		t := quatRotate(quatConj(c.worldRot[node]), [3]float64{skinPos[0] - p[0], skinPos[1] - p[1], skinPos[2] - p[2]})
		c.core.f32le(float32(t[0]))
		c.core.f32le(float32(t[1]))
		c.core.f32le(float32(t[2]))
	}
	startList(2)
	for range order {
		c.core.i32le(0)
	}
}

// computeWorldTransforms accumulates each geometry node's position and
// orientation down the tree.
func (c *compiler) computeWorldTransforms() {
	c.worldPos = make(map[*Node][3]float64, len(c.treeIndex))
	c.worldRot = make(map[*Node][4]float64, len(c.treeIndex))
	root := c.model.RootNode()
	if root == nil {
		return
	}
	type item struct{ n, parent *Node }
	stack := []item{{root, nil}}
	for len(stack) > 0 {
		it := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		if _, done := c.worldRot[it.n]; done {
			continue
		}
		lq := axisAngleToQuat(it.n.Orientation)
		local := [4]float64{float64(lq.X), float64(lq.Y), float64(lq.Z), float64(lq.W)}
		lp := [3]float64{float64(it.n.Position.X), float64(it.n.Position.Y), float64(it.n.Position.Z)}
		if it.parent == nil {
			c.worldPos[it.n], c.worldRot[it.n] = lp, quatNormalize(local)
		} else {
			pp, pq := c.worldPos[it.parent], c.worldRot[it.parent]
			r := quatRotate(pq, lp)
			c.worldPos[it.n] = [3]float64{pp[0] + r[0], pp[1] + r[1], pp[2] + r[2]}
			c.worldRot[it.n] = quatNormalize(quatMul(pq, local))
		}
		for _, child := range c.childrenOf(it.n) {
			stack = append(stack, item{child, it.n})
		}
	}
}

// Quaternions here are x, y, z, w.
func quatMul(a, b [4]float64) [4]float64 {
	return [4]float64{
		a[3]*b[0] + a[0]*b[3] + a[1]*b[2] - a[2]*b[1],
		a[3]*b[1] - a[0]*b[2] + a[1]*b[3] + a[2]*b[0],
		a[3]*b[2] + a[0]*b[1] - a[1]*b[0] + a[2]*b[3],
		a[3]*b[3] - a[0]*b[0] - a[1]*b[1] - a[2]*b[2],
	}
}

func quatConj(q [4]float64) [4]float64 { return [4]float64{-q[0], -q[1], -q[2], q[3]} }

func quatRotate(q [4]float64, v [3]float64) [3]float64 {
	r := quatMul(quatMul(q, [4]float64{v[0], v[1], v[2], 0}), quatConj(q))
	return [3]float64{r[0], r[1], r[2]}
}

func quatNormalize(q [4]float64) [4]float64 {
	l := math.Sqrt(q[0]*q[0] + q[1]*q[1] + q[2]*q[2] + q[3]*q[3])
	if l == 0 {
		return [4]float64{0, 0, 0, 1}
	}
	return [4]float64{q[0] / l, q[1] / l, q[2] / l, q[3] / l}
}
