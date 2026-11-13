// compiler_tree.go — node-identity tree resolution for the binary MDL compiler.
//
// MDL stores parentage by NAME, but node names are not unique in practice.
// BioWare ships models that reuse a name deliberately:
//
//   - bilateral creature rig parts — two "hand" dummies (one under each
//     forearm), four "foot" dummies on many-legged creatures;
//   - the mesh-plus-hook idiom, where a mesh node has a child dummy of the
//     same name ("head" danglymesh containing a "head" dummy);
//   - repeated emitters (vdr_sanctuary has two "omenemitter05").
//
// Those names are load-bearing: the engine attaches items and inherits
// supermodel animations by node name, so renaming a duplicate breaks the rig.
// The compiler therefore has to keep duplicates intact, which means it cannot
// use the name as a node's identity.
//
// Resolution rule: the game's (nwn-mdl-spec MDL-NODE-002). When the engine
// reads a node's `parent` line it looks the name up in the tree built so far
// with FindMdlNode: depth first from the root, each node's children in the
// order they were added, names compared case-insensitively, the first match
// winning. `parent NULL` makes the node the root, and a later one replaces it
// (MDL-NODE-003), so only the last root's tree is written.
//
//	torso_g
//	  armL
//	    hook   <- declared second, but first in tree order
//	  armR
//	    hook   <- declared first
//	  <child>  parent "hook" joins the hook under armL
//
// The nearest node of the name declared before the child is not what the
// game takes: a child declared after armR's hook, or after a node's own
// namesake child ("head" under "head"), still joins the copy the walk
// reaches first.
//
// A parent the lookup does not find crashes the game's compiler. Such a
// source (a child declared before its parent, or one whose parent is in a
// tree a later root dropped) falls back to the nearest node of that name
// declared before it, then to any node of that name, as the old name-keyed
// lookup did.
//
// When every name is unique this rule degenerates to a plain name lookup, so
// well-formed models compile exactly as they did before.
package mdl

import (
	"strings"
	"unicode"
	"unicode/utf8"
)

// treeLookup is the game's parent lookup over a list of nodes read in
// declaration order: each node is added under the node its parent names, as
// FindMdlNode finds it in the tree built so far.
type treeLookup[T comparable] struct {
	name     func(T) string
	root     T
	hasRoot  bool
	moved    bool // a later root replaced an earlier one
	kids     map[T][]T
	attached map[string][]T // lower-case name → nodes of that name added so far
}

func newTreeLookup[T comparable](name func(T) string, n int) *treeLookup[T] {
	return &treeLookup[T]{name: name, kids: make(map[T][]T, n), attached: make(map[string][]T, n)}
}

// setRoot makes n the tree's root, as `parent NULL` does.
func (t *treeLookup[T]) setRoot(n T) {
	if t.hasRoot {
		t.moved = true
	}
	t.root, t.hasRoot = n, true
	key := foldKey(t.name(n))
	t.attached[key] = append(t.attached[key], n)
}

// add records n as a child of parent.
func (t *treeLookup[T]) add(parent, n T) {
	t.kids[parent] = append(t.kids[parent], n)
	key := foldKey(t.name(n))
	t.attached[key] = append(t.attached[key], n)
}

// foldKey returns a key two names share exactly when strings.EqualFold says
// they are equal: each rune replaced by the smallest of its case-folding
// orbit.
func foldKey(s string) string {
	ascii := true
	for i := 0; i < len(s); i++ {
		if s[i] >= utf8.RuneSelf {
			ascii = false
			break
		}
	}
	if ascii {
		return strings.ToUpper(s) // an ASCII letter's orbit starts at its upper case
	}
	var b strings.Builder
	for _, r := range s {
		m := r
		for f := unicode.SimpleFold(r); f != r; f = unicode.SimpleFold(f) {
			if f < m {
				m = f
			}
		}
		b.WriteRune(m)
	}
	return b.String()
}

// find returns the first node named name in the tree under the root, depth
// first, children in the order they were added (FindMdlNode).
func (t *treeLookup[T]) find(name string) (T, bool) {
	var zero T
	if !t.hasRoot {
		return zero, false
	}
	cands := t.attached[foldKey(name)]
	if len(cands) == 0 {
		return zero, false
	}
	if len(cands) == 1 && !t.moved {
		// Every added node hangs under the one root: the only node of that
		// name is the one the walk finds.
		return cands[0], true
	}
	stack := []T{t.root}
	for len(stack) > 0 {
		n := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		if strings.EqualFold(t.name(n), name) {
			return n, true
		}
		kids := t.kids[n]
		for i := len(kids) - 1; i >= 0; i-- {
			stack = append(stack, kids[i])
		}
	}
	return zero, false
}

// resolveGeomTree returns the parent of each node and each node's children,
// both keyed by node identity rather than name, resolved the game's way
// (MDL-NODE-002). Child order follows the declaration order of nodes.
//
// A node whose Parent does not resolve (a root, "NULL", no parent line, or a
// dangling reference) is absent from parentOf.
func resolveGeomTree(nodes []*Node) (parentOf map[*Node]*Node, childrenOf map[*Node][]*Node) {
	parentOf = make(map[*Node]*Node, len(nodes))
	t := newTreeLookup(func(n *Node) string { return n.Name }, len(nodes))
	for i, n := range nodes {
		if n == nil || n.Parent == "" {
			continue
		}
		if isRootParent(n.Parent) {
			t.setRoot(n)
			continue
		}
		parent, ok := t.find(n.Parent)
		if !ok {
			// The game's compiler crashes here. Fall back to the nearest
			// node of that name declared before, then to any.
			for j := i - 1; j >= 0 && parent == nil; j-- {
				if nodes[j] != nil && nodes[j] != n && strings.EqualFold(nodes[j].Name, n.Parent) {
					parent = nodes[j]
				}
			}
			for _, cand := range nodes {
				if parent != nil {
					break
				}
				if cand != nil && cand != n && strings.EqualFold(cand.Name, n.Parent) {
					parent = cand
				}
			}
			if parent == nil {
				continue
			}
		}
		parentOf[n] = parent
		t.add(parent, n)
	}
	return parentOf, t.kids
}

// resolveAnimTree is resolveGeomTree for an animation's node list, which the
// engine reads the same way (its own tree). Returned pointers alias elements
// of nodes, so the caller must not append to the slice while the maps are in
// use.
func resolveAnimTree(nodes []AnimNode) (parentOf map[*AnimNode]*AnimNode, childrenOf map[*AnimNode][]*AnimNode) {
	parentOf = make(map[*AnimNode]*AnimNode, len(nodes))
	t := newTreeLookup(func(n *AnimNode) string { return n.Name }, len(nodes))
	for i := range nodes {
		n := &nodes[i]
		if n.Parent == "" {
			continue
		}
		if isRootParent(n.Parent) {
			t.setRoot(n)
			continue
		}
		parent, ok := t.find(n.Parent)
		if !ok {
			for j := i - 1; j >= 0 && parent == nil; j-- {
				if cand := &nodes[j]; cand != n && strings.EqualFold(cand.Name, n.Parent) {
					parent = cand
				}
			}
			for j := range nodes {
				if parent != nil {
					break
				}
				if cand := &nodes[j]; cand != n && strings.EqualFold(cand.Name, n.Parent) {
					parent = cand
				}
			}
			if parent == nil {
				continue
			}
		}
		parentOf[n] = parent
		t.add(parent, n)
	}
	return parentOf, t.kids
}

// nodeOccurrences groups nodes by lowercased name, preserving declaration
// order, so the Nth animation node named X can be paired with the Nth geometry
// node named X.
func nodeOccurrences(nodes []*Node) map[string][]*Node {
	out := make(map[string][]*Node, len(nodes))
	for _, n := range nodes {
		if n == nil {
			continue
		}
		key := strings.ToLower(n.Name)
		out[key] = append(out[key], n)
	}
	return out
}

// pairAnimNodesToGeom maps each animation node to the geometry node it drives.
// Names are matched by occurrence: the Nth anim node named X pairs with the Nth
// geometry node named X. Surplus anim nodes fall back to the last geometry
// occurrence, which is what a single-geometry-node model always did.
func pairAnimNodesToGeom(anim []AnimNode, geomOccur map[string][]*Node) map[*AnimNode]*Node {
	out := make(map[*AnimNode]*Node, len(anim))
	seen := make(map[string]int, len(anim))
	for i := range anim {
		an := &anim[i]
		key := strings.ToLower(an.Name)
		occ := geomOccur[key]
		if len(occ) == 0 {
			continue
		}
		idx := seen[key]
		seen[key]++
		if idx >= len(occ) {
			idx = len(occ) - 1
		}
		out[an] = occ[idx]
	}
	return out
}

// isRootParent reports whether a Parent field denotes "no parent".
func isRootParent(parent string) bool {
	return parent == "" || strings.EqualFold(parent, "NULL")
}

// ResolveNodeParents returns each geometry node's parent instance, resolved by
// tree position rather than by name. Nodes with no resolvable parent (the root,
// or a dangling Parent reference) are absent from the result.
//
// Exported for the checks package: anything that renames one of several
// same-named nodes first has to know which of them a given child actually
// hangs off, or it will repoint the wrong subtree.
func ResolveNodeParents(nodes []*Node) map[*Node]*Node {
	parentOf, _ := resolveGeomTree(nodes)
	return parentOf
}

// ResolveAnimNodeParents is ResolveNodeParents for an animation's node list.
// Returned pointers alias elements of nodes, so the caller must not append to
// the slice while the map is in use.
func ResolveAnimNodeParents(nodes []AnimNode) map[*AnimNode]*AnimNode {
	parentOf, _ := resolveAnimTree(nodes)
	return parentOf
}

// animNodeNumbers gives each node of an animation the part number the in-game
// compiler writes for it (nwn-mdl-spec MDL-NODE-040, probe zspec_animnum).
// The animation's tree is walked against the model's own geometry tree, by
// path: the roots match, and a node matches the child of the same name (in any
// letter case) of the geometry node its parent matched. A matched node takes
// that geometry node's number. A node the walk does not find gets -1 when its
// parent matched, and otherwise its declaration index in the animation. A name
// the geometry has under another parent does not match.
//
// The engine binds animation nodes to model nodes by number (MDL-NODE-043), so
// a node the geometry lacks must not take the root's 0, or its keys would move
// the whole model.
func (c *compiler) animNodeNumbers(nodes []AnimNode, root *AnimNode) map[*AnimNode]int32 {
	parentOf, _ := resolveAnimTree(nodes)
	geomRoot := c.model.RootNode()
	out := make(map[*AnimNode]int32, len(nodes))
	match := make(map[*AnimNode]*Node, len(nodes))
	done := make(map[*AnimNode]bool, len(nodes))
	decl := make(map[*AnimNode]int32, len(nodes))
	for i := range nodes {
		decl[&nodes[i]] = int32(i)
	}
	var number func(an *AnimNode) int32
	number = func(an *AnimNode) int32 {
		if done[an] {
			return out[an]
		}
		done[an] = true // set first: a parent cycle ends here as unmatched
		out[an] = decl[an]
		var g *Node
		parentMatched := false
		if an == root {
			g = geomRoot
		} else if p := parentOf[an]; p != nil {
			number(p)
			if pg := match[p]; pg != nil {
				parentMatched = true
				for _, ch := range c.childrenOf(pg) {
					if strings.EqualFold(ch.Name, an.Name) {
						g = ch
						break
					}
				}
			}
		}
		if g != nil {
			if id, ok := c.nodeIDs[g]; ok {
				match[an] = g
				out[an] = id
				return id
			}
		}
		if parentMatched {
			out[an] = -1
		}
		return out[an]
	}
	for i := range nodes {
		number(&nodes[i])
	}
	return out
}
