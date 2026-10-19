package mdl

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"strings"
	"sync"
)

// CompileOptions tunes a compile. The zero value reproduces Compile.
type CompileOptions struct {
	// SupermodelDirs are searched, in order, for the model's supermodel
	// (<name>.mdl, any letter case). When it is found the model's node numbers
	// follow the engine's rules for numbering against a supermodel; see
	// assignNodeIDsFromSupermodel. With no directories, or no match, nodes are
	// numbered 1..N in tree order, which is only right for a model with no
	// supermodel.
	SupermodelDirs []string

	// ResourceDirs are searched, recursively, for the materials (.mtr) that
	// decide whether a mesh is normal-mapped and so gets tangents baked in; see
	// materials.go. With none, only a mesh's own renderhint line counts.
	ResourceDirs []string

	// Warn, if set, receives non-fatal notes such as a supermodel that could
	// not be found.
	Warn func(msg string)
}

// superNode is one supermodel node as the engine numbered it.
type superNode struct {
	name     string // lower case
	number   int32  // -1 when the engine left the node unnumbered
	children []*superNode
}

// child returns n's first child named name (any letter case), or nil.
func (n *superNode) child(name string) *superNode {
	for _, k := range n.children {
		if k.name == strings.ToLower(name) {
			return k
		}
	}
	return nil
}

// supermodelInfo is what numbering a model against its supermodel needs: the
// supermodel's tree with its node numbers, and its count_nodes.
type supermodelInfo struct {
	path  string
	root  *superNode
	count int32
}

// superTree builds the supermodel's tree from its root, children in file
// order, numbering each node with number.
func superTree(model *Model, number func(*Node) int32) *superNode {
	root := model.RootNode()
	if root == nil {
		return nil
	}
	_, children := resolveGeomTree(model.Nodes)
	visited := make(map[*Node]bool)
	var build func(n *Node) *superNode
	build = func(n *Node) *superNode {
		visited[n] = true
		sn := &superNode{name: strings.ToLower(n.Name), number: number(n)}
		for _, k := range children[n] {
			if !visited[k] {
				sn.children = append(sn.children, build(k))
			}
		}
		return sn
	}
	return build(root)
}

// loadSupermodel reads a supermodel and returns its node numbering.
//
// A binary supermodel carries the numbers and count_nodes the engine compiled
// into it. An ASCII supermodel has none, so it is numbered the way the engine
// would number it: against its own supermodel if that can be found in dirs, and
// otherwise as a model with none — root 0, then tree order, count_nodes equal to
// its node count. Following the chain recursively reproduces the engine's
// count_nodes for the wemic body pmw0 (44 + 1 + 179 = 224 down a four-level
// chain of 44-node ASCII supermodels). seen guards against cycles.
func loadSupermodel(ref *resourceRef, dirs []string, seen map[string]bool) (*supermodelInfo, error) {
	key := ref.id + "\x00" + ref.stamp + "\x00" + strings.Join(dirs, "\x00")
	if v, ok := supermodelCache.Load(key); ok {
		return v.(*supermodelInfo), nil
	}
	info, err := readSupermodel(ref, dirs, seen)
	if err == nil {
		supermodelCache.Store(key, info)
	}
	return info, err
}

// supermodelCache holds loaded supermodels for the life of the process, keyed
// by the resource, its file's size and modification time, and the search path
// its own chain was resolved against. A batch of models that share a skeleton
// then parses it, and the skeleton's own supermodels, once rather than per
// model. Entries are read-only once stored.
var supermodelCache sync.Map

func readSupermodel(ref *resourceRef, dirs []string, seen map[string]bool) (*supermodelInfo, error) {
	path := ref.id
	data, err := ref.read()
	if err != nil {
		return nil, err
	}
	info := &supermodelInfo{path: path}

	if len(data) >= 12 && binary.LittleEndian.Uint32(data[0:4]) == 0 {
		model, err := Decompile(bytes.NewReader(data), int64(len(data)))
		if err != nil {
			return nil, err
		}
		const countNodesOffset = 12 + 8 + 64 + 4 // file prefix + func ptrs + name + root ptr
		if len(data) < countNodesOffset+4 {
			return nil, fmt.Errorf("%s: truncated binary header", path)
		}
		info.count = int32(binary.LittleEndian.Uint32(data[countNodesOffset:]))
		info.root = superTree(model, func(n *Node) int32 { return n.PartNumber })
		return info, nil
	}

	res, err := Parse(bytes.NewReader(data))
	if err != nil || res == nil || res.Model == nil {
		return nil, fmt.Errorf("%s: cannot parse supermodel: %v", path, err)
	}
	model := res.Model
	root := model.RootNode()
	if root == nil {
		return nil, fmt.Errorf("%s: supermodel has no root node", path)
	}
	c := newCompiler(model)
	if parent := findSupermodelFor(model, dirs, seen); parent != nil {
		if sm, err := loadSupermodel(parent, dirs, seen); err == nil {
			c.assignNodeIDsFromSupermodel(root, sm)
		}
	}
	if len(c.nodeIDs) == 0 {
		c.assignNodeIDs(root)
	}
	info.root = superTree(model, func(n *Node) int32 { return c.nodeIDs[n] })
	info.count = int32(len(c.nodeIDs))
	if c.nodeCountOverride > 0 {
		info.count = c.nodeCountOverride
	}
	return info, nil
}

// assignNodeIDsFromSupermodel numbers the geometry tree the way the in-game
// compiler does for a model that has a supermodel. Derived from the engine's own
// compiles of the oracle models (c_marilith2 against c_marilithe), of the
// taur_pheno centaurs (pmh9 against taur_ba) and of probes (nwn-mdl-spec zspec_kid,
// zspec_kid2, zspec_kid3; zcm_kidx in tests/fixtures/engine):
//
//   - The roots match. A node matches the supermodel node at the same path:
//     the child, of the same name in any letter case, of the supermodel node
//     its parent matched. A name the supermodel uses twice is told apart by
//     its path. A matched node takes the supermodel's number, so the
//     supermodel's animations address the right node.
//   - Any other node whose parent matched gets -1.
//   - Any other node gets base + its declaration index, where base is the
//     supermodel's count_nodes + 1.
//
// The header's count_nodes becomes base + the number of nodes in the tree.
func (c *compiler) assignNodeIDsFromSupermodel(root *Node, sm *supermodelInfo) {
	base := sm.count + 1
	decl := c.declarationIndex()
	match := make(map[*Node]*superNode, len(c.model.Nodes))
	var count int32
	c.walkTree(root, func(n, parent *Node, index int32) {
		if n == root {
			c.nodeIDs[n] = 0
			match[n] = sm.root
		} else {
			var sn *superNode
			if p := match[parent]; p != nil {
				sn = p.child(n.Name)
			}
			switch {
			case sn != nil && sn.number >= 0:
				c.nodeIDs[n] = sn.number
				match[n] = sn
			case match[parent] != nil:
				c.nodeIDs[n] = -1
			default:
				c.nodeIDs[n] = base + decl[n]
			}
		}
		c.treeIndex[n] = index
		count = index + 1
	})
	c.nodeCountOverride = base + count
}

// findSupermodelFor returns model's supermodel in dirs, or nil
// if it has none, names itself, was already visited, or cannot be found.
func findSupermodelFor(model *Model, dirs []string, seen map[string]bool) *resourceRef {
	name := strings.ToLower(strings.TrimSpace(model.SuperModel))
	if name == "" || name == "null" || name == strings.ToLower(model.Name) || seen[name] {
		return nil
	}
	seen[name] = true
	return findModelResource(name, dirs)
}

// resolveSupermodel finds and loads the model's supermodel from opts, or
// returns nil to number the nodes without one. A supermodel that was asked for
// but cannot be found or read is reported through opts.Warn.
func (c *compiler) resolveSupermodel(opts CompileOptions) *supermodelInfo {
	name := strings.TrimSpace(c.model.SuperModel)
	if name == "" || strings.EqualFold(name, "NULL") || strings.EqualFold(name, c.model.Name) {
		return nil
	}
	if len(opts.SupermodelDirs) == 0 {
		return nil
	}
	warn := func(format string, args ...any) {
		if opts.Warn != nil {
			opts.Warn(fmt.Sprintf(format, args...))
		}
	}
	ref := findModelResource(name, opts.SupermodelDirs)
	if ref == nil {
		warn("supermodel %q not found in %s; node numbers assume no supermodel and may not match its animations",
			name, strings.Join(opts.SupermodelDirs, ", "))
		return nil
	}
	sm, err := loadSupermodel(ref, opts.SupermodelDirs, map[string]bool{strings.ToLower(c.model.Name): true, strings.ToLower(name): true})
	if err != nil {
		warn("supermodel %q could not be read (%v); node numbers assume no supermodel", name, err)
		return nil
	}
	return sm
}
