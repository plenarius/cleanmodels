package mdl

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"os"
	"path/filepath"
	"strings"
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
	number int32 // -1 when the engine left the node unnumbered
	parent string
}

// supermodelInfo is what numbering a model against its supermodel needs: the
// supermodel's node numbers and its count_nodes.
type supermodelInfo struct {
	path  string
	nodes map[string]superNode // keyed by lower-case node name
	root  string               // lower-case name of the supermodel's root node
	count int32
}

// findSupermodelFile returns the path of <name>.mdl in the first directory that
// has it, comparing file names case-insensitively, or "" if there is none.
func findSupermodelFile(name string, dirs []string) string {
	want := strings.ToLower(name) + ".mdl"
	for _, dir := range dirs {
		entries, err := os.ReadDir(dir)
		if err != nil {
			continue
		}
		for _, e := range entries {
			if !e.IsDir() && strings.ToLower(e.Name()) == want {
				return filepath.Join(dir, e.Name())
			}
		}
	}
	return ""
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
func loadSupermodel(path string, dirs []string, seen map[string]bool) (*supermodelInfo, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	info := &supermodelInfo{path: path, nodes: make(map[string]superNode)}

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
		for _, n := range model.Nodes {
			info.nodes[strings.ToLower(n.Name)] = superNode{number: n.PartNumber, parent: strings.ToLower(n.Parent)}
		}
		if root := model.RootNode(); root != nil {
			info.root = strings.ToLower(root.Name)
		}
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
	if parent := findSupermodelFor(model, dirs, seen); parent != "" {
		if sm, err := loadSupermodel(parent, dirs, seen); err == nil {
			c.assignNodeIDsFromSupermodel(root, sm)
		}
	}
	if len(c.nodeIDs) == 0 {
		c.assignNodeIDs(root)
	}
	for n, id := range c.nodeIDs {
		info.nodes[strings.ToLower(n.Name)] = superNode{number: id, parent: strings.ToLower(n.Parent)}
	}
	info.root = strings.ToLower(root.Name)
	info.count = int32(len(c.nodeIDs))
	if c.nodeCountOverride > 0 {
		info.count = c.nodeCountOverride
	}
	return info, nil
}

// assignNodeIDsFromSupermodel numbers the geometry tree the way the in-game
// compiler does for a model that has a supermodel. Derived from the engine's own
// compiles of the oracle models (c_marilith2 against c_marilithe) and of the
// taur_pheno centaurs (pmh9 against taur_ba), where it reproduced every node
// number:
//
//   - A node that is also in the supermodel — same name, same parent, with the
//     two roots counting as equal parents — takes the supermodel's number, so
//     the supermodel's animations address the right node.
//   - Any other node whose parent is such a node, or the root, gets -1.
//   - Any other node gets base + its position in tree order (root at 0), where
//     base is the supermodel's count_nodes + 1.
//
// The header's count_nodes becomes base + the model's node count.
func (c *compiler) assignNodeIDsFromSupermodel(root *Node, sm *supermodelInfo) {
	base := sm.count + 1
	rootName := strings.ToLower(root.Name)

	type item struct{ n, parent *Node }
	visited := make(map[*Node]bool, len(c.model.Nodes))
	matched := make(map[*Node]bool, len(c.model.Nodes))
	stack := []item{{root, nil}}
	var index int32
	for len(stack) > 0 {
		it := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		n := it.n
		if n == nil || visited[n] {
			continue
		}
		visited[n] = true

		switch {
		case n == root:
			c.nodeIDs[n] = 0
			matched[n] = true
		default:
			name := strings.ToLower(n.Name)
			parent := strings.ToLower(n.Parent)
			sn, ok := sm.nodes[name]
			sameParent := parent == sn.parent || (parent == rootName && sn.parent == sm.root)
			switch {
			case ok && sn.number >= 0 && sameParent:
				c.nodeIDs[n] = sn.number
				matched[n] = true
			case it.parent != nil && matched[it.parent]:
				c.nodeIDs[n] = -1
			default:
				c.nodeIDs[n] = base + index
			}
		}
		c.treeIndex[n] = index
		index++

		children := c.childrenOf(n)
		for i := len(children) - 1; i >= 0; i-- {
			if !visited[children[i]] {
				stack = append(stack, item{children[i], n})
			}
		}
	}
	c.nodeCountOverride = base + index
}

// findSupermodelFor returns the path of model's supermodel file in dirs, or ""
// if it has none, names itself, was already visited, or cannot be found.
func findSupermodelFor(model *Model, dirs []string, seen map[string]bool) string {
	name := strings.ToLower(strings.TrimSpace(model.SuperModel))
	if name == "" || name == "null" || name == strings.ToLower(model.Name) || seen[name] {
		return ""
	}
	seen[name] = true
	return findSupermodelFile(name, dirs)
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
	path := findSupermodelFile(name, opts.SupermodelDirs)
	if path == "" {
		warn("supermodel %q not found in %s; node numbers assume no supermodel and may not match its animations",
			name, strings.Join(opts.SupermodelDirs, ", "))
		return nil
	}
	sm, err := loadSupermodel(path, opts.SupermodelDirs, map[string]bool{strings.ToLower(c.model.Name): true, strings.ToLower(name): true})
	if err != nil {
		warn("supermodel %q could not be read (%v); node numbers assume no supermodel", name, err)
		return nil
	}
	return sm
}
