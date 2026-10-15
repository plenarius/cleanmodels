package mdl

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"sort"
	"strings"
	"sync"
)

// A skinned body part (pmh0_neck001, pfe0_chest003, ...) is not bound to its
// own node tree. The engine attaches the part to the creature, walks up to the
// creature's skeleton and binds the skin there, so the skin's bone map holds
// tree positions in the skeleton (pmh0), not in the part file. The engine's
// compile of the HD neck pmh0_neck001 (3 nodes) has the bone map 4 5 2
// (neck_g, head_g, torso_g in pmh0) and bind lists of 56 entries, one per pmh0
// node. See NWMAIN_FINDINGS.md, findings 20-22.

// skeleton is a creature skeleton numbered the way the engine walks it when it
// binds a skin: depth first from the root, children in file order.
type skeleton struct {
	path  string
	model *Model
	order []*Node        // tree position -> node
	index map[string]int // lower-case node name -> first tree position

	// pos and rot are each tree position's rest transform in the skeleton's
	// space (rotations x, y, z, w).
	pos [][3]float64
	rot [][4]float64
}

// partAttachNodes is where the game hangs each body part on the skeleton by
// default: the stock capart.2da (MDLNAME -> NODENAME). A custom skeleton can rename the nodes in
// its own capart.2da (FOOTR -> j_kao); compile reads that table when it finds
// one (findCapart) and falls back to these only when it finds none. Keys are the part type as it appears in the model name
// (pmh0_neck001 -> neck). Confirmed for the neck: binding pmh0_neck001 at
// neck_g reproduces all 56 of the engine's bind entries, and recompiling every
// skinned Beamdog HD part (2,700 of them, every type here) reproduces the
// engine's bind lists. Heads and robes are not here. CNWCAnimBaseParts::
// LoadModel (0x9db550) Appends rows 0-17 into the skeleton's tree, but loads
// the robe (row 18) and the head (via CNWCAnimBaseHead::LoadModel, at head_g)
// as separate objects with AttachToObject, so their skins bind to their own
// tree (all 132 HD robes confirm it) and they compile like whole models.
var partAttachNodes = map[string]string{
	"footr": "rfoot_g", "footl": "lfoot_g",
	"shinr": "rshin_g", "shinl": "lshin_g",
	"legl": "lthigh_g", "legr": "rthigh_g",
	"pelvis": "pelvis_g", "chest": "torso_g", "belt": "belt_g",
	"neck":  "neck_g",
	"forer": "rforearm_g", "forel": "lforearm_g",
	"bicepr": "rbicep_g", "bicepl": "lbicep_g",
	"shor": "rshoulder_g", "shol": "lshoulder_g",
	"handr": "rhand_g", "handl": "lhand_g",
}

// partAttachNode returns the skeleton node a body part model hangs from
// (neck_g for pmh0_neck001), or "" when the name is not a body part's.
func partAttachNode(modelName string) string {
	if partSkeletonName(modelName) == "" {
		return ""
	}
	return partAttachNodes[partKind(modelName)]
}

// partKind returns a body part model's type as capart.2da's MDLNAME spells it,
// lower case (footr for pmv0_footr010).
func partKind(modelName string) string {
	name := strings.ToLower(strings.TrimSpace(modelName))
	return strings.TrimRight(name[strings.IndexByte(name, '_')+1:], "0123456789")
}

// BodyPartSkeleton reports the creature skeleton a body part model is bound to
// in game and the node the stock capart.2da hangs it from ("pmh0", "neck_g"
// for pmh0_neck001), or "", "" when the name is not a body part's. A custom
// skeleton's own capart.2da can name a different node.
func BodyPartSkeleton(modelName string) (skeleton, attach string) {
	attach = partAttachNode(modelName)
	if attach == "" {
		return "", ""
	}
	return partSkeletonName(modelName), attach
}

// attachIndex returns the tree position of a part's attach node: a node
// name (any letter case, as the engine matches names), or "root" for the
// skeleton's root.
func (sk *skeleton) attachIndex(node string) (int, bool) {
	node = strings.ToLower(node)
	if node == "root" {
		return 0, len(sk.order) > 0
	}
	i, ok := sk.index[node]
	return i, ok
}

// partSkeletonName returns the skeleton a body part model is attached to: the
// part prefix before the first underscore (pmh0 for pmh0_neck001), which the
// game builds from gender, race and phenotype. It returns "" for a name that
// does not look like a body part.
func partSkeletonName(modelName string) string {
	name := strings.ToLower(strings.TrimSpace(modelName))
	i := strings.IndexByte(name, '_')
	if i < 3 || name[0] != 'p' || i == len(name)-1 {
		return ""
	}
	return name[:i]
}

// skeletonCache holds loaded skeletons for the life of the process, keyed like
// supermodelCache. Entries are read-only once stored.
var skeletonCache sync.Map

// loadSkeleton finds <name>.mdl in locs and numbers its nodes in tree order.
// It returns nil, nil when the model is not found.
func loadSkeleton(name string, locs []string) (*skeleton, error) {
	ref := findModelResource(name, locs)
	if ref == nil {
		return nil, nil
	}
	key := ref.id + "\x00" + ref.stamp
	if v, ok := skeletonCache.Load(key); ok {
		return v.(*skeleton), nil
	}
	data, err := ref.read()
	if err != nil {
		return nil, err
	}
	var model *Model
	if len(data) >= 12 && binary.LittleEndian.Uint32(data[0:4]) == 0 {
		model, err = Decompile(bytes.NewReader(data), int64(len(data)))
	} else {
		var res *ParseResult
		res, err = Parse(bytes.NewReader(data))
		if res != nil {
			model = res.Model
		}
	}
	if err != nil {
		return nil, fmt.Errorf("%s: %v", ref.id, err)
	}
	if model == nil || model.RootNode() == nil {
		return nil, fmt.Errorf("%s: no node tree", ref.id)
	}
	c := newCompiler(model)
	c.assignNodeIDs(model.RootNode())
	sk := &skeleton{path: ref.id, model: model, order: make([]*Node, len(c.treeIndex)), index: map[string]int{}}
	for n, i := range c.treeIndex {
		sk.order[i] = n
	}
	c.computeWorldTransforms()
	sk.pos = make([][3]float64, len(sk.order))
	sk.rot = make([][4]float64, len(sk.order))
	for i, n := range sk.order {
		if _, dup := sk.index[strings.ToLower(n.Name)]; !dup {
			sk.index[strings.ToLower(n.Name)] = i
		}
		sk.pos[i], sk.rot[i] = c.worldPos[n], c.worldRot[n]
	}
	skeletonCache.Store(key, sk)
	return sk, nil
}

// resolvePartSkeleton binds a skinned body part to its creature skeleton when
// the skeleton is found in opts.SupermodelDirs. Without it the part's skins
// are numbered against the part's own tree, which the game reads as skeleton
// positions: each bone then takes the transform of whichever skeleton node
// has its number and the part renders torn. That is reported through
// opts.Warn.
func (c *compiler) resolvePartSkeleton(opts CompileOptions) {
	var skin *Node
	for _, n := range c.model.Nodes {
		if n != nil && n.Skin != nil {
			skin = n
			break
		}
	}
	attach := partAttachNode(c.model.Name)
	if skin == nil || attach == "" {
		return
	}
	warn := func(format string, args ...any) {
		if opts.Warn != nil {
			opts.Warn(fmt.Sprintf(format, args...))
		}
	}
	skelName := partSkeletonName(c.model.Name)
	const consequence = "its skins are bound to the part's own node tree, but the game binds a body part's skin to the creature skeleton by tree position, so the compiled part will render torn in game. Give --game-dir (stock skeletons) or a --supermodel-dir holding the skeleton, or ship the part as ASCII"
	sk, err := loadSkeleton(skelName, opts.SupermodelDirs)
	switch {
	case err != nil:
		warn("body part %q: skeleton %s.mdl could not be read (%v); %s", c.model.Name, skelName, err, consequence)
		return
	case sk == nil:
		warn("body part %q: skeleton %s.mdl not found; %s", c.model.Name, skelName, consequence)
		return
	}
	from := "the stock capart.2da (no capart.2da was found; put yours next to the model or give the hak or folder holding it with --resource-dir)"
	if table, path := findCapart(opts.SupermodelDirs); table != nil {
		if node, ok := table[partKind(c.model.Name)]; ok {
			attach, from = node, path
		}
	}
	at, ok := sk.attachIndex(attach)
	if !ok {
		warn("body part %q: skeleton %s has no node %s to attach the part to (attach node from %s); %s", c.model.Name, sk.path, attach, from, consequence)
		return
	}
	c.partSkel, c.partAttach = sk, at

	var missing []string
	seen := map[string]bool{}
	for _, n := range c.model.Nodes {
		if n == nil || n.Skin == nil {
			continue
		}
		for _, vw := range n.Skin.Weights {
			for _, b := range vw.Bones {
				k := strings.ToLower(b)
				if b == "" || seen[k] {
					continue
				}
				seen[k] = true
				if _, ok := sk.index[k]; !ok {
					missing = append(missing, b)
				}
			}
		}
	}
	if len(missing) > 0 {
		sort.Strings(missing)
		warn("body part %q: skin bones %s are not nodes of the skeleton %s; the game binds a part's skin to the skeleton only, so their influence is lost in game",
			c.model.Name, strings.Join(missing, ", "), sk.path)
	}
}

// PartSlot reports a body part model's skeleton and part type ("pmh0",
// "neck" for pmh0_neck001), including the head and the robe, or "", "" when
// the name is not a body part's. Unlike BodyPartSkeleton it does not say how
// the part binds: see BodyPartSkeleton for that.
func PartSlot(modelName string) (skeleton, kind string) {
	skeleton = partSkeletonName(modelName)
	if skeleton == "" {
		return "", ""
	}
	return skeleton, partKind(modelName)
}

// PartBinding is what a body part is bound to in game, as compile resolves it.
type PartBinding struct {
	Skeleton     string          // skeleton model name (pmh0)
	SkeletonPath string          // where it was found; "" when not found
	Attach       string          // attach node name
	AttachFrom   string          // capart.2da it came from, or "" for the stock table
	AttachFound  bool            // the skeleton has the attach node
	Bones        map[string]bool // lower-case node names of the skeleton
}

// ResolvePartBinding finds a body part's skeleton and attach node in locs the
// way compile does (findCapart, loadSkeleton). It returns nil when the model
// is not a body part bound to a skeleton (heads and robes bind to their own
// tree), and a PartBinding with an empty SkeletonPath when the skeleton is not
// found.
func ResolvePartBinding(modelName string, locs []string) (*PartBinding, error) {
	skel, attach := BodyPartSkeleton(modelName)
	if skel == "" {
		return nil, nil
	}
	b := &PartBinding{Skeleton: skel, Attach: attach}
	if table, path := findCapart(locs); table != nil {
		if node, ok := table[partKind(modelName)]; ok {
			b.Attach, b.AttachFrom = node, path
		}
	}
	sk, err := loadSkeleton(skel, locs)
	if err != nil || sk == nil {
		return b, err
	}
	b.SkeletonPath = sk.path
	_, b.AttachFound = sk.attachIndex(b.Attach)
	b.Bones = make(map[string]bool, len(sk.index))
	for name := range sk.index {
		b.Bones[name] = true
	}
	return b, nil
}
