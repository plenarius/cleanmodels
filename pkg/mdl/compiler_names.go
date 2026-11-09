package mdl

import (
	"fmt"
	"strings"
)

// maxNodeName is how many characters of a node name the binary keeps: the
// field is 32 bytes and the game's compiler writes the name NUL-terminated,
// so a longer name is cut to 31 (nwn-mdl-spec MDL-BIN-007; fixedStr).
const maxNodeName = 31

// cutNodeName is name as the binary stores it.
func cutNodeName(name string) string {
	if len(name) > maxNodeName {
		return name[:maxNodeName]
	}
	return name
}

// warnCutNames reports every node name that compile cuts to 31 characters:
// one warning per written geometry node, and per animation for an animation
// node whose name is no geometry node's. A cut that turns the name into
// another node's says so, since the game then finds the wrong node by that
// name (two Layonara heads had this).
func (c *compiler) warnCutNames() {
	if c.warn == nil {
		return
	}
	var geom []string
	for _, n := range c.model.Nodes {
		if n == nil {
			continue
		}
		if _, ok := c.nodeIDs[n]; ok {
			geom = append(geom, n.Name)
		}
	}
	for _, name := range geom {
		if len(name) > maxNodeName {
			c.warn(cutNameWarning("", name, geom))
		}
	}
	for _, a := range c.model.Animations {
		var names []string
		for i := range a.Nodes {
			names = append(names, a.Nodes[i].Name)
		}
		others := append(append([]string(nil), geom...), names...)
		warned := map[string]bool{}
		for _, name := range names {
			if len(name) <= maxNodeName || warned[name] || containsFold(geom, name) {
				continue
			}
			warned[name] = true
			c.warn(cutNameWarning(a.Name, name, others))
		}
	}
}

// cutNameWarning is the warning for name, cut; others are the names of the
// nodes it could now be taken for. anim names the animation of an animation
// node.
func cutNameWarning(anim, name string, others []string) string {
	cut := cutNodeName(name)
	msg := fmt.Sprintf("node %q: its name has %d characters and is cut to %d, as the game's compiler does: %q",
		name, len(name), maxNodeName, cut)
	if anim != "" {
		msg = fmt.Sprintf("animation %q: %s", anim, msg)
	}
	for _, o := range others {
		if strings.EqualFold(o, name) || !strings.EqualFold(cutNodeName(o), cut) {
			continue
		}
		if len(o) <= maxNodeName {
			return msg + fmt.Sprintf(", which is the name of node %q, so the game cannot tell the two apart", o)
		}
		return msg + fmt.Sprintf(", which node %q is also cut to, so the game cannot tell the two apart", o)
	}
	return msg
}

// containsFold reports whether names holds name, case-insensitively.
func containsFold(names []string, name string) bool {
	for _, n := range names {
		if strings.EqualFold(n, name) {
			return true
		}
	}
	return false
}
