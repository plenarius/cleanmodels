package mdl

import (
	"fmt"
	"strings"
)

// A node whose parent cannot be found, a second root, or a cycle is never
// reached when the tree is written, so it is left out of the binary. That used
// to happen without a word; the engine's own handling of such nodes is not
// established, so for now the compiler keeps its behaviour and says so.

const maxDroppedNamed = 5

func summariseDropped(kind string, names []string) string {
	shown := names
	more := ""
	if len(shown) > maxDroppedNamed {
		shown = shown[:maxDroppedNamed]
		more = fmt.Sprintf(" and %d more", len(names)-maxDroppedNamed)
	}
	return fmt.Sprintf("%d %s node(s) are not connected to the root and were left out of the binary: %s%s",
		len(names), kind, strings.Join(shown, ", "), more)
}

// warnDroppedGeometry reports geometry nodes that were not numbered, with why.
func (c *compiler) warnDroppedGeometry() {
	if c.warn == nil {
		return
	}
	var names []string
	for _, n := range c.model.Nodes {
		if n == nil {
			continue
		}
		if _, ok := c.nodeIDs[n]; ok {
			continue
		}
		reason := ""
		switch {
		case isRootParent(n.Parent):
			reason = "a second root"
		case c.geomNodeByName(n.Parent) == nil:
			reason = fmt.Sprintf("parent %q does not exist", n.Parent)
		default:
			reason = fmt.Sprintf("parent %q is itself not connected", n.Parent)
		}
		names = append(names, fmt.Sprintf("%s (%s)", n.Name, reason))
	}
	if len(names) > 0 {
		c.warn(summariseDropped("geometry", names))
	}
}

// warnDroppedAnim reports animation nodes the walk from the animation root
// never wrote.
func (c *compiler) warnDroppedAnim(anim *Animation, visited map[*AnimNode]bool) {
	if c.warn == nil {
		return
	}
	var names []string
	for i := range anim.Nodes {
		if !visited[&anim.Nodes[i]] {
			names = append(names, anim.Nodes[i].Name)
		}
	}
	if len(names) > 0 {
		c.warn(fmt.Sprintf("animation %q: %s", anim.Name, summariseDropped("animation", names)))
	}
}
