package mdl

import (
	"crypto/sha256"
	"encoding/hex"
	"strings"
	"sync"
)

// Dependency is one resource a compile looked up besides the model itself: a
// supermodel (and each supermodel down its chain), a body part's skeleton, the
// capart.2da that names its attach node, or a mesh's material. Found is false
// when no location had it, which matters too: adding the file later changes
// the output. A cache that stored a compile's dependencies can tell whether
// the stored output still holds by resolving each one again with
// ResolveDependency and comparing.
type Dependency struct {
	// Kind is the resource type: "mdl", "mtr" or "2da".
	Kind string `json:"kind"`
	// Name is the resref looked up, lower case.
	Name string `json:"name"`
	// List is the search path it was looked up in: "supermodel" for
	// CompileOptions.SupermodelDirs, "resource" for ResourceDirs.
	List string `json:"list"`
	// Found reports whether a location had it.
	Found bool `json:"found"`
	// SHA256 is the hex SHA-256 of the content found; empty when not found.
	SHA256 string `json:"sha256,omitempty"`
}

const (
	depKindModel    = "mdl"
	depKindMaterial = "mtr"
	depKind2DA      = "2da"

	depListSupermodel = "supermodel"
	depListResource   = "resource"
)

// contentHashes memoises a resource's hash for the life of the process, keyed
// like supermodelCache (the location's id and the file's size and time), so a
// batch hashes a shared supermodel or material once.
var contentHashes sync.Map

// hashResource returns ref's content hash, or "" when ref is nil or unreadable
// (an unreadable file is recorded as found with no hash, which never matches a
// later lookup, so the model is compiled again).
func hashResource(ref *resourceRef) string {
	if ref == nil {
		return ""
	}
	key := ref.id + "\x00" + ref.stamp
	if v, ok := contentHashes.Load(key); ok {
		return v.(string)
	}
	data, err := ref.read()
	if err != nil {
		return ""
	}
	sum := sha256.Sum256(data)
	h := hex.EncodeToString(sum[:])
	contentHashes.Store(key, h)
	return h
}

func newDependency(kind, name, list string, ref *resourceRef) Dependency {
	return Dependency{Kind: kind, Name: strings.ToLower(strings.TrimSpace(name)), List: list, Found: ref != nil, SHA256: hashResource(ref)}
}

// ResolveDependency looks d up again in opts' search paths, as the compile did,
// and returns what it finds now. The stored output of a compile is still valid
// when every one of its dependencies resolves to the same Found and SHA256 (and
// the model, the search paths and the compiler are unchanged).
func ResolveDependency(d Dependency, opts CompileOptions) Dependency {
	dirs := opts.SupermodelDirs
	if d.List == depListResource {
		dirs = opts.ResourceDirs
	}
	var ref *resourceRef
	switch d.Kind {
	case depKindModel:
		ref = findModelResource(d.Name, dirs)
	case depKindMaterial:
		ref = findMaterial(d.Name, dirs)
	case depKind2DA:
		ref = findResource(d.Name, resType2DA, ".2da", dirs)
	}
	return newDependency(d.Kind, d.Name, d.List, ref)
}

// lookup is a resource lookup as made, before it is hashed: supermodels keep
// the lookups down their chain this way, so a cached supermodel can report
// them again without hashing anything when no one asked for dependencies.
type lookup struct {
	kind, name, list string
	ref              *resourceRef
}

// record passes a lookup to opts.Dependency, if set. Nothing is hashed
// otherwise.
func (c *compiler) record(kind, name, list string, ref *resourceRef) {
	if c.dependency != nil {
		c.dependency(newDependency(kind, name, list, ref))
	}
}

// recordMaterial records a material lookup made for tangents.
func (c *compiler) recordMaterial(name string, ref *resourceRef) {
	c.record(depKindMaterial, name, depListResource, ref)
}
