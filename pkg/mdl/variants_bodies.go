package mdl

import (
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"sync"
)

// Bodies. A part-based creature's models are named for its body: gender
// (m, f), race letter (appearance.2da RACE: h for humans and half-elves, w for
// a custom wemic) and phenotype (0, 2, ...): pmw0_chest001. Phenotypes that
// only change animations (a fighting style) have a skeleton but no parts of
// their own; the game gives them their phenotype.2da DefaultPhenoType's. So
// the bodies variants are made for are the prefixes that have body parts.

// Body is a body found by VariantBodies.
type Body struct {
	Prefix    string `json:"prefix"`    // pmw0
	Gender    string `json:"gender"`    // "m" or "f"
	Race      string `json:"race"`      // race letter, "w"
	RaceName  string `json:"raceName"`  // "Wemic": appearance.2da LABEL, or the stock race's name
	Phenotype int    `json:"phenotype"` // 0
	// PhenotypeName is the phenotype's phenotype.2da Label ("Normal", "Large",
	// "Kensai"), or "" when not found.
	PhenotypeName string `json:"phenotypeName,omitempty"`
	Parts         int    `json:"parts"`    // body part models found (robes and cloaks included)
	Skeleton      bool   `json:"skeleton"` // its own skeleton model (pmw0) is found
	Robe          bool   `json:"robe"`     // a robe of this body is found (bone template for robe variants)
	Cloak         bool   `json:"cloak"`    // a cloak of this body is found (bone template for cloak variants)

	// Slots counts the body part models by slot (chest, robe, cloak, ...).
	Slots map[string]int `json:"slots"`

	// PartsFrom is the body whose base parts this one wears (itself, or for
	// a mounted or fighting-style phenotype with none, its DefaultPhenoType's),
	// and SkeletonFrom the skeleton the game gives it (itself, or for one
	// without its own, its race's phenotype 0).
	PartsFrom    string `json:"partsFrom"`
	SkeletonFrom string `json:"skeletonFrom"`

	// SameShape is the prefix of the first body (lowest phenotype) of the
	// same race and gender that wears the same base parts and whose skeleton
	// has every body joint where this one's has it, or "" when there is none
	// or this body is it. Fighting-style
	// and mounted phenotypes are most often the same shape as phenotype 0
	// with other animations: a variant for them is that body's, renamed.
	SameShape string `json:"sameShape,omitempty"`
}

// sameShapeTolerance is how far apart two skeletons' body joints may be and
// still be the same shape: 1 cm. Fighting-style skeletons differ from
// phenotype 0's by up to 4 mm (belt_g); mounted ones by a metre at the feet.
const sameShapeTolerance = 0.01

// stockRaceNames names the stock races by letter; humans and half-elves share h.
var stockRaceNames = map[string]string{
	"a": "Halfling", "d": "Dwarf", "e": "Elf", "g": "Gnome", "h": "Human", "o": "Half-orc",
}

// bodyPrefix matches a body prefix: p, gender, race letter, phenotype.
var bodyPrefix = regexp.MustCompile(`^p[mf][a-z][0-9]+$`)

// partModelName matches a body part, robe or cloak model: prefix, slot, number.
var partModelName = regexp.MustCompile(`^(p([mf])([a-z])([0-9]+))_([a-z]+?_?)([0-9]+)$`)

// VariantBodies lists the bodies that have body part models in locs (the
// same locations Variant reads: folders, haks, a game install), sorted by
// race (stock races first), gender and phenotype.
func VariantBodies(locs []string) []Body {
	bodies := map[string]*Body{}
	for name := range listModels(locs) {
		m := partModelName.FindStringSubmatch(name)
		if m == nil {
			continue
		}
		slot := VariantKind(name)
		if slot == "" {
			continue // heads and anything else named like a part
		}
		b := bodies[m[1]]
		if b == nil {
			ph, _ := strconv.Atoi(m[4])
			b = &Body{Prefix: m[1], Gender: m[2], Race: m[3], Phenotype: ph, Slots: map[string]int{}}
			bodies[m[1]] = b
		}
		b.Parts++
		b.Slots[slot]++
		b.Robe = b.Robe || slot == "robe"
		b.Cloak = b.Cloak || slot == "cloak"
	}
	labels := raceLabels(locs)
	phenoRows := phenotypes(locs)
	out := make([]Body, 0, len(bodies))
	for _, b := range bodies {
		b.Skeleton = findModelResource(b.Prefix, locs) != nil
		b.PartsFrom = partsFrom(b.Prefix, locs)
		b.SkeletonFrom = bodySkeleton(b.Prefix, locs)
		b.PhenotypeName = phenoRows[strconv.Itoa(b.Phenotype)].label
		b.RaceName = stockRaceNames[b.Race]
		if b.RaceName == "" {
			b.RaceName = labels[b.Race]
		}
		if b.RaceName == "" {
			b.RaceName = strings.ToUpper(b.Race)
		}
		out = append(out, *b)
	}
	sort.Slice(out, func(i, j int) bool {
		a, b := out[i], out[j]
		_, sa := stockRaceNames[a.Race]
		_, sb := stockRaceNames[b.Race]
		switch {
		case sa != sb:
			return sa
		case a.RaceName != b.RaceName:
			return a.RaceName < b.RaceName
		case a.Gender != b.Gender:
			return a.Gender > b.Gender // m before f
		}
		return a.Phenotype < b.Phenotype
	})

	// Shapes: compare each body's skeleton with the earlier bodies' of its
	// race and gender (out is sorted by phenotype within those).
	skels := map[string]*skeleton{}
	for i := range out {
		sk, _ := loadBodySkeleton(out[i].Prefix, locs)
		if sk == nil {
			continue
		}
		skels[out[i].Prefix] = sk
		for j := 0; j < i; j++ {
			o := out[j]
			if o.Race != out[i].Race || o.Gender != out[i].Gender || o.SameShape != "" || o.PartsFrom != out[i].PartsFrom {
				continue
			}
			if os := skels[o.Prefix]; os != nil && sameSkeletonShape(os, sk) {
				out[i].SameShape = o.Prefix
				break
			}
		}
	}
	return out
}

// sameSkeletonShape reports whether two skeletons have their body joints
// (isBodyJoint: the nodes parts hang from, and the head) in the same places,
// within sameShapeTolerance. Other nodes (effects, a mount's) do not count.
func sameSkeletonShape(a, b *skeleton) bool {
	n := 0
	for name, i := range a.index {
		if !isBodyJoint(name) {
			continue
		}
		j, ok := b.index[name]
		if !ok {
			return false
		}
		d := [3]float64{a.pos[i][0] - b.pos[j][0], a.pos[i][1] - b.pos[j][1], a.pos[i][2] - b.pos[j][2]}
		if len3(d) > sameShapeTolerance {
			return false
		}
		n++
	}
	for name := range b.index {
		if _, ok := a.index[name]; isBodyJoint(name) && !ok {
			return false
		}
	}
	return n > 0
}

// Engine rules (CNWCCreatureAppearance::CreateBodyParts, 0x734440, and
// CreateHead, 0x731bb0, in the Linux client; NWMAIN_FINDINGS.md finding 32): a body's skeleton is p+gender+
// race+phenotype, and when that model does not exist, the same race's
// phenotype 0 (pmb2 -> pmb0; never DefaultPhenoType, never human). Each part
// keeps the asked-for phenotype in its name (pmb2_chest050 is loaded onto
// pmb0), and when that model does not exist, the one of its phenotype's
// DefaultPhenoType in phenotype.2da (one step: pmh3_chest001 ->
// pmh0_chest001), or none. No scale goes with the phenotype.

// bodySkeleton returns the skeleton the game loads for a body prefix: its
// own, or its race's phenotype 0 one when its own is not in locs.
func bodySkeleton(prefix string, locs []string) string {
	if len(prefix) < 4 || findModelResource(prefix, locs) != nil || prefix[3:] == "0" {
		return prefix
	}
	return prefix[:3] + "0"
}

// loadBodySkeleton loads bodySkeleton(prefix).
func loadBodySkeleton(prefix string, locs []string) (*skeleton, error) {
	return loadSkeleton(bodySkeleton(prefix, locs), locs)
}

// basePart returns the base body part model (number 001) the game shows in
// slot (chest, ..., head) for a body prefix: <prefix>_<slot>001, or failing
// that its DefaultPhenoType's. When neither is in locs it returns the first.
func basePart(prefix, slot string, locs []string) string {
	name := prefix + "_" + slot + "001"
	if len(prefix) < 4 || findModelResource(name, locs) != nil {
		return name
	}
	def := phenotypes(locs)[prefix[3:]].def
	if def == "" || def == prefix[3:] {
		return name
	}
	if other := prefix[:3] + def + "_" + slot + "001"; findModelResource(other, locs) != nil {
		return other
	}
	return name
}

// partsFrom returns the body whose base parts a body prefix wears: its own
// when it has any (a slot it lacks, such as the belt, still falls back per
// part), else the body its DefaultPhenoType gives (basePart), or prefix.
func partsFrom(prefix string, locs []string) string {
	if hasBaseParts(prefix, locs) {
		return prefix
	}
	slots := make([]string, 0, len(partAttachNodes))
	for slot := range partAttachNodes {
		slots = append(slots, slot)
	}
	sort.Strings(slots)
	for _, slot := range slots {
		if n := basePart(prefix, slot, locs); findModelResource(n, locs) != nil {
			return n[:strings.IndexByte(n, '_')]
		}
	}
	return prefix
}

// hasBaseParts reports whether any base body part (<prefix>_<slot>001) of
// the body prefix is in locs.
func hasBaseParts(prefix string, locs []string) bool {
	for slot := range partAttachNodes {
		if findModelResource(prefix+"_"+slot+"001", locs) != nil {
			return true
		}
	}
	return false
}

// phenotypeRow is a phenotype.2da row: its Label and DefaultPhenoType.
type phenotypeRow struct {
	label, def string
}

// phenotypeCache holds parsed phenotype.2da tables by resource, read-only
// once stored.
var phenotypeCache sync.Map

// phenotypes returns phenotype.2da's rows by phenotype (row) number, as a
// decimal string; empty when the table is not found.
func phenotypes(locs []string) map[string]phenotypeRow {
	ref := findResource("phenotype", resType2DA, ".2da", locs)
	if ref == nil {
		return nil
	}
	key := ref.id + "\x00" + ref.stamp
	if v, ok := phenotypeCache.Load(key); ok {
		return v.(map[string]phenotypeRow)
	}
	rows := map[string]phenotypeRow{}
	if data, err := ref.read(); err == nil {
		if table, err := parse2DA(data); err == nil {
			for _, r := range table {
				row := phenotypeRow{label: strings.Trim(r["label"], "* ")}
				if _, err := strconv.Atoi(r["defaultphenotype"]); err == nil {
					row.def = r["defaultphenotype"]
				}
				rows[r[rowKey]] = row
			}
		}
	}
	phenotypeCache.Store(key, rows)
	return rows
}

// VariantKind returns what a model is to Variant, by its name: "robe",
// "cloak", a body part slot ("chest" for pmh0_chest050), or "" for anything
// else. Body.Slots counts models by the same names.
func VariantKind(name string) string {
	m := partModelName.FindStringSubmatch(strings.ToLower(strings.TrimSpace(name)))
	if m == nil {
		return ""
	}
	slot := strings.TrimRight(m[5], "_")
	if _, ok := partAttachNodes[slot]; ok || slot == "robe" || slot == "cloak" {
		return slot
	}
	return ""
}

// RenameBody returns a copy of m, a model for the body from, named for the
// body to: the model, node and parent names and the supermodel. For a body of
// the same shape (Body.SameShape), that is its variant.
func RenameBody(m *Model, from, to string) *Model { return cloneForBody(m, from, to) }

// raceLabels returns, by race letter, the LABEL of the first part-based row
// (MODELTYPE P) of appearance.2da in locs that has it: "Wemic" for w.
func raceLabels(locs []string) map[string]string {
	labels := map[string]string{}
	ref := findResource("appearance", resType2DA, ".2da", locs)
	if ref == nil {
		return labels
	}
	data, err := ref.read()
	if err != nil {
		return labels
	}
	rows, err := parse2DA(data)
	if err != nil {
		return labels
	}
	for _, r := range rows {
		if !strings.EqualFold(r["modeltype"], "P") {
			continue
		}
		race := strings.ToLower(r["race"])
		if len(race) != 1 || labels[race] != "" {
			continue
		}
		label := strings.Trim(strings.ReplaceAll(r["label"], "_", " "), "* ")
		if label != "" && label != "****" {
			labels[race] = label
		}
	}
	return labels
}

// listModelsCache holds listModels results by location list; folders are
// read again only by a new process. Read-only once stored.
var listModelsCache sync.Map

// listModels returns the lower-case names of every model in locs: the
// archives' and game install's, and the .mdl files directly in a folder.
func listModels(locs []string) map[string]bool {
	key := strings.Join(locs, "\x00")
	if v, ok := listModelsCache.Load(key); ok {
		return v.(map[string]bool)
	}
	names := map[string]bool{}
	defer listModelsCache.Store(key, names)
	for _, loc := range locs {
		if a := openArchive(loc); a != nil {
			for k := range a.entries {
				if k.typ == resTypeMDL {
					names[k.name] = true
				}
			}
			continue
		}
		entries, err := os.ReadDir(loc)
		if err != nil {
			continue
		}
		for _, e := range entries {
			if !e.IsDir() && strings.EqualFold(filepath.Ext(e.Name()), ".mdl") {
				names[strings.ToLower(strings.TrimSuffix(e.Name(), filepath.Ext(e.Name())))] = true
			}
		}
	}
	return names
}
