package mdl

import "strings"

// shinyWaterReplacements maps known stock NWN water bitmaps to their "shiny"
// replacements. Mirrors make_checks.pl shiny_water_replacement/2 (line 5370).
// The map is also used purely as a membership predicate by IsWatery: any key
// in this table is a watery bitmap regardless of any user-supplied key.
var shinyWaterReplacements = map[string]string{
	"tbw01_water01": "ttf01_water01",
	"tdt01_water01": "tcn01_water01",
	"tni01_water02": "tin01_water02",
	"tni02_water01": "tcn01_water01",
	"tno01_water01": "tin01_water02",
	"tno01_wtsea01": "tin01_water02",
	"twc03_wtsea01": "tin01_water02",
	"tdm02_water01": "tcn01_water01",
}

// ShinyWaterReplacement returns the shiny-water replacement bitmap name for
// the given bitmap, and true if a replacement exists. Lookup is
// case-insensitive on the input but the returned name is in canonical
// (lower-case) form.
func ShinyWaterReplacement(bitmap string) (string, bool) {
	v, ok := shinyWaterReplacements[strings.ToLower(strings.TrimSpace(bitmap))]
	return v, ok
}

// IsWateryBitmap reports whether the given bitmap name is "watery". A bitmap
// is watery when either:
//
//   - it appears in the shiny-water replacement table, or
//   - the user has provided a key (a space-separated list of substrings) and
//     the bitmap (case-insensitively) contains one of those substrings.
//
// Mirrors make_checks.pl is_watery/1 (line 5397).
func IsWateryBitmap(bitmap, waterKey string) bool {
	if bitmap == "" {
		return false
	}
	lb := strings.ToLower(strings.TrimSpace(bitmap))
	if _, ok := shinyWaterReplacements[lb]; ok {
		return true
	}
	if waterKey == "" {
		return false
	}
	for _, k := range strings.Fields(strings.ToLower(waterKey)) {
		if k != "" && strings.Contains(lb, k) {
			return true
		}
	}
	return false
}

// IsWateryNode reports whether a node is "watery": it must be a mesh-bearing
// node whose bitmap matches IsWateryBitmap.
func IsWateryNode(n *Node, waterKey string) bool {
	if n == nil || n.Mesh == nil {
		return false
	}
	return IsWateryBitmap(n.Mesh.Bitmap, waterKey)
}
