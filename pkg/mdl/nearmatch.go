package mdl

import "strings"

// NearMatch implements fuzzy parameter name matching for ASCII MDL parsing.
// The Prolog parser uses near_match/2 (and dwim_match/2) on every keyword to
// tolerate common typos and alternate spellings found in old community models.
//
// Matching strategy:
//  1. Exact match (fast path)
//  2. Known aliases (canonical misspellings from the MDL format)
//  3. Levenshtein distance <= 2 within the valid parameter set
//
// Ref: load_models.pl near_match/2 (line 904)

// knownAliases maps common misspellings/variants to their canonical names.
// These are guaranteed matches that don't need fuzzy comparison.
var knownAliases = map[string]string{
	"setfillumcolor":  "selfillumcolor",
	"n_dynamic_type":  "ndynamictype",
	"filedependancy":  "filedependency",
	"texture0":        "bitmap",
	"isdynamic":       "ndynamictype",
}

// NearMatchParam attempts to match input against a set of valid parameter names.
// Returns the matched name and true, or empty string and false.
func NearMatchParam(input string, validParams []string) (string, bool) {
	lower := strings.ToLower(input)

	// Exact match
	for _, p := range validParams {
		if lower == p {
			return p, true
		}
	}

	// Known alias
	if canonical, ok := knownAliases[lower]; ok {
		for _, p := range validParams {
			if canonical == p {
				return p, true
			}
		}
	}

	// Fuzzy match: Levenshtein distance <= 2. Tie-break by lexicographic name
	// so results do not depend on validParams slice order.
	bestDist := 3
	bestMatch := ""
	for _, p := range validParams {
		d := levenshtein(lower, p)
		if d < bestDist || (d == bestDist && (bestMatch == "" || p < bestMatch)) {
			bestDist = d
			bestMatch = p
		}
	}
	if bestDist <= 2 && bestMatch != "" {
		return bestMatch, true
	}

	return "", false
}

// levenshtein computes the edit distance between two strings.
func levenshtein(a, b string) int {
	if len(a) == 0 {
		return len(b)
	}
	if len(b) == 0 {
		return len(a)
	}

	ra := []rune(a)
	rb := []rune(b)
	la := len(ra)
	lb := len(rb)

	// Early exit: if length difference > 2, distance must be > 2
	if la-lb > 2 || lb-la > 2 {
		return 3
	}

	prev := make([]int, lb+1)
	curr := make([]int, lb+1)

	for j := 0; j <= lb; j++ {
		prev[j] = j
	}

	for i := 1; i <= la; i++ {
		curr[0] = i
		for j := 1; j <= lb; j++ {
			cost := 1
			if ra[i-1] == rb[j-1] {
				cost = 0
			}
			curr[j] = min(
				prev[j]+1,
				curr[j-1]+1,
				prev[j-1]+cost,
			)
		}
		prev, curr = curr, prev
	}

	return prev[lb]
}
