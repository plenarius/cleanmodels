package main

import (
	"fmt"
	"strings"
	"unicode"
)

// foldorbits prints Go's case-folding orbits for python/cleanmodels/gostr.py
// (strings.EqualFold). Prints each simple case-folding orbit (more than one rune) once, as hex runes.
func main() {
	seen := map[rune]bool{}
	var out []string
	for r := rune(0); r <= unicode.MaxRune; r++ {
		if seen[r] || unicode.SimpleFold(r) == r {
			continue
		}
		var orbit []string
		for f := r; ; {
			seen[f] = true
			orbit = append(orbit, fmt.Sprintf("%x", f))
			f = unicode.SimpleFold(f)
			if f == r {
				break
			}
		}
		out = append(out, strings.Join(orbit, " "))
	}
	fmt.Println(strings.Join(out, ","))
}
