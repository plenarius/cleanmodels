package main

import (
	"flag"
	"reflect"
	"testing"
)

// newTestFlagSet builds a FlagSet whose registered flags mirror the real
// commands closely enough to exercise reorderArgs: a bool flag with a short
// alias, and a value-taking int flag with a short alias.
func newTestFlagSet() *flag.FlagSet {
	fs := flag.NewFlagSet("test", flag.ContinueOnError)
	var (
		verbose bool
		workers int
	)
	fs.BoolVar(&verbose, "verbose", false, "")
	fs.BoolVar(&verbose, "v", false, "")
	fs.IntVar(&workers, "workers", 0, "")
	fs.IntVar(&workers, "w", 0, "")
	return fs
}

func TestReorderArgs(t *testing.T) {
	cases := []struct {
		name string
		in   []string
		want []string
	}{
		{"empty", nil, nil},
		{"only positionals", []string{"a.mdl", "b.mdl"}, []string{"a.mdl", "b.mdl"}},
		{"flags already first", []string{"--verbose", "a.mdl"}, []string{"--verbose", "a.mdl"}},
		{"bool flag after positional", []string{"a.mdl", "--verbose"}, []string{"--verbose", "a.mdl"}},
		{"bool flag between positionals", []string{"a.mdl", "--verbose", "b.mdl"}, []string{"--verbose", "a.mdl", "b.mdl"}},
		{"short bool after positional", []string{"a.mdl", "-v"}, []string{"-v", "a.mdl"}},
		{"value flag after positional", []string{"a.mdl", "-w", "2"}, []string{"-w", "2", "a.mdl"}},
		{"value flag keeps value with it", []string{"in.mdl", "out.mdl", "-w", "4"}, []string{"-w", "4", "in.mdl", "out.mdl"}},
		{"inline value after positional", []string{"a.mdl", "--workers=2"}, []string{"--workers=2", "a.mdl"}},
		{"double dash terminator re-emitted", []string{"-w", "2", "--", "-weird.mdl"}, []string{"-w", "2", "--", "-weird.mdl"}},
		{"double dash protects flag-looking positional", []string{"a.mdl", "--", "-x.mdl"}, []string{"--", "a.mdl", "-x.mdl"}},
		{"bare dash is positional", []string{"a.mdl", "-", "--verbose"}, []string{"--verbose", "a.mdl", "-"}},
		{"unknown flag not given a value", []string{"a.mdl", "--bogus"}, []string{"--bogus", "a.mdl"}},
		{"value flag at end without value", []string{"a.mdl", "-w"}, []string{"-w", "a.mdl"}},
		{"mixed order preserved within groups", []string{"--verbose", "a.mdl", "-w", "3", "b.mdl"}, []string{"--verbose", "-w", "3", "a.mdl", "b.mdl"}},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := reorderArgs(newTestFlagSet(), tc.in)
			if len(got) == 0 && len(tc.want) == 0 {
				return
			}
			if !reflect.DeepEqual(got, tc.want) {
				t.Errorf("reorderArgs(%q) = %q, want %q", tc.in, got, tc.want)
			}
		})
	}
}

// TestParseArgsAppliesTrailingFlags is the end-to-end guard for the reported
// bug: a flag placed after the positional arguments must actually be applied,
// and must not leak into fs.Args() where callers would treat it as an output
// path.
func TestParseArgsAppliesTrailingFlags(t *testing.T) {
	fs := flag.NewFlagSet("test", flag.ContinueOnError)
	verbose := fs.Bool("verbose", false, "")
	workers := fs.Int("w", 0, "")

	if err := parseArgs(fs, []string{"in.mdl", "out.mdl", "-w", "4", "--verbose"}); err != nil {
		t.Fatalf("parseArgs returned error: %v", err)
	}

	if !*verbose {
		t.Error("--verbose after positionals was not applied")
	}
	if *workers != 4 {
		t.Errorf("-w after positionals = %d, want 4", *workers)
	}
	got := fs.Args()
	want := []string{"in.mdl", "out.mdl"}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("positionals = %q, want %q (flags must not leak into Args)", got, want)
	}
}

// TestParseArgsDoubleDash confirms tokens after "--" survive as positionals
// even when they look like flags, so a file literally named like a flag can
// still be addressed.
func TestParseArgsDoubleDash(t *testing.T) {
	fs := flag.NewFlagSet("test", flag.ContinueOnError)
	verbose := fs.Bool("verbose", false, "")

	if err := parseArgs(fs, []string{"--verbose", "--", "-weird.mdl"}); err != nil {
		t.Fatalf("parseArgs returned error: %v", err)
	}
	if !*verbose {
		t.Error("--verbose before -- was not applied")
	}
	got := fs.Args()
	want := []string{"-weird.mdl"}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("positionals = %q, want %q", got, want)
	}
}
