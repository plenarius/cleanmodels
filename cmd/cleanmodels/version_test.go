package main

import (
	"strings"
	"testing"
)

// TestVersionSpellings pins every way of asking for the version (issue #17).
// "-v" is --verbose inside subcommands, so it only counts as a version request
// when it is the sole argument.
func TestVersionSpellings(t *testing.T) {
	for _, args := range [][]string{{"version"}, {"--version"}, {"-version"}, {"-v"}} {
		var code int
		stdout, _ := captureStdio(t, func() { code = run(args) })
		if code != exitOK {
			t.Errorf("%v: exit %d, want %d", args, code, exitOK)
		}
		if !strings.HasPrefix(stdout, "cleanmodels ") || !strings.Contains(stdout, "go1") {
			t.Errorf("%v: stdout %q does not look like a version line", args, stdout)
		}
	}
}

// TestVerboseAliasStillWorksWithArgs guards the other half of the "-v" rule:
// with anything else on the line it stays the verbose alias and must not print
// the version.
func TestVerboseAliasStillWorksWithArgs(t *testing.T) {
	var code int
	stdout, _ := captureStdio(t, func() { code = run([]string{"check", "-v", "/nonexistent/path.mdl"}) })
	if strings.HasPrefix(stdout, "cleanmodels ") && strings.Contains(stdout, "go1") {
		t.Errorf("check -v printed a version line: %q", stdout)
	}
	_ = code
}

// TestLegacyVerboseAlias: legacy mode accepts -v like every subcommand does.
func TestLegacyVerboseAlias(t *testing.T) {
	_, stderr := captureStdio(t, func() { run([]string{"-v", "/nonexistent/path.mdl"}) })
	if strings.Contains(stderr, "flag provided but not defined") {
		t.Errorf("legacy mode rejected -v:\n%s", stderr)
	}
}

func TestHelpMentionsVersion(t *testing.T) {
	_, stderr := captureStdio(t, func() { run([]string{"help"}) })
	if !strings.Contains(stderr, "--version") {
		t.Errorf("help does not mention --version:\n%s", stderr)
	}
}
