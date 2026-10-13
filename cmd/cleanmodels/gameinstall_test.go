package main

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func fakeInstall(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(dir, "data"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "data", "nwn_base.key"), []byte("KEY"), 0o644); err != nil {
		t.Fatal(err)
	}
	return dir
}

func noEnv(string) string { return "" }

// Order of preference: --game-dir, then $NWN_ROOT, then the usual locations.
func TestFindGameInstallOrder(t *testing.T) {
	steam, other := fakeInstall(t), fakeInstall(t)
	env := func(k string) string {
		if k == "NWN_ROOT" {
			return other
		}
		return ""
	}
	if g := findGameInstall("/explicit", env, []string{steam}); g.dir != "/explicit" || g.source != "--game-dir" {
		t.Errorf("flag should win: %+v", g)
	}
	if g := findGameInstall("", env, []string{steam}); g.dir != other || g.source != "$NWN_ROOT" {
		t.Errorf("NWN_ROOT should win over the search: %+v", g)
	}
	if g := findGameInstall("", noEnv, []string{steam}); g.dir != steam {
		t.Errorf("search should find the install: %+v", g)
	}
}

// A folder that merely exists is not an install: it has to hold the key file.
func TestFindGameInstallNeedsTheKeyFile(t *testing.T) {
	empty := t.TempDir()
	good := fakeInstall(t)
	if g := findGameInstall("", noEnv, []string{empty, "/does/not/exist"}); g.dir != "" {
		t.Errorf("found something that is not an install: %+v", g)
	}
	if g := findGameInstall("", noEnv, []string{empty, good}); g.dir != good {
		t.Errorf("should skip the empty folder and find %s: %+v", good, g)
	}
}

func TestReportGameInstall(t *testing.T) {
	var none, noneQuiet, bad, verbose bytes.Buffer
	reportGameInstallFor(&none, procOpts{}, gameInstall{})
	reportGameInstallFor(&noneQuiet, procOpts{quiet: true}, gameInstall{})
	reportGameInstallFor(&bad, procOpts{}, gameInstall{dir: t.TempDir(), source: "--game-dir"})
	good := fakeInstall(t)
	reportGameInstallFor(&verbose, procOpts{verbose: true}, gameInstall{dir: good, source: "$NWN_ROOT"})

	if !strings.Contains(none.String(), "no NWN install was found") || !strings.Contains(none.String(), "--game-dir") {
		t.Errorf("none: %q", none.String())
	}
	if noneQuiet.Len() != 0 {
		t.Errorf("--quiet should be silent: %q", noneQuiet.String())
	}
	if !strings.Contains(bad.String(), "not an NWN install") {
		t.Errorf("a folder without the key file should be called out: %q", bad.String())
	}
	if !strings.Contains(verbose.String(), good) {
		t.Errorf("verbose should name the install: %q", verbose.String())
	}
}
