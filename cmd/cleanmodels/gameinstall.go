package main

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"sync"
)

// gameInstall is where the game's own data (stock supermodels and materials) is
// read from when compiling, and how it was found.
type gameInstall struct {
	dir    string
	source string // "--game-dir", "$NWN_ROOT" or "found at a usual install location"
}

// hasGameData reports whether dir looks like an NWN install: it holds the key
// file the game's resources are indexed by.
func hasGameData(dir string) bool {
	st, err := os.Stat(filepath.Join(dir, "data", "nwn_base.key"))
	return err == nil && st.Mode().IsRegular()
}

// gameInstallCandidates lists the usual install locations on this platform.
// It is best effort: an install in another Steam library or a custom folder is
// found with --game-dir or $NWN_ROOT instead.
func gameInstallCandidates() []string {
	home, _ := os.UserHomeDir()
	steam := filepath.Join("steamapps", "common", "Neverwinter Nights")
	var c []string
	switch runtime.GOOS {
	case "windows":
		for _, pf := range []string{os.Getenv("ProgramFiles(x86)"), os.Getenv("ProgramFiles"), `C:\Program Files (x86)`} {
			if pf != "" {
				c = append(c, filepath.Join(pf, "Steam", steam), filepath.Join(pf, "GOG Galaxy", "Games", "NWN Enhanced Edition"))
			}
		}
		c = append(c, `C:\GOG Games\Neverwinter Nights Enhanced Edition`)
	case "darwin":
		if home != "" {
			c = append(c, filepath.Join(home, "Library", "Application Support", "Steam", steam))
		}
	default:
		if home != "" {
			c = append(c,
				filepath.Join(home, ".local", "share", "Steam", steam),
				filepath.Join(home, ".steam", "steam", steam),
				filepath.Join(home, ".steam", "root", steam),
				filepath.Join(home, ".var", "app", "com.valvesoftware.Steam", ".local", "share", "Steam", steam),
				filepath.Join(home, "GOG Games", "Neverwinter Nights Enhanced Edition"),
			)
		}
	}
	return c
}

// findGameInstall resolves the game data: --game-dir, then $NWN_ROOT, then the
// usual install locations. An explicit choice is used as given (the caller
// reports it if it holds no game data); only the search checks for the key file.
func findGameInstall(flagDir string, env func(string) string, candidates []string) gameInstall {
	if flagDir != "" {
		return gameInstall{flagDir, "--game-dir"}
	}
	if root := env("NWN_ROOT"); root != "" {
		return gameInstall{root, "$NWN_ROOT"}
	}
	for _, c := range candidates {
		if hasGameData(c) {
			return gameInstall{c, "found at a usual install location"}
		}
	}
	return gameInstall{}
}

var gameInstallCache sync.Map // --game-dir value -> gameInstall

// resolveGameInstall is findGameInstall for this machine, done once per run:
// compile asks for every file of a batch.
func resolveGameInstall(flagDir string) gameInstall {
	if v, ok := gameInstallCache.Load(flagDir); ok {
		return v.(gameInstall)
	}
	g := findGameInstall(flagDir, os.Getenv, gameInstallCandidates())
	gameInstallCache.Store(flagDir, g)
	return g
}

// reportGameInstall says, once per run, where compile is reading the game's data
// from, or that it found none. Without it a normal-mapped tile that needs its
// material compiles without tangents and nothing says why.
func reportGameInstall(w io.Writer, o procOpts) {
	reportGameInstallFor(w, o, resolveGameInstall(o.gameDir))
}

func reportGameInstallFor(w io.Writer, o procOpts, g gameInstall) {
	switch {
	case g.dir == "":
		if !o.quiet {
			fmt.Fprint(w, "note: no NWN install was found, so the game's materials and supermodels were not consulted:\n"+
				"      a mesh that is normal-mapped only through its material gets no tangents. Use --game-dir or NWN_ROOT.\n")
		}
	case !hasGameData(g.dir):
		fmt.Fprintf(w, "warning: %s (%s) holds no data/nwn_base.key, so it is not an NWN install and its materials will not be found\n", g.dir, g.source)
	case o.verbose && !o.quiet:
		fmt.Fprintf(w, "game data: %s (%s)\n", g.dir, g.source)
	}
}
