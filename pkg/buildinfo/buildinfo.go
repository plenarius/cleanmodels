// Package buildinfo formats a human-readable version string for the
// cleanmodels binaries.
//
// The release tag is set at link time via -ldflags -X main.version — the
// exact pushed tag for CI release builds, or the output of `git describe
// --tags --always --dirty` for local builds via `make build` (see
// Makefile). Go's VCS stamping has no notion of tags, so this package
// cannot derive one on its own and instead layers on what it can read
// automatically from the binary: the exact commit revision, its commit
// time, whether the working tree was dirty at build time, and the Go
// toolchain version used to build it.
package buildinfo

import (
	"runtime"
	"runtime/debug"
	"strings"
)

// shortRevisionLen matches the default length of `git rev-parse --short`.
const shortRevisionLen = 7

// Version returns tag augmented with build metadata, e.g.:
//
//	v1.2.3 (revision 1a2b3c4, 2026-08-20T10:04:11Z, go1.26.2)
//	dev (revision 1a2b3c4, dirty, 2026-08-20T10:04:11Z, go1.26.2)
//
// The Go version is always included. VCS metadata (revision, commit
// time, dirty flag) is only available when built from within a git
// checkout — e.g. not from an extracted module zip — and is omitted
// entirely if absent.
func Version(tag string) string {
	info, ok := debug.ReadBuildInfo()

	goVersion := runtime.Version()
	var parts []string

	if ok {
		if info.GoVersion != "" {
			goVersion = info.GoVersion
		}
		if rev, t, dirty := revision(info); rev != "" {
			parts = append(parts, "revision "+rev)
			if dirty {
				parts = append(parts, "dirty")
			}
			if t != "" {
				parts = append(parts, t)
			}
		}
	}
	parts = append(parts, goVersion)

	var b strings.Builder
	b.WriteString(tag)
	b.WriteString(" (")
	b.WriteString(strings.Join(parts, ", "))
	b.WriteString(")")
	return b.String()
}

// revision extracts VCS metadata embedded by the Go toolchain at build
// time. rev is empty when no revision is available.
func revision(info *debug.BuildInfo) (rev, t string, dirty bool) {
	for _, s := range info.Settings {
		switch s.Key {
		case "vcs.revision":
			rev = s.Value
			if len(rev) > shortRevisionLen {
				rev = rev[:shortRevisionLen]
			}
		case "vcs.time":
			t = s.Value
		case "vcs.modified":
			dirty = s.Value == "true"
		}
	}
	return rev, t, dirty
}
