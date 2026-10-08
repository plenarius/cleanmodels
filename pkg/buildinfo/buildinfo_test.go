package buildinfo

import (
	"runtime"
	"runtime/debug"
	"strings"
	"testing"
)

func TestFormat(t *testing.T) {
	cases := []struct {
		name       string
		tag        string
		rev        string
		commitTime string
		dirty      bool
		goVersion  string
		want       string
	}{
		{
			name: "tag, revision, time and go version",
			tag:  "v1.2.3", rev: "1a2b3c4", commitTime: "2026-08-20T10:04:11Z", goVersion: "go1.26.2",
			want: "v1.2.3 (revision 1a2b3c4, 2026-08-20T10:04:11Z, go1.26.2)",
		},
		{
			// "dirty" sits between the revision and the commit time: the tree
			// state qualifies the revision, so it reads as part of it.
			name: "dirty follows the revision",
			tag:  "dev", rev: "1a2b3c4", commitTime: "2026-08-20T10:04:11Z", dirty: true, goVersion: "go1.26.2",
			want: "dev (revision 1a2b3c4, dirty, 2026-08-20T10:04:11Z, go1.26.2)",
		},
		{
			// No VCS stamping (module zip build, or a git worktree, where the
			// Go toolchain records nothing). Only the Go version survives.
			name: "no revision drops all vcs fields",
			tag:  "v1.2.3", goVersion: "go1.26.2",
			want: "v1.2.3 (go1.26.2)",
		},
		{
			// A dirty flag without a revision must not leak out on its own.
			name: "dirty without a revision is omitted",
			tag:  "v1.2.3", dirty: true, goVersion: "go1.26.2",
			want: "v1.2.3 (go1.26.2)",
		},
		{
			name: "revision without a commit time",
			tag:  "v1.2.3", rev: "1a2b3c4", goVersion: "go1.26.2",
			want: "v1.2.3 (revision 1a2b3c4, go1.26.2)",
		},
		{
			name: "dirty revision without a commit time",
			tag:  "v1.2.3", rev: "1a2b3c4", dirty: true, goVersion: "go1.26.2",
			want: "v1.2.3 (revision 1a2b3c4, dirty, go1.26.2)",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := format(tc.tag, tc.rev, tc.commitTime, tc.dirty, tc.goVersion)
			if got != tc.want {
				t.Errorf("format() = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestRevision(t *testing.T) {
	setting := func(kv ...string) *debug.BuildInfo {
		info := &debug.BuildInfo{}
		for i := 0; i < len(kv); i += 2 {
			info.Settings = append(info.Settings, debug.BuildSetting{Key: kv[i], Value: kv[i+1]})
		}
		return info
	}

	cases := []struct {
		name      string
		info      *debug.BuildInfo
		wantRev   string
		wantTime  string
		wantDirty bool
	}{
		{
			name:    "full sha is shortened to 7 chars",
			info:    setting("vcs.revision", "1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b"),
			wantRev: "1a2b3c4",
		},
		{
			// Shorter-than-usual values pass through untouched rather than
			// being padded or rejected.
			name:    "short revision is left alone",
			info:    setting("vcs.revision", "1a2b"),
			wantRev: "1a2b",
		},
		{
			name:      "modified true means dirty",
			info:      setting("vcs.revision", "1a2b3c4", "vcs.modified", "true"),
			wantRev:   "1a2b3c4",
			wantDirty: true,
		},
		{
			name:      "modified false means clean",
			info:      setting("vcs.revision", "1a2b3c4", "vcs.modified", "false"),
			wantRev:   "1a2b3c4",
			wantDirty: false,
		},
		{
			name:     "commit time is passed through",
			info:     setting("vcs.revision", "1a2b3c4", "vcs.time", "2026-08-20T10:04:11Z"),
			wantRev:  "1a2b3c4",
			wantTime: "2026-08-20T10:04:11Z",
		},
		{
			// Builds with no VCS stamping at all.
			name: "no settings yields nothing",
			info: setting(),
		},
		{
			// Unrelated settings (compiler flags etc.) must not be mistaken
			// for VCS metadata.
			name: "unrelated settings are ignored",
			info: setting("-ldflags", "-s -w", "GOARCH", "amd64"),
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			rev, ts, dirty := revision(tc.info)
			if rev != tc.wantRev {
				t.Errorf("rev = %q, want %q", rev, tc.wantRev)
			}
			if ts != tc.wantTime {
				t.Errorf("time = %q, want %q", ts, tc.wantTime)
			}
			if dirty != tc.wantDirty {
				t.Errorf("dirty = %v, want %v", dirty, tc.wantDirty)
			}
		})
	}
}

// TestVersion covers the parts that hold regardless of how the test binary was
// built: the tag is preserved verbatim, a Go version is always reported, and
// the metadata is parenthesised.
func TestVersion(t *testing.T) {
	got := Version("v4.0.0-rc13")

	if !strings.HasPrefix(got, "v4.0.0-rc13 (") || !strings.HasSuffix(got, ")") {
		t.Errorf("Version() = %q, want the tag followed by parenthesised metadata", got)
	}
	// Either the toolchain's own recorded version or the runtime's; both start
	// with "go".
	if !strings.Contains(got, "go") {
		t.Errorf("Version() = %q, want it to report a Go version", got)
	}
	if !strings.Contains(got, runtime.Version()) {
		// Not fatal: a binary built by a different toolchain than the one
		// running the test would legitimately differ.
		t.Logf("Version() = %q does not contain runtime.Version() = %q", got, runtime.Version())
	}
}
