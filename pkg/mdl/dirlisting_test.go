package mdl

import (
	"fmt"
	"os"
	"path/filepath"
	"testing"
	"time"
)

// settle backdates dir so its listing is kept, as for a folder nothing has
// written to for a while.
func settle(t testing.TB, dir string) {
	t.Helper()
	old := time.Now().Add(-time.Hour)
	if err := os.Chtimes(dir, old, old); err != nil {
		t.Fatal(err)
	}
}

func refFile(ref *resourceRef) string {
	if ref == nil {
		return ""
	}
	return filepath.Base(ref.id)
}

// A folder is listed once and the listing reused, but a lookup still finds
// what a fresh listing would: names compare without case (the first in
// directory order wins), subdirectories are not files, and a file added,
// rewritten or removed since is seen.
func TestFindResourceDirListing(t *testing.T) {
	own, res := t.TempDir(), t.TempDir()
	writeFiles(t, own, map[string]string{"PART.mdl": "upper", "part.mdl": "lower", "only.MDL": "x"})
	writeFiles(t, res, map[string]string{"sub.mdl": "res", "only.mdl": "res", "capart.2da": "2DA"})
	if err := os.Mkdir(filepath.Join(own, "sub.mdl"), 0o755); err != nil {
		t.Fatal(err)
	}
	settle(t, own)
	settle(t, res)
	locs := []string{own, res}

	for pass := 1; pass <= 2; pass++ {
		if got := refFile(findModelResource("Part", locs)); got != "PART.mdl" {
			t.Errorf("pass %d: part = %q, want PART.mdl (first in directory order)", pass, got)
		}
		if ref := findModelResource("only", locs); ref == nil || filepath.Dir(ref.id) != own {
			t.Errorf("pass %d: only = %v, want the own folder's only.MDL", pass, ref)
		}
		if ref := findModelResource("sub", locs); ref == nil || filepath.Dir(ref.id) != res {
			t.Errorf("pass %d: sub = %v, want the file in res, not the directory in own", pass, ref)
		}
		if refFile(findResource("CAPART", resType2DA, ".2da", locs)) != "capart.2da" {
			t.Errorf("pass %d: capart.2da not found", pass)
		}
		if findModelResource("missing", locs) != nil {
			t.Errorf("pass %d: found a model that is not there", pass)
		}
	}
	if _, ok := dirListings.Load(own); !ok {
		t.Fatal("the own folder's listing was not kept")
	}

	// Rewritten in place: the folder's time doesn't move, the file's stamp does.
	before := findModelResource("only", locs).stamp
	writeFiles(t, own, map[string]string{"only.MDL": "rewritten"})
	if after := findModelResource("only", locs).stamp; after == before {
		t.Errorf("stamp %q unchanged after only.MDL was rewritten", after)
	}

	// Added and removed: the folder's time moves, so it is listed again.
	writeFiles(t, own, map[string]string{"new.mdl": "new"})
	if refFile(findModelResource("new", locs)) != "new.mdl" {
		t.Error("new.mdl, added after the folder was listed, not found")
	}
	if err := os.Remove(filepath.Join(own, "PART.mdl")); err != nil {
		t.Fatal(err)
	}
	if got := refFile(findModelResource("part", locs)); got != "part.mdl" {
		t.Errorf("after removing PART.mdl: part = %q, want part.mdl", got)
	}
}

// A folder changed in the last few seconds is listed for every lookup: its
// time could stay the same through a second change.
func TestFindResourceDirListingNotKeptWhileRecent(t *testing.T) {
	dir := t.TempDir()
	writeFiles(t, dir, map[string]string{"a.mdl": "a"})
	if findModelResource("a", []string{dir}) == nil {
		t.Fatal("a.mdl not found")
	}
	if _, ok := dirListings.Load(dir); ok {
		t.Fatal("kept the listing of a folder changed just now")
	}
}

// BenchmarkFindResourceWarm is a cache hit's lookups in a model's own folder of
// 10,000 files: a found supermodel and a missing one, as depsHold resolves
// them again on every hit.
func BenchmarkFindResourceWarm(b *testing.B) {
	dir := b.TempDir()
	for i := 0; i < 10000; i++ {
		if err := os.WriteFile(filepath.Join(dir, fmt.Sprintf("pmh0_chest%05d.mdl", i)), nil, 0o644); err != nil {
			b.Fatal(err)
		}
	}
	settle(b, dir)
	opts := CompileOptions{SupermodelDirs: []string{dir}}
	found := ResolveDependency(Dependency{Kind: depKindModel, Name: "pmh0_chest04321", List: depListSupermodel}, opts)
	missing := ResolveDependency(Dependency{Kind: depKindModel, Name: "a_ba", List: depListSupermodel}, opts)
	if !found.Found || missing.Found {
		b.Fatalf("found %+v, missing %+v", found, missing)
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if ResolveDependency(found, opts) != found || ResolveDependency(missing, opts) != missing {
			b.Fatal("a dependency resolved differently")
		}
	}
}
