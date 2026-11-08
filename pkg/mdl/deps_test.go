package mdl

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func compileDeps(t *testing.T, m *Model, opts CompileOptions) []Dependency {
	t.Helper()
	var deps []Dependency
	opts.Dependency = func(d Dependency) { deps = append(deps, d) }
	var buf bytes.Buffer
	if err := CompileWithOptions(m, &buf, opts); err != nil {
		t.Fatal(err)
	}
	return deps
}

func findDep(deps []Dependency, kind, name string) (Dependency, bool) {
	for _, d := range deps {
		if d.Kind == kind && d.Name == name {
			return d, true
		}
	}
	return Dependency{}, false
}

func allHold(deps []Dependency, opts CompileOptions) bool {
	for _, d := range deps {
		if ResolveDependency(d, opts) != d {
			return false
		}
	}
	return true
}

func writeFiles(t *testing.T, dir string, files map[string]string) {
	t.Helper()
	for name, body := range files {
		if err := os.WriteFile(filepath.Join(dir, name), []byte(body), 0o644); err != nil {
			t.Fatal(err)
		}
	}
}

// An ASCII supermodel is numbered against its own supermodel, so the whole
// chain is a dependency, and it stays one when the supermodel comes from the
// process's supermodel cache on a second compile.
func TestDependenciesSupermodelChain(t *testing.T) {
	dir := t.TempDir()
	grand := strings.ReplaceAll(superASCII, "sup", "grand")
	writeFiles(t, dir, map[string]string{
		"sup.mdl":   strings.Replace(superASCII, "setsupermodel sup NULL", "setsupermodel sup grand", 1),
		"grand.mdl": grand,
	})
	opts := CompileOptions{SupermodelDirs: []string{dir}}
	for pass := 1; pass <= 2; pass++ {
		deps := compileDeps(t, mustParseASCII(t, childASCII), opts)
		for _, name := range []string{"sup", "grand"} {
			d, ok := findDep(deps, "mdl", name)
			if !ok || !d.Found || d.SHA256 == "" || d.List != "supermodel" {
				t.Errorf("pass %d: dependency %s = %+v (present %v), want found with a hash", pass, name, d, ok)
			}
		}
		if !allHold(deps, opts) {
			t.Errorf("pass %d: dependencies do not resolve the same again", pass)
		}
	}

	deps := compileDeps(t, mustParseASCII(t, childASCII), opts)
	writeFiles(t, dir, map[string]string{"grand.mdl": grand + "\n# changed\n"})
	if d, _ := findDep(deps, "mdl", "grand"); ResolveDependency(d, opts) == d {
		t.Error("a changed grand.mdl still resolves to the stored hash")
	}
}

// A body part looks up its skeleton and the capart.2da that names its attach
// node; adding, changing or removing either changes what it resolves to.
func TestDependenciesPartSkeletonAndCapart(t *testing.T) {
	dir := t.TempDir()
	skel := `newmodel pcx0
setsupermodel pcx0 NULL
classification CHARACTER
beginmodelgeom pcx0
  node dummy pcx0
    parent NULL
  endnode
  node dummy rootdummy
    parent pcx0
  endnode
  node dummy j_kao
    parent rootdummy
  endnode
endmodelgeom
donemodel pcx0
`
	capart := "2DA V2.0\n\n    NAME    MDLNAME    NODENAME\n0   0       FOOTR      J_Kao\n"
	writeFiles(t, dir, map[string]string{"pcx0.mdl": skel})
	part := strings.NewReplacer("ptx0_neck001", "pcx0_footr010", "neck_g 0.5 head_g 0.5", "j_kao 1.0", "head_g 0.6 torso_g 0.4", "j_kao 1.0", "neck_g 1.0", "j_kao 1.0").Replace(testNeckPartASCII)
	opts := CompileOptions{SupermodelDirs: []string{dir}}

	deps := compileDeps(t, mustParseASCII(t, part), opts)
	if d, ok := findDep(deps, "mdl", "pcx0"); !ok || !d.Found {
		t.Fatalf("skeleton dependency %+v (present %v), want found", d, ok)
	}
	missing, ok := findDep(deps, "2da", "capart")
	if !ok || missing.Found {
		t.Fatalf("capart dependency %+v (present %v), want recorded as not found", missing, ok)
	}
	if !allHold(deps, opts) {
		t.Fatal("dependencies do not resolve the same again")
	}

	writeFiles(t, dir, map[string]string{"capart.2da": capart})
	if now := ResolveDependency(missing, opts); !now.Found || now == missing {
		t.Errorf("an added capart.2da resolves to %+v", now)
	}
	deps = compileDeps(t, mustParseASCII(t, part), opts)
	found, _ := findDep(deps, "2da", "capart")
	writeFiles(t, dir, map[string]string{"capart.2da": capart + "1   0       NECK       neck_g\n"})
	if ResolveDependency(found, opts) == found {
		t.Error("a changed capart.2da still resolves to the stored hash")
	}
	if err := os.Remove(filepath.Join(dir, "pcx0.mdl")); err != nil {
		t.Fatal(err)
	}
	skelDep, _ := findDep(deps, "mdl", "pcx0")
	if now := ResolveDependency(skelDep, opts); now.Found {
		t.Errorf("a removed skeleton resolves to %+v", now)
	}
}

// Materials decide tangents: each one looked up for a mesh is a dependency,
// found or not, and an empty or NULL name is not a lookup.
func TestDependenciesMaterials(t *testing.T) {
	dir := t.TempDir()
	writeFiles(t, dir, map[string]string{"wall.mtr": "renderhint NormalAndSpecMapped\n"})
	m := mustParseASCII(t, `newmodel matt
setsupermodel matt NULL
classification TILE
beginmodelgeom matt
  node dummy matt
    parent NULL
  endnode
  node trimesh plane
    parent matt
    bitmap wall
    materialname nosuchmat
    verts 3
      0 0 0
      1 0 0
      0 1 0
    tverts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2 1 0 1 2 1
  endnode
endmodelgeom
donemodel matt
`)
	opts := CompileOptions{ResourceDirs: []string{dir}}
	deps := compileDeps(t, m, opts)
	if d, ok := findDep(deps, "mtr", "nosuchmat"); !ok || d.Found || d.List != "resource" {
		t.Errorf("nosuchmat: %+v (present %v), want recorded as not found", d, ok)
	}
	wall, ok := findDep(deps, "mtr", "wall")
	if !ok || !wall.Found {
		t.Fatalf("wall: %+v (present %v), want found", wall, ok)
	}
	for _, d := range deps {
		if d.Name == "" || d.Name == "null" {
			t.Errorf("recorded a lookup of no material: %+v", d)
		}
	}
	if !allHold(deps, opts) {
		t.Fatal("dependencies do not resolve the same again")
	}
	// The material name comes first: once it exists, the lookup changes. A
	// folder's materials are indexed once per process (materialIndexes), as a
	// CLI run sees one state of the disk; the same folder spelled with a
	// trailing separator is indexed afresh, as a later run would.
	writeFiles(t, dir, map[string]string{"nosuchmat.mtr": "renderhint NormalAndSpecMapped\n"})
	if allHold(deps, CompileOptions{ResourceDirs: []string{dir + string(os.PathSeparator)}}) {
		t.Error("an added nosuchmat.mtr does not change the dependencies")
	}
}

// Without CompileOptions.Dependency nothing is recorded or hashed.
func TestNoDependencyHook(t *testing.T) {
	var buf bytes.Buffer
	if err := CompileWithOptions(mustParseASCII(t, childASCII), &buf, CompileOptions{}); err != nil {
		t.Fatal(err)
	}
}
