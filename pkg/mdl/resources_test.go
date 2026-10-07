package mdl

import (
	"encoding/binary"
	"os"
	"path/filepath"
	"testing"
)

type testRes struct {
	name string
	typ  uint16
	data string
}

func le32(b []byte, v int) { binary.LittleEndian.PutUint32(b, uint32(v)) }

func buildERF(sig string, res []testRes) []byte {
	n := len(res)
	keyOff, resOff := 160, 160+24*n
	dataOff := resOff + 8*n
	out := make([]byte, dataOff)
	copy(out, sig)
	le32(out[16:], n)
	le32(out[24:], keyOff)
	le32(out[28:], resOff)
	for i, r := range res {
		k := out[keyOff+i*24:]
		copy(k, r.name)
		le32(k[16:], i)
		binary.LittleEndian.PutUint16(k[20:], r.typ)
		le32(out[resOff+i*8:], len(out))
		le32(out[resOff+i*8+4:], len(r.data))
		out = append(out, r.data...)
	}
	return out
}

func TestResourcesFromERF(t *testing.T) {
	dir := t.TempDir()
	hak := filepath.Join(dir, "x.hak")
	data := buildERF("HAK V1.0", []testRes{
		{"Wall_Stone", resTypeMTR, "renderhint NormalAndSpecMapped\n"},
		{"pmh0", resTypeMDL, "newmodel pmh0\n"},
		{"ignored", 2017, "2da"},
	})
	if err := os.WriteFile(hak, data, 0o644); err != nil {
		t.Fatal(err)
	}

	ref := findMaterial("wall_stone", []string{hak})
	if ref == nil || materialRenderHint(ref) != "normalandspecmapped" {
		t.Fatalf("material not read from the archive: %v", ref)
	}
	m := findModelResource("PMH0", []string{hak})
	if m == nil {
		t.Fatal("model not found in the archive")
	}
	if b, err := m.read(); err != nil || string(b) != "newmodel pmh0\n" {
		t.Fatalf("model data = %q, %v", b, err)
	}
	if findModelResource("ignored", []string{hak}) != nil || findMaterial("missing", []string{hak}) != nil {
		t.Fatal("found a resource that is not there")
	}
	if !isNormalMapped(&MeshData{MaterialName: "wall_stone"}, []string{hak}) {
		t.Fatal("mesh with a normal-mapped material in a hak should get tangents")
	}
}

func TestResourcesFromGameInstall(t *testing.T) {
	root := t.TempDir()
	if err := os.Mkdir(filepath.Join(root, "data"), 0o755); err != nil {
		t.Fatal(err)
	}

	// A BIFF V1 file with two variable resources.
	payloads := []string{"first model", "second model"}
	bif := make([]byte, 20+2*16)
	copy(bif, "BIFFV1  ")
	le32(bif[8:], 2)
	le32(bif[16:], 20)
	for i, p := range payloads {
		e := bif[20+i*16:]
		le32(e, i)
		le32(e[4:], len(bif))
		le32(e[8:], len(p))
		le32(e[12:], int(resTypeMDL))
		bif = append(bif, p...)
	}
	// The key file names it with a backslash path in different case, as the game's do.
	const bifName = `Data\Pkg0.BIF`
	key := make([]byte, 64+12+22*2)
	copy(key, "KEY V1  ")
	le32(key[8:], 1)
	le32(key[12:], 2)
	le32(key[16:], 64)
	le32(key[20:], 64+12+len(bifName))
	le32(key[64+4:], 64+12)
	binary.LittleEndian.PutUint16(key[64+8:], uint16(len(bifName)))
	key = append(key[:64+12], append([]byte(bifName), make([]byte, 22*2)...)...)
	for i, n := range []string{"alpha", "beta"} {
		e := key[64+12+len(bifName)+i*22:]
		copy(e, n)
		binary.LittleEndian.PutUint16(e[16:], resTypeMDL)
		le32(e[18:], i)
	}
	le32(key[20:], 64+12+len(bifName))
	for name, b := range map[string][]byte{"nwn_base.key": key, "pkg0.bif": bif} {
		if err := os.WriteFile(filepath.Join(root, "data", name), b, 0o644); err != nil {
			t.Fatal(err)
		}
	}

	for name, want := range map[string]string{"alpha": payloads[0], "BETA": payloads[1]} {
		ref := findModelResource(name, []string{root})
		if ref == nil {
			t.Fatalf("%s not found in the install", name)
		}
		if b, err := ref.read(); err != nil || string(b) != want {
			t.Fatalf("%s = %q, %v; want %q", name, b, err, want)
		}
	}
	if findModelResource("gamma", []string{root}) != nil {
		t.Fatal("found a model that is not there")
	}
}
