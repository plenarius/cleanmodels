package mdl

import (
	"encoding/binary"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"
)

// Supermodels and materials are looked up in a list of locations. A location is
// one of:
//
//   - a directory (models are matched in the directory itself, materials
//     anywhere beneath it);
//   - an ERF-family archive: a .hak, .erf or .mod file;
//   - the game's install directory, recognised by data/nwn_base.key. Its
//     resources are read out of the BIF files the key files list, which is where
//     the stock body-part skeletons (pmh0, pfh0, ...) live.
//
// Only models (.mdl), materials (.mtr), tables (.2da, for capart.2da) and,
// for variant previews, palette textures (.plt) and TGA images are indexed
// from archives.

const (
	resTypeMDL uint16 = 2002
	resType2DA uint16 = 2017
	resTypeMTR uint16 = 2072
)

// resourceRef is a resource found in a location.
type resourceRef struct {
	id    string // unique, for caches and messages
	stamp string // changes when the underlying file does ("" for archives)
	read  func() ([]byte, error)
}

// archive is an indexed ERF or game install.
type archive struct {
	entries map[archiveKey]func() ([]byte, error)
}

type archiveKey struct {
	name string // lower-case resref
	typ  uint16
}

var archives sync.Map // location -> *archiveLoad

type archiveLoad struct {
	once sync.Once
	a    *archive // nil when the location is a plain directory or unreadable
}

func openArchive(loc string) *archive {
	v, _ := archives.LoadOrStore(loc, &archiveLoad{})
	l := v.(*archiveLoad)
	l.once.Do(func() {
		st, err := os.Stat(loc)
		if err != nil {
			return
		}
		if st.IsDir() {
			if _, err := os.Stat(filepath.Join(loc, "data", "nwn_base.key")); err == nil {
				l.a, _ = readGameInstall(loc)
			}
			return
		}
		l.a, _ = readERF(loc)
	})
	return l.a
}

func wantedType(t uint16) bool {
	return t == resTypeMDL || t == resTypeMTR || t == resType2DA || t == resTypePLT || t == resTypeTGA
}

func readAt(path string, off, size int64) ([]byte, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	buf := make([]byte, size)
	if _, err := f.ReadAt(buf, off); err != nil && err != io.EOF {
		return nil, err
	}
	return buf, nil
}

// readERF indexes a HAK/ERF/MOD archive.
func readERF(path string) (*archive, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	var hdr [160]byte
	if _, err := io.ReadFull(f, hdr[:]); err != nil {
		return nil, err
	}
	switch string(hdr[0:8]) {
	case "HAK V1.0", "ERF V1.0", "MOD V1.0":
	default:
		return nil, fmt.Errorf("%s: not an ERF archive", path)
	}
	count := int64(binary.LittleEndian.Uint32(hdr[16:]))
	keyOff := int64(binary.LittleEndian.Uint32(hdr[24:]))
	resOff := int64(binary.LittleEndian.Uint32(hdr[28:]))
	keys := make([]byte, count*24)
	if _, err := f.ReadAt(keys, keyOff); err != nil && err != io.EOF {
		return nil, err
	}
	table := make([]byte, count*8)
	if _, err := f.ReadAt(table, resOff); err != nil && err != io.EOF {
		return nil, err
	}
	a := &archive{entries: make(map[archiveKey]func() ([]byte, error))}
	for i := int64(0); i < count; i++ {
		k := keys[i*24 : i*24+24]
		typ := binary.LittleEndian.Uint16(k[20:])
		if !wantedType(typ) {
			continue
		}
		name := strings.ToLower(strings.TrimRight(string(k[:16]), "\x00"))
		off := int64(binary.LittleEndian.Uint32(table[i*8:]))
		size := int64(binary.LittleEndian.Uint32(table[i*8+4:]))
		a.entries[archiveKey{name, typ}] = func() ([]byte, error) { return readAt(path, off, size) }
	}
	return a, nil
}

// readGameInstall indexes every data/*.key file of an install. Files are listed
// in the order read; the first key file to name a resource wins, so
// nwn_base.key is read before nwn_retail.key's additions.
func readGameInstall(root string) (*archive, error) {
	dataDir := filepath.Join(root, "data")
	keyFiles, _ := filepath.Glob(filepath.Join(dataDir, "*.key"))
	a := &archive{entries: make(map[archiveKey]func() ([]byte, error))}
	// Loose files in ovr/ override the key files' resources, as in the game.
	if entries, err := os.ReadDir(filepath.Join(root, "ovr")); err == nil {
		for _, e := range entries {
			ext := strings.ToLower(filepath.Ext(e.Name()))
			typ, ok := map[string]uint16{".mdl": resTypeMDL, ".mtr": resTypeMTR, ".2da": resType2DA, ".plt": resTypePLT, ".tga": resTypeTGA}[ext]
			if e.IsDir() || !ok {
				continue
			}
			p := filepath.Join(root, "ovr", e.Name())
			a.entries[archiveKey{strings.ToLower(strings.TrimSuffix(e.Name(), filepath.Ext(e.Name()))), typ}] = func() ([]byte, error) { return os.ReadFile(p) }
		}
	}
	for _, kf := range keyFiles {
		if err := indexKeyFile(a, root, kf); err != nil {
			return nil, err
		}
	}
	return a, nil
}

func indexKeyFile(a *archive, root, path string) error {
	data, err := os.ReadFile(path)
	if err != nil {
		return err
	}
	if len(data) < 64 || string(data[0:8]) != "KEY V1  " {
		return fmt.Errorf("%s: not a KEY V1 file", path)
	}
	bifCount := int(binary.LittleEndian.Uint32(data[8:]))
	keyCount := int(binary.LittleEndian.Uint32(data[12:]))
	fileOff := int(binary.LittleEndian.Uint32(data[16:]))
	keyOff := int(binary.LittleEndian.Uint32(data[20:]))
	if fileOff+bifCount*12 > len(data) || keyOff+keyCount*22 > len(data) {
		return fmt.Errorf("%s: truncated", path)
	}
	bifs := make([]string, bifCount)
	for i := range bifs {
		e := data[fileOff+i*12:]
		nameOff := int(binary.LittleEndian.Uint32(e[4:]))
		nameLen := int(binary.LittleEndian.Uint16(e[8:]))
		if nameOff+nameLen > len(data) {
			return fmt.Errorf("%s: bad BIF name", path)
		}
		bifs[i] = resolveCase(root, strings.ReplaceAll(strings.TrimRight(string(data[nameOff:nameOff+nameLen]), "\x00"), `\`, "/"))
	}
	for i := 0; i < keyCount; i++ {
		e := data[keyOff+i*22:]
		typ := binary.LittleEndian.Uint16(e[16:])
		if !wantedType(typ) {
			continue
		}
		name := strings.ToLower(strings.TrimRight(string(e[:16]), "\x00"))
		id := binary.LittleEndian.Uint32(e[18:])
		bif, idx := int(id>>20), int64(id&0xFFFFF)
		if bif >= len(bifs) {
			continue
		}
		k := archiveKey{name, typ}
		if _, dup := a.entries[k]; dup {
			continue
		}
		bifPath := bifs[bif]
		a.entries[k] = func() ([]byte, error) { return readBIF(bifPath, idx) }
	}
	return nil
}

// readBIF reads variable resource idx out of a BIFF V1 file.
func readBIF(path string, idx int64) ([]byte, error) {
	hdr, err := readAt(path, 0, 20)
	if err != nil {
		return nil, err
	}
	if string(hdr[0:8]) != "BIFFV1  " {
		return nil, fmt.Errorf("%s: not a BIFF V1 file", path)
	}
	if idx >= int64(binary.LittleEndian.Uint32(hdr[8:])) {
		return nil, fmt.Errorf("%s: resource %d out of range", path, idx)
	}
	ent, err := readAt(path, int64(binary.LittleEndian.Uint32(hdr[16:]))+idx*16, 16)
	if err != nil {
		return nil, err
	}
	return readAt(path, int64(binary.LittleEndian.Uint32(ent[4:])), int64(binary.LittleEndian.Uint32(ent[8:])))
}

// resolveCase finds rel under root ignoring letter case, as the key files'
// "data\nwn_base.bif" does not match the file names on a case-sensitive disk.
func resolveCase(root, rel string) string {
	cur := root
	for _, part := range strings.Split(rel, "/") {
		next := filepath.Join(cur, part)
		if _, err := os.Stat(next); err != nil {
			if entries, err := os.ReadDir(cur); err == nil {
				for _, e := range entries {
					if strings.EqualFold(e.Name(), part) {
						next = filepath.Join(cur, e.Name())
						break
					}
				}
			}
		}
		cur = next
	}
	return cur
}

// dirListing is one directory's file names, as of its modification time.
type dirListing struct {
	mtime time.Time
	names map[string]string // lower-case file name -> file name
}

// dirListings indexes each directory findResource searches: listing a model's
// own folder and the resource folders again for every lookup cost a batch
// compile most of its time on cache hits (a parts folder holds thousands of
// files). A listing is reused while the directory's modification time is
// unchanged, which the file system updates when a name is added, removed or
// renamed there; that costs one stat a lookup instead of a full listing. Inputs
// normally don't change during a run, but a run can add files to a folder it
// searches (variants writes beside its source, an in-place compile replaces
// each model through a temporary file), and a lookup must find what a fresh
// listing would.
var dirListings sync.Map // directory -> *dirListing

// dirNames returns loc's file names (not subdirectories) by lower-case name,
// or false when loc can't be listed. When names differ only in case, the
// first in os.ReadDir's order wins, as when the listing was scanned for each
// lookup.
func dirNames(loc string) (map[string]string, bool) {
	st, err := os.Stat(loc)
	if err != nil || !st.IsDir() {
		return nil, false
	}
	if v, ok := dirListings.Load(loc); ok {
		if l := v.(*dirListing); l.mtime.Equal(st.ModTime()) {
			return l.names, true
		}
	}
	entries, err := os.ReadDir(loc)
	if err != nil {
		return nil, false
	}
	names := make(map[string]string, len(entries))
	for _, e := range entries {
		if e.IsDir() {
			continue
		}
		k := strings.ToLower(e.Name())
		if _, dup := names[k]; !dup {
			names[k] = e.Name()
		}
	}
	// Kept with the time read before listing, so a change during the listing
	// moves the time on and the next lookup lists the folder again. A folder
	// changed in the last few seconds is not kept at all: a file system's
	// clock is coarse (from a few milliseconds to 2 seconds on FAT), so a
	// second change that soon could leave the time as it was.
	if time.Since(st.ModTime()) >= dirListingSettle {
		dirListings.Store(loc, &dirListing{mtime: st.ModTime(), names: names})
	}
	return names, true
}

// dirListingSettle is how long a directory must have gone unchanged before
// its listing is reused.
const dirListingSettle = 3 * time.Second

// findModelResource looks for <name>.mdl in locs, first match wins.
func findModelResource(name string, locs []string) *resourceRef {
	return findResource(name, resTypeMDL, ".mdl", locs)
}

// findResource looks for <name><ext> of type typ in locs, first match wins.
func findResource(name string, typ uint16, ext string, locs []string) *resourceRef {
	name = strings.ToLower(name)
	for _, loc := range locs {
		if a := openArchive(loc); a != nil {
			if read, ok := a.entries[archiveKey{name, typ}]; ok {
				return &resourceRef{id: loc + "#" + name + ext, read: read}
			}
			continue
		}
		names, ok := dirNames(loc)
		if !ok {
			continue
		}
		if file, ok := names[name+ext]; ok {
			p := filepath.Join(loc, file)
			// The file itself is stat'ed for every lookup, as before: a
			// file rewritten in place (an in-place compile) gets a new
			// stamp, so the stamp-keyed caches (supermodelCache,
			// contentHashes) don't hand out what it held before.
			stamp := ""
			if info, err := os.Lstat(p); err == nil {
				stamp = fmt.Sprintf("%d:%d", info.Size(), info.ModTime().UnixNano())
			}
			return &resourceRef{id: p, stamp: stamp, read: func() ([]byte, error) { return os.ReadFile(p) }}
		}
	}
	return nil
}
