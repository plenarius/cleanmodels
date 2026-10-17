package mdl

import (
	"encoding/binary"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// Preview textures. Robes, cloaks and armour parts are coloured in game from
// palette textures (PLT): each pixel names a layer (skin, hair, metal 1 and 2,
// cloth 1 and 2, leather 1 and 2, tattoo 1 and 2) and a shade, and the
// creature's chosen colour for that layer picks a row of the layer's palette
// image (pal_cloth01.tga: 256 shades across, one colour per row). For a
// preview, writePreviewTexture colours a PLT with fixed rows and writes it as
// a plain TGA any viewer reads.

// pltLayerPalettes are the palette images of the ten PLT layers, in layer
// order, and the row (colour) used for each in previews.
var pltLayerPalettes = [10]struct {
	image string
	row   int
}{
	{"pal_skin01", 0},   // skin: light tan
	{"pal_hair01", 0},   // hair: brown
	{"pal_armor01", 0},  // metal 1: steel
	{"pal_armor02", 8},  // metal 2: pale gold
	{"pal_cloth01", 52}, // cloth 1: red-brown (not the preview body's blue)
	{"pal_cloth01", 12}, // cloth 2: taupe
	{"pal_leath01", 4},  // leather 1: tan
	{"pal_leath01", 12}, // leather 2: grey-brown
	{"pal_tattoo01", 0}, // tattoo 1
	{"pal_tattoo01", 4}, // tattoo 2
}

const (
	resTypeTGA uint16 = 3
	resTypePLT uint16 = 6
)

// partTextureNames returns the textures the game tries for a robe, cloak or
// body part model, in order (NWMAIN_FINDINGS.md finding 30): its own name,
// the same with phenotype 0, the human's of its gender, the male human's;
// then the bitmap the model names.
func partTextureNames(model, bitmap string) []string {
	model = strings.ToLower(model)
	var names []string
	add := func(n string) {
		for _, x := range names {
			if x == n {
				return
			}
		}
		if n != "" && n != "null" {
			names = append(names, n)
		}
	}
	if prefix := VariantPrefix(model); bodyPrefix.MatchString(prefix) {
		rest := model[len(prefix):]
		add(model)
		add(prefix[:3] + "0" + rest)
		add("p" + prefix[1:2] + "h0" + rest)
		add("pmh0" + rest)
	}
	add(strings.ToLower(bitmap))
	return names
}

// previewTexture finds the first of names as a PLT in locs, colours it and
// writes <name>.tga to dir (once per name). It returns the texture's name,
// or "" when none is found or it cannot be read.
func previewTexture(names []string, locs []string, dir string, written map[string]bool) string {
	for _, name := range names {
		if written[name] {
			return name
		}
		ref := findResource(name, resTypePLT, ".plt", locs)
		if ref == nil {
			continue
		}
		data, err := ref.read()
		if err != nil {
			return ""
		}
		w, h, rgba, err := colourPLT(data, locs)
		if err != nil {
			return ""
		}
		if err := writeTGA(filepath.Join(dir, name+".tga"), w, h, rgba); err != nil {
			return ""
		}
		written[name] = true
		return name
	}
	return ""
}

// colourPLT turns a PLT into RGBA pixels (rows in the file's order) with the
// preview rows of pltLayerPalettes; a palette image that is not found gives
// that layer grey shades.
func colourPLT(data []byte, locs []string) (w, h int, rgba []byte, err error) {
	if len(data) < 24 || string(data[:8]) != "PLT V1  " {
		return 0, 0, nil, fmt.Errorf("not a PLT")
	}
	w = int(binary.LittleEndian.Uint32(data[16:]))
	h = int(binary.LittleEndian.Uint32(data[20:]))
	if w <= 0 || h <= 0 || w > 4096 || h > 4096 || len(data) < 24+2*w*h {
		return 0, 0, nil, fmt.Errorf("PLT size %dx%d does not fit its data", w, h)
	}
	palettes := map[string][][4]byte{}
	shades := func(layer int) [][4]byte {
		p := pltLayerPalettes[layer]
		key := fmt.Sprintf("%s/%d", p.image, p.row)
		if s, ok := palettes[key]; ok {
			return s
		}
		var s [][4]byte
		if ref := findResource(p.image, resTypeTGA, ".tga", locs); ref != nil {
			if d, err := ref.read(); err == nil {
				s = tgaRow(d, p.row)
			}
		}
		if len(s) != 256 {
			s = make([][4]byte, 256)
			for i := range s {
				s[i] = [4]byte{byte(i), byte(i), byte(i), 255}
			}
		}
		palettes[key] = s
		return s
	}
	rgba = make([]byte, 4*w*h)
	for i := 0; i < w*h; i++ {
		value, layer := data[24+2*i], int(data[25+2*i])
		if layer > 9 {
			layer = 9
		}
		c := shades(layer)[value]
		copy(rgba[4*i:], c[:])
	}
	return w, h, rgba, nil
}

// tgaRow returns row (counted from the top of the image) of an uncompressed
// 24- or 32-bit TGA as RGBA, or nil.
func tgaRow(d []byte, row int) [][4]byte {
	if len(d) < 18 || d[2] != 2 {
		return nil
	}
	idLen := int(d[0])
	w := int(binary.LittleEndian.Uint16(d[12:]))
	h := int(binary.LittleEndian.Uint16(d[14:]))
	bpp := int(d[16]) / 8
	if (bpp != 3 && bpp != 4) || row < 0 || row >= h {
		return nil
	}
	fileRow := row
	if d[17]&0x20 == 0 { // bottom-left origin: the top row is last
		fileRow = h - 1 - row
	}
	off := 18 + idLen + fileRow*w*bpp
	if off+w*bpp > len(d) {
		return nil
	}
	out := make([][4]byte, w)
	for x := 0; x < w; x++ {
		p := d[off+x*bpp:]
		a := byte(255)
		if bpp == 4 {
			a = p[3]
		}
		out[x] = [4]byte{p[2], p[1], p[0], a}
	}
	return out
}

// writeTGA writes RGBA pixels (rows bottom first, as PLTs and NWN's TGAs
// store them) as an uncompressed 32-bit TGA.
func writeTGA(path string, w, h int, rgba []byte) error {
	buf := make([]byte, 18+4*w*h)
	buf[2] = 2
	binary.LittleEndian.PutUint16(buf[12:], uint16(w))
	binary.LittleEndian.PutUint16(buf[14:], uint16(h))
	buf[16] = 32
	buf[17] = 8 // 8 alpha bits, bottom-left origin
	for i := 0; i < w*h; i++ {
		buf[18+4*i] = rgba[4*i+2]
		buf[19+4*i] = rgba[4*i+1]
		buf[20+4*i] = rgba[4*i]
		buf[21+4*i] = rgba[4*i+3]
	}
	return os.WriteFile(path, buf, 0o644)
}
