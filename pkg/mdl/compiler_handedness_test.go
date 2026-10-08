package mdl

import (
	"bytes"
	"encoding/binary"
	"math"
	"testing"
)

// normalMappedQuad is a two-triangle quad flagged NormalAndSpecMapped, which is
// what makes the compiler generate a tangent basis at all.
const normalMappedQuad = `# NWN MDL
filedependancy none
newmodel nmquad
setsupermodel nmquad NULL
classification TILE
setanimationscale 1.00
beginmodelgeom nmquad
  node dummy nmquad
    parent NULL
  endnode
  node trimesh top
    parent nmquad
    position 0 0 0
    orientation 0 0 1 0
    renderhint NormalAndSpecMapped
    bitmap blank
    render 1
    shadow 0
    verts 4
      -1 -1 0
       1 -1 0
       1  1 0
      -1  1 0
    tverts 4
      0 0 0
      1 0 0
      1 1 0
      0 1 0
    faces 2
      0 1 2  1  0 1 2  0
      0 2 3  1  0 2 3  0
  endnode
endmodelgeom
donemodel nmquad
`

// meshHeaderFields locates the compiled trimesh header via its texture0 string
// and returns the volatile-block tokens we care about. Offsets are derived the
// same way as TestCompiledMeshHasIndexBuffer: texture0 sits at mesh header
// +120, so the marker index is (header base + 120).
type meshHeaderFields struct {
	numVertices                                           uint16
	vertsTok, uvTok, normalTok, tangentTok, handednessTok uint32
	indexCount, indexTok                                  uint32
	haveIndex                                             bool
	mdxLen                                                uint32
}

func readMeshHeaderFields(t *testing.T, binData []byte) meshHeaderFields {
	t.Helper()
	idx := bytes.Index(binData, []byte("blank\x00"))
	if idx < 0 {
		t.Fatal(`could not locate texture0 ("blank") in compiled output`)
	}
	u32 := func(off int) uint32 { return binary.LittleEndian.Uint32(binData[off:]) }

	f := meshHeaderFields{
		mdxLen:        u32(8),
		numVertices:   binary.LittleEndian.Uint16(binData[idx+328:]), // +448
		vertsTok:      u32(idx + 324),                                // +444
		uvTok:         u32(idx + 332),                                // +452
		normalTok:     u32(idx + 348),                                // +468
		tangentTok:    u32(idx + 368),                                // +488
		handednessTok: u32(idx + 376),                                // +496
	}
	// m_listVertexTokenIndices ProxyList at +416, i.e. idx+296. Its single
	// element (and vertexindicescount's, 12 bytes earlier) are core-relative.
	tokenListOff := idx + 296
	countListOff := tokenListOff - 12
	if u32(tokenListOff+4) == 1 && u32(countListOff+4) == 1 {
		f.haveIndex = true
		f.indexCount = u32(12 + int(u32(countListOff)))
		f.indexTok = u32(12 + int(u32(tokenListOff)))
	}
	return f
}

// TestCompiledHandednessIsOneFloatPerVertex pins the m_hHandednessToken stream
// at +496. It holds handedness — a single ±1 float per vertex — not the
// bitangent vectors we used to write there, which were three floats per vertex
// of unit-vector components (so a 4-vertex mesh wrote 48 bytes of values like
// 0,1,0 where the engine expected 16 bytes of ±1). Verified against the
// game-compiled tangent corpus, where every such stream is exactly ±1.0.
func TestCompiledHandednessIsOneFloatPerVertex(t *testing.T) {
	binData := mustCompile(t, mustParseASCII(t, normalMappedQuad))
	f := readMeshHeaderFields(t, binData)

	const unset = 0xFFFFFFFF
	if f.tangentTok == unset {
		t.Fatal("no tangent stream emitted; NormalAndSpecMapped should generate one")
	}
	if f.handednessTok == unset {
		t.Fatal("handedness token is unset even though tangents were written")
	}
	if f.numVertices == 0 {
		t.Fatal("numVertices is 0")
	}

	// The handedness stream is the last thing written to the volatile block,
	// so its extent pins the per-vertex width: 4 bytes each, not 12.
	gotBytes := int(f.mdxLen) - int(f.handednessTok)
	wantBytes := int(f.numVertices) * 4
	if gotBytes != wantBytes {
		t.Errorf("handedness stream is %d bytes for %d vertices, want %d (4B/vertex); %d would mean Vec3 bitangents",
			gotBytes, f.numVertices, wantBytes, int(f.numVertices)*12)
	}

	// Every value must be exactly ±1.
	mdxBase := 12 + int(binary.LittleEndian.Uint32(binData[4:]))
	for i := 0; i < int(f.numVertices); i++ {
		off := mdxBase + int(f.handednessTok) + i*4
		v := math.Float32frombits(binary.LittleEndian.Uint32(binData[off:]))
		if math.Abs(math.Abs(float64(v))-1.0) > 1e-6 {
			t.Errorf("handedness[%d] = %v, want +1 or -1 (a 0 or fractional value means bitangent components leaked in)", i, v)
		}
	}
}

// TestCompiledVolatileStreamOrder pins the volatile block layout order. Real
// binaries lay it out vertices → UVs → normals → index buffer → tangents →
// handedness (verified on retail tdc01_g02_01: normals 78660, index 78828,
// tangent 78864, handedness 79032). We used to append the index buffer last,
// after the tangent streams.
func TestCompiledVolatileStreamOrder(t *testing.T) {
	binData := mustCompile(t, mustParseASCII(t, normalMappedQuad))
	f := readMeshHeaderFields(t, binData)

	if !f.haveIndex {
		t.Fatal("mesh with faces has no index buffer lists")
	}
	if f.indexCount == 0 {
		t.Fatal("index count is 0")
	}

	steps := []struct {
		name string
		off  uint32
	}{
		{"vertices", f.vertsTok},
		{"UVs", f.uvTok},
		{"normals", f.normalTok},
		{"index buffer", f.indexTok},
		{"tangents", f.tangentTok},
		{"handedness", f.handednessTok},
	}
	for i := 1; i < len(steps); i++ {
		if steps[i].off <= steps[i-1].off {
			t.Errorf("%s (offset %d) must come after %s (offset %d)",
				steps[i].name, steps[i].off, steps[i-1].name, steps[i-1].off)
		}
	}
}

// TestHandednessRoundTrip covers the read path: a mirrored UV chart produces a
// -1 handedness, and that sign must survive compile → decompile. Reading the
// stream as Vec3 (the old behaviour) consumed three times the data present and
// spilled into whatever followed, so the recovered signs were arbitrary.
func TestHandednessRoundTrip(t *testing.T) {
	m := mustDecompile(t, mustCompile(t, mustParseASCII(t, normalMappedQuad)))
	n := m.FindNode("top")
	if n == nil || n.Mesh == nil {
		t.Fatal("mesh node 'top' missing after roundtrip")
	}
	if len(n.Mesh.Tangents) == 0 {
		t.Fatal("tangents were not recovered")
	}
	for i, tan := range n.Mesh.Tangents {
		if math.Abs(math.Abs(float64(tan.W))-1.0) > 1e-6 {
			t.Errorf("tangent[%d].W = %v, want +1 or -1", i, tan.W)
		}
	}
}
