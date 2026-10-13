package mdl

import (
	"strings"
	"testing"
)

func TestParseMinimalModel(t *testing.T) {
	input := `
#MAXMODEL ASCII
newmodel testmodel
  classification CHARACTER
  setsupermodel testmodel NULL
  setanimationscale 1
#MAXGEOM ASCII
beginmodelgeom testmodel
  node dummy testmodel
    parent NULL
  endnode
endmodelgeom testmodel
donemodel testmodel
`
	result, err := Parse(strings.NewReader(input))
	if err != nil {
		t.Fatal(err)
	}

	m := result.Model
	if m.Name != "testmodel" {
		t.Errorf("name = %q, want %q", m.Name, "testmodel")
	}
	if m.Classification != "CHARACTER" {
		t.Errorf("classification = %q, want %q", m.Classification, "CHARACTER")
	}
	if m.SuperModel != "NULL" {
		t.Errorf("supermodel = %q, want %q", m.SuperModel, "NULL")
	}
	if len(m.Nodes) != 1 {
		t.Fatalf("nodes = %d, want 1", len(m.Nodes))
	}
	if m.Nodes[0].NodeType() != "dummy" {
		t.Errorf("node type = %q, want dummy", m.Nodes[0].NodeType())
	}
}

func TestParseTrimeshNode(t *testing.T) {
	input := `
newmodel test
  classification CHARACTER
  setsupermodel test NULL
beginmodelgeom test
  node trimesh mesh01
    parent test
    ambient 0.2 0.2 0.2
    diffuse 0.8 0.8 0.8
    specular 0 0 0
    shininess 10
    bitmap texture01
    render 1
    shadow 0
    beaming 0
    transparencyhint 0
    verts 3
      0 0 0
      1 0 0
      0 1 0
    faces 1
      0 1 2 1 0 1 2 0
  endnode
endmodelgeom test
donemodel test
`
	result, err := Parse(strings.NewReader(input))
	if err != nil {
		t.Fatal(err)
	}

	m := result.Model
	if len(m.Nodes) != 1 {
		t.Fatalf("nodes = %d, want 1", len(m.Nodes))
	}

	n := m.Nodes[0]
	if n.NodeType() != "trimesh" {
		t.Errorf("node type = %q, want trimesh", n.NodeType())
	}
	if n.Mesh == nil {
		t.Fatal("mesh data is nil")
	}
	if len(n.Mesh.Verts) != 3 {
		t.Errorf("verts = %d, want 3", len(n.Mesh.Verts))
	}
	if len(n.Mesh.Faces) != 1 {
		t.Errorf("faces = %d, want 1", len(n.Mesh.Faces))
	}
	if n.Mesh.Bitmap != "texture01" {
		t.Errorf("bitmap = %q, want texture01", n.Mesh.Bitmap)
	}
}

func TestParseAnimation(t *testing.T) {
	input := `
newmodel test
  classification CHARACTER
  setsupermodel test NULL
beginmodelgeom test
  node dummy test
    parent NULL
  endnode
endmodelgeom test
newanim walk test
  length 1.0
  transtime 0.25
  animroot test
  event 0.5 footstep
  node dummy test
    parent NULL
    positionkey 2
      0 0 0 0
      1 1 0 0
  endnode
doneanim walk test
donemodel test
`
	result, err := Parse(strings.NewReader(input))
	if err != nil {
		t.Fatal(err)
	}

	m := result.Model
	if len(m.Animations) != 1 {
		t.Fatalf("animations = %d, want 1", len(m.Animations))
	}

	anim := m.Animations[0]
	if anim.Name != "walk" {
		t.Errorf("anim name = %q, want walk", anim.Name)
	}
	if anim.Length != 1.0 {
		t.Errorf("anim length = %v, want 1.0", anim.Length)
	}
	if len(anim.Events) != 1 {
		t.Errorf("events = %d, want 1", len(anim.Events))
	}
	if len(anim.Nodes) != 1 {
		t.Fatalf("anim nodes = %d, want 1", len(anim.Nodes))
	}
	if len(anim.Nodes[0].PositionKeys) != 2 {
		t.Errorf("position keys = %d, want 2", len(anim.Nodes[0].PositionKeys))
	}
}

func TestParseEmitterNode(t *testing.T) {
	input := `
newmodel test
  classification EFFECT
  setsupermodel test NULL
beginmodelgeom test
  node emitter fx_emit
    parent test
    deadspace 0.5
    blastradius 0
    blastlength 0
    xgrid 5
    ygrid 5
    spawntype 0
    update fountain
    render normal
    blend normal
    texture fxpa_smoke
    birthrate 10
    lifeexp 2
    mass 0.5
    velocity 5
    alphastart 1
    alphamid 0.5
    alphaend 0
    colorstart 1 1 1
    colorend 0.5 0.5 0.5
    sizestart 0.1
    sizeend 0.5
    p2p 0
    p2p_sel 0
    affectedbywind 1
    m_istinted 0
    bounce 0
    random 1
    inherit 0
    inheritvel 0
    inherit_local 0
    splat 0
    inherit_part 0
  endnode
endmodelgeom test
donemodel test
`
	result, err := Parse(strings.NewReader(input))
	if err != nil {
		t.Fatal(err)
	}

	m := result.Model
	if len(m.Nodes) != 1 {
		t.Fatalf("nodes = %d, want 1", len(m.Nodes))
	}

	n := m.Nodes[0]
	if n.NodeType() != "emitter" {
		t.Errorf("type = %q, want emitter", n.NodeType())
	}
	if n.Emitter.BirthRate != 10 {
		t.Errorf("birthrate = %v, want 10", n.Emitter.BirthRate)
	}
	if n.Emitter.AffectedByWind != 1 {
		t.Errorf("affectedbywind = %d, want 1", n.Emitter.AffectedByWind)
	}
}

func TestNearMatch(t *testing.T) {
	tests := []struct {
		input string
		valid []string
		want  string
		ok    bool
	}{
		{"bitmap", []string{"bitmap", "render"}, "bitmap", true},
		{"setfillumcolor", []string{"selfillumcolor"}, "selfillumcolor", true},
		{"n_dynamic_type", []string{"ndynamictype"}, "ndynamictype", true},
		{"bitmpa", []string{"bitmap", "render"}, "bitmap", true},       // Levenshtein 2
		{"zzzzz", []string{"bitmap", "render"}, "", false},             // too far
	}

	for _, tt := range tests {
		got, ok := NearMatchParam(tt.input, tt.valid)
		if ok != tt.ok || got != tt.want {
			t.Errorf("NearMatchParam(%q) = (%q, %v), want (%q, %v)", tt.input, got, ok, tt.want, tt.ok)
		}
	}
}
