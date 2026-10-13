package mdl

import (
	"errors"
	"strings"
	"testing"
)

// A file with no sign of being an MDL used to parse into an empty model, so
// check passed it and compile wrote a bogus binary from it.
func TestParseRejectsInputThatIsNotAModel(t *testing.T) {
	cases := map[string]string{
		"empty":        "",
		"blank lines":  "\n\n   \n",
		"comments":     "# just a comment\n# another\n",
		"plain text":   "this is not a model\n",
		"html":         "<html><body>404 Not Found</body></html>\n",
		"binary noise": "\x01\x02\x03\xff\xfe garbage\n",
	}
	for name, src := range cases {
		pr, err := Parse(strings.NewReader(src))
		if !errors.Is(err, ErrNotModel) {
			t.Errorf("%s: err = %v, want ErrNotModel", name, err)
		}
		if pr == nil || pr.Model == nil {
			t.Errorf("%s: result must still carry a model so callers do not nil-dereference", name)
		}
	}
}

func TestParseEmptyAndNonModelMessagesDiffer(t *testing.T) {
	_, e1 := Parse(strings.NewReader(""))
	_, e2 := Parse(strings.NewReader("hello\n"))
	if !strings.Contains(e1.Error(), "empty") {
		t.Errorf("empty file message: %v", e1)
	}
	if !strings.Contains(e2.Error(), "newmodel") {
		t.Errorf("non-model message should say what an MDL starts with: %v", e2)
	}
}

// Structure without a newmodel line is a damaged model, not a non-model: it is
// kept, with a warning.
func TestParseKeepsAModelWithoutNewmodelAndWarns(t *testing.T) {
	src := "beginmodelgeom x\n  node dummy x\n    parent NULL\n  endnode\nendmodelgeom\n"
	pr, err := Parse(strings.NewReader(src))
	if err != nil {
		t.Fatalf("err = %v", err)
	}
	if !hasError(pr.Errors, `no "newmodel" line`) {
		t.Errorf("missing warning: %+v", pr.Errors)
	}
	if len(pr.Model.Nodes) != 1 {
		t.Errorf("node dropped: %+v", pr.Model.Nodes)
	}
}

func TestParseOfARealModelIsUnchanged(t *testing.T) {
	src := "newmodel m\nsetsupermodel m NULL\nclassification TILE\nsetanimationscale 1.0\nbeginmodelgeom m\n  node dummy m\n    parent NULL\n  endnode\nendmodelgeom\ndonemodel m\n"
	pr, err := Parse(strings.NewReader(src))
	if err != nil || len(pr.Errors) != 0 {
		t.Errorf("err=%v warnings=%+v", err, pr.Errors)
	}
}
