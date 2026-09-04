package mdl

import "testing"

// TestClassificationToCodeAcceptsPluralForms pins the fix for issue #12's
// real root cause: vdr_magearmor.mdl.txt declares "classification Effects"
// (plural), which the parser uppercases to "EFFECTS" — one letter away from
// the "EFFECT" ClassificationToCode used to require. That silently fell
// through to the zero-value "OTHER" (m_nTypeMask=0) with no warning, instead
// of the "EFFECT" (m_nTypeMask=1) every retail VFX model actually carries
// (verified against vdr_magearmor2.mdl). The engine's VFX animation
// playback (impact/duration/cessation) is presumably gated on the model's
// classification, so a model silently misclassified as OTHER would never
// drive its own alpha-fade animations — while particle emitters, whose
// basic behavior doesn't depend on that playback, kept working regardless.
// That fits every symptom: geometry/silhouette fine, alpha-animated meshes
// frozen invisible, emitters unaffected.
func TestClassificationToCodeAcceptsPluralForms(t *testing.T) {
	cases := []struct {
		in   string
		want int
	}{
		{"EFFECT", 1},
		{"EFFECTS", 1},
		{"TILE", 2},
		{"TILES", 2},
		{"CHARACTER", 4},
		{"CHARACTERS", 4},
		{"DOOR", 8},
		{"DOORS", 8},
		{"OTHER", 0},
		{"NOT_A_REAL_CLASS", 0},
	}
	for _, tc := range cases {
		if got := ClassificationToCode(tc.in); got != tc.want {
			t.Errorf("ClassificationToCode(%q) = %d, want %d", tc.in, got, tc.want)
		}
	}
}
