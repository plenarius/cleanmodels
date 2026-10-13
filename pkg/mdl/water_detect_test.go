package mdl

import "testing"

func TestIsWateryBitmap_TableMembership(t *testing.T) {
	if !IsWateryBitmap("tbw01_water01", "") {
		t.Error("expected tbw01_water01 to be watery via shiny-water table")
	}
	if !IsWateryBitmap("TDT01_WATER01", "") {
		t.Error("expected case-insensitive match")
	}
	if IsWateryBitmap("foo_grass", "") {
		t.Error("foo_grass should not be watery without a key")
	}
}

func TestIsWateryBitmap_KeyMatch(t *testing.T) {
	if !IsWateryBitmap("foo_water_3", "water") {
		t.Error("expected key=water to match foo_water_3")
	}
	if !IsWateryBitmap("foo_lava_b", "lava sea") {
		t.Error("expected multi-token key to match")
	}
	if IsWateryBitmap("foo_grass", "water") {
		t.Error("foo_grass shouldn't match key=water")
	}
}

func TestShinyWaterReplacement(t *testing.T) {
	r, ok := ShinyWaterReplacement("tdt01_water01")
	if !ok || r != "tcn01_water01" {
		t.Fatalf("expected tdt01_water01 -> tcn01_water01, got %q (ok=%v)", r, ok)
	}
	if _, ok := ShinyWaterReplacement("nope"); ok {
		t.Error("expected ShinyWaterReplacement to return ok=false for unknown bitmap")
	}
}

// IsWateryBitmap takes a single key but tests pass multi via spaces; this
// helper proves Fields() splitting.
func init() {
	_ = IsWateryBitmap // keep symbol used
}
