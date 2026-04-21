package mdl

import (
	"strings"
)

// PlaceableTransparency reparents transparent meshes (those whose bitmap
// contains key) to a "Modela" node for CHARACTER classification models.
// This enables the engine's transparency sorting for placeables.
func PlaceableTransparency(model *Model, key string) []string {
	var out []string
	if model == nil || key == "" {
		return out
	}
	if !strings.EqualFold(model.Classification, "CHARACTER") {
		return out
	}
	return ReparentToModela(model, key, "placeable-transparency")
}
