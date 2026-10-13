package checks

import (
	"fmt"
	"math"

	"github.com/plenarius/cleanmodels/pkg/mdl"
)

const (
	eeTriangleFaceLimit = 21845
	eeSkinBoneLimit     = 64
	weightSumTolerance  = 0.01
)

func init() {
	Register("vertex_face_consistency", "geometry", true, "Clamp out-of-range vertex indices in faces", checkVertexFaceConsistency)
	Register("tvert_consistency", "geometry", true, "Clamp out-of-range UV indices in faces", checkTvertConsistency)
	Register("tvert_range", "geometry", true, "Replace texture coordinates too large to mean anything (>= 2^24, or non-finite) with 0", checkTvertRange)
	Register("degenerate_faces", "geometry", false, "Detect zero-area faces with repeated vertex indices", checkDegenerateFaces)
	Register("triangle_limit", "geometry", false, "Detect meshes exceeding the EE triangle limit", checkTriangleLimit)
	Register("bone_limit", "geometry", false, "Detect skin meshes exceeding the EE bone limit", checkBoneLimit)
	Register("unnormalized_weights", "geometry", true, "Renormalize skin bone weights to sum to 1.0", checkUnnormalizedWeights)
	Register("constraint_count", "geometry", true, "Fix danglymesh constraint count to match vertices", checkConstraintCount)
	Register("faceless_trimesh", "geometry", true, "Set render=0 on mesh nodes with no faces", checkFacelessTrimesh)
	Register("multiple_edges", "geometry", false, "Detect non-manifold edges that cause shadow tearing", checkMultipleEdges)
}

func meshGeometryNodeType(t string) bool {
	switch t {
	case "trimesh", "skin", "aabb", "danglymesh", "animmesh":
		return true
	default:
		return false
	}
}

func facelessNodeType(t string) bool {
	switch t {
	case "trimesh", "skin", "aabb":
		return true
	default:
		return false
	}
}

func checkVertexFaceConsistency(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil || !meshGeometryNodeType(n.NodeType()) {
			continue
		}
		nv := int32(len(n.Mesh.Verts))
		if nv == 0 {
			continue
		}
		for fi := range n.Mesh.Faces {
			f := &n.Mesh.Faces[fi]
			for vi := range f.Verts {
				if f.Verts[vi] < 0 || f.Verts[vi] >= nv {
					old := f.Verts[vi]
					fixed := false
					if fix {
						f.Verts[vi] = nv - 1
						fixed = true
					}
					out = append(out, mdl.CheckResult{
						Check:    "vertex_face_consistency",
						Node:     n.Name,
						Severity: mdl.SevError,
						Fixed:    fixed,
						Message: fmt.Sprintf(
							"%s: face %d vertex index %d out of range [0, %d), clamped to %d",
							file, fi, old, nv, nv-1,
						),
					})
				}
			}
		}
	}
	return out
}

func checkTvertConsistency(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil || len(n.Mesh.TVerts) == 0 || !meshGeometryNodeType(n.NodeType()) {
			continue
		}
		nt := int32(len(n.Mesh.TVerts))
		for fi := range n.Mesh.Faces {
			f := &n.Mesh.Faces[fi]
			for ui := range f.UVs {
				if f.UVs[ui] < 0 || f.UVs[ui] >= nt {
					old := f.UVs[ui]
					fixed := false
					if fix {
						f.UVs[ui] = nt - 1
						fixed = true
					}
					out = append(out, mdl.CheckResult{
						Check:    "tvert_consistency",
						Node:     n.Name,
						Severity: mdl.SevError,
						Fixed:    fixed,
						Message: fmt.Sprintf(
							"%s: face %d UV index %d out of range [0, %d), clamped to %d",
							file, fi, old, nt, nt-1,
						),
					})
				}
			}
		}
	}
	return out
}

func checkDegenerateFaces(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil || !meshGeometryNodeType(n.NodeType()) {
			continue
		}
		for fi, f := range n.Mesh.Faces {
			v0, v1, v2 := f.Verts[0], f.Verts[1], f.Verts[2]
			if v0 == v1 || v1 == v2 || v0 == v2 {
				out = append(out, mdl.CheckResult{
					Check:    "degenerate_faces",
					Node:     n.Name,
					Severity: mdl.SevWarning,
					Message: fmt.Sprintf(
						"%s: face %d has repeated vertex indices [%d %d %d] (use --strip-degenerate to remove)",
						file, fi, v0, v1, v2,
					),
					Fixed: false,
				})
			}
		}
	}
	return out
}

func checkTriangleLimit(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil {
			continue
		}
		nf := len(n.Mesh.Faces)
		if nf > eeTriangleFaceLimit {
			out = append(out, mdl.CheckResult{
				Check:    "triangle_limit",
				Node:     n.Name,
				Severity: mdl.SevWarning,
				Message: fmt.Sprintf(
					"%s: node has %d faces, exceeding EE limit of %d",
					file, nf, eeTriangleFaceLimit,
				),
				Fixed: false,
			})
		}
	}
	return out
}

func checkBoneLimit(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Skin == nil || n.Mesh == nil {
			continue
		}
		unique := make(map[string]struct{})
		for _, vw := range n.Skin.Weights {
			for _, b := range vw.Bones {
				if b != "" {
					unique[b] = struct{}{}
				}
			}
		}
		if len(unique) > eeSkinBoneLimit {
			out = append(out, mdl.CheckResult{
				Check:    "bone_limit",
				Node:     n.Name,
				Severity: mdl.SevWarning,
				Message: fmt.Sprintf(
					"%s: skin references %d unique bones, maximum is %d",
					file, len(unique), eeSkinBoneLimit,
				),
				Fixed: false,
			})
		}
	}
	return out
}

func checkUnnormalizedWeights(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Skin == nil {
			continue
		}
		for vi := range n.Skin.Weights {
			vw := &n.Skin.Weights[vi]
			var sum float64
			for _, w := range vw.Weights {
				sum += float64(w)
			}
			if math.Abs(sum-1.0) > weightSumTolerance {
				oldSum := sum
				fixed := false
				if fix {
					if sum > 0 {
						scale := float32(1.0 / sum)
						for wi := range vw.Weights {
							vw.Weights[wi] *= scale
						}
						fixed = true
					} else if len(vw.Weights) > 0 {
						vw.Weights[0] = 1.0
						for wi := 1; wi < len(vw.Weights); wi++ {
							vw.Weights[wi] = 0
						}
						fixed = true
					}
				}
				out = append(out, mdl.CheckResult{
					Check:    "unnormalized_weights",
					Node:     n.Name,
					Severity: mdl.SevWarning,
					Fixed:    fixed,
					Message: fmt.Sprintf(
						"%s: vertex weight %d summed to %g, renormalized to 1.0",
						file, vi, oldSum,
					),
				})
			}
		}
	}
	return out
}

func checkConstraintCount(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Dangly == nil || n.Mesh == nil {
			continue
		}
		vc := len(n.Mesh.Verts)
		cc := len(n.Dangly.Constraints)
		if cc == vc {
			continue
		}
		fixed := false
		action := "padded with zeros"
		if cc > vc {
			action = "truncated"
		}
		if fix {
			if cc < vc {
				for len(n.Dangly.Constraints) < vc {
					n.Dangly.Constraints = append(n.Dangly.Constraints, 0)
				}
			} else {
				n.Dangly.Constraints = n.Dangly.Constraints[:vc]
			}
			fixed = true
		}
		out = append(out, mdl.CheckResult{
			Check:    "constraint_count",
			Node:     n.Name,
			Severity: mdl.SevError,
			Fixed:    fixed,
			Message: fmt.Sprintf(
				"%s: danglymesh had %d constraints but %d vertices, %s",
				file, cc, vc, action,
			),
		})
	}
	return out
}

func checkFacelessTrimesh(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil || !facelessNodeType(n.NodeType()) {
			continue
		}
		if len(n.Mesh.Faces) != 0 {
			continue
		}
		if n.Mesh.Render != 1 {
			continue
		}
		fixed := false
		if fix {
			n.Mesh.Render = 0
			fixed = true
		}
		out = append(out, mdl.CheckResult{
			Check:    "faceless_trimesh",
			Node:     n.Name,
			Severity: mdl.SevWarning,
			Fixed:    fixed,
			Message: fmt.Sprintf(
				"%s: %s node %q had no faces but render=1, set render=0",
				file, n.NodeType(), n.Name,
			),
		})
	}
	return out
}

// checkMultipleEdges detects edges shared by 3+ faces on shadow-casting meshes.
// "Multiple edges" cause shadow tearing/stretching in NWN's stencil shadow
// renderer. See: nwn.wiki Model Shadows page.
// The fix (--split-multiedge) duplicates vertices at affected edges so each
// edge is shared by at most 2 faces, eliminating shadow artifacts.
func checkMultipleEdges(model *mdl.Model, file string, _ bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || !n.IsShadowCaster() {
			continue
		}

		edgeFaces := mdl.BuildEdgeFaceMap(n.Mesh.Faces)

		multiCount := 0
		for _, faces := range edgeFaces {
			if len(faces) > 2 {
				multiCount++
			}
		}
		if multiCount > 0 {
			out = append(out, mdl.CheckResult{
				Check:    "multiple_edges",
				Node:     n.Name,
				Severity: mdl.SevWarning,
				Message: fmt.Sprintf(
					"%s: shadow-casting node %q has %d multiple edge(s) (%d+ faces sharing an edge), causes shadow tearing (use --split-multiedge to fix)",
					file, n.Name, multiCount, 3,
				),
				Fixed: false,
			})
		}
	}
	return out
}

// maxUsableTexCoord is where a float32 texture coordinate stops carrying a
// position. At 2^24 the spacing between representable floats is 2, so the
// fractional part, which is all a repeating texture looks at, is always 0: every
// value from there up samples the same texel. Real models use values up to a few
// hundred (large tiling reaches the tens of thousands and still resolves), so
// nothing legitimate comes near this.
const maxUsableTexCoord = 1 << 24

// unusableTexCoord reports whether f cannot be a texture coordinate.
func unusableTexCoord(f float32) bool {
	v := float64(f)
	return math.IsNaN(v) || math.IsInf(v, 0) || math.Abs(v) >= maxUsableTexCoord
}

// checkTvertRange finds texture coordinates that are non-finite or at or above
// 2^24 in magnitude. Exporters occasionally write uninitialised floats there
// (7.34e+23 and 1e+30 turn up in real tilesets and placeables). The value is
// unrecoverable, so with fix set only the offending component is replaced with
// 0, which samples the same texel as the original under a repeating texture and
// keeps the good component. Everything below the limit is left alone, including
// large but meaningful tiling. One result is reported per node and UV set.
func checkTvertRange(model *mdl.Model, file string, fix bool) []mdl.CheckResult {
	if model == nil {
		return nil
	}
	var out []mdl.CheckResult
	for _, n := range model.Nodes {
		if n == nil || n.Mesh == nil || !meshGeometryNodeType(n.NodeType()) {
			continue
		}
		sets := [][]mdl.Vec3{n.Mesh.TVerts, n.Mesh.TVerts1, n.Mesh.TVerts2, n.Mesh.TVerts3}
		for set, tv := range sets {
			var bad []int
			for i := range tv {
				if unusableTexCoord(tv[i].X) || unusableTexCoord(tv[i].Y) {
					bad = append(bad, i)
				}
			}
			if len(bad) == 0 {
				continue
			}
			first := tv[bad[0]]
			example := fmt.Sprintf("tvert %d = (%g, %g)", bad[0], first.X, first.Y)
			if fix {
				for _, i := range bad {
					if unusableTexCoord(tv[i].X) {
						tv[i].X = 0
					}
					if unusableTexCoord(tv[i].Y) {
						tv[i].Y = 0
					}
				}
			}
			setName := "UV set 0"
			if set > 0 {
				setName = fmt.Sprintf("UV set %d", set)
			}
			action := "would be set to 0"
			if fix {
				action = "set to 0"
			}
			out = append(out, mdl.CheckResult{
				Check:    "tvert_range",
				Node:     n.Name,
				Severity: mdl.SevWarning,
				Fixed:    fix,
				Message: fmt.Sprintf(
					"%s: %d texture coordinate(s) in %s are not usable values (%s); %s",
					file, len(bad), setName, example, action,
				),
			})
		}
	}
	return out
}
