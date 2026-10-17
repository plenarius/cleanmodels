package mdl

import (
	"fmt"
	"strings"
	"sync"
)

// parse2DA reads a text 2DA V2.0 table: the column names, then each row's
// values by lower-case column name, and its row number under rowKey. "****"
// (empty) is kept as is; quoted values may contain spaces.
func parse2DA(data []byte) ([]map[string]string, error) {
	lines := strings.Split(strings.ReplaceAll(string(data), "\r\n", "\n"), "\n")
	if len(lines) == 0 || !strings.HasPrefix(strings.TrimSpace(lines[0]), "2DA") {
		return nil, fmt.Errorf("not a 2DA table")
	}
	var cols []string
	var rows []map[string]string
	for _, line := range lines[1:] {
		f := split2DALine(line)
		if len(f) == 0 || strings.EqualFold(f[0], "DEFAULT:") {
			continue
		}
		if cols == nil {
			cols = make([]string, len(f))
			for i, c := range f {
				cols[i] = strings.ToLower(c)
			}
			continue
		}
		row := map[string]string{rowKey: f[0]}
		for i, c := range cols {
			if i+1 < len(f) {
				row[c] = f[i+1] // f[0] is the row number
			}
		}
		rows = append(rows, row)
	}
	return rows, nil
}

// rowKey holds a parse2DA row's number (no column can be named so: names
// have no spaces).
const rowKey = "# row"

func split2DALine(line string) []string {
	var out []string
	for i := 0; i < len(line); {
		for i < len(line) && (line[i] == ' ' || line[i] == '\t') {
			i++
		}
		if i >= len(line) {
			break
		}
		if line[i] == '"' {
			j := strings.IndexByte(line[i+1:], '"')
			if j < 0 {
				out = append(out, line[i+1:])
				break
			}
			out = append(out, line[i+1:i+1+j])
			i += j + 2
			continue
		}
		j := i
		for j < len(line) && line[j] != ' ' && line[j] != '\t' {
			j++
		}
		out = append(out, line[i:j])
		i = j
	}
	return out
}

// capartCache holds parsed capart.2da tables by resource, read-only once stored.
var capartCache sync.Map

// findCapart reads the first capart.2da in locs (a hak or folder with a custom
// one comes before the game install's stock table) and returns its attach
// nodes by part type (MDLNAME, lower case) and where it was read from. It
// returns nil when no capart.2da is found or it cannot be read.
func findCapart(locs []string) (map[string]string, string) {
	ref := findResource("capart", resType2DA, ".2da", locs)
	if ref == nil {
		return nil, ""
	}
	key := ref.id + "\x00" + ref.stamp
	if v, ok := capartCache.Load(key); ok {
		return v.(map[string]string), ref.id
	}
	data, err := ref.read()
	if err != nil {
		return nil, ""
	}
	rows, err := parse2DA(data)
	if err != nil {
		return nil, ""
	}
	nodes := map[string]string{}
	for _, r := range rows {
		kind, node := strings.ToLower(r["mdlname"]), r["nodename"]
		if kind != "" && kind != "****" && node != "" && node != "****" {
			nodes[kind] = node
		}
	}
	capartCache.Store(key, nodes)
	return nodes, ref.id
}
