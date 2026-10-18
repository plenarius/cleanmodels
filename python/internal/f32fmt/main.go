// Command f32fmt prints, for each float32 bit pattern read from stdin (one
// decimal uint32 per line), the text the ASCII writer uses for that value
// (fmtFloat: strconv.FormatFloat(v, 'g', -1, 32)). python/parity_test.py
// compares the Python port's formatting with it.
package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
)

func main() {
	in := bufio.NewScanner(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	for in.Scan() {
		b, err := strconv.ParseUint(in.Text(), 10, 32)
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		v := math.Float32frombits(uint32(b))
		fmt.Fprintln(out, strconv.FormatFloat(float64(v), 'g', -1, 32))
	}
}
