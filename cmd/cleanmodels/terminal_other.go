//go:build !darwin && !freebsd && !linux && !windows

package main

func isTerminal(fd int) bool {
	return false
}

// terminalWidth has no portable fallback on unknown platforms.
func terminalWidth(fd int) int {
	return 0
}
