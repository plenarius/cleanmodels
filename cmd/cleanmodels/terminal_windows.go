//go:build windows

package main

func isTerminal(fd int) bool {
	return false
}

// terminalWidth on windows is not detected without additional API calls;
// callers fall back to the historical default. Future work: use
// GetConsoleScreenBufferInfo when fd is a console handle.
func terminalWidth(fd int) int {
	return 0
}
