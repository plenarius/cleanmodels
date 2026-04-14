//go:build windows

package main

func isTerminal(fd int) bool {
	return false
}
