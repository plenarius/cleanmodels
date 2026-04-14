//go:build !darwin && !freebsd && !linux && !windows

package main

func isTerminal(fd int) bool {
	return false
}
