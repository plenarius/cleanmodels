//go:build darwin || freebsd

package main

import (
	"syscall"
	"unsafe"
)

func isTerminal(fd int) bool {
	var termios syscall.Termios
	_, _, err := syscall.Syscall6(syscall.SYS_IOCTL, uintptr(fd), uintptr(syscall.TIOCGETA), uintptr(unsafe.Pointer(&termios)), 0, 0, 0)
	return err == 0
}

// TIOCGWINSZ on darwin/freebsd is 0x40087468.
const tiocgwinsz = 0x40087468

type winsize struct {
	rows, cols, xpixel, ypixel uint16
}

// terminalWidth returns the column count for fd, or 0 if unavailable.
func terminalWidth(fd int) int {
	var ws winsize
	_, _, err := syscall.Syscall6(syscall.SYS_IOCTL, uintptr(fd), tiocgwinsz, uintptr(unsafe.Pointer(&ws)), 0, 0, 0)
	if err != 0 {
		return 0
	}
	return int(ws.cols)
}
