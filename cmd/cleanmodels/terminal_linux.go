//go:build linux

package main

import (
	"syscall"
	"unsafe"
)

func isTerminal(fd int) bool {
	var termios syscall.Termios
	_, _, err := syscall.Syscall6(syscall.SYS_IOCTL, uintptr(fd), 0x5401, uintptr(unsafe.Pointer(&termios)), 0, 0, 0)
	return err == 0
}

// TIOCGWINSZ on linux is 0x5413.
const tiocgwinsz = 0x5413

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
