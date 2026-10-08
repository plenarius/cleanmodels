package mdl

import (
	"bufio"
	"io"
	"os"
)

// writeBufferSize controls the bufio.Writer wrapped around the temp file
// in atomicWriteFile. ASCII MDLs stream out via many small fmt.Fprintf
// calls; without buffering each one becomes a write syscall.
const writeBufferSize = 64 * 1024

func atomicWriteFile(path string, writeFn func(io.Writer) error) error {
	var origMode os.FileMode
	if info, err := os.Stat(path); err == nil {
		origMode = info.Mode()
	}

	tmp := path + ".tmp"
	f, err := os.Create(tmp)
	if err != nil {
		return err
	}
	bw := bufio.NewWriterSize(f, writeBufferSize)
	writeErr := writeFn(bw)
	if writeErr == nil {
		writeErr = bw.Flush()
	}
	closeErr := f.Close()
	if writeErr != nil {
		os.Remove(tmp)
		return writeErr
	}
	if closeErr != nil {
		os.Remove(tmp)
		return closeErr
	}
	if origMode != 0 {
		os.Chmod(tmp, origMode)
	}
	return os.Rename(tmp, path)
}
