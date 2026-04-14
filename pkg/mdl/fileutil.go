package mdl

import (
	"io"
	"os"
)

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
	writeErr := writeFn(f)
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
