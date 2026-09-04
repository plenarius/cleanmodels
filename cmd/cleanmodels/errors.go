package main

import (
	"fmt"
	"os"
	"strings"
)

// cliError wraps an error with an optional suggestion for the user.
type cliError struct {
	err        error
	suggestion string
}

func (e *cliError) Error() string {
	if e.suggestion != "" {
		return fmt.Sprintf("%v\n  hint: %s", e.err, e.suggestion)
	}
	return e.err.Error()
}

func (e *cliError) Unwrap() error { return e.err }

// wrapWithSuggestion inspects common error patterns and adds actionable hints.
func wrapWithSuggestion(err error, path string) error {
	if err == nil {
		return nil
	}
	msg := err.Error()

	if os.IsNotExist(err) {
		return &cliError{err: fmt.Errorf("file not found: %s", path)}
	}

	if os.IsPermission(err) {
		return &cliError{
			err:        fmt.Errorf("cannot access %s", path),
			suggestion: "check file permissions",
		}
	}

	if strings.Contains(msg, "unexpected EOF") || strings.Contains(msg, "truncated") {
		return &cliError{
			err:        err,
			suggestion: "the file may be corrupted or incomplete",
		}
	}

	return err
}

// suggestRecursive returns an error suggesting --recursive for empty directories.
func suggestRecursive(dir string) string {
	return fmt.Sprintf("no .mdl files found in %s -- use --recursive to search subdirectories", dir)
}
