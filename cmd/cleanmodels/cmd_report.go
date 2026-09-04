package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"mime/multipart"
	"net/http"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"time"
)

const (
	reportEndpoint  = "https://cleanmodels.layonara.com/report"
	maxFileSize     = 10 * 1024 * 1024 // 10 MB per file
	maxTotalSize    = 25 * 1024 * 1024 // 25 MB total
)

// Embedded at build time via ldflags alongside version.
var clientKey = ""

type reportDiagnostics struct {
	Version     string `json:"version"`
	OS          string `json:"os"`
	Arch        string `json:"arch"`
	Command     string `json:"command"`
	ErrorOutput string `json:"error_output"`
	Notes       string `json:"notes,omitempty"`
}

func cmdReport(args []string) int {
	fs := flag.NewFlagSet("cleanmodels report", flag.ContinueOnError)
	fs.SetOutput(os.Stderr)

	command := fs.String("command", "", "the cleanmodels command that was run")
	errorOutput := fs.String("error", "", "error output or description of the problem (optional)")
	notes := fs.String("notes", "", "additional notes or context")
	endpoint := fs.String("endpoint", reportEndpoint, "report relay endpoint URL")

	fs.Usage = func() {
		fmt.Fprintf(os.Stderr, `Usage: cleanmodels report [flags] <file.mdl> [file2.mdl ...]

Submit a bug report with model files to the cleanmodels issue tracker.
The report is sent to a relay service that creates a GitHub issue on your behalf.
No GitHub account is required.

Use --error to describe the problem. If omitted, the report is submitted
as a general "incorrect output" issue — useful when a model compiles or
decompiles without errors but looks wrong in-game.

Flags:
`)
		fs.PrintDefaults()
		fmt.Fprintf(os.Stderr, `
Examples:
  cleanmodels report model.mdl
  cleanmodels report --error "decompile crash" model.mdl
  cleanmodels report --command "repair --fix-pivots" --error "missing faces after repair" tile.mdl
`)
	}

	if err := parseArgs(fs, args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return exitOK
		}
		return exitUsage
	}

	files := fs.Args()
	if len(files) == 0 {
		fmt.Fprintf(os.Stderr, "cleanmodels report: at least one model file is required\n")
		fs.Usage()
		return exitUsage
	}

	if *errorOutput == "" {
		*errorOutput = "Incorrect output (no error reported — model may compile/decompile but produce wrong results)"
	}

	var totalSize int64
	for _, f := range files {
		info, err := os.Stat(f)
		if err != nil {
			fmt.Fprintf(os.Stderr, "cleanmodels report: %s: %v\n", f, err)
			return exitErrors
		}
		if !info.Mode().IsRegular() {
			fmt.Fprintf(os.Stderr, "cleanmodels report: %s is not a regular file\n", f)
			return exitErrors
		}
		if info.Size() > maxFileSize {
			fmt.Fprintf(os.Stderr, "cleanmodels report: %s exceeds 10MB per-file limit\n", f)
			return exitErrors
		}
		totalSize += info.Size()
	}
	if totalSize > maxTotalSize {
		fmt.Fprintf(os.Stderr, "cleanmodels report: total file size (%d MB) exceeds 25MB limit\n", totalSize/(1024*1024))
		return exitErrors
	}

	diag := reportDiagnostics{
		Version:     version,
		OS:          runtime.GOOS,
		Arch:        runtime.GOARCH,
		Command:     *command,
		ErrorOutput: *errorOutput,
		Notes:       *notes,
	}

	fmt.Fprintf(os.Stderr, "Preparing report with %d file(s)...\n", len(files))

	body, contentType, err := buildMultipartBody(diag, files)
	if err != nil {
		fmt.Fprintf(os.Stderr, "cleanmodels report: failed to build request: %v\n", err)
		return exitErrors
	}

	fmt.Fprintf(os.Stderr, "Submitting to %s...\n", *endpoint)

	issueURL, err := submitReport(*endpoint, body, contentType)
	if err != nil {
		fmt.Fprintf(os.Stderr, "cleanmodels report: submission failed: %v\n", err)
		return exitErrors
	}

	fmt.Fprintf(os.Stderr, "Report submitted successfully!\n")
	fmt.Println(issueURL)
	return exitOK
}

func buildMultipartBody(diag reportDiagnostics, files []string) (*bytes.Buffer, string, error) {
	var buf bytes.Buffer
	w := multipart.NewWriter(&buf)

	diagJSON, err := json.Marshal(diag)
	if err != nil {
		return nil, "", fmt.Errorf("marshal diagnostics: %w", err)
	}

	diagField, err := w.CreateFormField("diagnostics")
	if err != nil {
		return nil, "", err
	}
	if _, err := diagField.Write(diagJSON); err != nil {
		return nil, "", err
	}

	for _, path := range files {
		f, err := os.Open(path)
		if err != nil {
			return nil, "", fmt.Errorf("open %s: %w", path, err)
		}

		part, err := w.CreateFormFile("files", filepath.Base(path))
		if err != nil {
			f.Close()
			return nil, "", err
		}
		if _, err := io.Copy(part, f); err != nil {
			f.Close()
			return nil, "", fmt.Errorf("read %s: %w", path, err)
		}
		f.Close()
	}

	if err := w.Close(); err != nil {
		return nil, "", err
	}

	return &buf, w.FormDataContentType(), nil
}

func submitReport(endpoint string, body *bytes.Buffer, contentType string) (string, error) {
	req, err := http.NewRequest("POST", endpoint, body)
	if err != nil {
		return "", err
	}
	req.Header.Set("Content-Type", contentType)
	req.Header.Set("User-Agent", "cleanmodels/"+version)
	if clientKey != "" {
		req.Header.Set("X-Client-Key", clientKey)
	}

	client := &http.Client{Timeout: 30 * time.Second}
	resp, err := client.Do(req)
	if err != nil {
		return "", fmt.Errorf("network error: %w", err)
	}
	defer resp.Body.Close()

	respBody, err := io.ReadAll(io.LimitReader(resp.Body, 1<<20))
	if err != nil {
		return "", fmt.Errorf("read response: %w", err)
	}

	if resp.StatusCode == 429 {
		return "", fmt.Errorf("rate limited — please try again later")
	}

	if resp.StatusCode != 200 {
		var errResp struct {
			Error string `json:"error"`
		}
		if json.Unmarshal(respBody, &errResp) == nil && errResp.Error != "" {
			return "", fmt.Errorf("server error (%d): %s", resp.StatusCode, errResp.Error)
		}
		errText := strings.TrimSpace(string(respBody))
		if len(errText) > 512 {
			errText = errText[:512] + "..."
		}
		return "", fmt.Errorf("server error (%d): %s", resp.StatusCode, errText)
	}

	var result struct {
		IssueURL     string `json:"issue_url"`
		Deduplicated bool   `json:"deduplicated"`
	}
	if err := json.Unmarshal(respBody, &result); err != nil {
		return "", fmt.Errorf("invalid response: %w", err)
	}

	if result.IssueURL == "" {
		return "", fmt.Errorf("server returned success but no issue URL")
	}

	if result.Deduplicated {
		fmt.Fprintf(os.Stderr, "Note: a matching report already exists.\n")
	}

	return result.IssueURL, nil
}
