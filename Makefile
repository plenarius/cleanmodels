.PHONY: build build-all test clean lint

MODULE  := github.com/plenarius/cleanmodels
BINARY  := cleanmodels
CMD     := ./cmd/cleanmodels

# Latest tag reachable from the commit being built, e.g. v1.2.3 or
# v1.2.3-4-gfe6322f (4 commits past v1.2.3) or fe6322f (no tags yet).
# CI's release job overrides this with the exact pushed tag instead
# (see .github/workflows/ci.yml).
VERSION := $(shell git describe --tags --always --dirty 2>/dev/null || echo dev)
LDFLAGS := -X main.version=$(VERSION)

build:
	go build -ldflags="$(LDFLAGS)" -o $(BINARY) $(CMD)

build-all:
	GOOS=windows GOARCH=amd64 go build -ldflags="$(LDFLAGS)" -o dist/$(BINARY).exe $(CMD)
	GOOS=darwin  GOARCH=arm64 go build -ldflags="$(LDFLAGS)" -o dist/$(BINARY)-darwin-arm64 $(CMD)
	GOOS=darwin  GOARCH=amd64 go build -ldflags="$(LDFLAGS)" -o dist/$(BINARY)-darwin-amd64 $(CMD)
	GOOS=linux   GOARCH=amd64 go build -ldflags="$(LDFLAGS)" -o dist/$(BINARY)-linux-amd64 $(CMD)

test:
	go test ./...

test-coverage:
	go test -coverprofile=coverage.out ./...
	go tool cover -html=coverage.out

lint:
	go vet ./...

clean:
	rm -f $(BINARY)
	rm -rf dist/
	rm -f coverage.out
