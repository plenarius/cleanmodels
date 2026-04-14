.PHONY: build build-all test clean lint

MODULE := github.com/plenarius/cleanmodels
BINARY := cleanmodels
CMD    := ./cmd/cleanmodels

build:
	go build -o $(BINARY) $(CMD)

build-all:
	GOOS=windows GOARCH=amd64 go build -o dist/$(BINARY).exe $(CMD)
	GOOS=darwin  GOARCH=arm64 go build -o dist/$(BINARY)-darwin-arm64 $(CMD)
	GOOS=darwin  GOARCH=amd64 go build -o dist/$(BINARY)-darwin-amd64 $(CMD)
	GOOS=linux   GOARCH=amd64 go build -o dist/$(BINARY)-linux-amd64 $(CMD)

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
