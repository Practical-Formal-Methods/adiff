
VERSION= $(shell stack query "locals" | grep version | cut -d" " -f 4 | tr -d "'")
SDIST= $(shell stack path --dist-dir)
BINARY= vdiff/$(shell stack path --dist-dir)/build/vdiff/vdiff
BINARY-VIEWER= vdiff/$(shell stack path --dist-dir)/build/vdiff-viewer/vdiff-viewer
INTEGRATION= vdiff/$(shell stack path --dist-dir)/build/integration/integration

default: compile docker-images


compile:
	stack build

vdiff-docker: Dockerfile
	docker build -t vdiff:latest  .

clean:
	stack clean

# regression tests
test:
	stack test vdiff:regression


# integration
test-integration: vdiff-docker
	stack build vdiff:integration --no-run-tests
	docker run vdiff:latest /bin/bash -lc "integration --color=always"


# docker images
docker-images:
	docker build -t vdiff/all-verifiers docker/vdiff/all-verifiers
	docker build -t vdiff/build docker/vdiff/build
	docker build -t vdiff/vdiff -f docker/vdiff/vdiff/Dockerfile .


.PHONY: test test-integration docker-images
