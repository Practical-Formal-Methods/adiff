
default: img-run

compile: img-build
	stack build

vdiff-docker: Dockerfile
	docker build -t vdiff:latest  .

clean:
	stack clean

install: img-run
	stack build --copy-bins --no-docker

# regression tests
test: img-run
	stack test

# docker images

img-all-verifiers:
	docker build -t vdiff/all-verifiers docker/vdiff/all-verifiers

img-build: img-all-verifiers
	docker build -t vdiff/build docker/vdiff/build

img-run: compile
	stack install
	docker build -t vdiff/vdiff -f docker/vdiff/vdiff/Dockerfile .


.PHONY: test test-integration img-all-verifiers img-build img-run compile install
