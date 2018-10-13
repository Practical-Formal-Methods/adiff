
default: img-run

compile: img-build
	stack build

adiff-docker: Dockerfile
	docker build -t adiff:latest  .

clean:
	stack clean

install: img-run
	stack build --copy-bins --no-docker

# regression tests
test: img-run
	stack test

# docker images

img-all-verifiers:
	docker build -t adiff/all-verifiers docker/adiff/all-verifiers

img-build: img-all-verifiers
	docker build -t adiff/build docker/adiff/build

img-run: compile
	stack install
	docker build -t adiff/adiff -f docker/adiff/adiff/Dockerfile .


.PHONY: test test-integration img-all-verifiers img-build img-run compile install
