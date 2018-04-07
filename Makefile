
VERSION= $(shell stack query "locals" | grep version | cut -d" " -f 4 | tr -d "'")
SDIST= $(shell stack path --dist-dir)
BINARY= $(shell stack path --dist-dir)/build/vdiff/vdiff
INTEGRATION= $(shell stack path --dist-dir)/build/integration/integration

default: compile vdiff-docker


compile:
	stack build

vdiff-docker: Dockerfile
	docker build -t vdiff:latest  .

clean:
	stack clean
	rm -f docker/*.tmp
	rm -f Dockerfile

#Dockerfile : docker
#	stack sdist
#	echo "ENV  vdiff_version=$(VERSION)" > docker/vdiff.tmp
#	echo "COPY \"$(SDIST)/vdiff-$(VERSION).tar.gz\" /tmp" >> docker/vdiff.tmp
#	echo "RUN cd /tmp && tar -xf vdiff-$(VERSION).tar.gz && cd vdiff-$(VERSION) && stack init && stack install" >> docker/vdiff.tmp

Dockerfile : docker compile
	echo "# Install vdiff into docker" > docker/vdiff.tmp
	echo "RUN apt-get install -y time vim" >> docker/vdiff.tmp
	echo "ENV vdiff_version=\"$(VERSION)\"" >> docker/vdiff.tmp
	echo "COPY $(BINARY) /root/.local/bin" >> docker/vdiff.tmp
	echo "COPY $(INTEGRATION) /root/.local/bin" >> docker/vdiff.tmp

# concatenate the different elements of the dockerfile
	cat 	docker/base.in\
	      	docker/ultimate.in\
	       	docker/cpachecker.in\
	       	docker/seahorn.in\
	       	docker/vdiff.tmp\
	       	docker/samples.in > Dockerfile

# regression tests
test:
	stack test vdiff:regression


# integration
test-integration: vdiff-docker
	stack test vdiff:integration --no-run-tests
	docker run vdiff:latest /bin/bash -lc "integration --color=always"


.PHONY : test test-integration
