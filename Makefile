
VERSION= $(shell stack query "locals" | grep version | cut -d" " -f 4 | tr -d "'")
SDIST= $(shell stack path --dist-dir)
BINARY= $(shell stack path --dist-dir)/build/vdiff/vdiff

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
	echo "ENV  vdiff_version=$(VERSION)" > docker/vdiff.tmp
	echo "COPY $(BINARY) /root/.local/bin" > docker/vdiff.tmp

# concatenate the different elements of the dockerfile
	cat docker/base.in  docker/ultimate.in docker/vdiff.tmp docker/samples.in > Dockerfile



