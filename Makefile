
VERSION= $(shell stack query "locals" | grep version | cut -d" " -f 4 | tr -d "'")
SDIST= $(shell stack path --dist-dir)

default: compile Dockerfile


compile:
	stack build


clean:
	stack clean
	rm -f docker/*.tmp
	rm -f Dockerfile

Dockerfile : docker compile
	stack sdist
	echo "ENV  vdiff_version=$(VERSION)" > docker/vdiff.tmp
	echo "COPY \"$(SDIST)/vdiff-$(VERSION).tar.gz\" /tmp" >> docker/vdiff.tmp
	echo "RUN cd /tmp && tar -xf vdiff-$(VERSION).tar.gz && cd vdiff-$(VERSION) && stack init && stack install" >> docker/vdiff.tmp

# concatenate the different elements of the dockerfile
	cat docker/base.in docker/vdiff.tmp > Dockerfile



