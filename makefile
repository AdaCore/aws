
# $Id$

.SILENT: all build clean distrib

all:
	echo "Targets :"
	echo "build:        to build everything"
	echo "clean:        to clean directories"
	echo "distrib:      to build a tarball distribution"

build:
	make -C src build
	make -C demos build

clean:
	make -C src clean
	make -C demos clean

distrib:
	tar cf aws.tar makefile src/makefile demos/makefile \
		src/*.ad[sb] demos/*.ad[sb]
	gzip -9 aws.tar
