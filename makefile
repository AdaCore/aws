
# $Id$

.SILENT: all build_std build_ssl clean distrib

all:
	echo "Targets :"
	echo "build_std:    to build everything (without SSL support)"
	echo "build_ssl:    to build everything (with SSL support)"
	echo "clean:        to clean directories"
	echo "distrib:      to build a tarball distribution"

build_aws_std:
	make -C src build MODE=std

build_aws_ssl:
	make -C src build MODE=ssl

build_demo_std:
	make -C demos build MODE=std

build_demo_ssl:
	make -C demos build MODE=ssl

build_ssllib:
	make -C ssl build

build_std: build_include build_aws_std build_demo_std

build_ssl: build_ssllib build_include build_aws_ssl build_demo_ssl

build_doc:
	make -C docs build

build_include:
	make -C include build

clean:
	make -C include clean
	make -C src clean
	make -C demos clean
	make -C ssl clean
	make -C docs clean

distrib:
	-rm -f aws.tar*
	tar cf aws.tar makefile src/makefile demos/makefile src/ChangeLog \
		src/*.ad[sb] demos/[rw]*.ads demos/[ahmrw]*.adb demos/*.gif \
		docs/aws.texi docs/aws.html docs/aws.txt docs/aws.info* \
		docs/aws.ps docs/makefile win32/*.a win32/*.txt \
		demos/cert.pem ssl/*.ad* ssl/ChangeLog ssl/makefile \
		demos/page*.html demos/status.tmplt docs/TODO \
		include/*.ad[sb] include/makefile demos/com*.adb
	gzip -9 aws.tar
	mv aws.tar.gz aws-`grep " Version" src/aws.ads | cut -c 41-43`.tar.gz
