
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

build_std: build_aws_std build_doc build_demo_std

build_ssl: build_aws_ssl build_doc build_demo_ssl

build_doc:
	make -C docs build

clean:
	make -C src clean
	make -C demos clean

distrib:
	-rm -f aws.tar*
	tar cf aws.tar makefile src/makefile demos/makefile src/ChangeLog \
		src/*.ad[sb] demos/r*.ads demos/[ar]*.adb demos/*.gif \
		docs/aws.texi docs/aws.html docs/aws.txt docs/aws.info \
		docs/aws.ps win32/*.a
	gzip -9 aws.tar
