
# $Id$

.SILENT: all build_std build_ssl clean distrib

# update INCLUDES to point to the libraries directories for POSIX and Sockets
# or use GNAT ADA_INCLUDE_PATH or ADA_OBJECTS_PATH

INCLUDES =

all:
	echo "Targets :"
	echo ""
	echo "build_ssl:    build with SSL support (Secure Socket Layer)"
	echo "build_std:    build without SSL support"
	echo ""
	echo "gnat_oslib:   OS_Lib implementation for GNAT only [default]"
	echo "posix_oslib:  OS_Lib implementation based on POSIX"
	echo "win32_oslib:  OS_Lib implementation for Win32 only"
	echo ""
	echo "build_doc:    build documentation (need texinfo support)"
	echo ""
	echo "clean:        to clean directories"
	echo "distrib:      to build a tarball distribution"

build_aws_std:
	make -C src std_mode
	make -C src build MODE=std INCLUDES=$(INCLUDES)

build_aws_ssl:
	make -C src ssl_mode
	make -C src build MODE=ssl INCLUDES=$(INCLUDES)

build_demo_std:
	make -C demos build MODE=std INCLUDES=$(INCLUDES)

build_demo_ssl:
	make -C demos build MODE=ssl INCLUDES=$(INCLUDES)

build_ssllib:
	make -C ssl build INCLUDES=$(INCLUDES)

build_std: build_include build_aws_std build_demo_std

build_ssl: build_ssllib build_include build_aws_ssl build_demo_ssl

gnat_oslib:
	make -c src gnat_oslib

posix_oslib:
	make -c src posix_oslib

win32_oslib:
	make -c src win32_oslib

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
	cp message readme.txt
	tar cf aws.tar makefile readme.txt src/makefile demos/makefile \
		src/ChangeLog \
		src/*.ad[sb] demos/[hrw]*.ads demos/[ahmrw]*.adb demos/*.png \
		docs/aws.texi docs/[at]*.html docs/aws.txt docs/aws.info* \
		docs/aws.ps docs/makefile win32/*.a win32/*.txt \
		demos/cert.pem ssl/*.ad* ssl/ChangeLog ssl/makefile \
		demos/page*.html demos/status.tmplt docs/TODO \
		include/*.ad[sb] include/makefile demos/com*.adb \
		docs/openssl.license
	rm readme.txt
	gzip -9 aws.tar
	mv aws.tar.gz aws-`grep " Version" src/aws.ads | cut -c 41-44`.tar.gz
