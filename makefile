
# $Id$

.SILENT: all build_std build_ssl clean distrib

# update INCLUDES to point to the libraries directories for POSIX and Sockets
# or use GNAT ADA_INCLUDE_PATH or ADA_OBJECTS_PATH

# External packages to be configured

XMLADA	= /home/obry/Projets/build/xml

INCLUDES = -I$(XMLADA)/xml -I$(XMLADA)/sax -I$(XMLADA)/input_sources \
	-I$(XMLADA)/unicode

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
	echo "build_doc:    build documentation (needs texinfo support)"
	echo "build_soap:   build SOAP library (needs XML-Ada package)"
	echo ""
	echo "clean:        to clean directories"
	echo "distrib:      to build a tarball distribution"

build_aws_std:
	make -C src std_mode
	make -C src build MODE=std INCLUDES="$(INCLUDES)"

build_aws_ssl:
	make -C src ssl_mode
	make -C src build MODE=ssl INCLUDES="$(INCLUDES)"

build_demo_std:
	make -C demos build MODE=std INCLUDES="$(INCLUDES)"

build_demo_ssl:
	make -C demos build MODE=ssl INCLUDES="$(INCLUDES)"

build_ssllib:
	make -C ssl build INCLUDES="$(INCLUDES)"

build_soap: build_include
	make -C soap build INCLUDES="$(INCLUDES)"

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
	rm *.~*.*~

distrib: build_doc
	-rm -f aws-*.tar*
	(VERSION=`grep " Version" src/aws.ads | cut -c 43-45`; \
	AWS=aws-$${VERSION}; \
	mkdir $${AWS}; \
	mkdir $${AWS}/src; \
	mkdir $${AWS}/demos; \
	mkdir $${AWS}/docs; \
	mkdir $${AWS}/icons; \
	mkdir $${AWS}/include; \
	mkdir $${AWS}/soap; \
	mkdir $${AWS}/ssl; \
	mkdir $${AWS}/win32; \
	cp message $${AWS}/readme.txt; \
	cp AUTHORS makefile $${AWS};\
	cp src/makefile src/ChangeLog src/*.ad[sb] $${AWS}/src;\
	cp demos/makefile demos/[shrw]*.ads demos/[ahmrsw]*.adb $${AWS}/demos;\
	cp demos/*.png demos/cert.pem demos/page*.html $${AWS}/demos;\
	cp demos/aws_*.thtml demos/com*.adb  demos/ws.ini $${AWS}/demos;\
	cp docs/aws.texi docs/[at]*.html docs/aws.txt $${AWS}/docs;\
	cp docs/aws.info* docs/aws.ps docs/makefile $${AWS}/docs;\
	cp docs/TODO docs/openssl.license $${AWS}/docs;\
	cp win32/*.a win32/*.txt $${AWS}/win32;\
	cp ssl/*.ad[sb] ssl/ChangeLog ssl/makefile $${AWS}/ssl;\
	cp include/*.ad[sb] include/makefile $${AWS}/include;\
	cp include/readme.txt $${AWS}/include;\
	cp icons/*.gif $${AWS}/icons;\
	cp soap/*.ad[sb] soap/makefile $${AWS}/soap;\
	tar cf $${AWS}.tar $${AWS};\
	gzip -9 $${AWS}.tar;\
	rm -fr $${AWS})
