
# $Id$

.SILENT: all build build clean distrib install set_std set_ssl

include makefile.conf

###########################################################################
# update INCLUDES to point to the libraries directories for POSIX and Sockets
# or use GNAT ADA_INCLUDE_PATH or ADA_OBJECTS_PATH

# External packages to be configured

#XMLADA	= /usr/Ada.Libraries/XMLada

ifdef XMLADA
# This configuration is for XmlAda 0.7
v_XMLADA = 0.7
INCLUDES = -I$(XMLADA)/include/xmlada -I$(XMLADA)/lib
LIB_DOM  = -lxmlada_dom-$(v_XMLADA)
LIB_UNIC = -lxmlada_unicode-$(v_XMLADA)
LIB_SAX  = -lxmlada_sax-$(v_XMLADA)
LIB_IS   =  -lxmlada_input_sources-$(v_XMLADA)
LIBS	 = -L$(XMLADA)/lib $(LIB_IS) $(LIB_DOM) $(LIB_UNIC) $(LIB_SAX)
endif

INSTALL	 = /usr/Ada.Libraries/AWS

# compiler
RELEASE_GFLAGS	= -O2 -gnatn
DEBUG_GFLAGS	= -g -m -gnatwu -gnaty3abcefhiklmnoprst

# linker
RELEASE_LFLAGS	= -s
DEBUG_LFLAGS	=

ifdef DEBUG
GFLAGS		= $(DEBUG_GFLAGS)
LFLAGS		= $(DEBUG_LFLAGS)
else
GFLAGS		= $(RELEASE_GFLAGS)
LFLAGS		= $(RELEASE_LFLAGS)
endif

# NO NEED TO CHANGE ANYTHING PAST THIS POINT
###########################################################################

all:
	echo ""
	echo "Targets :"
	echo ""
	echo "  Configurations :"
	echo ""
	echo "    set_ssl:      build with SSL support (Secure Socket Layer)"
	echo "    set_std:      build without SSL support [default]"
	echo ""
	echo "    gnat_oslib:   OS_Lib implementation for GNAT only [default]"
	echo "    posix_oslib:  OS_Lib implementation based on POSIX"
	echo "    win32_oslib:  OS_Lib implementation for Win32 only"
	echo ""
	echo "  Build :"
	echo ""
	echo "    build:        build AWS library and demos"
	echo "    build_doc:    build documentation (needs texinfo support)"
	echo "    build_soap:   build SOAP library (needs XML-Ada package)"
	echo ""
	echo "  Support :"
	echo ""
	echo "    clean:        to clean directories"
	echo "    distrib:      to build a tarball distribution"
	echo "    install:      install AWS library"
	echo "    run_regtests: run tests"

ALL_OPTIONS	= GFLAGS="$(GFLAGS)" INCLUDES="$(INCLUDES)" LIBS="$(LIBS)" LFLAGS="$(LFLAGS)" MODE="$(MODE)" XMLADA="$(XMLADA)"

set_ssl:
	echo "MODE=ssl" > makefile.conf
	${MAKE} -C src ssl_mode

set_std:
	echo "MODE=std" > makefile.conf
	${MAKE} -C src std_mode

build: build_ssllib build_include build_aws build_win32 build_demo

build_aws:
	${MAKE} -C src build $(ALL_OPTIONS)

build_demo:
	${MAKE} -C demos build $(ALL_OPTIONS)

build_soap_demo:
	${MAKE} -C demos build_soap $(ALL_OPTIONS)

build_ssllib:
ifeq ($(MODE),ssl)
	${MAKE} -C ssl build $(ALL_OPTIONS)
endif

build_soaplib: build_include
	${MAKE} -C soap build $(ALL_OPTIONS)

build_soap: build_soaplib build_soap_demo

gnat_oslib:
	${MAKE} -C src gnat_oslib

posix_oslib:
	${MAKE} -C src posix_oslib

win32_oslib:
	${MAKE} -C src win32_oslib

build_doc:
	${MAKE} -C docs build

build_include:
	${MAKE} -C include build $(ALL_OPTIONS)

build_win32:
	${MAKE} -C win32 build $(ALL_OPTIONS)

run_regtests:
	${MAKE} -C regtests run $(ALL_OPTIONS)

clean:
	${MAKE} -C include clean
	${MAKE} -C src clean
	${MAKE} -C demos clean
	${MAKE} -C ssl clean
	${MAKE} -C docs clean
	${MAKE} -C soap clean
	${MAKE} -C regtests clean
	${MAKE} -C win32 clean
	-rm *.~*.*~
	rm makefile.conf
	echo MODE=std > makefile.conf

distrib: clean build_doc
	-rm -f aws-*.tar*
	(VERSION=`grep " Version" src/aws.ads | cut -c 43-45`; \
	AWS=aws-$${VERSION}; \
	mkdir $${AWS}; \
	mkdir $${AWS}/src; \
	mkdir $${AWS}/demos; \
	mkdir $${AWS}/regtests; \
	mkdir $${AWS}/docs; \
	mkdir $${AWS}/icons; \
	mkdir $${AWS}/include; \
	mkdir $${AWS}/soap; \
	mkdir $${AWS}/ssl; \
	mkdir $${AWS}/win32; \
	cp AUTHORS makefile makefile.conf readme.txt $${AWS};\
	cp src/makefile src/ChangeLog src/*.ad[sb] $${AWS}/src;\
	cp demos/makefile demos/404.thtml demos/di*.adb $${AWS}/demos;\
	cp demos/[shrw]*.ads demos/[ahmrstvw]*.adb $${AWS}/demos;\
	cp demos/*.png demos/cert.pem demos/page*.html $${AWS}/demos;\
	cp demos/aws_*.thtml demos/com*.adb  demos/ws.ini $${AWS}/demos;\
	cp regtests/*.out regtests/*.ad* regtests/makefile $${AWS}/regtests;\
	cp regtests/ChangeLog $${AWS}/regtests;\
	cp docs/aws.texi.tmplt docs/build.adb $${AWS}/docs;\
	cp docs/aws.texi docs/[at]*.html docs/aws.txt $${AWS}/docs;\
	cp docs/aws.info* docs/aws.ps docs/makefile $${AWS}/docs;\
	cp docs/gentexifile docs/TODO docs/openssl.license $${AWS}/docs;\
	cp win32/*.dll win32/makefile win32/*.txt $${AWS}/win32;\
	cp win32/aws.ico win32/aws.rc $${AWS}/win32;\
	cp ssl/*.ad[sb] ssl/ChangeLog ssl/makefile $${AWS}/ssl;\
	cp include/*.ad[sb] include/makefile $${AWS}/include;\
	cp include/readme.txt $${AWS}/include;\
	cp icons/*.gif $${AWS}/icons;\
	cp soap/*.ad[sb] soap/makefile soap/ChangeLog $${AWS}/soap;\
	tar cf $${AWS}.tar $${AWS};\
	gzip -9 $${AWS}.tar;\
	rm -fr $${AWS})

install:
	-rm -fr $(INSTALL)
	mkdir $(INSTALL)
	mkdir $(INSTALL)/lib
	mkdir $(INSTALL)/include
	mkdir $(INSTALL)/icons
	mkdir $(INSTALL)/images
	mkdir $(INSTALL)/templates
	mkdir $(INSTALL)/docs
	mkdir $(INSTALL)/components
	ar cr libaws.a src/*.o
	-ar cr libaws.a ssl/*.o
	-ar cr libaws.a soap/*.o
	cp src/*.ad[sb] ssl/*.ad[sb] $(INSTALL)/include
	-cp soap/*.ad[sb] $(INSTALL)/include
	cp src/*.ali $(INSTALL)/lib
	-cp ssl/*.ali $(INSTALL)/lib
	-cp soap/*.ali $(INSTALL)/lib
	chmod uog-w $(INSTALL)/lib/*.ali
	mv libaws.a $(INSTALL)/lib
	-cp docs/aws.html $(INSTALL)/docs
	cp docs/templates_parser.html $(INSTALL)/docs
	-cp docs/aws.txt $(INSTALL)/docs
	-cp docs/*.info* $(INSTALL)/docs
	cp demos/*.thtml $(INSTALL)/templates
	cp icons/*.gif $(INSTALL)/icons
	cp demos/aws_*.png $(INSTALL)/images
	-cp ssl/*.a $(INSTALL)/lib
	-cp include/*.ad? include/*.o include/*.ali $(INSTALL)/components
