
# $Id$

.SILENT: all build build clean distrib install set_std set_ssl build_tarball
.SILENT: display build_aws build_lib build_doc build_tools build_soap
.SILENT: build_soap_demos build_ssllib build_soaplib build_win32 build_include
.SILENT: build_demos run_regtests

include makefile.conf

#############################################################################
# update INCLUDES to point to the libraries directories for POSIX and Sockets
# or use GNAT ADA_INCLUDE_PATH or ADA_OBJECTS_PATH

# External packages to be configured

# Either set ADASOCKETS, XMLADA here or you can update ADA_INCLUDE_PATH and
# ADA_OBJECTS_PATH environments variables.

# Adasockets, required.
ADASOCKETS = /usr/Ada.Libraries/adasockets

# XMLADA package, needed if you want to build SOAP's AWS support.
# comment out XMLADA if you don't want to build with SOAP support.

XMLADA	 = /usr/Ada.Libraries/xmlada

# AWS will be installed under $(INSTALL)/AWS
INSTALL	 = $(HOME)

ifdef ADASOCKETS
INCLUDES = -I$(ADASOCKETS)/lib/adasockets
LIBS     = -L$(ADASOCKETS)/lib -ladasockets
endif

ifdef XMLADA
INCLUDES := -I$(XMLADA)/include/xmlada -I$(XMLADA)/lib $(INCLUDES)
LIB_DOM  = -lxmlada_dom
LIB_UNIC = -lxmlada_unicode
LIB_SAX  = -lxmlada_sax
LIB_IS   = -lxmlada_input_sources
LIBS	 := -L$(XMLADA)/lib $(LIB_IS) $(LIB_DOM) $(LIB_UNIC) $(LIB_SAX) $(LIBS)
endif

ifeq (${OS}, Windows_NT)
EXEEXT = .exe
else
EXEEXT =
endif

STYLE_FLAGS	= -gnatwcfipru -gnatwe -gnaty3abcefhiklmnoprst

# compiler
RELEASE_GFLAGS	= -O2 -gnatn
DEBUG_GFLAGS	= -g -m

# linker
RELEASE_LFLAGS	= -s
DEBUG_LFLAGS	=

ifdef DEBUG
GFLAGS		= $(DEBUG_GFLAGS) $(STYLE_FLAGS)
LFLAGS		= $(DEBUG_LFLAGS)
MAKE_OPT	=
else
GFLAGS		= $(RELEASE_GFLAGS) $(STYLE_FLAGS)
LFLAGS		= $(RELEASE_LFLAGS)
MAKE_OPT	= -s
endif

#############################################################################
# NO NEED TO CHANGE ANYTHING PAST THIS POINT
#############################################################################

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
	echo "    display       Display current configuration"
	echo ""
	echo "  Build :"
	echo ""
	echo "    build:        build AWS library, tools and demos"
	echo "    build_lib:    build AWS library only"
	echo "    build_tools:  build AWS tools only"
	echo "    build_doc:    build documentation (needs texinfo support)"
	echo "    build_soap:   build SOAP library (needs XMLAda package)"
	echo ""
	echo "  Support :"
	echo ""
	echo "    clean:        to clean directories"
	echo "    distrib:      to build a tarball distribution"
	echo "    install:      install AWS library"
	echo "    run_regtests: run tests"

ALL_OPTIONS	= $(MAKE_OPT) GFLAGS="$(GFLAGS)" INCLUDES="$(INCLUDES)" LIBS="$(LIBS)" LFLAGS="$(LFLAGS)" MODE="$(MODE)" XMLADA="$(XMLADA)"

set_ssl:
	echo "MODE=ssl" > makefile.conf
	make -C demos clean $(ALL_OPTIONS)
	make -C ssl clean $(ALL_OPTIONS)

set_std:
	echo "MODE=std" > makefile.conf
	make -C demos clean $(ALL_OPTIONS)
	make -C ssl clean $(ALL_OPTIONS)

build_lib: build_ssllib build_include build_aws build_win32

ifdef XMLADA
build: build_lib build_soaplib build_demos build_soap_demos
else
build: build_lib build_demos
endif

build_aws:
	echo ""
	echo === Build library
	${MAKE} -C src build $(ALL_OPTIONS)

build_tools:
	echo ""
	echo === Build tools
	${MAKE} -C tools build $(ALL_OPTIONS)

build_demos: build_lib build_tools
	echo ""
	echo === Build demos
	${MAKE} -C demos build $(ALL_OPTIONS)

build_soap_demos:
	echo ""
	echo === Build SOAP demos
	${MAKE} -C demos build_soap $(ALL_OPTIONS)

build_ssllib:
ifeq ($(MODE),ssl)
	echo ""
	echo === Build SSL support
	${MAKE} -C ssl build $(ALL_OPTIONS)
else
	echo ""
	echo === Build SSL wrappers
	${MAKE} -C ssl wrappers $(ALL_OPTIONS)
endif

build_soaplib: build_include
	echo ""
	echo === Build SOAP library
	${MAKE} -C soap build $(ALL_OPTIONS)

build_soap: build_lib build_soaplib build_soap_demo

gnat_oslib:
	${MAKE} -C src gnat_oslib

posix_oslib:
	${MAKE} -C src posix_oslib

win32_oslib:
	${MAKE} -C src win32_oslib

build_doc:
	echo ""
	echo === Build doc
	${MAKE} -C docs build $(ALL_OPTIONS)

build_include:
	echo ""
	echo === Build components
	${MAKE} -C include build $(ALL_OPTIONS)

build_win32:
	echo ""
	echo === Build win32 specific packages
	${MAKE} -C win32 build $(ALL_OPTIONS)

build_apiref:
	echo ""
	echo === Build API References
	${MAKE} -s -C docs apiref

run_regtests: build_tools
	echo ""
	echo === Run regression tests
	${MAKE} -C regtests run $(ALL_OPTIONS)

clean: clean_noapiref
	${MAKE} -C docs clean_apiref

clean_noapiref:
	${MAKE} -C include clean
	${MAKE} -C src clean
	${MAKE} -C demos clean
	${MAKE} -C ssl clean
	${MAKE} -C docs clean
	${MAKE} -C soap clean
	${MAKE} -C regtests clean
	${MAKE} -C win32 clean
	${MAKE} -C tools clean
	-rm *.~*.*~
	rm makefile.conf
	echo MODE=std > makefile.conf

display:
	echo ""
	echo AWS current configuration
	echo ""
ifeq (${OS}, Windows_NT)
	echo "Windows OS detected"
	echo "   To build AWS on this OS you need to have a set of UNIX like"
	echo "   tools (cp, mv, mkdir, chmod...) You should install"
	echo "   Cygwin or Msys toolset"
	echo ""
else
	echo "UNIX like OS detected"
endif
	echo "Install directory     : " $(INSTALL)
ifdef XMLADA
	echo "XMLada activated      : " $(XMLADA)
else
	echo "XMLada not activate, SOAP will not be built"
endif
ifdef ADASOCKETS
	echo "AdaSockets package in : " $(ADASOCKETS)
else
	echo "AdaSockets not set in makefile, be sure to update "
	echo "ADA_INCLUDE_PATH and ADA_OBJECTS_PATH"
endif

build_tarball:
	-rm -f aws-*.tar*
	(VERSION=`grep " Version" src/aws.ads | cut -d\" -f2`; \
	AWS=aws-$${VERSION}; \
	mkdir $${AWS}; \
	mkdir $${AWS}/src; \
	mkdir $${AWS}/demos; \
	mkdir $${AWS}/regtests; \
	mkdir $${AWS}/docs; \
	mkdir $${AWS}/docs/html; \
	mkdir $${AWS}/icons; \
	mkdir $${AWS}/include; \
	mkdir $${AWS}/soap; \
	mkdir $${AWS}/ssl; \
	mkdir $${AWS}/win32; \
	mkdir $${AWS}/tools; \
	cp INSTALL AUTHORS makefile makefile.conf readme.txt $${AWS};\
	cp src/makefile src/ChangeLog src/*.ad[sb] $${AWS}/src;\
	cp demos/makefile demos/404.thtml demos/di*.adb $${AWS}/demos;\
	cp demos/[shrw]*.ads demos/[ahmrstvw]*.adb $${AWS}/demos;\
	cp demos/*.png demos/cert.pem demos/page*.html $${AWS}/demos;\
	cp demos/aws_*.thtml demos/com*.adb  demos/ws.ini $${AWS}/demos;\
	cp regtests/*.out regtests/*.ad* regtests/makefile $${AWS}/regtests;\
	cp regtests/ChangeLog $${AWS}/regtests;\
	cp docs/aws.texi.tmplt docs/build.adb $${AWS}/docs;\
	cp docs/aws.texi docs/[at]*.html docs/aws.txt $${AWS}/docs;\
	cp docs/aws.info* docs/aws.ps docs/aws.pdf docs/makefile $${AWS}/docs;\
	cp docs/gentexifile docs/TODO docs/openssl.license $${AWS}/docs;\
	cp -r docs/html/* $${AWS}/docs/html;\
	cp win32/*.dll win32/makefile win32/*.txt $${AWS}/win32;\
	cp win32/aws.ico win32/aws.rc win32/wldap32.def $${AWS}/win32;\
	cp ssl/*.ad[sb] ssl/ChangeLog ssl/makefile $${AWS}/ssl;\
	cp include/*.ad[sb] include/makefile $${AWS}/include;\
	cp include/readme.txt $${AWS}/include;\
	cp icons/*.gif $${AWS}/icons;\
	cp soap/*.ad[sb] soap/makefile soap/ChangeLog $${AWS}/soap;\
	cp tools/*.ad[sb] tools/makefile $${AWS}/tools;\
	tar cf $${AWS}.tar $${AWS};\
	gzip -9 $${AWS}.tar;\
	rm -fr $${AWS})

distrib: build_apiref clean_noapiref build_doc build_tarball clean

force:

install: force
	-rm -fr $(INSTALL)/AWS
	mkdir $(INSTALL)/AWS
	mkdir $(INSTALL)/AWS/lib
	mkdir $(INSTALL)/AWS/include
	mkdir $(INSTALL)/AWS/icons
	mkdir $(INSTALL)/AWS/images
	mkdir $(INSTALL)/AWS/templates
	mkdir $(INSTALL)/AWS/docs
	mkdir $(INSTALL)/AWS/docs/html
	mkdir $(INSTALL)/AWS/components
	mkdir $(INSTALL)/AWS/tools
	ar cr libaws.a src/*.o
	-ar cr libaws.a ssl/*.o
	-ar cr libaws.a soap/*.o
	cp src/a*.ad[sb] ssl/*.ad[sb] $(INSTALL)/AWS/include
	-cp soap/*.ad[sb] $(INSTALL)/AWS/include
	cp src/a*.ali $(INSTALL)/AWS/lib
	-cp ssl/*.ali $(INSTALL)/AWS/lib
	-cp soap/*.ali $(INSTALL)/AWS/lib
	chmod uog-w $(INSTALL)/AWS/lib/*.ali
	mv libaws.a $(INSTALL)/AWS/lib
	-cp docs/aws.html $(INSTALL)/AWS/docs
	cp docs/templates_parser.html $(INSTALL)/AWS/docs
	-cp docs/aws.txt $(INSTALL)/AWS/docs
	-cp docs/*.info* $(INSTALL)/AWS/docs
	-cp -r docs/html/* $(INSTALL)/AWS/docs/html
	cp demos/*.thtml $(INSTALL)/AWS/templates
	cp icons/*.gif $(INSTALL)/AWS/icons
	cp demos/aws_*.png $(INSTALL)/AWS/images
	-cp ssl/*.a $(INSTALL)/AWS/lib
	-cp include/*.ad? include/*.o include/*.ali $(INSTALL)/AWS/components
	-cp tools/awsres${EXEEXT} $(INSTALL)/AWS/tools
	-chmod -R og+r $(INSTALL)/AWS
ifeq (${OS}, Windows_NT)
	cp win32/libldap.a $(INSTALL)/AWS/lib
	cp win32/*.dll $(INSTALL)/AWS/lib
endif
