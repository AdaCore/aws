
# $Id$

.SILENT: all build build clean clean_noapiref distrib install build_tarball
.SILENT: display build_aws build_lib build_doc build_tools build_soap
.SILENT: build_soap_demos build_ssllib build_soaplib build_win32 build_include
.SILENT: build_demos run_regtests setup build_apiref

# NOTE: You should not have to change this makefile. Configuration options can
# be changed in makefile.conf

include makefile.conf

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
# -gnatwk (constant) should be added but in GNAT 3.16a it reports problems on
# withed packages.

# compiler
RELEASE_GFLAGS	= -q -O2 -gnatn
DEBUG_GFLAGS	= -g -m -gnata

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

CP = cp -p

all:
	echo ""
	echo "Targets :"
	echo ""
	echo "  Configurations :"
	echo ""
	echo "    gnatsockets:  Use GNAT.Sockets [default]"
	echo "    adasockets:   Use AdaSockets"
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
	echo "    build_tools:  build AWS library and tools"
	echo "    build_doc:    build documentation (needs texinfo support)"
	echo "    build_soap:   build SOAP library (needs XMLAda package)"
	echo ""
	echo "  Support :"
	echo ""
	echo "    clean:        to clean directories"
	echo "    distrib:      to build a tarball distribution"
	echo "    install:      install AWS library"
	echo "    run_regtests: run tests"

ALL_OPTIONS	= $(MAKE_OPT) GFLAGS="$(GFLAGS)" INCLUDES="$(INCLUDES)" LIBS="$(LIBS)" LFLAGS="$(LFLAGS)" MODE="$(MODE)" XMLADA="$(XMLADA)" EXEEXT="$(EXEEXT)" LDAP="$(LDAP)" DEBUG="$(DEBUG)" RM="$(RM)" CP="$(CP)" MV="$(MV)" MKDIR="$(MKDIR)" AR="$(AR)" GREP="$(GREP)" SED="$(SED)" DIFF="$(DIFF)" CHMOD="$(CHMOD)" GZIP="$(GZIP)" TAR="$(TAR)" GNATMAKE="$(GNATMAKE)" DLLTOOL="$(DLLTOOL)" DLL2DEF="$(DLL2DEF)" WINDRES="$(WINDRES)" GNATMAKE_FOR_HOST="$(GNATMAKE_FOR_HOST)"

build_lib: build_ssllib build_include build_aws build_win32

ifdef XMLADA
build: build_lib build_soaplib build_demos
else
build: build_lib build_demos
endif

build_aws:
	echo ""
	echo "=== Build AWS library"
	${MAKE} -C src build $(ALL_OPTIONS)

build_tools:
	echo ""
	echo "=== Build tools"
	${MAKE} -C tools build $(ALL_OPTIONS)

build_demos: build_lib build_tools
	echo ""
	echo "=== Build demos"
	${MAKE} -C demos build $(ALL_OPTIONS)

build_ssllib:
	echo ""
	echo "=== Build SSL support"
	${MAKE} -C ssl build $(ALL_OPTIONS)

build_soaplib: build_include build_lib
	echo ""
	echo "=== Build SOAP library"
	${MAKE} -C soap build $(ALL_OPTIONS)

build_soap: build_soaplib

gnatsockets:
	${MAKE} -C src gnatsockets $(ALL_OPTIONS)

adasockets:
	${MAKE} -C src adasockets $(ALL_OPTIONS)

gnat_oslib:
	${MAKE} -C src gnat_oslib $(ALL_OPTIONS)

posix_oslib:
	${MAKE} -C src posix_oslib $(ALL_OPTIONS)

win32_oslib:
	${MAKE} -C src win32_oslib $(ALL_OPTIONS)

build_doc:
	echo ""
	echo "=== Build doc"
	${MAKE} -C docs build $(ALL_OPTIONS)

build_include:
	echo ""
	echo "=== Build components"
	${MAKE} -C include build $(ALL_OPTIONS)

build_win32:
	echo ""
	echo "=== Build win32 specific packages"
	${MAKE} -C win32 build $(ALL_OPTIONS)

build_apiref:
	echo ""
	echo "=== Build API References"
	${MAKE} -s -C docs apiref $(ALL_OPTIONS)

run_regtests: build_tools
	echo ""
	echo "=== Run regression tests"
	${MAKE} -C regtests run $(ALL_OPTIONS) GDB_REGTESTS="$(GDB_REGTESTS)"

clean: clean_noapiref
	${MAKE} -C docs clean_apiref $(ALL_OPTIONS)

clean_noapiref:
	${MAKE} -C include clean $(ALL_OPTIONS)
	${MAKE} -C config clean $(ALL_OPTIONS)
	${MAKE} -C src clean $(ALL_OPTIONS)
	${MAKE} -C demos clean $(ALL_OPTIONS)
	${MAKE} -C ssl clean $(ALL_OPTIONS)
	${MAKE} -C docs clean $(ALL_OPTIONS)
	${MAKE} -C soap clean $(ALL_OPTIONS)
	${MAKE} -C regtests clean $(ALL_OPTIONS)
	${MAKE} -C win32 clean $(ALL_OPTIONS)
	${MAKE} -C tools clean $(ALL_OPTIONS)
	${MAKE} -C lib clean $(ALL_OPTIONS)
	-rm -f *.~*.*~

display:
	echo ""
	echo AWS current configuration
	echo ""
ifeq (${OS}, Windows_NT)
	echo "Windows OS detected"
	echo "   To build AWS on this OS you need to have a set of UNIX like"
	echo "   tools (cp, mv, mkdir, chmod...) You should install"
	echo "   Cygwin or Msys toolset."
	echo ""
else
	echo "UNIX like OS detected"
endif
	echo "Install directory     : " $(INSTALL)
ifdef XMLADA
	echo "XMLada activated      : " $(XMLADA)
else
	echo "XMLada not activated, SOAP will not be built"
endif
ifdef ADASOCKETS
	echo "AdaSockets package in : " $(ADASOCKETS)
else
	echo "Using GNAT.Sockets"
endif

build_tarball:
	-$(RM) -f aws-*.tar*
	(VERSION=`grep " Version" src/aws.ads | cut -d\" -f2`; \
	AWS=aws-$${VERSION}; \
	$(MKDIR) $${AWS}; \
	$(MKDIR) $${AWS}/src; \
	$(MKDIR) $${AWS}/xsrc; \
	$(MKDIR) $${AWS}/demos; \
	$(MKDIR) $${AWS}/regtests; \
	$(MKDIR) $${AWS}/docs; \
	$(MKDIR) $${AWS}/docs/html; \
	$(MKDIR) $${AWS}/icons; \
	$(MKDIR) $${AWS}/include; \
	$(MKDIR) $${AWS}/include/zlib; \
	$(MKDIR) $${AWS}/lib; \
	$(MKDIR) $${AWS}/soap; \
	$(MKDIR) $${AWS}/ssl; \
	$(MKDIR) $${AWS}/win32; \
	$(MKDIR) $${AWS}/tools; \
	$(MKDIR) $${AWS}/config; \
	$(MKDIR) $${AWS}/config/src; \
	$(CP) INSTALL AUTHORS makefile makefile.conf readme.txt $${AWS};\
	$(CP) src/makefile src/ChangeLog src/*.ad[sb] $${AWS}/src;\
	$(CP) demos/makefile demos/404.thtml demos/di*.adb $${AWS}/demos;\
	$(CP) demos/[hirsw]*.ads demos/[ahimrstvw]*.adb $${AWS}/demos;\
	$(CP) demos/*.png demos/cert.pem demos/page*.html $${AWS}/demos;\
	$(CP) demos/aws_*.thtml demos/com*.adb  demos/ws.ini $${AWS}/demos;\
	$(CP) demos/*.wsdl $${AWS}/demos;\
	$(CP) regtests/*.out regtests/*.ad* regtests/*.wsdl $${AWS}/regtests;\
	$(CP) regtests/ChangeLog regtests/*.tmplt $${AWS}/regtests;\
	$(CP) regtests/ftp.thtml regtests/zerolength.html $${AWS}/regtests;\
	$(CP) regtests/makefile regtests/*.ini $${AWS}/regtests;\
	$(CP) docs/aws.texi.tmplt docs/build.adb docs/makefile $${AWS}/docs;\
	$(CP) docs/aws.texi docs/[at]*.html docs/aws.txt $${AWS}/docs;\
	$(CP) docs/aws.info* docs/aws.ps docs/aws.pdf $${AWS}/docs;\
	$(CP) docs/gentexifile docs/TODO docs/openssl.license $${AWS}/docs;\
	$(CP) -r docs/html/* $${AWS}/docs/html;\
	$(CP) win32/*.dll win32/makefile win32/*.txt $${AWS}/win32;\
	$(CP) win32/aws.ico win32/aws.rc win32/wldap32.def $${AWS}/win32;\
	$(CP) ssl/*.ad[sb] ssl/ChangeLog ssl/makefile $${AWS}/ssl;\
	$(CP) include/*.ad[sb] include/makefile $${AWS}/include;\
	$(CP) include/zlib/*.[ch] include/zlib/makefile $${AWS}/include/zlib;\
	$(CP) include/readme.txt $${AWS}/include;\
	$(CP) lib/makefile $${AWS}/lib;\
	$(CP) icons/*.gif $${AWS}/icons;\
	$(CP) soap/*.ad[sb] soap/makefile soap/ChangeLog $${AWS}/soap;\
	$(CP) tools/*.ad[sb] tools/makefile $${AWS}/tools;\
	$(CP) config/*.ad[sb] config/ChangeLog config/makefile $${AWS}/config;\
	$(CP) config/src/*.ad[sb] $${AWS}/config/src;\
	$(CP) xsrc/*.ad[sb] xsrc/README xsrc/ChangeLog $${AWS}/xsrc;\
	$(TAR) cf $${AWS}.tar $${AWS};\
	$(GZIP) -9 $${AWS}.tar;\
	$(RM) -fr $${AWS})

distrib: build_apiref clean_noapiref build_doc build_tarball clean

force:

install: force
	-rm -fr $(INSTALL)/AWS
	$(MKDIR) $(INSTALL)/AWS
	$(MKDIR) $(INSTALL)/AWS/lib
	$(MKDIR) $(INSTALL)/AWS/include
	$(MKDIR) $(INSTALL)/AWS/icons
	$(MKDIR) $(INSTALL)/AWS/images
	$(MKDIR) $(INSTALL)/AWS/templates
	$(MKDIR) $(INSTALL)/AWS/docs
	$(MKDIR) $(INSTALL)/AWS/docs/html
	$(MKDIR) $(INSTALL)/AWS/components
	$(MKDIR) $(INSTALL)/AWS/tools
	$(AR) cr libaws.a src/*.o
	$(AR) cr libaws.a ssl/*.o
	-$(AR) cr libaws.a soap/*.o
	$(CP) src/a*.ad[sb] ssl/*.ad[sb] $(INSTALL)/AWS/include
	-$(CP) soap/*.ad[sb] $(INSTALL)/AWS/include
	$(CP) src/a*.ali $(INSTALL)/AWS/lib
	-$(CP) ssl/*.ali $(INSTALL)/AWS/lib
	-$(CP) soap/*.ali $(INSTALL)/AWS/lib
	$(CHMOD) uog-w $(INSTALL)/AWS/lib/*.ali
	$(MV) libaws.a $(INSTALL)/AWS/lib
	-$(MV) lib/libnosslaws.a $(INSTALL)/AWS/lib
	-$(CP) docs/aws.html $(INSTALL)/AWS/docs
	$(CP) docs/templates_parser.html $(INSTALL)/AWS/docs
	-$(CP) docs/aws.txt $(INSTALL)/AWS/docs
	-$(CP) docs/*.info* $(INSTALL)/AWS/docs
	-$(CP) -r docs/html/* $(INSTALL)/AWS/docs/html
	$(CP) demos/*.thtml $(INSTALL)/AWS/templates
	$(CP) icons/*.gif $(INSTALL)/AWS/icons
	$(CP) demos/aws_*.png $(INSTALL)/AWS/images
	-$(CP) include/*.ad? $(INSTALL)/AWS/components
	-$(CP) include/*.o include/*.ali $(INSTALL)/AWS/components
	-$(CP) tools/awsres${EXEEXT} $(INSTALL)/AWS/tools
	-$(CP) tools/wsdl2aws${EXEEXT} $(INSTALL)/AWS/tools
	-$(CHMOD) -R og+r $(INSTALL)/AWS
ifeq (${OS}, Windows_NT)
	$(CP) lib/lib*.a $(INSTALL)/AWS/lib
	$(CP) lib/*.dll $(INSTALL)/AWS/lib
endif

#############################################################################
# Configuration for GNAT Projet Files

MODULES = config win32 ssl include src tools demos

MODULES_BUILD = ${MODULES:%=%_build}

MODULES_SETUP = ${MODULES:%=%_setup}

MODULES_CLEAN = ${MODULES:%=%_clean}

ifdef XMLADA
PRJ_XMLADA=Installed
else
PRJ_XMLADA=Disabled
endif

ifdef DEBUG
PRJ_BUILD=Debug
else
PRJ_BUILD=Release
endif

GALL_OPTIONS := $(ALL_OPTIONS) \
	PRJ_BUILD="$(PRJ_BUILD)" PRJ_XMLADA="$(PRJ_XMLADA)"

${MODULES_BUILD}: force
	${MAKE} -C ${@:%_build=%} gbuild $(GALL_OPTIONS)

${MODULES_SETUP}: force
	${MAKE} -C ${@:%_setup=%} gsetup $(GALL_OPTIONS)

${MODULES_CLEAN}: force
	${MAKE} -C ${@:%_clean=%} gclean $(GALL_OPTIONS)

gbuild: $(MODULES_BUILD)

gclean: $(MODULES_CLEAN)
	-rm -fr .build

gxmlada:
	echo "project XMLAda is" > xmlada.gpr
	echo " Path := \"$(XMLADA)\";" >> xmlada.gpr
	echo " for Source_Dirs use (Path & \"/include/xmlada\");" >> xmlada.gpr
	echo " for Object_Dir use Path & \"/include/xmlada\";" >> xmlada.gpr
	echo " for Library_Dir use Path & \"/lib\";" >> xmlada.gpr
	echo " LIB_Path := \"-L\" & Path & \"/lib\";" >> xmlada.gpr
	echo "end XMLAda;" >> xmlada.gpr

gsetup: $(MODULES_SETUP)
