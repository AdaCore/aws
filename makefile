
# $Id$

.SILENT:

# NOTE: You should not have to change this makefile. Configuration options can
# be changed in makefile.conf

include makefile.conf

ifeq (${OS}, Windows_NT)
EXEEXT	= .exe
SOEXT	= .dll
else
SOEXT	= .so
EXEEXT	=
endif

ifdef DEBUG
MAKE_OPT	=
BDIR		= .build/debug
else
MAKE_OPT	= -s
BDIR		= .build/release
endif

#############################################################################
# NO NEED TO CHANGE ANYTHING PAST THIS POINT
#############################################################################

APP := $(ADA_PROJECT_PATH)

ifeq (${OS}, Windows_NT)
export ADA_PROJECT_PATH = $(PWD)/.build/projects\;${APP}
else
export ADA_PROJECT_PATH = $(PWD)/.build/projects:${APP}
endif

all:
	echo ""
	echo "Targets :"
	echo ""
	echo "  Build :"
	echo ""
	echo "    setup:        setup build, must be done before build"
	echo "    build:        build AWS library, tools and demos"
	echo "    build_doc:    build documentation (needs texinfo support)"
	echo ""
	echo "  Support :"
	echo ""
	echo "    clean:        to clean directories"
	echo "    distrib:      to build a tarball distribution"
	echo "    install:      install AWS library"
	echo "    run_regtests: run non regression tests"

EXTRA_TESTS = true

ALL_OPTIONS	= $(MAKE_OPT) SOCKET="$(SOCKET)" XMLADA="$(XMLADA)" \
	ASIS="$(ASIS)" EXEEXT="$(EXEEXT)" LDAP="$(LDAP)" DEBUG="$(DEBUG)" \
	RM="$(RM)" CP="$(CP)" MV="$(MV)" MKDIR="$(MKDIR)" AR="$(AR)" \
	GREP="$(GREP)" SED="$(SED)" DIFF="$(DIFF)" CHMOD="$(CHMOD)" \
	GZIP="$(GZIP)" TAR="$(TAR)" GNATMAKE="$(GNATMAKE)" TOUCH="$(TOUCH)" \
	DLLTOOL="$(DLLTOOL)" DLL2DEF="$(DLL2DEF)" WINDRES="$(WINDRES)" \
	GNATMAKE_FOR_HOST="$(GNATMAKE_FOR_HOST)" ADASOCKETS="$(ADASOCKETS)" \
	EXTRA_TESTS="$(EXTRA_TESTS)" GCC="$(GCC)" AWK="$(AWK)" CAT="$(CAT)" \
	GCC_FOR_HOST="$(GCC_FOR_HOST)" BDIR="$(BDIR)" INSTALL="$(INSTALL)"

build_doc:
	echo ""
	echo "=== Build doc"
	${MAKE} -C docs build $(GALL_OPTIONS)
	${MAKE} -C templates_parser doc $(GALL_OPTIONS)

build_apiref:
	echo ""
	echo "=== Build API References"
	${MAKE} -s -C docs apiref $(ALL_OPTIONS)

tp_regtests:
	echo ""
	echo "=== Run Templates Parser regression tests"
	${MAKE} -C templates_parser/regtests test $(GALL_OPTIONS)

aws_regtests:
	echo ""
	echo "=== Run regression tests"
	${MAKE} -C regtests run $(GALL_OPTIONS) GDB_REGTESTS="$(GDB_REGTESTS)"

run_regtests: tp_regtests aws_regtests

common_tarball:
	$(CHMOD) uog+rx win32/*.dll
	(VERSION=`grep " Version" src/aws.ads | cut -d\" -f2`; \
	AWS=aws-$${VERSION}; \
	$(MKDIR) $${AWS}; \
	$(MKDIR) $${AWS}/src; \
	$(MKDIR) $${AWS}/demos; \
	$(MKDIR) $${AWS}/regtests; \
	$(MKDIR) $${AWS}/docs; \
	$(MKDIR) $${AWS}/docs/html; \
	$(MKDIR) $${AWS}/icons; \
	$(MKDIR) $${AWS}/include; \
	$(MKDIR) $${AWS}/include/zlib; \
	$(MKDIR) $${AWS}/lib; \
	$(MKDIR) $${AWS}/ssl; \
	$(MKDIR) $${AWS}/win32; \
	$(MKDIR) $${AWS}/tools; \
	$(MKDIR) $${AWS}/config; \
	$(MKDIR) $${AWS}/config/src; \
	$(MKDIR) $${AWS}/config/projects; \
	$(MKDIR) $${AWS}/support; \
	$(MKDIR) $${AWS}/templates_parser; \
	$(MKDIR) $${AWS}/templates_parser/docs; \
	$(MKDIR) $${AWS}/templates_parser/src; \
	$(MKDIR) $${AWS}/templates_parser/xsrc; \
	$(MKDIR) $${AWS}/templates_parser/regtests; \
	$(MKDIR) $${AWS}/templates_parser/regtests/dir; \
	$(MKDIR) $${AWS}/templates_parser/regtests/dir/subdir; \
	$(MKDIR) $${AWS}/web_elements; \
	$(MKDIR) $${AWS}/web_elements/icons; \
	$(MKDIR) $${AWS}/web_elements/javascripts; \
	$(MKDIR) $${AWS}/web_elements/menu_css; \
	$(MKDIR) $${AWS}/web_elements/notebook; \
	$(MKDIR) $${AWS}/web_elements/rounded_boxes; \
	\
	for file in \
           `$(AWK) '$$1!="--" && $$1!="" {print $$0} \
		    $$2=="FULL" {exit}' MANIFEST`; \
        do \
		$(CP) $$file $${AWS}/$$file; \
	done;\
	\
	$(CP) -r docs/html/* $${AWS}/docs/html)

build_tarball:
	(VERSION=`grep " Version" src/aws.ads | cut -d\" -f2`; \
	AWS=aws-$${VERSION}; \
	$(RM) -f $${AWS}.tar.gz; \
	$(MKDIR) $${AWS}/xsrc; \
	$(MKDIR) $${AWS}/soap; \
	\
	for file in \
           `$(AWK) 'BEGIN{p=0} \
		    p==1 && $$1!="--" && $$1!="" {print $$0} \
		    $$2=="FULL"{p=1}' MANIFEST`; \
        do \
		$(CP) $$file $${AWS}/$$file; \
	done;\
	\
	$(TAR) cf $${AWS}.tar $${AWS};\
	$(GZIP) -9 $${AWS}.tar;\
	$(RM) -fr $${AWS})

build_http_tarball:
	(VERSION=`grep " Version" src/aws.ads | cut -d\" -f2`; \
	AWS=aws-http-$${VERSION}; \
	$(MV) aws-$${VERSION} $${AWS}; \
	$(RM) -f $${AWS}.tar.gz; \
	$(SED) 's/$$(LIBSSL) $$(LIBCRYPTO)//' \
	   win32/makefile > $${AWS}/win32/makefile;\
	$(SED) 's/sha.ads sha-process_data.adb sha-strings.adb//' \
	   include/makefile > $${AWS}/include/makefile;\
	$(TAR) cf $${AWS}.tar $${AWS};\
	$(GZIP) -9 $${AWS}.tar;\
	$(CP) win32/makefile $${AWS}/win32/makefile;\
	$(CP) include/makefile $${AWS}/include/makefile;\
	$(MV) $${AWS} aws-$${VERSION})

build_tarballs: common_tarball build_http_tarball build_tarball

distrib: build_apiref build_doc build_tarballs

force:

#############################################################################
# Configuration for GNAT Projet Files

EXTRA_MODULES = demos regtests
MODULES = config ssl include src win32 tools ${EXTRA_MODULES}

MODULES_BUILD = ${MODULES:%=%_build}

MODULES_SETUP = ${MODULES:%=%_setup}

MODULES_CLEAN = ${MODULES:%=%_clean}

## XML/Ada

ifeq (${XMLADA}, true)
PRJ_XMLADA=Installed
GEXT_MODULE := gxmlada_clean
else
PRJ_XMLADA=Disabled
GEXT_MODULE := gxmlada_dummy
endif

ifndef TP_XMLADA
TP_XMLADA="$(PRJ_XMLADA)"
endif

## ASIS

ifeq (${ASIS}, true)
PRJ_ASIS=Installed
GEXT_MODULE := $(GEXT_MODULE) gasis_clean
else
PRJ_ASIS=Disabled
GEXT_MODULE := $(GEXT_MODULE) gasis_dummy
endif

## AdaSockets

ifdef ADASOCKETS
GEXT_MODULE := $(GEXT_MODULE) gadasockets
PRJ_SOCKLIB=AdaSockets
else
GEXT_MODULE := $(GEXT_MODULE) gsockets_dummy
ifdef IPv6
PRJ_SOCKLIB=IPv6
else
PRJ_SOCKLIB=GNAT
endif
endif

## Debug

ifdef DEBUG
PRJ_BUILD=Debug
else
PRJ_BUILD=Release
endif

GALL_OPTIONS := $(ALL_OPTIONS) \
	PRJ_BUILD="$(PRJ_BUILD)" \
	PRJ_XMLADA="$(PRJ_XMLADA)" \
	PRJ_ASIS="$(PRJ_ASIS)" \
	PRJ_SOCKLIB="$(PRJ_SOCKLIB)" \
	PRJ_OSLIB="$(OSLIB)" \
	TP_XMLADA="$(TP_XMLADA)"

${MODULES_BUILD}: force
	${MAKE} -C ${@:%_build=%} build $(GALL_OPTIONS)

${MODULES_SETUP}: force
	${MAKE} -C ${@:%_setup=%} setup $(GALL_OPTIONS)

${MODULES_CLEAN}: force
	${MAKE} -C ${@:%_clean=%} clean $(GALL_OPTIONS)

build: $(MODULES_BUILD)

clean: $(MODULES_CLEAN)
	${MAKE} -C templates_parser clean
	-rm -fr .build

PRJDIR = .build/projects

gasis_dummy:
	echo "project ASIS is" > $(PRJDIR)/asis.gpr;
	echo "   for Source_Dirs use ();" >> $(PRJDIR)/asis.gpr;
	echo "end ASIS;" >> $(PRJDIR)/asis.gpr;

gasis_clean:
	-$(RM) -f $(PRJDIR)/asis.gpr

gxmlada_dummy:
	echo "project XMLADA is" > $(PRJDIR)/xmlada.gpr
	echo "   for Source_Dirs use ();" >> $(PRJDIR)/xmlada.gpr
	echo "end XMLADA;" >> $(PRJDIR)/xmlada.gpr

gxmlada_clean:
	-$(RM) -f $(PRJDIR)/xmlada.gpr

gadasockets:
	echo "project Sockets is" > $(PRJDIR)/sockets.gpr
	echo "   Path := \"$(ADASOCKETS)/lib\";" >> $(PRJDIR)/sockets.gpr
	echo "   Src_Path := Path & \"/adasockets\";" >> $(PRJDIR)/sockets.gpr
	echo "   for Source_Dirs use (Src_Path);" >> $(PRJDIR)/sockets.gpr
	echo "   for Object_Dir use Src_Path;" >> $(PRJDIR)/sockets.gpr
	echo "   for Library_Name use \"adasockets\";" >> $(PRJDIR)/sockets.gpr
	echo "   for Library_Kind use \"static\";" >> $(PRJDIR)/sockets.gpr
	echo "   for Library_Dir use Path;" >> $(PRJDIR)/sockets.gpr
	echo "end Sockets;" >> $(PRJDIR)/sockets.gpr

gsockets_dummy:
	echo "project Sockets is" > $(PRJDIR)/sockets.gpr
	echo "   for Source_Dirs use ();" >> $(PRJDIR)/sockets.gpr
	echo "end Sockets;" >> $(PRJDIR)/sockets.gpr

gposix:
	echo "project POSIX is" > $(PRJDIR)/posix.gpr
	echo "   Path := \"$(POSIX)\";" >> $(PRJDIR)/posix.gpr
	echo "   for Source_Dirs use (Path);" >> $(PRJDIR)/posix.gpr
	echo "   for Object_Dir use Path;" >> $(PRJDIR)/posix.gpr
	echo "end POSIX;" >> $(PRJDIR)/posix.gpr

gposix_dummy:
	echo "project POSIX is" > $(PRJDIR)/posix.gpr
	echo "   for Source_Dirs use ();" >> $(PRJDIR)/posix.gpr
	echo "end POSIX;" >> $(PRJDIR)/posix.gpr

setup_dir:
	-$(MKDIR) -p $(PRJDIR)

setup_config:
	echo 'project Config is' > $(PRJDIR)/config.gpr
	echo '   Lib_Kind := "static";' >> $(PRJDIR)/config.gpr
	echo '   for Source_Dirs use ();' >> $(PRJDIR)/config.gpr
	echo '   type SOCKLIB_Type is ("GNAT", "AdaSockets", "IPv6");' \
		>> $(PRJDIR)/config.gpr
ifdef ADASOCKETS
	echo '   SOCKLIB : SOCKLIB_Type := "AdaSockets";' \
		>> $(PRJDIR)/config.gpr
else
ifdef IPv6
	echo '   SOCKLIB : SOCKLIB_Type := "IPv6";' >> $(PRJDIR)/config.gpr
else
	echo '   SOCKLIB : SOCKLIB_Type := "GNAT";' >> $(PRJDIR)/config.gpr
endif
endif
	echo '   type OSLIB_Type is ("GNAT", "Win32", "POSIX");' \
		>> $(PRJDIR)/config.gpr
ifeq (${OSLIB}, GNAT)
	echo '   OSLIB : OSLIB_Type := "GNAT";' >> $(PRJDIR)/config.gpr
endif
ifeq (${OSLIB}, Win32)
	echo '   OSLIB : OSLIB_Type := "Win32";' >> $(PRJDIR)/config.gpr
endif
ifeq (${OSLIB}, POSIX)
	echo '   OSLIB : OSLIB_Type := "POSIX";' >> $(PRJDIR)/config.gpr
endif
	echo 'end Config;' >> $(PRJDIR)/config.gpr

setup: setup_dir $(GEXT_MODULE) $(MODULES_SETUP) setup_config

# Install directories

I_BIN	= $(INSTALL)/bin
I_INC	= $(INSTALL)/include/aws
I_CPN	= $(INSTALL)/include/aws/components
I_LIB	= $(INSTALL)/lib/aws
I_DOC	= $(INSTALL)/doc/aws
I_GPR	= $(INSTALL)/lib/gnat
I_AGP	= $(INSTALL)/lib/gnat/aws
I_TPL	= $(INSTALL)/share/aws/templates
I_IMG	= $(INSTALL)/share/aws/images
I_SBN	= $(INSTALL)/share/aws/bin

install_dirs: force
	$(MKDIR) $(I_BIN)
	$(MKDIR) $(I_INC)
	$(MKDIR) $(I_CPN)
	$(MKDIR) $(I_LIB)
	$(MKDIR) $(I_DOC)
	$(MKDIR) $(I_GPR)
	$(MKDIR) $(I_AGP)
	$(MKDIR) $(I_TPL)
	$(MKDIR) $(I_IMG)
	$(MKDIR) $(I_SBN)

install: install_dirs
	$(CP) -p src/a*.ad[sb] ssl/*.ad[sb] $(I_INC)
	$(CP) -p templates_parser/src/t*.ad[sb] $(I_INC)
	$(CP) -p config/aws-net-std__* $(I_INC)
	$(CP) -p config/aws-os_lib__* $(I_INC)
	$(CP) -p config/templates_parser-* $(I_INC)
ifeq ($(XMLADA),true)
	$(CP) -p soap/*.ad[sb] $(I_INC)
	$(CP) -p xsrc/*.ad[sb] $(I_INC)
	$(CP) -p templates_parser/xsrc/*.ad[sb] $(I_INC)
endif
	$(CP) -p $(BDIR)/lib/* $(I_LIB)
	-$(CP) -p $(BDIR)/ssl/lib/* $(I_LIB)
	-$(CP) -p $(BDIR)/ssl/obj/*.ali $(I_LIB)
	$(CP) lib/libz.a $(I_LIB)
	-$(CP) docs/aws.html $(I_DOC)
	-$(CP) docs/aws_api.xml $(I_DOC)
	-$(CP) docs/gps_index.xml $(I_DOC)
	-$(CP) templates_parser/docs/templates_parser.html $(I_DOC)
	-$(CP) templates_parser/docs/templates_parser.info* $(I_DOC)
	-$(CP) docs/aws.txt $(I_DOC)
	-$(CP) docs/*.info* $(I_DOC)
	-$(CP) -r docs/html $(I_DOC)
	$(CP) demos/*.thtml $(I_TPL)
	$(CP) demos/wm_login.html $(I_TPL)
	$(CP) demos/aws_*.png $(I_IMG)
	$(CP) -p include/*.ad? $(I_CPN)
	-$(CP) -p $(BDIR)/include/*.o $(I_CPN)
	-$(CP) -p $(BDIR)/include/*.ali $(I_CPN)
	-$(CP) $(BDIR)/tools/awsres${EXEEXT} $(I_BIN)
	-$(STRIP) $(I_BIN)/awsres${EXEEXT}
	-$(CP) $(BDIR)/tools/hotplug_password${EXEEXT} $(I_BIN)
	-$(STRIP) $(I_BIN)/hotplug_password${EXEEXT}
	-$(CP) $(BDIR)/tools/wsdl2aws${EXEEXT} $(I_BIN)
	-$(STRIP) $(I_BIN)/wsdl2aws${EXEEXT}
	-$(CP) $(BDIR)/tools/ada2wsdl-main${EXEEXT} $(I_BIN)/ada2wsdl${EXEEXT}
	-$(STRIP) $(I_BIN)/ada2wsdl${EXEEXT}
	-$(CP) $(BDIR)/demos/agent${EXEEXT} $(I_SBN)
	-$(STRIP) $(I_SBN)/agent${EXEEXT}
ifeq (${OS}, Windows_NT)
	-$(CP) -p $(BDIR)/win32/lib/* $(I_LIB)
	$(CP) -p lib/lib*.a $(I_LIB)
	-$(CP) -p win32/*.dll $(I_LIB)
endif
	$(CP) config/projects/components.gpr $(I_CPN)
	$(CP) config/projects/aws.gpr $(I_GPR)
	$(CP) config/projects/aws_ssl.gpr $(I_GPR)
	$(CP) config/projects/*_lib.gpr $(I_AGP)
	$(CP) config/projects/shared.gpr $(I_AGP)
	$(CP) $(PRJDIR)/config.gpr $(I_AGP)
# Regenerate the SSL project to properly point to the ssl/crypto libraries
	$(MAKE) -C ssl SOCKET=ssl setup_ssl
	$(CP) ssl/ssl_shared.gpr $(I_AGP)
	-$(CHMOD) -R og+r $(INSTALL)
	-$(CHMOD) uog-w $(I_LIB)
	-$(CHMOD) uog-w $(I_CPN)
# We need to touch the libraries as we have changed the .gpr
	-$(TOUCH) $(I_LIB)/*.a
	-$(TOUCH) $(I_LIB)/*$(SOEXT)
	${MAKE} -C web_elements install $(GALL_OPTIONS)
