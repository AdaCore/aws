
# $Id$

.SILENT:

# NOTE: You should not have to change this makefile. Configuration options can
# be changed in makefile.conf

include makefile.conf

ifeq (${OS}, Windows_NT)
EXEEXT = .exe
else
EXEEXT =
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
	${MAKE} -C docs build $(GALL_OPTIONS)
	${MAKE} -C templates_parser/docs doc $(GALL_OPTIONS)

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
ifeq (${XMLADA}, true)
	echo "XMLada activated."
else
	echo "XMLada not activated, no SOAP/WSDL support"
endif
ifdef ADASOCKETS
	echo "AdaSockets package in : " $(ADASOCKETS)
else
	echo "Using GNAT.Sockets"
endif

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
	$(MKDIR) $${AWS}/include/ai302; \
	$(MKDIR) $${AWS}/include/zlib; \
	$(MKDIR) $${AWS}/lib; \
	$(MKDIR) $${AWS}/ssl; \
	$(MKDIR) $${AWS}/win32; \
	$(MKDIR) $${AWS}/tools; \
	$(MKDIR) $${AWS}/config; \
	$(MKDIR) $${AWS}/config/src; \
	$(MKDIR) $${AWS}/config/projects; \
	$(MKDIR) $${AWS}/support; \
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

install: force
	-rm -fr $(INSTALL)/AWS
	$(MKDIR) $(INSTALL)/AWS
	$(MKDIR) $(INSTALL)/AWS/obj
	$(MKDIR) $(INSTALL)/AWS/lib
	$(MKDIR) $(INSTALL)/AWS/include
	$(MKDIR) $(INSTALL)/AWS/icons
	$(MKDIR) $(INSTALL)/AWS/images
	$(MKDIR) $(INSTALL)/AWS/templates
	$(MKDIR) $(INSTALL)/AWS/docs
	$(MKDIR) $(INSTALL)/AWS/docs/html
	$(MKDIR) $(INSTALL)/AWS/components
	$(MKDIR) $(INSTALL)/AWS/components/ai302
	$(MKDIR) $(INSTALL)/AWS/tools
	$(MKDIR) $(INSTALL)/AWS/projects
	$(CP) -p src/[at]*.ad[sb] ssl/*.ad[sb] $(INSTALL)/AWS/include
ifeq ($(XMLADA),true)
	$(CP) -p soap/*.ad[sb] $(INSTALL)/AWS/include
	$(CP) -p xsrc/*.ad[sb] $(INSTALL)/AWS/include
endif
	$(CP) -p $(BDIR)/lib/* $(INSTALL)/AWS/lib
	$(CP) -p $(BDIR)/obj/* $(INSTALL)/AWS/obj
	-$(CP) -p $(BDIR)/ssl/lib/* $(INSTALL)/AWS/lib
	-$(CP) -p $(BDIR)/ssl/obj/* $(INSTALL)/AWS/obj
	-$(CP) -p $(BDIR)/ssl/obj/* $(INSTALL)/AWS/lib
	$(CP) lib/libz.a $(INSTALL)/AWS/lib
	-$(CP) docs/aws.html $(INSTALL)/AWS/docs
	-$(CP) docs/aws_api.xml $(INSTALL)/AWS/docs
	$(CP) docs/templates_parser.html $(INSTALL)/AWS/docs
	-$(CP) docs/aws.txt $(INSTALL)/AWS/docs
	-$(CP) docs/*.info* $(INSTALL)/AWS/docs
	-$(CP) -r docs/html/* $(INSTALL)/AWS/docs/html
	$(CP) demos/*.thtml $(INSTALL)/AWS/templates
	$(CP) demos/wm_login.html $(INSTALL)/AWS/templates
	$(CP) demos/aws_*.png $(INSTALL)/AWS/images
	$(CP) -p include/*.ad? $(INSTALL)/AWS/components
ifeq (${AI302},Internal)
	$(CP) -p include/ai302/*.ad? $(INSTALL)/AWS/components/ai302
	$(CP) -p $(BDIR)/include/ai302/* $(INSTALL)/AWS/components/ai302
	$(CP) config/projects/ai302.gpr $(INSTALL)/AWS/components/ai302
	$(SED) -e 's,ai302,\.\./components/ai302/ai302,g' \
		< config/projects/aws.gpr > $(INSTALL)/AWS/projects/aws.gpr
	$(SED) -e 's,ai302,\.\./components/ai302/ai302,g' \
		< config/projects/aws_ssl.gpr \
		> $(INSTALL)/AWS/projects/aws_ssl.gpr
else
	$(CP) config/projects/aws.gpr $(INSTALL)/AWS/projects
	$(CP) config/projects/aws_ssl.gpr $(INSTALL)/AWS/projects
endif
ifneq ($(XMLADA),true)
	-$(CP) $(PRJDIR)/xmlada.gpr $(INSTALL)/AWS/projects
endif
	-$(CP) -p $(BDIR)/include/*.o $(INSTALL)/AWS/components
	-$(CP) -p $(BDIR)/include/*.ali $(INSTALL)/AWS/components
	-$(CP) $(BDIR)/tools/awsres${EXEEXT} $(INSTALL)/AWS/tools
	-$(CP) $(BDIR)/tools/hotplug_password${EXEEXT} $(INSTALL)/AWS/tools
	-$(CP) $(BDIR)/tools/wsdl2aws${EXEEXT} $(INSTALL)/AWS/tools
	-$(CP) $(BDIR)/tools/ada2wsdl-main${EXEEXT} \
		$(INSTALL)/AWS/tools/ada2wsdl${EXEEXT}
ifeq (${OS}, Windows_NT)
	-$(CP) -p $(BDIR)/win32/lib/* $(INSTALL)/AWS/lib
	-$(CP) -p $(BDIR)/win32/obj/* $(INSTALL)/AWS/obj
	$(CP) -p lib/lib*.a $(INSTALL)/AWS/lib
	-$(CP) -p win32/*.dll $(INSTALL)/AWS/lib
endif
	$(CP) config/projects/components.gpr $(INSTALL)/AWS/components
	$(CP) config/projects/*_lib.gpr $(INSTALL)/AWS/projects
# Regenerate the SSL project to properly point to the ssl/crypto libraries
	$(MAKE) -C ssl SOCKET=ssl setup_ssl
	$(CP) ssl/ssl_shared.gpr $(INSTALL)/AWS/projects
	-$(CHMOD) -R og+r $(INSTALL)/AWS
	-$(CHMOD) uog-w $(INSTALL)/AWS/components/*.ali
	-$(CHMOD) uog-w $(INSTALL)/AWS/lib/*.ali
	-$(CHMOD) uog-w $(INSTALL)/AWS/obj/*.ali
# We need to touch the libraries as we have changed the .gpr
	-$(TOUCH) $(INSTALL)/AWS/lib/*.a
	make -C web_elements install $(GALL_OPTIONS)

# install-gnat is used to install AWS into the GNAT standard library
# location. In this case there is no need to set ADA_PROJECT_PATH. This is
# supported starting with GNAT 5.03.

install-gnat: install
	$(SED) -e 's,\.\./,AWS/,g;s,nossl_,AWS/projects/nossl_,g' \
		< $(INSTALL)/AWS/projects/aws.gpr > aws_tmp.gpr
	$(SED) -e 's,\.\./,AWS/,g;s,crypto,AWS/projects/crypto,g' \
		-e 's,ssl_,AWS/projects/ssl_,g' \
		< $(INSTALL)/AWS/projects/aws_ssl.gpr > aws_ssl_tmp.gpr
ifneq ($(XMLADA),true)
	$(SED) -e 's,xmlada,AWS/projects/xmlada,g' aws_tmp.gpr \
		> $(INSTALL)/aws.gpr
	$(SED) -e 's,xmlada,AWS/projects/xmlada,g' aws_ssl_tmp.gpr \
		> $(INSTALL)/aws_ssl.gpr
	$(RM) -f aws_tmp.gpr aws_ssl_tmp.gpr
else
	$(MV) aws_tmp.gpr $(INSTALL)/aws.gpr
	$(MV) aws_ssl_tmp.gpr $(INSTALL)/aws_ssl.gpr
endif
	$(RM) -f $(INSTALL)/AWS/projects/aws.gpr
	$(RM) -f $(INSTALL)/AWS/projects/aws_ssl.gpr

#############################################################################
# Configuration for GNAT Projet Files

EXTRA_MODULES = demos regtests
MODULES = config ssl include src win32 tools templates_parser ${EXTRA_MODULES}

MODULES_BUILD = ${MODULES:%=%_build}

MODULES_SETUP = ${MODULES:%=%_setup}

MODULES_CLEAN = ${MODULES:%=%_clean}

## XML/Ada

ifeq (${XMLADA}, true)
PRJ_XMLADA=Installed
GEXT_MODULE := gxmlada
else
PRJ_XMLADA=Disabled
GEXT_MODULE := gxmlada_dummy
endif

## AdaSockets

ifdef ADASOCKETS
GEXT_MODULE := $(GEXT_MODULE) gadasockets
else
GEXT_MODULE := $(GEXT_MODULE) gsockets_dummy
endif

## ASIS

ifdef ASIS
ifeq (${ASIS}, Installed)
PRJ_ASIS=Installed
GEXT_MODULE := $(GEXT_MODULE) gasis_clean
else
ifeq (${ASIS}, Disabled)
PRJ_ASIS=Disabled
GEXT_MODULE := $(GEXT_MODULE) gasis_clean
else
PRJ_ASIS=Installed
GEXT_MODULE := $(GEXT_MODULE) gasis
endif
endif

else
PRJ_ASIS=Disabled
GEXT_MODULE := $(GEXT_MODULE) gasis_dummy
endif

# AI302

ifeq ($(AI302), Internal)
PRJ_AI302=Internal
GEXT_MODULE := $(GEXT_MODULE) gai302_internal
else
PRJ_AI302=External
GEXT_MODULE := $(GEXT_MODULE) gai302_external
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
	PRJ_ASIS="$(PRJ_ASIS)"

${MODULES_BUILD}: force
	${MAKE} -C ${@:%_build=%} build $(GALL_OPTIONS)

${MODULES_SETUP}: force
	${MAKE} -C ${@:%_setup=%} setup $(GALL_OPTIONS)

${MODULES_CLEAN}: force
	${MAKE} -C ${@:%_clean=%} clean $(GALL_OPTIONS)

build: $(MODULES_BUILD)

clean: $(MODULES_CLEAN)
	-rm -fr .build

PRJDIR = .build/projects

gasis:
	echo "project ASIS is" > $(PRJDIR)/asis.gpr;
	echo "   Path := \"$(ASIS)\";" >> $(PRJDIR)/asis.gpr;
	echo "   for Source_Dirs use (Path);" >> $(PRJDIR)/asis.gpr;
	echo "   for Object_Dir use Path;" >> $(PRJDIR)/asis.gpr;
	echo "   LIB_Path := \"-L\" & Path;" >> $(PRJDIR)/asis.gpr;
	echo "end ASIS;" >> $(PRJDIR)/asis.gpr;

gasis_dummy:
	echo "project ASIS is" > $(PRJDIR)/asis.gpr;
	echo "   for Source_Dirs use ();" >> $(PRJDIR)/asis.gpr;
	echo "   LIB_Path := \"\";" >> $(PRJDIR)/asis.gpr;
	echo "end ASIS;" >> $(PRJDIR)/asis.gpr;

gasis_clean:
	-$(RM) -f $(PRJDIR)/asis.gpr

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

gxmlada:
	-$(RM) -f $(PRJDIR)/xmlada.gpr

gxmlada_dummy:
	echo "project XMLADA is" > $(PRJDIR)/xmlada.gpr
	echo "   for Source_Dirs use ();" >> $(PRJDIR)/xmlada.gpr
	echo "end XMLADA;" >> $(PRJDIR)/xmlada.gpr

gai302_internal:
	echo "project AI302 is" > $(PRJDIR)/ai302.gpr
	echo '   for Source_Dirs use ("../../include/ai302");' \
		>> $(PRJDIR)/ai302.gpr
	echo '   type Build_Type is ("Debug", "Release");' \
		>> $(PRJDIR)/ai302.gpr
	echo '   Build : Build_Type := external ("PRJ_BUILD", "Debug");' \
		>> $(PRJDIR)/ai302.gpr
	echo "   case Build is" >> $(PRJDIR)/ai302.gpr
	echo '      when "Debug" =>' >> $(PRJDIR)/ai302.gpr
	echo '         for Object_Dir use "../../.build/debug/include/ai302";' \
		>> $(PRJDIR)/ai302.gpr
	echo '      when "Release" =>' >> $(PRJDIR)/ai302.gpr
	echo '         for Object_Dir use "../../.build/release/include/ai302";' \
		>> $(PRJDIR)/ai302.gpr
	echo "   end case;" >> $(PRJDIR)/ai302.gpr
	echo "end AI302;" >> $(PRJDIR)/ai302.gpr

gai302_external:
	-$(RM) -f $(PRJDIR)/ai302.gpr

setup_dir:
	-$(MKDIR) -p $(PRJDIR)

setup: setup_dir $(GEXT_MODULE) $(MODULES_SETUP)
