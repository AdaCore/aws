
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
PTH := $(PATH)

ifeq (${OS}, Windows_NT)
export ADA_PROJECT_PATH = $(PWD)/.build/projects\;${APP}

AWS_PTH	= $(PWD)/$(BDIR)/lib:$(PWD)/win32:$(PWD)/lib:$(PWD)/$(BDIR)/include/lib
AWS_PTH	:= $(PWD)/$(BDIR)/ssl/lib:$(PWD)/$(BDIR)/ssl/nlib:$(AWS_PTH)
AWS_PTH := $(PWD)/$(BDIR)/include/ai302/lib:$(AWS_PTH)
export PATH = $(AWS_PTH):${PTH}
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
	GZIP="$(GZIP)" TAR="$(TAR)" GNATMAKE="$(GNATMAKE)" \
	DLLTOOL="$(DLLTOOL)" DLL2DEF="$(DLL2DEF)" WINDRES="$(WINDRES)" \
	GNATMAKE_FOR_HOST="$(GNATMAKE_FOR_HOST)" ADASOCKETS="$(ADASOCKETS)" \
	EXTRA_TESTS="$(EXTRA_TESTS)" GCC="$(GCC)" AWK="$(AWK)" CAT="$(CAT)" \
	GCC_FOR_HOST="$(GCC_FOR_HOST)" BDIR="$(BDIR)" INSTALL="$(INSTALL)" \
	SHARED="$(SHARED)" SOEXT="$(SOEXT)"

build_doc:
	echo ""
	echo "=== Build doc"
	${MAKE} -C docs build_doc $(GALL_OPTIONS)
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
	$(CHMOD) a+rx win32/*.dll
	(VERSION=`grep " Version" src/aws.ads | cut -d\" -f2`; \
	AWS=aws-$${VERSION}; \
	$(MKDIR) $${AWS}; \
	$(MKDIR) $${AWS}/src; \
	$(MKDIR) $${AWS}/demos; \
	$(MKDIR) $${AWS}/regtests; \
	$(MKDIR) $${AWS}/gps; \
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
	$(MKDIR) $${AWS}/templates_parser; \
	$(MKDIR) $${AWS}/templates_parser/docs; \
	$(MKDIR) $${AWS}/templates_parser/src; \
	$(MKDIR) $${AWS}/templates_parser/include; \
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
MODULES = config ssl include win32 src tools gps ${EXTRA_MODULES}

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

gai302_internal:
	echo 'with "aws_config";' > $(PRJDIR)/ai302.gpr
	echo 'project AI302 is' >> $(PRJDIR)/ai302.gpr
	echo '   for Source_Dirs use ("../../include/ai302");' \
		>> $(PRJDIR)/ai302.gpr
	echo '   type Build_Type is ("Debug", "Release");' \
		>> $(PRJDIR)/ai302.gpr
	echo '   Build : Build_Type := external ("PRJ_BUILD", "Debug");' \
		>> $(PRJDIR)/ai302.gpr
	echo "   case Build is" >> $(PRJDIR)/ai302.gpr
	echo '      when "Debug" =>' >> $(PRJDIR)/ai302.gpr
	echo '         for Object_Dir use "../../.build/debug/include/ai302/obj";' \
		>> $(PRJDIR)/ai302.gpr
	echo '         for Library_Dir use "../../.build/debug/include/ai302/lib";' \
		>> $(PRJDIR)/ai302.gpr
	echo '      when "Release" =>' >> $(PRJDIR)/ai302.gpr
	echo '         for Object_Dir use "../../.build/release/include/ai302/obj";' \
		>> $(PRJDIR)/ai302.gpr
	echo '         for Library_Dir use "../../.build/release/include/ai302/lib";' \
		>> $(PRJDIR)/ai302.gpr
	echo '   end case;' >> $(PRJDIR)/ai302.gpr
	echo '   for Library_Name use "ai302";' >> $(PRJDIR)/ai302.gpr
	echo '   for Library_Kind use AWS_Config.Lib_Kind;' \
		>> $(PRJDIR)/ai302.gpr

	echo '   package Compiler is' >> $(PRJDIR)/ai302.gpr
	echo '      case Build is' >> $(PRJDIR)/ai302.gpr
	echo '         when "Debug" =>' >> $(PRJDIR)/ai302.gpr
	echo '            for Default_Switches ("Ada") use ("-g", "-gnata");' \
	        >> $(PRJDIR)/ai302.gpr
	echo '         when "Release" =>' >> $(PRJDIR)/ai302.gpr
	echo '            for Default_Switches ("Ada") use ("-O2");' \
	        >> $(PRJDIR)/ai302.gpr
	echo '      end case;' >> $(PRJDIR)/ai302.gpr
	echo '   end Compiler;' >> $(PRJDIR)/ai302.gpr

	echo 'end AI302;' >> $(PRJDIR)/ai302.gpr

gai302_external:
	-$(RM) -f $(PRJDIR)/ai302.gpr

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
	-$(MKDIR) -p templates_parser/obj

CONFGPR	= $(PRJDIR)/aws_config.gpr

setup_config:
	echo 'project AWS_Config is' > $(CONFGPR)
	echo '   type Lib_Type is ("static", "relocatable");' >> $(CONFGPR)
ifeq ($(SHARED), true)
	echo '   Lib_Kind : Lib_Type := "relocatable";' >> $(CONFGPR)
else
	echo '   Lib_Kind : Lib_Type := "static";' >> $(CONFGPR)
endif
	echo '   for Source_Dirs use ();' >> $(CONFGPR)
	echo '   type SOCKLIB_Type is ("GNAT", "AdaSockets", "IPv6");' \
		>> $(CONFGPR)
ifdef ADASOCKETS
	echo '   SOCKLIB : SOCKLIB_Type := "AdaSockets";' \
		>> $(CONFGPR)
else
ifdef IPv6
	echo '   SOCKLIB : SOCKLIB_Type := "IPv6";' >> $(CONFGPR)
else
	echo '   SOCKLIB : SOCKLIB_Type := "GNAT";' >> $(CONFGPR)
endif
endif
	echo '   type OSLIB_Type is ("GNAT", "Win32", "POSIX");' \
		>> $(CONFGPR)
ifeq (${OSLIB}, GNAT)
	echo '   OSLIB : OSLIB_Type := "GNAT";' >> $(CONFGPR)
endif
ifeq (${OSLIB}, Win32)
	echo '   OSLIB : OSLIB_Type := "Win32";' >> $(CONFGPR)
endif
ifeq (${OSLIB}, POSIX)
	echo '   OSLIB : OSLIB_Type := "POSIX";' >> $(CONFGPR)
endif
	echo 'end AWS_Config;' >> $(CONFGPR)

setup: setup_dir $(GEXT_MODULE) $(MODULES_SETUP) setup_config

# Install directories

I_BIN	= $(INSTALL)/bin
I_INC	= $(INSTALL)/include/aws
I_CPN	= $(INSTALL)/include/aws/components
I_AIC	= $(INSTALL)/include/aws/components/ai302
I_LIB	= $(INSTALL)/lib/aws
I_GPR	= $(INSTALL)/lib/gnat
I_AGP	= $(INSTALL)/lib/gnat/aws
I_TPL	= $(INSTALL)/share/examples/aws/templates
I_IMG	= $(INSTALL)/share/examples/aws/images
I_SBN	= $(INSTALL)/share/examples/aws/bin
I_DOC	= $(INSTALL)/share/doc/aws
I_PLG	= $(INSTALL)/share/gps/plug-ins

install_clean:
	$(RM) -fr $(I_INC)
	$(RM) -fr $(I_LIB)
	$(RM) -fr $(I_AGP)
	$(RM) -fr $(INSTALL)/share/examples/aws
	$(RM) -fr $(I_DOC)
	$(RM) -f $(I_GPR)/aws.gpr
	$(RM) -f $(I_GPR)/aws_ssl.gpr

install_dirs: install_clean
	$(MKDIR) $(I_BIN)
	$(MKDIR) $(I_INC)
	$(MKDIR) $(I_CPN)
	$(MKDIR) $(I_AIC)
	$(MKDIR) $(I_LIB)
	$(MKDIR) $(I_DOC)
	$(MKDIR) $(I_GPR)
	$(MKDIR) $(I_AGP)
	$(MKDIR) $(I_TPL)
	$(MKDIR) $(I_IMG)
	$(MKDIR) $(I_SBN)
	$(MKDIR) $(I_PLG)

install: install_dirs
	$(CP) src/a*.ad[sb] ssl/*.ad[sb] $(I_INC)
	$(CP) templates_parser/src/t*.ad[sb] $(I_INC)
	$(CP) config/aws-net-std__* $(I_INC)
	$(CP) config/aws-os_lib__* $(I_INC)
	$(CP) config/templates_parser-* $(I_INC)
ifeq (${AI302},Internal)
	$(CP) include/ai302/*.ad? $(I_AIC)
	$(CP) $(BDIR)/include/ai302/lib/* $(I_LIB)
	$(CP) config/projects/ai302.gpr $(I_AGP)
	$(SED) -e 's,ai302,aws/ai302,g' \
		< config/projects/aws.gpr > $(I_GPR)/aws.gpr
	$(SED) -e 's,ai302,aws/ai302,g' \
		< config/projects/aws_ssl.gpr \
		> $(I_GPR)/aws_ssl.gpr
else
	$(CP) config/projects/aws.gpr $(I_GPR)
	$(CP) config/projects/aws_ssl.gpr $(I_GPR)
endif
ifeq ($(XMLADA),true)
	$(CP) soap/*.ad[sb] $(I_INC)
	$(CP) xsrc/*.ad[sb] $(I_INC)
	$(CP) templates_parser/xsrc/*.ad[sb] $(I_INC)
	$(CP) $(BDIR)/tools/wsdl2aws${EXEEXT} $(I_BIN)
	$(STRIP) $(I_BIN)/wsdl2aws${EXEEXT}
endif
	$(CP) $(BDIR)/lib/* $(I_LIB)
	-$(CP) $(BDIR)/ssl/lib/* $(I_LIB)
	-$(CP) $(BDIR)/ssl/nlib/* $(I_LIB)
	-$(CP) docs/aws.html $(I_DOC)
	-$(CP) -r docs/html $(I_DOC)
	-$(CP) templates_parser/docs/templates_parser.html $(I_DOC)
	-$(CP) templates_parser/docs/templates_parser.info* $(I_DOC)
	-$(CP) docs/aws.txt $(I_DOC)
	-$(CP) docs/*.info* $(I_DOC)
	$(CP) gps/aws_api.xml $(I_PLG)
	$(CP) gps/aws.py $(I_PLG)
	$(CP) gps/aws.xml $(I_PLG)
	$(CP) gps/ada2wsdl.xml $(I_PLG)
	$(CP) gps/wsdl2aws.xml $(I_PLG)
	$(CP) demos/*.thtml $(I_TPL)
	$(CP) demos/wm_login.html $(I_TPL)
	$(CP) demos/aws_*.png $(I_IMG)
	$(CP) include/*.ad? $(I_CPN)
	-$(CP) $(BDIR)/include/lib/* $(I_LIB)
	-$(CP) $(BDIR)/tools/awsres${EXEEXT} $(I_BIN)
	-$(STRIP) $(I_BIN)/awsres${EXEEXT}
	-$(CP) $(BDIR)/tools/hotplug_password${EXEEXT} $(I_BIN)
	-$(STRIP) $(I_BIN)/hotplug_password${EXEEXT}
ifeq (${ASIS},true)
	$(CP) $(BDIR)/tools/ada2wsdl-main${EXEEXT} $(I_BIN)/ada2wsdl${EXEEXT}
	$(STRIP) $(I_BIN)/ada2wsdl${EXEEXT}
endif
ifeq ($(SOCKET),ssl)
	$(CP) $(BDIR)/demos/agent${EXEEXT} $(I_SBN)
	$(STRIP) $(I_SBN)/agent${EXEEXT}
endif
ifeq (${OS}, Windows_NT)
	$(CP) $(BDIR)/win32/lib/* $(I_LIB)
	$(CP) lib/lib*.a $(I_LIB)
	$(CP) win32/*.dll $(I_LIB)
	$(CP) win32/*.dll $(I_LIB)/..
endif
	$(CP) config/projects/aws_components.gpr $(I_AGP)
	$(CP) config/projects/*_lib.gpr $(I_AGP)
	$(CP) config/projects/aws_shared.gpr $(I_AGP)
	$(CP) config/projects/aws_libz.gpr $(I_AGP)
	$(CP) config/projects/aws_ssl_support.gpr $(I_AGP)
	$(CP) $(PRJDIR)/aws_config.gpr $(I_AGP)
# Copy all shared libraries into the main lib directory
ifeq (${SHARED}, true)
	$(CP) lib/*$(SOEXT) $(I_LIB)
	$(CP) $(I_LIB)/*$(SOEXT) $(I_LIB)/..
else
	$(CP) lib/libz.a $(I_LIB)
endif
# Regenerate the SSL project to properly point to the ssl/crypto libraries
	$(MAKE) -C ssl SOCKET=ssl setup_ssl_install
	$(CP) ssl/aws_ssl_shared.gpr $(I_AGP)
	-$(CHMOD) a-w $(I_LIB)/*
	${MAKE} -C web_elements install $(GALL_OPTIONS)
