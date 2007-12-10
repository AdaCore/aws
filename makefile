############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                         Copyright (C) 2003-2007                          #
#                                 AdaCore                                  #
#                                                                          #
#  This library is free software; you can redistribute it and/or modify    #
#  it under the terms of the GNU General Public License as published by    #
#  the Free Software Foundation; either version 2 of the License, or (at   #
#  your option) any later version.                                         #
#                                                                          #
#  This library is distributed in the hope that it will be useful, but     #
#  WITHOUT ANY WARRANTY; without even the implied warranty of              #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       #
#  General Public License for more details.                                #
#                                                                          #
#  You should have received a copy of the GNU General Public License       #
#  along with this library; if not, write to the Free Software Foundation, #
#  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          #
#                                                                          #
#  As a special exception, if other files instantiate generics from this   #
#  unit, or you link this unit with other files to produce an executable,  #
#  this  unit  does not  by itself cause  the resulting executable to be   #
#  covered by the GNU General Public License. This exception does not      #
#  however invalidate any other reasons why the executable file  might be  #
#  covered by the  GNU Public License.                                     #
############################################################################

.SILENT:

# NOTE: You should not have to change this makefile. Configuration options can
# be changed in makefile.conf

include makefile.conf

ifeq (${OS}, Windows_NT)
EXEEXT	= .exe
SOEXT	= .dll
else
ifeq ($(UNAME), Darwin)
SOEXT   = .dylib
else
SOEXT	= .so
endif
EXEEXT	=
endif

ifeq ($(DEBUG), true)
MAKE_OPT	=
BDIR		= .build/debug
else
MAKE_OPT	= -s
BDIR		= .build/release
endif

ifeq ($(SHARED), true)
BDIR		:= $(BDIR)/relocatable
LIBRARY_TYPE	= relocatable
else
BDIR		:= $(BDIR)/static
LIBRARY_TYPE	= static
endif

TEST_MODE	= Separated
# Can be set to "Grouped" to use a single driver for most tests. This
# speed-up the non regression.

#############################################################################
# NO NEED TO CHANGE ANYTHING PAST THIS POINT
#############################################################################

APP := $(ADA_PROJECT_PATH)
PTH := $(PATH)

ifeq (${OS}, Windows_NT)
DPWD = $(subst /cygdrive/c/,c:/,$(PWD))
export ADA_PROJECT_PATH = $(DPWD)/.build/projects\;${APP}

APTH = $(PWD)/$(BDIR)/lib/src:$(PWD)/$(BDIR)/lib/win32:$(PWD)/$(BDIR)/lib/ssl
APTH := :$(PWD)/$(BDIR)/lib/include:$(PWD)/$(BDIR)/lib/zlib:$(APTH)
export PATH = $(APTH):${PTH}
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
	GZIP="$(GZIP)" TAR="$(TAR)" DLLTOOL="$(DLLTOOL)" DLL2DEF="$(DLL2DEF)" \
	WINDRES="$(WINDRES)" GNAT_FOR_HOST="$(GNAT_FOR_HOST)" \
	EXTRA_TESTS="$(EXTRA_TESTS)" \
	GCC="$(GCC)" AWK="$(AWK)" CAT="$(CAT)" GCC_FOR_HOST="$(GCC_FOR_HOST)" \
	BDIR="$(BDIR)" INSTALL="$(INSTALL)" SHARED="$(SHARED)" \
	SOEXT="$(SOEXT)" BUILD_DOC_SCRIPT="false" GNAT="$(GNAT)" \
	T2A="../../$(BDIR)/tools/templates2ada" LIBRARY_TYPE="$(LIBRARY_TYPE)"

build_doc:
	echo ""
	echo "=== Build doc"
	${MAKE} -C docs build_doc $(GALL_OPTIONS)

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
	$(MKDIR) $${AWS}/templates_parser/tools; \
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

distrib: build_tarballs

force:

#############################################################################
# Configuration for GNAT Projet Files

EXTRA_MODULES = demos regtests
MODULES = config win32 include ssl src tools docs gps ${EXTRA_MODULES}

MODULES_BUILD = ${MODULES:%=%_build}

MODULES_SETUP = ${MODULES:%=%_setup}

MODULES_INSTALL = ${MODULES:%=%_install}

MODULES_CLEAN = ${MODULES:%=%_clean}

MODULES_CHECK = ${MODULES:%=%_check}

## XML/Ada

ifeq (${XMLADA}, true)
PRJ_XMLADA=Installed
GEXT_MODULE := gxmlada_setup
else
PRJ_XMLADA=Disabled
GEXT_MODULE := gxmlada_dummy
endif

ifndef TP_XMLADA
TP_XMLADA=$(PRJ_XMLADA)
endif

## Ldap

ifeq (${LDAP}, true)
PRJ_LDAP=Installed
else
PRJ_LDAP=Disabled
endif

## ASIS

ifeq (${ASIS}, true)
PRJ_ASIS=Installed
GEXT_MODULE := $(GEXT_MODULE) gasis_clean
else
PRJ_ASIS=Disabled
GEXT_MODULE := $(GEXT_MODULE) gasis_dummy
endif

## Sockets

ifeq ($(IPv6), true)
PRJ_SOCKLIB=IPv6
else
PRJ_SOCKLIB=GNAT
endif

## Debug

ifeq ($(DEBUG), true)
PRJ_BUILD=Debug
else
PRJ_BUILD=Release
endif

# Install directories

I_BIN	= $(INSTALL)/bin
I_INC	= $(INSTALL)/include/aws
I_CPN	= $(INSTALL)/include/aws/components
I_LIB	= $(INSTALL)/lib/aws/$(LIBRARY_TYPE)
I_GPR	= $(INSTALL)/lib/gnat
I_AGP	= $(INSTALL)/lib/gnat/aws
I_TPL	= $(INSTALL)/share/examples/aws/templates
I_IMG	= $(INSTALL)/share/examples/aws/images
I_SBN	= $(INSTALL)/share/examples/aws/bin
I_WEL	= $(INSTALL)/share/examples/aws/web_elements
I_DOC	= $(INSTALL)/share/doc/aws
I_PLG	= $(INSTALL)/share/gps/plug-ins

GALL_OPTIONS := $(ALL_OPTIONS) \
	PRJ_BUILD="$(PRJ_BUILD)" \
	PRJ_XMLADA="$(PRJ_XMLADA)" \
	PRJ_ASIS="$(PRJ_ASIS)" \
	PRJ_SOCKLIB="$(PRJ_SOCKLIB)" \
	PRJ_LDAP="$(PRJ_LDAP)" \
	PRJ_LIBRARY_KIND="$(PRJ_LIBRARY_KIND)" \
	TP_XMLADA="$(TP_XMLADA)" \
	I_BIN="$(I_BIN)" \
	I_INC="$(I_INC)" \
	I_CPN="$(I_CPN)" \
	I_LIB="$(I_LIB)" \
	I_GPR="$(I_GPR)" \
	I_AGP="$(I_AGP)" \
	I_TPL="$(I_TPL)" \
	I_IMG="$(I_IMG)" \
	I_SBN="$(I_SBN)" \
	I_WEL="$(I_WEL)" \
	I_DOC="$(I_DOC)" \
	I_PLG="$(I_PLG)" \
	TEST_MODE="$(TEST_MODE)"

${MODULES_BUILD}: force
	${MAKE} -C ${@:%_build=%} build $(GALL_OPTIONS)

${MODULES_SETUP}: force
	${MAKE} -C ${@:%_setup=%} setup $(GALL_OPTIONS)

${MODULES_INSTALL}: force
	${MAKE} -C ${@:%_install=%} install $(GALL_OPTIONS)

${MODULES_CLEAN}: force
	${MAKE} -C ${@:%_clean=%} clean $(GALL_OPTIONS)

${MODULES_CHECK}: force
	${MAKE} -C ${@:%_check=%} check $(GALL_OPTIONS)

build: $(MODULES_BUILD)

clean: $(MODULES_CLEAN)
	${MAKE} -C templates_parser clean
	-rm -fr .build

check: $(MODULES_CHECK)

PRJDIR = .build/projects

gasis_dummy:
	echo "project ASIS is" > $(PRJDIR)/asis.gpr;
	echo "   for Source_Dirs use ();" >> $(PRJDIR)/asis.gpr;
	echo "end ASIS;" >> $(PRJDIR)/asis.gpr;

gasis_clean:
	-$(RM) -f $(PRJDIR)/asis.gpr

gxmlada_dummy:
	echo "project AWS_XMLADA is" > $(PRJDIR)/aws_xmlada.gpr
	echo "   for Source_Dirs use ();" >> $(PRJDIR)/aws_xmlada.gpr
	echo "end AWS_XMLADA;" >> $(PRJDIR)/aws_xmlada.gpr

gxmlada_setup:
	echo 'with "xmlada";' > $(PRJDIR)/aws_xmlada.gpr
	echo "project AWS_XMLADA is" >> $(PRJDIR)/aws_xmlada.gpr
	echo "   for Source_Dirs use ();" >> $(PRJDIR)/aws_xmlada.gpr
	echo "end AWS_XMLADA;" >> $(PRJDIR)/aws_xmlada.gpr

setup_dir:
	-$(MKDIR) -p $(PRJDIR)
	-$(MKDIR) -p templates_parser/obj

CONFGPR	= $(PRJDIR)/aws_config.gpr
CONFADC	= $(BDIR)/gnat.adc

ifeq (${SOCKET}, ssl)
SOCKET = openssl
endif

SSL_SUFFIX=$(SOCKET)

ifeq (${SOCKET}, std)
SSL_SUFFIX = dummy
endif

setup_config:
	echo 'project AWS_Config is' > $(CONFGPR)
	echo '   for Source_Dirs use ();' >> $(CONFGPR)
	echo '   type SOCKLIB_Type is ("GNAT", "IPv6");' \
		>> $(CONFGPR)
	echo 'pragma Source_File_Name' > $(CONFADC)
	echo -n '  (AWS.Net.Std, Body_File_Name => ' >> $(CONFADC)
ifeq ($(IPv6), true)
	echo '   SOCKLIB : SOCKLIB_Type := "IPv6";' >> $(CONFGPR)
	echo '"aws-net-std__ipv6.adb");' >> $(CONFADC)
else
	echo '   SOCKLIB : SOCKLIB_Type := "GNAT";' >> $(CONFGPR)
	echo '"aws-net-std__gnat.adb");' >> $(CONFADC)
endif
	echo '   type SOCKET_Type is ("std", "openssl", "gnutls");' \
	  >> $(CONFGPR)
	echo '   SOCKET : SOCKET_Type := "$(SOCKET)";' >> $(CONFGPR)
	echo 'end AWS_Config;' >> $(CONFGPR)
	echo 'pragma Source_File_Name' >> $(CONFADC)
	echo '  (SSL.Thin, Spec_File_Name => "ssl-thin__$(SSL_SUFFIX).ads");' \
	  >> $(CONFADC)
	echo 'pragma Source_File_Name' >> $(CONFADC)
	echo -n '  (AWS.Net.SSL,' >> $(CONFADC)
	echo ' Body_File_Name => "aws-net-ssl__$(SSL_SUFFIX).adb");' \
	  >> $(CONFADC)
	echo 'pragma Source_File_Name' >> $(CONFADC)
	echo '  (AWS.Net.SSL.Certificate,' >> $(CONFADC)
	echo -n '   Body_File_Name =>' >> $(CONFADC)
	echo ' "aws-net-ssl-certificate__$(SSL_SUFFIX).adb");' >> $(CONFADC)
	echo 'pragma Source_File_Name' >> $(CONFADC)
	echo '  (Templates_Parser.Configuration,' >> $(CONFADC)
	echo '   Spec_File_Name => "templates_parser-configuration__aws.ads");'\
	  >> $(CONFADC)
	echo 'pragma Source_File_Name' >> $(CONFADC)
	echo '  (Templates_Parser.Input,' >> $(CONFADC)
	echo '   Body_File_Name => "templates_parser-input__aws.adb");'\
	  >> $(CONFADC)

# Set up all modules to create all the directories. This way it is possible
# to build AWS using GPS using any settings.

setup_modules: $(MODULES_SETUP) setup_config

setup_debug_static:
	$(MAKE) DEBUG=true SHARED=false setup_modules

setup_release_static:
	$(MAKE) DEBUG=false SHARED=false setup_modules

setup_debug_relocatable:
	$(MAKE) DEBUG=true SHARED=true setup_modules

setup_release_relocatable:
	$(MAKE) DEBUG=false SHARED=true setup_modules

setup_final:
	$(MAKE) -C ssl $(GALL_OPTIONS) setup_config

setup_debug: setup_debug_static setup_debug_relocatable

setup_release: setup_release_static setup_release_relocatable

setup: setup_dir setup_debug setup_release setup_final setup_tp $(GEXT_MODULE)

setup_tp:
	$(MAKE) -C templates_parser setup $(GALL_OPTIONS)

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
	$(MKDIR) $(I_LIB)
	$(MKDIR) $(I_DOC)
	$(MKDIR) $(I_GPR)
	$(MKDIR) $(I_AGP)
	$(MKDIR) $(I_TPL)
	$(MKDIR) $(I_IMG)
	$(MKDIR) $(I_SBN)
	$(MKDIR) $(I_PLG)
	$(MKDIR) $(I_WEL)

install: install_dirs $(MODULES_INSTALL)
	$(CP) templates_parser/src/t*.ad[sb] $(I_INC)
ifeq ($(XMLADA),true)
	$(CP) templates_parser/xsrc/*.ad[sb] $(I_INC)
endif
	-$(CP) templates_parser/docs/templates_parser.html $(I_DOC)
	-$(CP) templates_parser/docs/templates_parser.info* $(I_DOC)
	$(CP) templates_parser/tools/templates.tads $(I_TPL)
	$(CP) $(CONFADC) $(I_LIB)
	$(CP) $(CONFGPR) $(I_AGP)
	$(CP) $(PRJDIR)/aws_xmlada.gpr $(I_AGP)
# Copy all shared libraries into the main lib directory
ifeq (${SHARED}, true)
	$(CP) $(I_LIB)/*$(SOEXT) $(I_LIB)/../..
endif
	-$(CHMOD) a-w $(I_LIB)/*
