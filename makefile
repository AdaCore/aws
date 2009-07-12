############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2003-2009, AdaCore                     #
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

#  NOTE: You should not have to change this makefile. Configuration options
#  can be changed in makefile.conf

include makefile.conf

-include makefile.setup

LIBRARY_TYPE = static

ifeq (${OS}, Windows_NT)
EXEEXT	= .exe
SOEXT	= .dll
else
ifeq ($(UNAME), Darwin)
SOEXT   = .dylib
else
ifeq ($(UNAME), HP-UX)
SOEXT	= .sl
else
SOEXT	= .so
endif
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

TEST_MODE	= Separated
#  Can be set to "Grouped" to use a single driver for most tests. This
#  speed-up the non regression.

#############################################################################
#  NO NEED TO CHANGE ANYTHING PAST THIS POINT
#############################################################################

all: build

ALL_OPTIONS	= $(MAKE_OPT) SOCKET="$(SOCKET)" XMLADA="$(XMLADA)" \
	ASIS="$(ASIS)" EXEEXT="$(EXEEXT)" LDAP="$(LDAP)" DEBUG="$(DEBUG)" \
	RM="$(RM)" CP="$(CP)" MV="$(MV)" MKDIR="$(MKDIR)" AR="$(AR)" \
	GREP="$(GREP)" SED="$(SED)" DIFF="$(DIFF)" CHMOD="$(CHMOD)" \
	GZIP="$(GZIP)" TAR="$(TAR)" DLLTOOL="$(DLLTOOL)" DLL2DEF="$(DLL2DEF)" \
	WINDRES="$(WINDRES)" GPRBUILD="$(GPRBUILD)" \
	AWK="$(AWK)" CAT="$(CAT)" GCC="$(GCC)" \
	BDIR="$(BDIR)" prefix="$(prefix)" ENABLE_SHARED="$(ENABLE_SHARED)" \
	SOEXT="$(SOEXT)" BUILD_DOC_SCRIPT="false" GNAT="$(GNAT)" \
	T2A="../../$(BDIR)/static/tools/templates2ada" \
	LIBRARY_TYPE="$(LIBRARY_TYPE)" CJOBS="$(CJOBS)" PYTHON="$(PYTHON)"

build_doc:
	echo ""
	echo "=== Build doc"
	${MAKE} -C docs build_doc $(GALL_OPTIONS)

tp_regtests: run_tp_regtests run_tp_regtests_result

run_tp_regtests:
#  Do not report Templates Parser regression here
#  as we want to continue for passing the AWS tests.
#  The check is done later using run_tp_regtests_result.
	echo ""
	echo "=== Run Templates Parser regression tests"
	-${MAKE} -C templates_parser/regtests test $(GALL_OPTIONS)

run_tp_regtests_result:
	${MAKE} -C templates_parser/regtests test_result $(GALL_OPTIONS)

aws_regtests:
	echo ""
	echo "=== Run regression tests"
	echo ""
	$(MAKE) -C regtests aws_regtests $(ALL_OPTIONS)

run_regtests: run_tp_regtests aws_regtests run_tp_regtests_result

distrib:
	(VERSION=`grep " Version" src/aws.ads | cut -d\" -f2`; \
	AWS=aws-$${VERSION}; \
	$(RM) -f $${AWS}.tar.gz; \
	$(MKDIR) $${AWS}; \
	\
	while read file; \
	do \
		for file in $$file; do \
			dir=$$(dirname $$file); \
			if [ ! -d $${AWS}/$$dir ]; then \
				$(MKDIR) $${AWS}/$$dir; \
			fi; \
			$(CP) $$file $${AWS}/$$file; \
		done; \
	done < ./MANIFEST; \
	\
	$(CP) -fr docs/html/* $${AWS}/docs/html; \
	$(TAR) cf $${AWS}.tar $${AWS};\
	$(GZIP) -9 $${AWS}.tar;\
	$(RM) -fr $${AWS})

force:

#############################################################################
#  Configuration for GNAT Projet Files

ifeq (${DEMOS}, true)
EXTRA_MODULES = demos
endif

MODULES = config win32 include ssl src tools docs gps regtests ${EXTRA_MODULES}

MODULES_SETUP = ${MODULES:%=%_setup}

MODULES_INSTALL = ${MODULES:%=%_install}

MODULES_CHECK = ${MODULES:%=%_check}

#  XML/Ada

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

#  Ldap

ifeq (${LDAP}, true)
PRJ_LDAP=Installed
else
PRJ_LDAP=Disabled
endif

#  ASIS

ifeq (${ASIS}, true)
PRJ_ASIS=Installed
GEXT_MODULE := $(GEXT_MODULE) gasis_setup
else
PRJ_ASIS=Disabled
GEXT_MODULE := $(GEXT_MODULE) gasis_dummy
endif

#  Sockets

ifeq ($(IPv6), true)
PRJ_SOCKLIB=IPv6
else
PRJ_SOCKLIB=GNAT
endif

#  Debug

ifeq ($(DEBUG), true)
PRJ_BUILD=Debug
else
PRJ_BUILD=Release
endif

#  Install directories

I_BIN	= $(prefix)/bin
I_INC	= $(prefix)/include/aws
I_CPN	= $(prefix)/include/aws/components
I_LIB	= $(prefix)/lib/aws
I_GPR	= $(prefix)/lib/gnat
I_AGP	= $(prefix)/lib/gnat/aws
I_TPL	= $(prefix)/share/examples/aws/templates
I_IMG	= $(prefix)/share/examples/aws/images
I_SBN	= $(prefix)/share/examples/aws/bin
I_WEL	= $(prefix)/share/examples/aws/web_elements
I_DOC	= $(prefix)/share/doc/aws
I_PLG	= $(prefix)/share/gps/plug-ins

GALL_OPTIONS := $(ALL_OPTIONS) \
	PRJ_BUILD="$(PRJ_BUILD)" \
	PRJ_XMLADA="$(PRJ_XMLADA)" \
	PRJ_ASIS="$(PRJ_ASIS)" \
	PRJ_SOCKLIB="$(PRJ_SOCKLIB)" \
	PRJ_LDAP="$(PRJ_LDAP)" \
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

${MODULES_SETUP}: force
	${MAKE} -C ${@:%_setup=%} setup $(GALL_OPTIONS)

${MODULES_INSTALL}: force
	${MAKE} -C ${@:%_install=%} install $(GALL_OPTIONS)

${MODULES_CHECK}: force
	${MAKE} -C ${@:%_check=%} check $(GALL_OPTIONS)

GPROPTS = -XPRJ_BUILD=$(PRJ_BUILD) -XPRJ_SOCKLIB=$(PRJ_SOCKLIB) \
		-XPRJ_ASIS=$(PRJ_ASIS) -XPRJ_LDAP=$(PRJ_LDAP) \
		-XPRJ_XMLADA=$(PRJ_XMLADA) -XSOCKET=$(SOCKET)

build:
	$(GPRBUILD) -p -j$(CJOBS) $(GPROPTS) \
		-XLIBRARY_TYPE=static tools/tools.gpr
ifeq (${ENABLE_SHARED}, true)
	$(GPRBUILD) -p -j$(CJOBS) $(GPROPTS) \
		-XLIBRARY_TYPE=relocatable src/src.gpr
endif
ifeq (${DEMOS}, true)
	${MAKE} -C demos $(GALL_OPTIONS) after-build
	$(GPRBUILD) -p -j$(CJOBS) $(GPROPTS) \
		-XLIBRARY_TYPE=static demos/demos.gpr
endif
	$(GPRBUILD) -p -j$(CJOBS) $(GPROPTS) \
		-XLIBRARY_TYPE=static gps/gps_support.gpr
	${MAKE} -C gps $(GALL_OPTIONS) after-build

clean:
	-$(GPRCLEAN) $(GPROPTS) -XLIBRARY_TYPE=static tools/tools.gpr
ifeq (${ENABLE_SHARED}, true)
	-$(GPRCLEAN) $(GPROPTS) -XLIBRARY_TYPE=relocatable src/src.gpr
endif
ifeq (${DEMOS}, true)
	-$(GPRCLEAN) $(GPROPTS) -XLIBRARY_TYPE=static demos/demos.gpr
endif
	-$(GPRCLEAN) $(GPROPTS) -XLIBRARY_TYPE=static gps/gps_support.gpr
	-${MAKE} -C regtests $(GALL_OPTIONS) clean
	-${MAKE} -C docs $(GALL_OPTIONS) clean
	-${MAKE} -C templates_parser clean AWS=AWS
	-${RM} -fr .build
	-${RM} -f makefile.setup

check: $(MODULES_CHECK)

PRJDIR = .build/projects

gasis_dummy:
	echo "project AWS_ASIS is" > $(PRJDIR)/aws_asis.gpr;
	echo "   for Source_Dirs use ();" >> $(PRJDIR)/aws_asis.gpr;
	echo "end AWS_ASIS;" >> $(PRJDIR)/aws_asis.gpr;

gasis_setup:
	echo 'with "asis";' > $(PRJDIR)/aws_asis.gpr
	echo "project AWS_ASIS is" >> $(PRJDIR)/aws_asis.gpr
	echo "   for Source_Dirs use ();" >> $(PRJDIR)/aws_asis.gpr
	echo "end AWS_ASIS;" >> $(PRJDIR)/aws_asis.gpr

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
	echo >> $(CONFGPR)
	echo '   type SOCKLIB_Type is ("GNAT", "IPv6");' >> $(CONFGPR)
	echo 'pragma Source_File_Name' > $(CONFADC)
	echo -n '  (AWS.Net.Std, Body_File_Name => ' >> $(CONFADC)
ifeq ($(IPv6), true)
	echo '   SOCKLIB : SOCKLIB_Type := "IPv6";' >> $(CONFGPR)
	echo '"aws-net-std__ipv6.adb");' >> $(CONFADC)
else
	echo '   SOCKLIB : SOCKLIB_Type := "GNAT";' >> $(CONFGPR)
	echo '"aws-net-std__gnat.adb");' >> $(CONFADC)
endif
	echo >> $(CONFGPR)
	echo '   type SOCKET_Type is ("std", "openssl", "gnutls");' \
	  >> $(CONFGPR)
	echo '   SOCKET : SOCKET_Type := "$(SOCKET)";' >> $(CONFGPR)
	echo >> $(CONFGPR)
	echo '   Default_Library_Type := "'$(DEFAULT_LIBRARY_TYPE)'";' \
		>> $(CONFGPR)
	echo >> $(CONFGPR)
	echo '   type OS_Type is ("Windows_NT", "UNIX");' >> $(CONFGPR)
ifeq ($(OS), Windows_NT)
	echo '   OS : OS_Type := "Windows_NT";' >> $(CONFGPR)
else
	echo '   OS : OS_Type := "UNIX";' >> $(CONFGPR)
endif
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

#  Set up all modules to create all the directories. This way it is possible
#  to build AWS using GPS using any settings.

setup_modules: $(MODULES_SETUP)

setup_final: setup_config
	$(MAKE) -C ssl $(GALL_OPTIONS) setup_config

gen_setup:
	echo "prefix=$(prefix)" > makefile.setup
	echo "DEFAULT_LIBRARY_TYPE=$(DEFAULT_LIBRARY_TYPE)" >> makefile.setup
	echo "ENABLE_SHARED=$(ENABLE_SHARED)" >> makefile.setup
	echo "XMLADA=$(XMLADA)" >> makefile.setup
	echo "ASIS=$(ASIS)" >> makefile.setup
	echo "IPv6=$(IPv6)" >> makefile.setup
	echo "SOCKET=$(SOCKET)" >> makefile.setup
	echo "LDAP=$(LDAP)" >> makefile.setup
	echo "DEBUG=$(DEBUG)" >> makefile.setup
	echo "CJOBS=$(CJOBS)" >> makefile.setup
	echo "DEMOS=$(DEMOS)" >> makefile.setup

setup: gen_setup setup_dir setup_modules setup_final setup_tp $(GEXT_MODULE)

setup_tp:
	$(MAKE) -C templates_parser setup $(GALL_OPTIONS)

install_clean:
	$(RM) -fr $(I_INC)
	$(RM) -fr $(I_LIB)
	$(RM) -fr $(I_AGP)
	$(RM) -fr $(prefix)/share/examples/aws
	$(RM) -fr $(I_DOC)
	$(RM) -f $(I_GPR)/aws.gpr

install_dirs: install_clean
	$(MKDIR) -p $(I_BIN)
	$(MKDIR) -p $(I_INC)
	$(MKDIR) -p $(I_CPN)
	$(MKDIR) -p $(I_LIB)/static
ifeq (${ENABLE_SHARED}, true)
	$(MKDIR) -p $(I_LIB)/relocatable
endif
	$(MKDIR) -p $(I_DOC)
	$(MKDIR) -p $(I_GPR)
	$(MKDIR) -p $(I_AGP)
	$(MKDIR) -p $(I_TPL)
	$(MKDIR) -p $(I_IMG)
	$(MKDIR) -p $(I_SBN)
	$(MKDIR) -p $(I_PLG)
	$(MKDIR) -p $(I_WEL)

install: install_dirs $(MODULES_INSTALL)
	$(CP) templates_parser/src/t*.ad[sb] $(I_INC)
ifeq ($(XMLADA),true)
	$(CP) templates_parser/xsrc/*.ad[sb] $(I_INC)
endif
	-$(CP) templates_parser/docs/templates_parser.html $(I_DOC)
	-$(CP) templates_parser/docs/templates_parser.info* $(I_DOC)
	$(CP) templates_parser/tools/templates.tads $(I_TPL)
	$(CP) $(CONFADC) $(I_LIB)/static
	$(CP) $(CONFGPR) $(I_AGP)
	$(CP) $(PRJDIR)/aws_xmlada.gpr $(I_AGP)
#  Copy all shared libraries into the main bin directory
ifeq (${ENABLE_SHARED}, true)
ifeq ($(OS), Windows_NT)
	$(CP) $(I_LIB)/relocatable/*$(SOEXT) $(I_BIN)
endif
	$(CP) $(CONFADC) $(I_LIB)/relocatable
	-$(CHMOD) a-w $(I_LIB)/relocatable/*
endif
	-$(CHMOD) a-w $(I_LIB)/static/*
