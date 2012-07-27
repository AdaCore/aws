############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2003-2012, AdaCore                     #
#                                                                          #
#  This is free software;  you can redistribute it  and/or modify it       #
#  under terms of the  GNU General Public License as published  by the     #
#  Free Software  Foundation;  either version 3,  or (at your option) any  #
#  later version.  This software is distributed in the hope  that it will  #
#  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty #
#  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     #
#  General Public License for  more details.                               #
#                                                                          #
#  You should have  received  a copy of the GNU General  Public  License   #
#  distributed  with  this  software;   see  file COPYING3.  If not, go    #
#  to http://www.gnu.org/licenses for a complete copy of the license.      #
############################################################################

.SILENT:

BROOTDIR=.build

#  NOTE: You should not have to change this makefile. Configuration options
#  can be changed in makefile.conf

include makefile.conf
#  default setup

-include makefile.setup
#  user setup

include makefile.checks
#  consistency checks

LIBRARY_TYPE = static

ifeq (${OS}, Windows_NT)
EXEEXT	= .exe
SOEXT	= .dll
OS      = Windows_NT
else
ifeq ($(UNAME), Darwin)
SOEXT   = .dylib
OS      = Darwin
else
ifeq ($(UNAME), HP-UX)
SOEXT	= .sl
else
SOEXT	= .so
endif
OS      = UNIX
endif
EXEEXT	=
endif

ifeq ($(DEBUG), true)
MAKE_OPT	=
BDIR		= $(BROOTDIR)/$(PLATFORM)/debug
NBDIR           = $(BROOTDIR)/native/debug
else
MAKE_OPT	= -s
BDIR		= $(BROOTDIR)/$(PLATFORM)/release
NBDIR           = $(BROOTDIR)/native/release
endif

#############################################################################
#  NO NEED TO CHANGE ANYTHING PAST THIS POINT
#############################################################################

all: build

ALL_OPTIONS	= $(MAKE_OPT) SOCKET="$(SOCKET)" XMLADA="$(XMLADA)" \
	ASIS="$(ASIS)" EXEEXT="$(EXEEXT)" LDAP="$(LDAP)" DEBUG="$(DEBUG)" \
	RM="$(RM)" CP="$(CP)" MKDIR="$(MKDIR)" SED="$(SED)" GCC="$(GCC)" \
	CHMOD="$(CHMOD)" WINDRES="$(WINDRES)" GPRBUILD="$(GPRBUILD)" \
	NBDIR="$(NBDIR)" prefix="$(prefix)" ENABLE_SHARED="$(ENABLE_SHARED)" \
	SOEXT="$(SOEXT)" BUILD_DOC_SCRIPT="false" GNAT="$(GNAT)" \
	T2A="../../$(BDIR)/static/tools/templates2ada" \
	LIBRARY_TYPE="$(LIBRARY_TYPE)" PYTHON="$(PYTHON)" \
	BDIR="$(BDIR)" PLATFORM="$(PLATFORM)" ZLIB="$(ZLIB)" OS="$(OS)"

build_doc:
	echo ""
	echo "=== Build doc"
	${MAKE} -C docs build_doc $(GALL_OPTIONS)

run_regtests:
	echo ""
	echo "=== Run regression tests"
	echo ""
	$(MAKE) -C regtests aws_regtests $(ALL_OPTIONS)

force:

#############################################################################
#  Configuration for GNAT Projet Files

MODULES = config win32 include ssl src tools docs gps regtests \
	web_elements demos

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

#  Target PLATFORM is the option for gprbuild --target option, PLATFORM is
#  the name used in the project file.

ifeq ($(strip $(findstring vxworks, $(TARGET))),vxworks)
   PLATFORM=vxworks
else
   PLATFORM=$(TARGET)
endif

#  Install directories

I_BIN	= $(prefix)/bin
I_INC	= $(prefix)/include/aws
TI_INC	= $(prefix)/include/aws/$(PLATFORM)
I_CPN	= $(prefix)/include/aws/components
I_LIB	= $(prefix)/lib/aws/$(PLATFORM)
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
	PLATFORM="$(PLATFORM)" \
	I_BIN="$(I_BIN)" \
	I_INC="$(I_INC)" \
	TI_INC="$(TI_INC)" \
	I_CPN="$(I_CPN)" \
	I_LIB="$(I_LIB)" \
	I_GPR="$(I_GPR)" \
	I_AGP="$(I_AGP)" \
	I_TPL="$(I_TPL)" \
	I_IMG="$(I_IMG)" \
	I_SBN="$(I_SBN)" \
	I_WEL="$(I_WEL)" \
	I_DOC="$(I_DOC)" \
	I_PLG="$(I_PLG)"

${MODULES_SETUP}: force
	${MAKE} -C ${@:%_setup=%} setup $(GALL_OPTIONS)

${MODULES_INSTALL}: force
	${MAKE} -C ${@:%_install=%} install $(GALL_OPTIONS)

${MODULES_CHECK}: force
	${MAKE} -C ${@:%_check=%} check $(GALL_OPTIONS)

GPROPTS = -XPRJ_BUILD=$(PRJ_BUILD) -XPRJ_SOCKLIB=$(PRJ_SOCKLIB) \
		-XPRJ_ASIS=$(PRJ_ASIS) -XPRJ_LDAP=$(PRJ_LDAP) \
		-XPRJ_XMLADA=$(PRJ_XMLADA) -XSOCKET=$(SOCKET) \
		-XPROCESSORS=$(PROCESSORS)

#######################################################################
#  build

build-native:
	$(GPRBUILD) -p $(GPROPTS) -XPLATFORM=native \
		-XLIBRARY_TYPE=static tools/tools.gpr
ifeq (${ENABLE_SHARED}, true)
	$(GPRBUILD) -p $(GPROPTS) -XPLATFORM=native \
		-XLIBRARY_TYPE=relocatable src/src.gpr
endif
	$(GPRBUILD) -p $(GPROPTS) -XPLATFORM=native \
		-XLIBRARY_TYPE=static gps/gps_support.gpr
	${MAKE} -C gps $(GALL_OPTIONS) after-build

build-cross:
	$(GPRBUILD) -p --target=$(TARGET) $(GPROPTS) \
		-XPLATFORM=$(PLATFORM) -XLIBRARY_TYPE=static \
		src/src.gpr

ifeq (${TARGET}, native)
build: build-native
else
build: build-cross
endif

#######################################################################
#  clean

clean-native:
	-$(GPRCLEAN) $(GPROPTS) -XLIBRARY_TYPE=static tools/tools.gpr
ifeq (${ENABLE_SHARED}, true)
	-$(GPRCLEAN) $(GPROPTS) -XLIBRARY_TYPE=relocatable src/src.gpr
endif
	-$(GPRCLEAN) $(GPROPTS) -XLIBRARY_TYPE=static gps/gps_support.gpr

clean-cross:
	-$(GPRCLEAN) $(GPROPTS) --target=$(TARGET) -XLIBRARY_TYPE=static \
		-XPLATFORM=$(PLATFORM) src/src.gpr

ifeq (${TARGET}, native)
clean: clean-native
else
clean: clean-cross
endif
	-${MAKE} -C regtests $(GALL_OPTIONS) clean
	-${MAKE} -C docs $(GALL_OPTIONS) clean
	-${RM} -fr $(BROOTDIR)
	-${RM} -f makefile.setup

check: $(MODULES_CHECK)

PRJDIR = $(BROOTDIR)/projects

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
	echo '   type Boolean_Type is ("true", "false");' >> $(CONFGPR)
	echo '   Zlib_Exists : Boolean_Type := "$(ZLIB)";' >> $(CONFGPR)
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
	echo '   Default_Target := "'$(DEFAULT_TARGET)'";' >> $(CONFGPR)
	echo >> $(CONFGPR)
	echo '   type OS_Type is ("Windows_NT", "UNIX");' >> $(CONFGPR)
ifeq ($(OS), Windows_NT)
	echo '   OS : OS_Type := "Windows_NT";' >> $(CONFGPR)
else
	echo '   OS : OS_Type := "UNIX";' >> $(CONFGPR)
endif
	echo >> $(CONFGPR)
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

gen_setup:
	echo "prefix=$(prefix)" > makefile.setup
	echo "DEFAULT_LIBRARY_TYPE=$(DEFAULT_LIBRARY_TYPE)" >> makefile.setup
	echo "DEFAULT_TARGET=$(DEFAULT_TARGET)" >> makefile.setup
	echo "ENABLE_SHARED=$(ENABLE_SHARED)" >> makefile.setup
	echo "ZLIB=$(ZLIB)" >> makefile.setup
	echo "XMLADA=$(XMLADA)" >> makefile.setup
	echo "ASIS=$(ASIS)" >> makefile.setup
	echo "IPv6=$(IPv6)" >> makefile.setup
	echo "SOCKET=$(SOCKET)" >> makefile.setup
	echo "LDAP=$(LDAP)" >> makefile.setup
	echo "DEBUG=$(DEBUG)" >> makefile.setup
	echo "PROCESSORS=$(PROCESSORS)" >> makefile.setup
	echo "TARGET=$(TARGET)" >> makefile.setup

setup: gen_setup setup_dir setup_modules setup_config setup_tp $(GEXT_MODULE)

setup_tp:
	$(MAKE) -C templates_parser setup $(GALL_OPTIONS)

install_clean:
	$(RM) -fr $(DESTDIR)$(I_INC)/$(PLATFORM)
	$(RM) -fr $(DESTDIR)$(I_LIB)
	$(RM) -fr $(DESTDIR)$(I_AGP)
	$(RM) -fr $(DESTDIR)$(prefix)/share/examples/aws
	$(RM) -fr $(DESTDIR)$(I_DOC)
	$(RM) -f $(DESTDIR)$(I_GPR)/aws.gpr

install_dirs: install_clean
	$(MKDIR) -p $(DESTDIR)$(I_BIN)
	$(MKDIR) -p $(DESTDIR)$(I_INC)
	$(MKDIR) -p $(DESTDIR)$(TI_INC)
	$(MKDIR) -p $(DESTDIR)$(I_CPN)
	$(MKDIR) -p $(DESTDIR)$(I_LIB)/static
ifeq (${ENABLE_SHARED}, true)
	$(MKDIR) -p $(DESTDIR)$(I_LIB)/relocatable
endif
	$(MKDIR) -p $(DESTDIR)$(I_DOC)
	$(MKDIR) -p $(DESTDIR)$(I_GPR)
	$(MKDIR) -p $(DESTDIR)$(I_AGP)
	$(MKDIR) -p $(DESTDIR)$(I_TPL)
	$(MKDIR) -p $(DESTDIR)$(I_IMG)
	$(MKDIR) -p $(DESTDIR)$(I_SBN)
	$(MKDIR) -p $(DESTDIR)$(I_PLG)
	$(MKDIR) -p $(DESTDIR)$(I_WEL)

install: install_dirs $(MODULES_INSTALL)
	$(CP) templates_parser/src/t*.ad[sb] $(DESTDIR)$(I_INC)
ifeq ($(XMLADA),true)
	$(CP) templates_parser/xsrc/*.ad[sb] $(DESTDIR)$(I_INC)
endif
	$(CP) templates_parser/tools/templates.tads $(DESTDIR)$(I_TPL)
	$(CP) $(CONFADC) $(DESTDIR)$(I_LIB)/static
	$(CP) $(CONFGPR) $(DESTDIR)$(I_AGP)
	$(CP) $(PRJDIR)/aws_xmlada.gpr $(DESTDIR)$(I_AGP)
#  Copy all shared libraries into the main bin directory
ifeq (${ENABLE_SHARED}, true)
ifeq ($(OS), Windows_NT)
	$(CP) $(I_LIB)/relocatable/*$(SOEXT) $(DESTDIR)$(I_BIN)
endif
	$(CP) $(CONFADC) $(DESTDIR)$(I_LIB)/relocatable
	-$(CHMOD) a-w $(DESTDIR)$(I_LIB)/relocatable/*
endif
	-$(CHMOD) a-w $(DESTDIR)$(I_LIB)/static/*
