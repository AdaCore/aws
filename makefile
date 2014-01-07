############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2003-2014, AdaCore                     #
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

include makefile.checks
#  consistency checks

ifeq (${PRJ_TARGET}, Windows_NT)
EXEEXT	= .exe
OS      = Windows_NT
else
ifeq ($(PRJ_TARGET), Darwin)
OS      = Darwin
else
OS      = UNIX
endif
EXEEXT	=
endif

ifeq ($(DEBUG), true)
MAKE_OPT	=
BDIR		= $(BROOTDIR)/$(TARGET)/debug
else
MAKE_OPT	= -s
BDIR		= $(BROOTDIR)/$(TARGET)/release
endif

#############################################################################
#  NO NEED TO CHANGE ANYTHING PAST THIS POINT
#############################################################################

all: build

ALL_OPTIONS	= $(MAKE_OPT) SOCKET="$(SOCKET)" XMLADA="$(XMLADA)" \
	ASIS="$(ASIS)" EXEEXT="$(EXEEXT)" LDAP="$(LDAP)" DEBUG="$(DEBUG)" \
	RM="$(RM)" CP="$(CP)" MKDIR="$(MKDIR)" SED="$(SED)" GCC="$(GCC)" \
	GPRBUILD="$(GPRBUILD)" ZLIB="$(ZLIB)" BDIR="$(BDIR)" \
	prefix="$(prefix)" ENABLE_SHARED="$(ENABLE_SHARED)" \
	SOEXT="$(SOEXT)" BUILD_DOC_SCRIPT="false" GNAT="$(GNAT)" \
	T2A="../../$(BDIR)/static/tools/templates2ada" \
	LIBRARY_TYPE="$(LIBRARY_TYPE)" PYTHON="$(PYTHON)" \
	TARGET="$(TARGET)" IS_CROSS=$(IS_CROSS) GPRINSTALL="$(GPRINSTALL)"

build-doc:
	echo ""
	echo "=== Build doc"
	${MAKE} -C docs html
	${MAKE} -C templates_parser/docs html

run_regtests:
	echo ""
	echo "=== Run regression tests"
	echo ""
	$(MAKE) -C regtests aws_regtests $(ALL_OPTIONS)

force:

#############################################################################
#  Configuration for GNAT Projet Files

MODULES = config win32 include ssl src tools gps regtests web_elements demos

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

ifeq ($(IS_CROSS), true)
TPREFIX=$(DESTDIR)$(prefix)/$(TARGET)
else
TPREFIX=$(DESTDIR)$(prefix)
endif

#  Install directories

I_INC	= $(TPREFIX)/include/aws
I_LIB	= $(TPREFIX)/lib/aws
I_TPL	= $(TPREFIX)/share/examples/aws/templates
I_IMG	= $(TPREFIX)/share/examples/aws/images
I_WEL	= $(TPREFIX)/share/examples/aws/web_elements
I_DOC	= $(TPREFIX)/share/doc/aws
I_PLG	= $(TPREFIX)/share/gps/plug-ins

GALL_OPTIONS := $(ALL_OPTIONS) \
	PRJ_BUILD="$(PRJ_BUILD)" \
	PRJ_XMLADA="$(PRJ_XMLADA)" \
	PRJ_ASIS="$(PRJ_ASIS)" \
	PRJ_SOCKLIB="$(PRJ_SOCKLIB)" \
	PRJ_LDAP="$(PRJ_LDAP)" \
	PRJ_TARGET="$(PRJ_TARGET)" \
	TP_XMLADA="$(TP_XMLADA)" \
	I_INC="$(I_INC)" \
	I_LIB="$(I_LIB)" \
	I_TPL="$(I_TPL)" \
	I_IMG="$(I_IMG)" \
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
		-XPRJ_XMLADA=$(PRJ_XMLADA) -XTARGET=$(TARGET) \
		-XPROCESSORS=$(PROCESSORS) -XSOCKET=$(SOCKET) \
		-XPRJ_TARGET=$(PRJ_TARGET)

#######################################################################
#  build

build-native:
	$(GPRBUILD) -p $(GPROPTS) -XLIBRARY_TYPE=static tools/tools.gpr
ifeq (${ENABLE_SHARED}, true)
	$(GPRBUILD) -p $(GPROPTS) -XLIBRARY_TYPE=relocatable aws.gpr
endif
	$(GPRBUILD) -p $(GPROPTS) -XLIBRARY_TYPE=static gps/gps_support.gpr
	${MAKE} -C gps $(GALL_OPTIONS) after-build

build-cross:
	$(GPRBUILD) -p --target=$(TARGET) $(GPROPTS) \
		-XLIBRARY_TYPE=static tools/tools.gpr
ifeq (${ENABLE_SHARED}, true)
	$(GPRBUILD) -p --target=$(TARGET) $(GPROPTS) \
		-XLIBRARY_TYPE=relocatable aws.gpr
endif

ifeq (${IS_CROSS}, true)
build: build-cross
else
build: build-native
endif

#######################################################################
#  clean

clean-native:
	-$(GPRCLEAN) $(GPROPTS) -XLIBRARY_TYPE=static tools/tools.gpr
ifeq (${ENABLE_SHARED}, true)
	-$(GPRCLEAN) $(GPROPTS) -XLIBRARY_TYPE=relocatable aws.gpr
endif
	-$(GPRCLEAN) $(GPROPTS) -XLIBRARY_TYPE=static gps/gps_support.gpr

clean-cross:
	-$(GPRCLEAN) $(GPROPTS) --target=$(TARGET) \
		-XLIBRARY_TYPE=static aws.gpr
ifeq (${ENABLE_SHARED}, true)
	-$(GPRCLEAN) $(GPROPTS) --target=$(TARGET) \
		-XLIBRARY_TYPE=relocatable aws.gpr
endif

ifeq (${IS_CROSS}, true)
clean: clean-cross
else
clean: clean-native
endif
	-${MAKE} -C regtests $(GALL_OPTIONS) clean
	-${MAKE} -C docs $(GALL_OPTIONS) clean
	-${RM} -fr $(BROOTDIR)
	-${RM} -f makefile.setup

#######################################################################
#  install

install-clean:
ifneq (,$(wildcard $(TPREFIX)/share/gpr/manifests/aws))
	-$(GPRINSTALL) $(GPROPTS) --uninstall \
		--prefix=$(TPREFIX) aws.gpr
	-$(GPRINSTALL) $(GPROPTS) --uninstall \
		--prefix=$(TPREFIX) tools/tools.gpr
endif
	$(RM) -fr $(TPREFIX)/share/examples/aws
	$(RM) -fr $(TPREFIX)/share/doc/aws

install-dirs:
	$(MKDIR) -p $(I_DOC)
	$(MKDIR) -p $(I_TPL)
	$(MKDIR) -p $(I_IMG)
	$(MKDIR) -p $(I_PLG)
	$(MKDIR) -p $(I_WEL)

install-native:
	$(GPRINSTALL) $(GPROPTS) -p -f --prefix=$(TPREFIX) \
		-XLIBRARY_TYPE=$(DEFAULT_LIBRARY_TYPE) aws.gpr
	$(GPRINSTALL) $(GPROPTS) -p -f --prefix=$(TPREFIX) \
		-XLIBRARY_TYPE=static --mode=usage tools/tools.gpr
ifeq (${ENABLE_SHARED}, true)
	$(GPRINSTALL) $(GPROPTS) -p -f --prefix=$(TPREFIX) \
		-XLIBRARY_TYPE=$(OTHER_LIBRARY_TYPE) \
		--build-name=$(OTHER_LIBRARY_TYPE) aws.gpr
endif

install-cross:
	$(GPRINSTALL) $(GPROPTS) -p -f --prefix=$(TPREFIX) \
		--target=$(TARGET) \
		-XLIBRARY_TYPE=$(DEFAULT_LIBRARY_TYPE) aws.gpr
	$(GPRINSTALL) $(GPROPTS) -p -f --prefix=$(TPREFIX) --mode=usage \
		--target=$(TARGET)  -XLIBRARY_TYPE=static tools/tools.gpr
ifeq (${ENABLE_SHARED}, true)
	$(GPRINSTALL) $(GPROPTS) -p -f --prefix=$(TPREFIX) \
		--target=$(TARGET) -XLIBRARY_TYPE=$(OTHER_LIBRARY_TYPE) \
		--build-name=$(OTHER_LIBRARY_TYPE) aws.gpr
endif

ifeq (${IS_CROSS}, true)
install: install-clean install-dirs install-cross $(MODULES_INSTALL)
else
install: install-clean install-dirs install-native $(MODULES_INSTALL)
endif
	$(CP) templates_parser/tools/templates.tads $(I_TPL)

#######################################################################

check: $(MODULES_CHECK)

PRJDIR = $(BROOTDIR)/projects

gasis_dummy:
	echo "abstract project AWS_ASIS is" > $(PRJDIR)/aws_asis.gpr;
	echo "   for Source_Dirs use ();" >> $(PRJDIR)/aws_asis.gpr;
	echo "end AWS_ASIS;" >> $(PRJDIR)/aws_asis.gpr;

gasis_setup:
	echo 'with "asis";' > $(PRJDIR)/aws_asis.gpr
	echo "abstract project AWS_ASIS is" >> $(PRJDIR)/aws_asis.gpr
	echo "   for Source_Dirs use ();" >> $(PRJDIR)/aws_asis.gpr
	echo "end AWS_ASIS;" >> $(PRJDIR)/aws_asis.gpr

gxmlada_dummy:
	echo "abstract project AWS_XMLADA is" > $(PRJDIR)/aws_xmlada.gpr
	echo "   for Source_Dirs use ();" >> $(PRJDIR)/aws_xmlada.gpr
	echo "end AWS_XMLADA;" >> $(PRJDIR)/aws_xmlada.gpr

gxmlada_setup:
	echo 'with "xmlada";' > $(PRJDIR)/aws_xmlada.gpr
	echo "abstract project AWS_XMLADA is" >> $(PRJDIR)/aws_xmlada.gpr
	echo "   for Source_Dirs use ();" >> $(PRJDIR)/aws_xmlada.gpr
	echo "end AWS_XMLADA;" >> $(PRJDIR)/aws_xmlada.gpr

setup_dir:
	-$(MKDIR) -p $(PRJDIR)

CONFGPR	= $(PRJDIR)/aws_config.gpr

ifeq (${SOCKET}, ssl)
SOCKET = openssl
endif

SSL_SUFFIX=$(SOCKET)

ifeq (${SOCKET}, std)
SSL_SUFFIX = dummy
endif

setup_config:
	echo 'abstract project AWS_Config is' > $(CONFGPR)
	echo '   for Source_Dirs use ();' >> $(CONFGPR)
	echo >> $(CONFGPR)
	echo '   type Boolean_Type is ("true", "false");' >> $(CONFGPR)
	echo '   Zlib_Exists : Boolean_Type := "$(ZLIB)";' >> $(CONFGPR)
	echo >> $(CONFGPR)
	echo '   type SOCKET_Type is ("std", "openssl", "gnutls");' \
	  >> $(CONFGPR)
	echo '   SOCKET : SOCKET_Type := "$(SOCKET)";' >> $(CONFGPR)
	echo >> $(CONFGPR)
	echo 'end AWS_Config;' >> $(CONFGPR)

#  Set up all modules to create all the directories. This way it is possible
#  to build AWS using GPS using any settings.

setup_modules: $(MODULES_SETUP)

gen_setup:
	echo "prefix=$(prefix)" > makefile.setup
	echo "DEFAULT_LIBRARY_TYPE=$(DEFAULT_LIBRARY_TYPE)" >> makefile.setup
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
