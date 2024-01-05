############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2003-2024, AdaCore                     #
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

#  BLD_DIR : root build directory
#  TGT_DIR : target directory
#  PRJ_DIR : project directory
#  STP_DIR : setup directory
#  KND_DIR : kind dir, the debug or release dir
#  CMN_DIR : directory for common generated files

BLD_DIR := $(CURDIR)
SRC_DIR := $(CURDIR)

#  NOTE: You should not have to change this makefile. Configuration options
#  can be changed in makefile.conf

#  Is out of tree build
ifneq ($(BLD_DIR), $(SRC_DIR))
GPROOTOPTS = --relocate-build-tree=$(BLD_DIR) --root-dir=$(SRC_DIR)
ISOOT := true
else
ISOOT := false
endif

include $(SRC_DIR)/makefile.conf
#  default setup

include $(SRC_DIR)/makefile.checks
#  consistency checks

TGT_DIR := $(BLD_DIR)/$(TARGET)
PRJ_DIR := $(TGT_DIR)/projects
STP_DIR := $(TGT_DIR)/setup
CMN_DIR := $(TGT_DIR)/common

ifeq ($(DEBUG), true)
MAKE_OPT	=
KND_DIR		= $(TGT_DIR)/debug
else
MAKE_OPT	= -s
KND_DIR		= $(TGT_DIR)/release
endif

# Target dir

ifeq ($(ISOOT), true)
OOTDIR := /$(TARGET)
else
OOTDIR := /$(TARGET)
endif

# Add path to generated project files
GPR_PROJECT_PATH := $(PRJ_DIR):$(GPR_PROJECT_PATH)

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

#ifeq ($(DEBUG), true)
#MAKE_OPT	=
#BDIR		= $(BDIR)$(OOTDIR)/debug
#else
#MAKE_OPT	= -s
#BDIR		= $(BDIR)$(OOTDIR)/release
#endif

LIBAWS_TYPES := static

ifeq (${ENABLE_SHARED},true)
   LIBAWS_TYPES += relocatable static-pic
endif

#############################################################################
#  NO NEED TO CHANGE ANYTHING PAST THIS POINT
#############################################################################

all: build

ALL_OPTIONS	= $(MAKE_OPT) SOCKET="$(SOCKET)" XMLADA="$(XMLADA)" \
	EXEEXT="$(EXEEXT)" LDAP="$(LDAP)" DEBUG="$(DEBUG)" \
	RM="$(RM)" CP="$(CP)" MKDIR="$(MKDIR)" SED="$(SED)" GCC="$(GCC)" \
	GPRBUILD="$(GPRBUILD)" ZLIB="$(ZLIB)" TDIR="$(TDIR)" \
	prefix="$(prefix)" ENABLE_SHARED="$(ENABLE_SHARED)" \
	SOEXT="$(SOEXT)" GNAT="$(GNAT)" SSL_DYNAMIC="$(SSL_DYNAMIC)" \
	T2A="../../$(TDIR)/static/tools/templates2ada" \
	LIBRARY_TYPE="$(LIBRARY_TYPE)" PYTHON="$(PYTHON)" \
	TARGET="$(TARGET)" IS_CROSS=$(IS_CROSS) GPRINSTALL="$(GPRINSTALL)" \
	SRC_DIR="$(SRC_DIR)" BLD_DIR="$(BLD_DIR)" PRJ_DIR=$(PRJ_DIR) \
	TGT_DIR="$(TGT_DIR)" STP_DIR="$(STP_DIR)" KND_DIR="$(KND_DIR)" \
	CMN_DIR="$(CMN_DIR)" ISOOT="$(ISOOT)"

build-doc:
	echo ""
	echo "=== Build doc"
	${MAKE} -C docs html latexpdf
	${MAKE} -C templates_parser/docs html latexpdf

run_regtests:
	echo ""
	echo "=== Run regression tests"
	echo ""
	$(MAKE) -C regtests aws_regtests $(ALL_OPTIONS)

force:

#############################################################################
#  Configuration for GNAT Projet Files

MODULES = config include ssl src gps regtests demos

MODULES_SETUP = ${MODULES:%=%_setup} templates_parser_setup

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

#  LAL

ifeq (${LAL}, true)
PRJ_LAL=Installed
GEXT_MODULE := $(GEXT_MODULE) lal_setup
else
PRJ_LAL=Disabled
GEXT_MODULE := $(GEXT_MODULE) lal_dummy
endif

#  Sockets

PRJ_SOCKLIB=$(NETLIB)

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

GALL_OPTIONS := $(ALL_OPTIONS) \
	PRJ_BUILD="$(PRJ_BUILD)" \
	PRJ_XMLADA="$(PRJ_XMLADA)" \
	PRJ_LAL="$(PRJ_LAL)" \
	PRJ_SOCKLIB="$(PRJ_SOCKLIB)" \
	PRJ_LDAP="$(PRJ_LDAP)" \
	PRJ_TARGET="$(PRJ_TARGET)" \
	TP_XMLADA="$(TP_XMLADA)" \
	I_INC="$(I_INC)"

${MODULES_SETUP}: force
	${MAKE} -C ${@:%_setup=%} setup $(GALL_OPTIONS)

${MODULES_INSTALL}: force
	${MAKE} -C ${@:%_install=%} install $(GALL_OPTIONS)

${MODULES_CHECK}: force
	${MAKE} -C ${@:%_check=%} check $(GALL_OPTIONS)

GPROPTS = -XPRJ_BUILD=$(PRJ_BUILD) -XPRJ_SOCKLIB=$(PRJ_SOCKLIB) \
		-XPRJ_LDAP=$(PRJ_LDAP) \
		-XPRJ_XMLADA=$(PRJ_XMLADA) -XPRJ_LAL=$(PRJ_LAL) \
		-XPROCESSORS=$(PROCESSORS) -XSOCKET=$(SOCKET) \
		-XPRJ_TARGET=$(PRJ_TARGET) -XTARGET=$(TARGET) \
	        -XTHREAD_SANITIZER=$(THREAD_SANITIZER) \
                -XSSL_DYNAMIC=$(SSL_DYNAMIC) -XTGT_DIR=$(TGT_DIR) \
		$(GPROOTOPTS)

GPR_STATIC = -XLIBRARY_TYPE=static -XXMLADA_BUILD=static
GPR_SHARED = -XLIBRARY_TYPE=relocatable -XXMLADA_BUILD=relocatable

#######################################################################
#  build

#  build awsres tool as needed by wsdl2aws

build-awsres-tool-native:
	$(GPRBUILD) -p $(GPROPTS) $(GPR_STATIC) -XTO_BUILD=awsres.adb \
		tools/tools.gpr

build-tools-native: gen-templates build-lib-native
	$(GPRBUILD) -p $(GPROPTS) $(GPR_STATIC) tools/tools.gpr

build-libs-%:
	$(GPRBUILD) -p $(GPROPTS) \
		-XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* aws.gpr

build-lib-native: ${LIBAWS_TYPES:%=build-libs-%}

build-gps-support: build-lib-native
	$(GPRBUILD) -p $(GPROPTS) $(GPR_STATIC) gps/gps_support.gpr
	${MAKE} -C gps $(GALL_OPTIONS) after-build

build-native: build-tools-native build-gps-support

build-tools-cross: build-lib-cross
	$(GPRBUILD) -p --target=$(TARGET) $(GPROPTS) \
		$(GPR_STATIC) tools/tools.gpr

build-libs-cross-%:
	$(GPRBUILD) -p --target=$(TARGET) $(GPROPTS) \
		-XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* aws.gpr

build-lib-cross: ${LIBAWS_TYPES:%=build-libs-cross-%}

build-dynamo:
	make -C config build-dynamo $(ALL_OPTIONS)

gen-templates: build-awsres-tool-native force
	make -C tools/wsdl2aws-templates \
		gen-templates $(ALL_OPTIONS)

build-cross: build-tools-cross

ifeq (${IS_CROSS}, true)
build: gen-templates build-cross
else
build: gen-templates build-native
endif

gps: setup
	$(GPS) $(GPROPTS) $(GPR_SHARED) -Paws.gpr &

#######################################################################
#  clean

clean-libs-%:
	$(GPRCLEAN) $(GPROPTS) -XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* aws.gpr

clean-lib-native: ${LIBAWS_TYPES:%=clean-libs-%}

clean-native: clean-libs-native
	-$(GPRCLEAN) $(GPROPTS) $(GPR_STATIC) tools/tools.gpr
	-$(GPRCLEAN) $(GPROPTS) $(GPR_STATIC) gps/gps_support.gpr

clean-libs-cross-%:
	$(GPRCLEAN) --target=$(TARGET) \
		-XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* aws.gpr

clean-cross: ${LIBAWS_TYPES:%=clean-libs-cross-%}

ifeq (${IS_CROSS}, true)
clean: clean-cross
else
clean: clean-native
endif
	-${MAKE} -C regtests $(GALL_OPTIONS) clean
	-${MAKE} -C docs $(GALL_OPTIONS) clean
	-${RM} -fr $(BDIR)

#######################################################################
#  install

install-clean:
ifneq (,$(wildcard $(TPREFIX)/share/gpr/manifests/aws))
	-$(GPRINSTALL) $(GPROPTS) --uninstall --prefix=$(TPREFIX) aws
endif

GPRINST_OPTS=-p -f --prefix=$(TPREFIX) \
	--build-var=LIBRARY_TYPE --build-var=AWS_BUILD

install-libs-%:
	$(GPRINSTALL) $(GPROPTS) $(GPRINST_OPTS) \
		-XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* \
		--build-name=$* aws.gpr

install-lib-native: ${LIBAWS_TYPES:%=install-libs-%}

install-tools-native:
	$(GPRINSTALL) $(GPROPTS) $(GPRINST_OPTS) $(GPR_STATIC) --mode=usage \
		--build-name=static \
		--install-name=aws tools/tools.gpr

install-native: install-clean install-lib-native install-tools-native

install-libs-cross-%:
	$(GPRINSTALL) $(GPROPTS) $(GPRINST_OPTS) \
		--target=$(TARGET) -XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* \
		--build-name=$* aws.gpr

install-lib-cross: ${LIBAWS_TYPES:%=install-libs-cross-%}

install-tools-cross:
	$(GPRINSTALL) $(GPROPTS)  $(GPRINST_OPTS) --mode=usage \
		--target=$(TARGET) $(GPROPTS) \
		--install-name=aws tools/tools.gpr

install-cross: install-clean install-libs-cross install-tools-cross

ifeq (${IS_CROSS}, true)
install: install-cross
else
install: install-native
endif

#######################################################################

check: $(MODULES_CHECK)

lal_dummy:
	echo "abstract project AWS_LAL is" > $(PRJ_DIR)/aws_lal.gpr
	echo "   for Source_Dirs use ();" >> $(PRJ_DIR)/aws_lal.gpr
	echo "end AWS_LAL;" >> $(PRJ_DIR)/aws_lal.gpr

lal_setup:
	echo 'with "libadalang";' > $(PRJ_DIR)/aws_lal.gpr
	echo "abstract project AWS_LAL is" >> $(PRJ_DIR)/aws_lal.gpr
	echo "   for Source_Dirs use ();" >> $(PRJ_DIR)/aws_lal.gpr
	echo "end AWS_LAL;" >> $(PRJ_DIR)/aws_lal.gpr

gxmlada_dummy:
	echo "abstract project AWS_XMLADA is" > $(PRJ_DIR)/aws_xmlada.gpr
	echo "   for Source_Dirs use ();" >> $(PRJ_DIR)/aws_xmlada.gpr
	echo "end AWS_XMLADA;" >> $(PRJ_DIR)/aws_xmlada.gpr

gxmlada_setup:
	echo 'with "xmlada";' > $(PRJ_DIR)/aws_xmlada.gpr
	echo "abstract project AWS_XMLADA is" >> $(PRJ_DIR)/aws_xmlada.gpr
	echo "   for Source_Dirs use ();" >> $(PRJ_DIR)/aws_xmlada.gpr
	echo "end AWS_XMLADA;" >> $(PRJ_DIR)/aws_xmlada.gpr

setup_dir:
	$(MKDIR) -p $(PRJ_DIR)
	$(MKDIR) -p $(BLD_DIR)
	$(MKDIR) -p $(CMN_DIR)/src

CONFGPR	= $(PRJ_DIR)/aws_config.gpr

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

MSETUP := $(TGT_DIR)/makefile.setup

gen_setup: setup_dir
	echo "prefix=$(prefix)" > $(MSETUP)
	echo "ENABLE_SHARED=$(ENABLE_SHARED)" >> $(MSETUP)
	echo "ZLIB=$(ZLIB)" >> $(MSETUP)
	echo "XMLADA=$(XMLADA)" >> $(MSETUP)
	echo "LAL=$(LAL)" >> $(MSETUP)
	echo "NETLIB=$(NETLIB)" >> $(MSETUP)
	echo "SOCKET=$(SOCKET)" >> $(MSETUP)
	echo "SSL_DYNAMIC=$(SSL_DYNAMIC)" >> $(MSETUP)
	echo "LDAP=$(LDAP)" >> $(MSETUP)
	echo "DEBUG=$(DEBUG)" >> $(MSETUP)
	echo "PROCESSORS=$(PROCESSORS)" >> $(MSETUP)
	echo "TARGET=$(TARGET)" >> $(MSETUP)
	echo "PRJ_TARGET=$(PRJ_TARGET)" >> $(MSETUP)
	echo "THREAD_SANITIZER=$(THREAD_SANITIZER)" >> $(MSETUP)
	echo "GSOAP=false" >> $(MSETUP)
	echo "SERVER_HTTP2=$(SERVER_HTTP2)" >> $(MSETUP)
	echo "CLIENT_HTTP2=$(CLIENT_HTTP2)" >> $(MSETUP)

setup: gen_setup setup_modules setup_config setup_tp $(GEXT_MODULE)

setup_tp:
	$(MAKE) -C templates_parser setup $(GALL_OPTIONS)
