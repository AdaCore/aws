############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2003-2025, AdaCore                     #
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

ifndef VERBOSE_MAKE
.SILENT:
endif

LIBGPR	= $(PRJ_DIR)/aws_lib_shared.gpr
XOSCONS = $(abspath $(STP_DIR)/bin/xoscons)

ifeq (${SOCKET}, openssl)
LPATH	= $(dir $(shell $(GCC) -print-file-name=libssl.a))
else ifeq (${SOCKET}, gnutls)
LPATH	= $(dir $(shell $(GCC) -print-file-name=libgnutls.a))
endif

ZPATH	= $(dir $(shell $(GCC) -print-file-name=libz.a))

#  For native platforms check if the Zlib is working fine and has a correct
#  version. For embedded platforms we only check if the static libz exists.

ifneq (${PRJ_TARGET}, vxworks)
ifeq (${ZLIB}, false)
ZPATH = ./
endif
endif

ifeq (${SOCKET}, std)
NO_SSL_LINK = 1
endif

ifeq (${SSL_DYNAMIC}, true)
NO_SSL_LINK = 1
SSL_THIN=$(SDIR)/src/ssl-thin.ads
endif

GPROPTS = 	-XPRJ_TARGET=$(PRJ_TARGET) -XTARGET=$(TARGET) \
		-XLIBRARY_TYPE=static

#  Is out of tree build
ifeq ($(ISOOT), true)
	GPROPTS += --relocate-build-tree=$(BLD_DIR) --root-dir=$(SRC_DIR)
endif

$(SDIR)/src:
	$(MKDIR) $(STP_DIR)/src

$(PRJ_DIR):
	$(MKDIR) $(PRJ_DIR)

$(XOSCONS)$(EXEEXT): $(SDIR)/src $(SRC_DIR)/config/setup/xoscons.adb
	-$(GPRBUILD) -p -XPRJ_BUILD=Debug $(GPROPTS) -Psetup xoscons

$(SDIR)/dynamo$(EXEEXT): $(SDIR)/src $(SRC_DIR)/ssl/dynamo/dynamo.adb
	-$(GPRBUILD) -p $(GPROPTS) -Pssl/dynamo/dynamo.gpr -g -bargs -E

setup_extlib: $(PRJ_DIR)
	echo "abstract project AWS_Lib_Shared is" > $(LIBGPR)
	echo "   for Source_Files use ();" >> $(LIBGPR)
	echo '   type SSL_Library_Kind is ("relocatable", "static");' >> $(LIBGPR)
	echo '   SSL_Library_Type : SSL_Library_Kind := external ("SSL_LIBRARY_TYPE", "relocatable");' >> $(LIBGPR)
	echo '   LIB_Path := "${LPATH}";' >> $(LIBGPR)
ifdef NO_SSL_LINK
	echo '   S_SSL_Lib := "";' >> $(LIBGPR)
	echo '   R_SSL_Lib := "";' >> $(LIBGPR)
	echo '   S_CRY_Lib := "";' >> $(LIBGPR)
	echo '   R_CRY_Lib := "";' >> $(LIBGPR)
	echo '   S_TLS_Lib := "";' >> $(LIBGPR)
	echo '   R_TLS_Lib := "";' >> $(LIBGPR)
else
	echo '   S_SSL_Lib := "ssl";' >> $(LIBGPR)
	echo '   R_SSL_Lib := "ssl";' >> $(LIBGPR)
	echo '   S_CRY_Lib := "crypto";' >> $(LIBGPR)
	echo '   R_CRY_Lib := "crypto";' >> $(LIBGPR)
	echo '   S_TLS_Lib := "gnutls";' >> $(LIBGPR)
	echo '   R_TLS_Lib := "gnutls";' >> $(LIBGPR)
endif
	echo '   --  libz' >> $(LIBGPR)
ifeq (${ZPATH}, ./)
	echo '   LIBZ_Path := Project'\''Project_Dir ' >> $(LIBGPR)
	echo '     & "../../../lib/aws/static";' >> $(LIBGPR)
else
	echo '   LIBZ_Path := "${ZPATH}";' >> $(LIBGPR)
endif
	echo "end AWS_Lib_Shared;" >> $(LIBGPR)

force: ;

setup: $(SDIR)/src/os_lib.ads $(SSL_THIN) setup_extlib

ifeq (${PRJ_TARGET}, vxworks)
$(SDIR)/src/aws-os_lib.ads: $(SDIR)/src force
	$(CP) $(SRC_DIR)/src/os_lib__vxworks.ads \
		$(STP_DIR)/src/os_lib.ads
else
$(SDIR)/src/os_lib.ads: $(SDIR)/src $(XOSCONS)$(EXEEXT) force
	echo Setup OS specific definitions
	$(CP) $(SRC_DIR)/config/setup/os_lib-tmplt.c $(STP_DIR)/src
ifeq ($(IS_CROSS), true)
	( cd $(STP_DIR)/src; \
		$(TARGET)-gcc -C -E -DTARGET=\"$(TARGET)\" os_lib-tmplt.c \
			-DSOCKET_$(SOCKET) \
			> os_lib-tmplt.i; \
		$(TARGET)-gcc -S os_lib-tmplt.i; \
		$(XOSCONS) os_lib )
else
	( cd $(STP_DIR)/src; \
		gcc -C -E -DTARGET=\"$(TARGET)\" os_lib-tmplt.c \
			-DSOCKET_$(SOCKET) \
			> os_lib-tmplt.i; \
		gcc -S os_lib-tmplt.i; \
		$(XOSCONS) os_lib )
endif
	$(RM) -f $(STP_DIR)/src/os_lib-tmplt*
endif

$(SSL_THIN): $(STP_DIR)/src $(STP_DIR)/dynamo$(EXEEXT)
	echo Setup SSL dynamic binding
	$(STP_DIR)/dynamo$(EXEEXT) ssl/ssl-thin__$(SOCKET).ads $(SSL_THIN) \
		SSL.Loader

build-dynamo: $(STP_DIR)/dynamo$(EXEEXT)

install:

check:
