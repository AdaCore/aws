############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2003-2010, AdaCore                     #
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

LIBGPR	= ../$(BDIR)/../../projects/aws_lib_shared.gpr
LPATH	= $(dir $(shell $(GCC) -print-file-name=libssl.a))
ZPATH	= $(dir $(shell $(GCC) -print-file-name=libz.a))
SDIR	= ../$(BDIR)/../setup
NSDIR	= ../$(NBDIR)/../setup

setup_dir:
	-$(MKDIR) -p $(SDIR)/src $(NSDIR)/src
ifeq ($(PLATFORM), native)
	-$(GPRBUILD) -p -XPRJ_BUILD=Debug -XLIBRARY_TYPE=static \
		-XPLATFORM=native -Psetup
endif

setup_extlib:
	echo "project AWS_Lib_Shared is" > $(LIBGPR)
	echo "   for Source_Files use ();" >> $(LIBGPR)
	echo '   type SSL_Library_Kind is ("relocatable", "static");' >> $(LIBGPR)
	echo '   SSL_Library_Type : SSL_Library_Kind := external ("SSL_LIBRARY_TYPE", "relocatable");' >> $(LIBGPR)
ifeq (${SOCKET}, std)
	echo '   LIB_Path := "";' >> $(LIBGPR)
	echo '   S_SSL_Lib := "";' >> $(LIBGPR)
	echo '   R_SSL_Lib := "";' >> $(LIBGPR)
	echo '   S_CRY_Lib := "";' >> $(LIBGPR)
	echo '   R_CRY_Lib := "";' >> $(LIBGPR)
	echo '   S_TLS_Lib := "";' >> $(LIBGPR)
	echo '   R_TLS_Lib := "";' >> $(LIBGPR)
else
	echo '   LIB_Path := "${LPATH}";' >> $(LIBGPR)
	echo '   S_SSL_Lib := "ssl";' >> $(LIBGPR)
ifeq (${OS}, Windows_NT)
	echo '   R_SSL_Lib := "ssl32";' >> $(LIBGPR)
	echo '   S_CRY_Lib := "crypto";' >> $(LIBGPR)
	echo '   R_CRY_Lib := "eay32";' >> $(LIBGPR)
else
	echo '   R_SSL_Lib := "ssl";' >> $(LIBGPR)
	echo '   S_CRY_Lib := "crypto";' >> $(LIBGPR)
	echo '   R_CRY_Lib := "crypto";' >> $(LIBGPR)
endif
	echo '   S_TLS_Lib := "gcrypt";' >> $(LIBGPR)
	echo '   R_TLS_Lib := "gcrypt";' >> $(LIBGPR)
endif
	echo '   --  libz' >> $(LIBGPR)
ifeq (${ZPATH}, ./)
	echo '   LIBZ_Path := "${prefix}/lib/aws/native/static";' >> $(LIBGPR)
else
	echo '   LIBZ_Path := "${ZPATH}";' >> $(LIBGPR)
endif
	echo "end AWS_Lib_Shared;" >> $(LIBGPR)

force: ;

setup: setup_dir $(NSDIR)/src/aws-os_lib.ads setup_extlib

$(NSDIR)/src/aws-os_lib.ads: makefile setup/check_config.c force
	echo Setup OS specific definitions
ifeq ($(PLATFORM), native)
	$(NSDIR)/check_config $(NSDIR)/src/aws-os_lib.ads
else
	$(CP) src/aws-os_lib__$(PLATFORM).ads $(SDIR)/src/aws-os_lib.ads
endif

install:
	$(CP) $(SDIR)/src/aws-os_lib.ads $(TI_INC)
	$(CP) src/aws-net-std__* $(I_INC)
	$(CP) src/aws-net-ssl__* $(I_INC)
	$(CP) src/aws-net-ssl-certificate__* $(I_INC)
	$(CP) src/ssl-thin__* $(I_INC)
	$(CP) src/templates_parser-* $(I_INC)
	$(CP) projects/aws_components.gpr $(I_AGP)
	$(CP) projects/aws.gpr $(I_GPR)
	$(CP) projects/*_lib.gpr $(I_AGP)
	$(CP) projects/aws_shared.gpr $(I_AGP)
	$(CP) projects/aws_libz.gpr $(I_AGP)
	$(CP) projects/aws_ssl_support.gpr $(I_AGP)
	$(CP) $(LIBGPR) $(I_AGP)

check:
