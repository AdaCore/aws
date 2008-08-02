############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2003-2008, AdaCore                     #
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

ifeq (${OS}, Windows_NT)
EXEEXT = .exe
else
EXEEXT =
endif

SDIR = ../.build/setup

DUMMY_OPTS = -XPRJ_BUILD=Debug -XLIBRARY_TYPE=static

setup_dummy_dir:
# Next line is needed because a current limitation on Project Files
# where the Object_Dir and Library_Dir must exists even if not
# actually used.
	-$(MKDIR) -p ../.build/debug/static/obj ../.build/debug/static/lib

setup_dir: setup_dummy_dir
	-$(MKDIR) -p $(SDIR)/obj
	-$(MKDIR) -p $(SDIR)/lib

setup: setup_dir ../src/aws-translator-conversion.adb ../src/aws-os_lib.ads

../src/aws-translator-conversion.adb: makefile
	echo Setup conversion support
	$(GNAT_FOR_HOST) make -Psetup $(DUMMY_OPTS)
	$(SDIR)/test_uconv
	$(MV) aws-translator-conversion.adb ../src

../src/aws-os_lib.ads: makefile src/check_config.c
	echo Setup OS specific definitions
	$(GCC_FOR_HOST) -o $(SDIR)/check_config src/check_config.c
	$(SDIR)/check_config ../src/aws-os_lib.ads

build:

install:
	$(CP) aws-net-std__* $(I_INC)
	$(CP) aws-net-ssl__* $(I_INC)
	$(CP) aws-net-ssl-certificate__* $(I_INC)
	$(CP) ssl-thin__* $(I_INC)
	$(CP) templates_parser-* $(I_INC)
	$(CP) projects/aws_components.gpr $(I_AGP)
	$(CP) projects/aws.gpr $(I_GPR)
	$(CP) projects/*_lib.gpr $(I_AGP)
	$(CP) projects/aws_shared.gpr $(I_AGP)
	$(CP) projects/aws_libz.gpr $(I_AGP)
	$(CP) projects/aws_ssl_support.gpr $(I_AGP)

check:

clean:
	-$(RM) -f *.~*.*~
