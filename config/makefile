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

SDIR  = ../$(BDIR)/../setup
NSDIR = ../$(NBDIR)/../setup

setup_dir:
	-$(MKDIR) -p $(SDIR)/src $(NSDIR)/src
ifeq ($(PLATFORM), native)
	-$(GPRBUILD) -p -XPRJ_BUILD=Debug -XLIBRARY_TYPE=static \
		-XPLATFORM=native -Psetup
endif

force: ;

setup: setup_dir $(NSDIR)/src/aws-os_lib.ads

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

check:
