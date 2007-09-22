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

force:

aws-translator-conversion.adb:
	${MAKE} -C ../config setup

aws-os_lib-definitions.ads: ../config/src/check_config.c
	$(MAKE) -C ../config setup

setup:
	-$(MKDIR) -p ../$(BDIR)/obj
	-$(MKDIR) -p ../$(BDIR)/lib

build:
	$(GNAT) make -Psrc -XBuild=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA} \
		-XSOCKLIB=${PRJ_SOCKLIB}

clean:
	-$(RM) -f aws-translator-conversion.adb aws-os_lib-definitions.ads

check:
	echo Rule \"raised exception is not declared...\"
	echo "   Expected 3 violations in aws-server-hotplug.adb"
	echo "   Expected 3 violations in aws-server.adb"
	$(GNAT) check -rules -from=../aws.checks \
		-Psrc -XBuild=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA} \
		-XSOCKLIB=${PRJ_SOCKLIB}
