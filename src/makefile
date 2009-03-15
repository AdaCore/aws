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

setup:

install:
	$(CP) a*.ad[sb] $(I_INC)
	$(CP) ../$(BDIR)/static/lib/src/* $(I_LIB)/static
ifeq (${ENABLE_SHARED}, true)
	$(CP) ../$(BDIR)/relocatable/lib/src/* $(I_LIB)/relocatable
endif
ifeq ($(XMLADA),true)
	$(CP) ../soap/*.ad[sb] $(I_INC)
	$(CP) ../xsrc/*.ad[sb] $(I_INC)
endif

check:
	echo Rule \"raised exception is not declared...\"
	echo "   Expected 3 violations in aws-server-hotplug.adb"
	echo "   Expected 3 violations in aws-server.adb"
	$(GNAT) check -rules -from=../aws.checks -Psrc
