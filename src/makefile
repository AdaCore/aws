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

setup:

install:
	$(CP) core/*.ad[sb] extended/*.ad[sb] $(DESTDIR)$(I_INC)
	$(CP) ../$(BDIR)/static/lib/src/* $(DESTDIR)$(I_LIB)/static
ifeq (${ENABLE_SHARED}, true)
	$(CP) ../$(BDIR)/relocatable/lib/src/* $(DESTDIR)$(I_LIB)/relocatable
endif
ifeq ($(XMLADA),true)
	$(CP) soap/*.ad[sb] $(DESTDIR)$(I_INC)
	$(CP) xsrc/*.ad[sb] $(DESTDIR)$(I_INC)
endif
ifeq ($(LDAP),true)
	$(CP) ldap/*.ad[sb] $(DESTDIR)$(I_INC)
endif

check:
	echo Rule \"raised exception is not declared...\"
	echo "   Expected 3 violations in aws-server-hotplug.adb"
	echo "   Expected 3 violations in aws-server.adb"
	$(GNAT) check -rules -from=../aws.checks -Psrc
