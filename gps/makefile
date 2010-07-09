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

ALL_API = $(sort $(shell ls ../src/core/*.ads ../src/extended/*.ads \
	../src/soap/*.ads  ../src/xsrc/*.ads \
	../templates_parser/src/*.ads ../templates_parser/xsrc/*.ads))

setup:

install:
	-$(CP) aws_api.xml $(I_PLG)
	$(CP) aws.py $(I_PLG)
	$(CP) aws.xml $(I_PLG)
	$(CP) ada2wsdl.xml $(I_PLG)
	$(CP) wsdl2aws.xml $(I_PLG)

check:

after-build:
	../$(BDIR)/../../native/common/bin/gen_gps_ref $(ALL_API)

clean:
	-$(RM) -f *~ aws_api.xml
