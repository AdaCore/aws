############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2003-2015, AdaCore                     #
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

ALL_API = $(sort $(shell ls ../src/core/*.ads ../src/extended/*.ads \
	../src/soap/*.ads  ../src/xsrc/*.ads \
	../templates_parser/src/*.ads ../templates_parser/xsrc/*.ads))

setup:

install:

check:

after-build:
	../$(BDIR)/../../$(TARGET)/common/bin/gen_gps_ref $(ALL_API)

clean:
	-$(RM) -f *~ aws_api.xml
