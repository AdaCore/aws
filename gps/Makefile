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

ifndef VERBOSE_MAKE
.SILENT:
endif

SDIR := $(SRC_DIR)/src

ALL_API = $(sort $(shell ls $(SDIR)/core/*.ads $(SDIR)/extended/*.ads \
	$(SDIR)/soap/*.ads $(SDIR)/xsrc/*.ads \
	$(SRC_DIR)/templates_parser/src/*.ads \
	$(SRC_DIR)/templates_parser/xsrc/*.ads))

setup:

install:

check:

after-build:
	$(TGT_DIR)/common/bin/gen_gps_ref $(ALL_API)

clean:
	-$(RM) -f *~ aws_api.xml
