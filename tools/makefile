############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2003-2014, AdaCore                     #
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

SDIR = ../$(BDIR)/../setup

ifdef ASIS
GEN   = $(SDIR)/tsrc/ada2wsdl-options.adb
endif

force:

$(GEN): force
	echo "--  This file is generated automatically" > $(GEN)
	echo "--  DO NOT EDIT, see tools/makefile" >> $(GEN)
	echo "with Ada2WSDL.Parser;" >> $(GEN)
	echo "package body Ada2WSDL.Options is" >> $(GEN)
	echo "   procedure Set_Default is" >> $(GEN)
	echo "   begin" >> $(GEN)
	echo "      pragma Style_Checks (Off);" >> $(GEN)
	echo "      Parser.Add_Option (\"-I"$(I_INC)"\");" \
		>> $(GEN)
	echo "   end Set_Default;" >> $(GEN)
	echo "end Ada2WSDL.Options;" >> $(GEN)

setup_dir:
	-$(MKDIR) $(SDIR)/tsrc

setup: setup_dir $(GEN)

install:

check:
	$(GNAT) check -Ptools -rules -from=../aws.checks
