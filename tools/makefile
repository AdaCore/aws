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

ifdef ASIS
GEN   = ada2wsdl-options.adb
endif

force:

ada2wsdl-options.adb: force
	echo "--  This file is generated automatically" > ada2wsdl-options.adb
	echo "--  DO NOT EDIT, see tools/makefile" >> ada2wsdl-options.adb
	echo "with Ada2WSDL.Parser;" >> ada2wsdl-options.adb
	echo "package body Ada2WSDL.Options is" >> ada2wsdl-options.adb
	echo "   procedure Set_Default is" >> ada2wsdl-options.adb
	echo "   begin" >> ada2wsdl-options.adb
	echo "      pragma Style_Checks (Off);" >> ada2wsdl-options.adb
	echo "      Parser.Add_Option (\"-I"$(I_CPN)"\");" \
		>> ada2wsdl-options.adb
	echo "      Parser.Add_Option (\"-I"$(I_INC)"\");" \
		>> ada2wsdl-options.adb
	echo "   end Set_Default;" >> ada2wsdl-options.adb
	echo "end Ada2WSDL.Options;" >> ada2wsdl-options.adb

setup: $(GEN)
	$(MKDIR) -p ../$(BDIR)/static/obj/tools
	$(MKDIR) -p ../$(BDIR)/static/tools

build:
	$(GNAT) make -XLIBRARY_TYPE=static -Ptools

install:
	-$(CP) ../$(BDIR)/static/tools/awsres${EXEEXT} $(I_BIN)
	-$(CP) ../$(BDIR)/static/tools/hotplug_password${EXEEXT} $(I_BIN)
	-$(CP) ../$(BDIR)/static/tools/templates2ada${EXEEXT} $(I_BIN)
	-$(CP) ../$(BDIR)/static/tools/templatespp${EXEEXT} $(I_BIN)
	-$(CP) ../$(BDIR)/static/tools/webxref${EXEEXT} $(I_BIN)
ifeq ($(XMLADA),true)
	$(CP) ../$(BDIR)/static/tools/wsdl2aws${EXEEXT} $(I_BIN)
endif
ifeq (${ASIS},true)
	$(CP) ../$(BDIR)/static/tools/ada2wsdl-main${EXEEXT} \
		$(I_BIN)/ada2wsdl${EXEEXT}
endif

check:
	$(GNAT) check -Ptools -rules -from=../aws.checks

clean:
	$(GNAT) clean -XLIBRARY_TYPE=static -Ptools
	$(RM) -f ada2wsdl-options.adb
