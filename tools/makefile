
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
	$(MKDIR) -p ../$(BDIR)/tools/obj

build:
	$(GNATMAKE) -Ptools -XBuild=${PRJ_BUILD} \
		-XXMLADA=${PRJ_XMLADA} -XASIS=${PRJ_ASIS}

clean:
	$(RM) -fr ../$(BDIR)/tools
