
# $Id$

.SILENT:

GARGS	 = -q -m $(GFLAGS) -I../src -I../soap -I../ssl -I../include $(INCLUDES)

TOOLS	 = awsres
GEN      =

ifdef XMLADA
TOOLS := $(TOOLS) wsdl2aws
endif

ifdef ASIS
TOOLS := $(TOOLS)  ada2wsdl-main
GEN   = ada2wsdl-options.adb
endif

EXECS    = $(patsubst  ada2wsdl-main,ada2wsdl,$(TOOLS))
ALL_EXEC = $(addsuffix $(EXEEXT), $(EXECS))

build: $(GEN) $(TOOLS)

force:

$(TOOLS): force
	echo Building `basename $@ -main`
	$(GNATMAKE) $(GARGS) $@ -o `basename $@ -main` \
		-largs -s -L../lib $(LIBS) -lz

ada2wsdl-options.adb: force
	echo "--  This file is generated automatically" > ada2wsdl-options.adb
	echo "--  DO NOT EDIT, see tools/makefile" >> ada2wsdl-options.adb
	echo "with Ada2WSDL.Parser;" >> ada2wsdl-options.adb
	echo "package body Ada2WSDL.Options is" >> ada2wsdl-options.adb
	echo "   procedure Set_Default is" >> ada2wsdl-options.adb
	echo "   begin" >> ada2wsdl-options.adb
	echo "      Parser.Add_Option (\"-I"$(XMLADA)"/include/xmlada\");" \
		>> ada2wsdl-options.adb
	echo "      Parser.Add_Option (\"-I"$(INSTALL)"/AWS/components\");" \
		>> ada2wsdl-options.adb
	echo "      Parser.Add_Option (\"-I"$(INSTALL)"/AWS/include\");" \
		>> ada2wsdl-options.adb
	echo "   end Set_Default;" >> ada2wsdl-options.adb
	echo "end Ada2WSDL.Options;" >> ada2wsdl-options.adb

clean:
	-$(RM) -f *.o *.ali $(ALL_EXEC) b~*.ad[sb] *~
	-$(RM) -f ada2wsdl-options.adb

#############################################################################
# Configuration for GNAT Projet Files

gsetup: $(GEN) 
	$(MKDIR) -p ../.build/debug/tools/obj
	$(MKDIR) -p ../.build/release/tools/obj

gbuild:
	$(GNATMAKE) -Ptools -XAWS_BUILD=${PRJ_BUILD} \
		-XXMLADA=${PRJ_XMLADA} -XASIS=${PRJ_ASIS}

gclean:
	$(RM) -fr ../.build/debug/tools
	$(RM) -fr ../.build/release/tools
