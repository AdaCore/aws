
# $Id$

.SILENT: build clean awsres

GARGS	 = -q $(GFLAGS) -I../src -I../soap -I../ssl -I../include $(INCLUDES)

TOOLS	 = awsres 

ifdef XMLADA
TOOLS := $(TOOLS) wsdl2aws
endif

ifdef ASIS
TOOLS := $(TOOLS) ada2wsdl-main
endif

EXECS    = $(patsubst  ada2wsdl-main,ada2wsdl,$(TOOLS))
ALL_EXEC = $(addsuffix $(EXEEXT), $(EXECS))

build: $(TOOLS)

force:

$(TOOLS): force
	echo Building `basename $@ -main`
	$(GNATMAKE) $(GARGS) $@ -o `basename $@ -main` \
		-largs -s -L../lib $(LIBS) -lz

clean:
	-$(RM) -f *.o *.ali $(ALL_EXEC) b~*.ad[sb] *~

#############################################################################
# Configuration for GNAT Projet Files

gsetup: 
	$(MKDIR) -p ../.build/debug/tools/obj
	$(MKDIR) -p ../.build/release/tools/obj

gbuild:
	$(GNATMAKE) -Ptools -XAWS_BUILD=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}

gclean:
	$(RM) -fr ../.build/debug/tools
	$(RM) -fr ../.build/release/tools
