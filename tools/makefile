
# $Id$

.SILENT: build clean awsres

GARGS	 = -q $(GFLAGS) -I../src -I../soap -I../ssl -I../include $(INCLUDES)

TOOLS	 = awsres 

ifdef XMLADA
TOOLS := $(TOOLS) wsdl2aws
endif

ALL_EXEC = $(addsuffix $(EXEEXT), $(TOOLS))

build: $(TOOLS)

force:

$(TOOLS): force
	echo Building $@ tools
	gnatmake $(GARGS) $@ -largs -s $(LIBS)

clean:
	-rm -f *.o *.ali $(ALL_EXEC) b~*.ad[sb]
