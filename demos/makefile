
# $Id$

.SILENT: all auth runme agent ws clean soap hotplug com_1 com_2 hello_world
.SILENT: main build_soap

#GFLAGSY = $(patsubst -gnaty,-gnaty3abcefhiklmnoprt,$(GFLAGS))
# above declaration is ok for GNAT 3.14 but GNAT 3.13 does not support
# selective style check options.
GFLAGSY = $(patsubst -gnaty,,$(GFLAGS))

ifeq ($(MODE),ssl)
# With SSL support

GARGS = $(GFLAGSY) -i -I../src -I../ssl -I../include -I../soap $(INCLUDES)
BARGS = -E

ifeq (${OS}, Windows_NT)
LARGS = $(LIBS) -L../ssl -lssl -lcrypto
else
LARGS = $(LIBS) -lssl -lcrypto
endif

else
# Without SSL support

GARGS = $(GFLAGSY) -i -I../src -I../include -I../soap $(INCLUDES)
BARGS = -E
LARGS = $(LIBS)
endif

build: runme agent auth ws com_1 com_2 main hotplug hello_world

force:

runme: force
	gnatmake $(GARGS) runme -bargs $(BARGS) -largs $(LARGS)

ws: force
	gnatmake $(GARGS) ws -bargs $(BARGS) -largs $(LARGS)

agent: force
	gnatmake $(GARGS) agent -bargs $(BARGS) -largs $(LARGS)

auth: force
	gnatmake $(GARGS) auth -bargs $(BARGS) -largs $(LARGS)

com_1: force
	gnatmake $(GARGS) com_1 -bargs $(BARGS) -largs $(LARGS)

com_2: force
	gnatmake $(GARGS) com_2 -bargs $(BARGS) -largs $(LARGS)

main: force
	gnatmake $(GARGS) main -bargs $(BARGS) -largs $(LARGS)

hotplug: force
	gnatmake $(GARGS) hotplug -bargs $(BARGS) -largs $(LARGS)

hello_world: force
	gnatmake $(GARGS) hello_world -bargs $(BARGS) -largs $(LARGS)

build_soap: force
	(if [ -e ../soap/soap.o ] ; then \
	gnatmake $(GARGS) soap_client -bargs $(BARGS) -largs $(LARGS); \
	gnatmake $(GARGS) soap_server -bargs $(BARGS) -largs $(LARGS); \
	gnatmake $(GARGS) soap_svs -bargs $(BARGS) -largs $(LARGS); \
	gnatmake $(GARGS) soap_cvs -bargs $(BARGS) -largs $(LARGS); \
	else \
	echo ""; \
	echo Please build SOAP library before building SOAP demos;\
	fi)
clean:
	-rm -f *.o *.ali *.exe b~*.* *.~*.*.~
