
# $Id$

.SILENT: all auth runme agent ws clean soap hotplug com_1 com_2 hello_world
.SILENT: main build_soap

GFLAGSY = $(patsubst -gnaty,-gnaty3abcefhiklmnoprt,$(GFLAGS))

ifeq ($(MODE),ssl)
# With SSL support

GARGS = $(GFLAGSY) -i -I../src -I../ssl -I../include -I../soap $(INCLUDES)
BARGS = -E

ifeq (${OS}, Windows_NT)
LARGS = $(LIBS) -L../win32 -lssl -lcrypto
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

runme:
	gnatmake $(GARGS) runme -bargs $(BARGS) -largs $(LARGS)

ws:
	gnatmake $(GARGS) ws -bargs $(BARGS) -largs $(LARGS)

agent:
	gnatmake $(GARGS) agent -bargs $(BARGS) -largs $(LARGS)

auth:
	gnatmake $(GARGS) auth -bargs $(BARGS) -largs $(LARGS)

com_1:
	gnatmake $(GARGS) com_1 -bargs $(BARGS) -largs $(LARGS)

com_2:
	gnatmake $(GARGS) com_2 -bargs $(BARGS) -largs $(LARGS)

main:
	gnatmake $(GARGS) main -bargs $(BARGS) -largs $(LARGS)

hotplug:
	gnatmake $(GARGS) hotplug -bargs $(BARGS) -largs $(LARGS)

hello_world:
	gnatmake $(GARGS) hello_world -bargs $(BARGS) -largs $(LARGS)

build_soap:
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
