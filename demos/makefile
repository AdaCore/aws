
# $Id$

.SILENT: all auth runme agent ws wps clean soap hotplug com_1 com_2 hello_world
.SILENT: main build_soap vh_demo dispatch text_input res_demo test_ldap

GFLAGSY = $(patsubst -gnatwe -gnaty3abcefhiklmnoprst,-gnaty3abcefhiklmnoprt,$(GFLAGS))
# We do not want to check for separate spec for the demos (s option).

ifeq ($(MODE),ssl)
# With SSL support

GARGS = -m $(GFLAGSY) -i -I../src -I../ssl -I../include -I../soap $(INCLUDES)
BARGS = -E

ifeq (${OS}, Windows_NT)
LARGS = $(LFLAGS) $(LIBS) -L../win32 -L../ssl -lssl -lcrypto ../win32/aws.coff
else
LARGS = $(LFLAGS) $(LIBS) -lssl -lcrypto -ladasockets
endif

else
# Without SSL support

GARGS = -m $(GFLAGSY) -i -I../src -I../include -I../soap $(INCLUDES)
BARGS = -E

ifeq (${OS}, Windows_NT)
LARGS = $(LFLAGS) $(LIBS) -L../win32 ../win32/aws.coff
else
LARGS = $(LFLAGS) $(LIBS) -ladasockets
endif
endif

DEMOS = runme agent auth ws wps com_1 com_2 main hotplug hello_world \
	text_input vh_demo dispatch res_demo test_ldap

ifdef XMLADA
DEMOS  := test_jabber
endif

build: $(DEMOS)

force:

runme: force
	gnatmake $(GARGS) runme -bargs $(BARGS) -largs $(LARGS)

ws: force
	gnatmake $(GARGS) ws -bargs $(BARGS) -largs $(LARGS)

wps: force
	gnatmake $(GARGS) wps -bargs $(BARGS) -largs $(LARGS)

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

text_input: force
	gnatmake $(GARGS) text_input -bargs $(BARGS) -largs $(LARGS)

vh_demo: force
	gnatmake $(GARGS) vh_demo -bargs $(BARGS) -largs $(LARGS)

dispatch: force
	gnatmake $(GARGS) dispatch -bargs $(BARGS) -largs $(LARGS)

res_demo: force
	../tools/awsres -q -r rdemo adains.png page3.html
	gnatmake $(GARGS) res_demo -bargs $(BARGS) -largs $(LARGS)

test_ldap: force
	gnatmake $(GARGS) test_ldap -bargs $(BARGS) -largs $(LARGS) -lldap

test_jabber: force
	gnatmake $(GARGS) test_jabber -bargs $(BARGS) -largs $(LARGS)

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
	-rm -f *.o *.ali *.exe b~*.* *.~*.*.~ rdemo*
	-rm runme-*.log
