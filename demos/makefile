
# $Id$

.SILENT: all auth runme agent ws clean

ifeq ($(MODE),ssl)
# For SSL support, note that -lgdi32 is Win32 specific and should be removed
# under others platforms.

GARGS = -I../src -I../ssl -I../include $(INCLUDES) -i -gnatwu -g
BARGS = -E
LARGS = -L../win32 -lssl -lcrypto -lRSAglue -lgdi32

else
# Without SSL support

GARGS = -I../src -I../include $(INCLUDES) -i -gnatwu -g
BARGS = -E
LARGS =
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

clean:
	-rm -f .o *.ali *.exe b~runme.* b~agent.*
