
# $Id$

.SILENT: all runme agent

# just comment or uncomment the right lines below.

# For SSL support, note that -lgdi32 is Win32 specific and should be removed
# under others platforms.

ifeq ($(MODE),ssl)
GARGS = -I../src -I../ssl -I/usr/Ada.Libraries/sockets -i -gnatwu -g
BARGS = -E
LARGS = -L../win32 -lssl -lcrypto -lRSAglue -lgdi32

else
# Without SSL support

GARGS = -I../src -I/usr/Ada.Libraries/sockets -i -gnatwu -g
BARGS = -E
LARGS =
endif

build: runme agent auth

runme:
	gnatmake $(GARGS) runme -bargs $(BARGS) -largs $(LARGS)

agent:
	gnatmake $(GARGS) agent -bargs $(BARGS) -largs $(LARGS)

auth:
	gnatmake $(GARGS) auth -bargs $(BARGS) -largs $(LARGS)

clean:
	-rm -f .o *.ali *.exe b~runme.* b~agent.*
