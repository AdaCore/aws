
# $Id$

.SILENT: all runme agent

GARGS = -I../src -I/usr/tools/sockets -i -gnatwu -g
BARGS = -E

build: runme agent auth

runme:
	gnatmake $(GARGS) runme -bargs $(BARGS)

agent:
	gnatmake $(GARGS) agent -bargs $(BARGS)

auth:
	gnatmake $(GARGS) auth -bargs $(BARGS)

clean:
	-rm -f .o *.ali *.exe b~runme.* b~agent.*
