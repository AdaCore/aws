
# $Id$

.SILENT: all runme agent

GARGS = -I../src -I/usr/tools/sockets -i

build: runme agent auth

runme:
	gnatmake $(GARGS) runme

agent:
	gnatmake $(GARGS) agent

auth:
	gnatmake $(GARGS) auth

clean:
	rm *.o *.ali *.exe b~runme.* b~agent.*
