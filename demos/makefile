
# $Id$

.SILENT: all runme agent

GARGS = -I../src -I/usr/tools/sockets -i

build: runme agent

runme:
	gnatmake $(GARGS) runme

agent:
	gnatmake $(GARGS) agent

clean:
	rm *.o *.ali *.exe b~runme.* b~agent.*
