# $Id$

.SILENT: build clean awsres

GARGS	= -q $(GFLAGS) -I../src $(INCLUDES)

build: awsres

force:

awsres: force
	echo Building awsres tools
	gnatmake $(GARGS) awsres -largs -s

clean:
	-rm *.o *.ali *.exe b~*.ad[sb]
