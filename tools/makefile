# $Id$

.SILENT: build clean awsres

GARGS	= $(GFLAGS) -I../src $(INCLUDES)

build: awsres

force:

awsres: force
	gnatmake $(GARGS) awsres -largs -s

clean:
	-rm *.o *.ali *.exe b~*.ad[sb]
