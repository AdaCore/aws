
# $Id$

.SILENT: all build distrib

GFLAGS	= -I/usr/tools/sockets -g -gnatwu

all:
	echo "Targets:"
	echo ""
	echo "build:     build library"
	echo ""

build:
	gnatmake $(GFLAGS) aws
	for file in *.adb; do gnatmake $(GFLAGS) $$file; done

clean:
	-rm *.o *.ali *.exe aws.tar*
