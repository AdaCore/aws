
# $Id$

.SILENT: all build distrib

GFLAGS	= -g -gnatwu -gnaty

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
