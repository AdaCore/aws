
# $Id$

.SILENT: all build clean

GARGS = -c $(INCLUDES) -gnatwu -O2

build:
	gnatmake $(GARGS) ssl.adb

clean:
	-rm *.ali *.o
