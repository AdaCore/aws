
# $Id$

.SILENT: all build clean

GARGS = -c $(INCLUDES) -gnatwu -O2

build:
	gnatmake -q $(GARGS) ssl.adb

clean:
	-rm *.ali *.o *.a
