
# $Id$

.SILENT: all build clean

GARGS = -c -I/usr/Ada.Libraries/sockets -gnatwu -O2

build:
	gnatmake $(GARGS) ssl.adb

clean:
	-rm *.ali *.o
