
# $Id$

.SILENT: all build clean

GARGS = -c -I/usr/Ada.Libraries/sockets -gnatwu -O2

build:
	gcc $(GARGS) ssl.adb
	gcc $(GARGS) ssl-thin.ads

clean:
	-rm *.ali *.o
