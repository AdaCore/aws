
# $Id$

.SILENT: all build clean wrappers

GARGS = -c $(INCLUDES) -gnatwu -O2

build:
	echo Building ssl-thin.ads
	gnatmake -q $(GARGS) ssl-thin.ads

wrappers:
	echo Building ssl-wrappers.adb
	gnatmake -q $(GARGS) ssl-wrappers.adb

clean:
	-rm *.ali *.o *.a
