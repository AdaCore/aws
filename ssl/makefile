
# $Id$

.SILENT: all build clean wrappers

GARGS = -c $(INCLUDES) $(GFLAGS) -gnatwu

build:
	echo Building ssl-thin.ads
	gnatmake -q $(GARGS) ssl-thin.ads
	echo Building ssl-wrappers.adb
	gnatmake -q $(GARGS) ssl-wrappers.adb
	ar cr libnosslaws.a ssl-wrappers.o
	rm ssl-wrappers.o

clean:
	-rm *.ali *.o *.a
