
# $Id$

.SILENT: all build clean

GARGS = -u $(INCLUDES) $(GFLAGS)

force:

ssl-thin: force
	echo Building ssl-thin
	gnatmake -q $(GARGS) ssl-thin

ssl-wrappers: force
	echo Building ssl-wrappers
	gnatmake -q $(GARGS) ssl-wrappers

libnosslaws.a:
	ar cr libnosslaws.a ssl-wrappers.o
	rm ssl-wrappers.o

build: ssl-thin ssl-wrappers libnosslaws.a

clean:
	-rm *.ali *.o *.a
