
# $Id$

.SILENT: all build clean

GARGS = -u $(INCLUDES) $(GFLAGS)

force:

ssl-thin: force
	echo Building ssl-thin
	gnatmake -q $(GARGS) ssl
	gnatmake -q $(GARGS) ssl-thin

ssl-wrappers: force
	echo Building ssl-wrappers
	gnatmake -q $(GARGS) ssl-wrappers

libnosslaws.a:
	ar cr libnosslaws.a ssl-wrappers.o
	rm ssl-wrappers.o

build: ssl-thin ssl-wrappers libnosslaws.a

clean:
	-rm -f *.ali *.o *.a

#############################################################################
# Configuration for GNAT Projet Files

gbuild:
	gnatmake -Pssl -XAWS_BUILD=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}
	gnatmake -Pnossl -XAWS_BUILD=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}

gsetup:
	-mkdir -p ../.build/debug/ssl/obj
	-mkdir -p ../.build/debug/ssl/lib
	-mkdir -p ../.build/debug/ssl/lobj
	-mkdir -p ../.build/release/ssl/obj
	-mkdir -p ../.build/release/ssl/lib
	-mkdir -p ../.build/release/ssl/lobj

gclean: clean
	$(RM) -fr ../.build/debug/ssl
	$(RM) -fr ../.build/release/ssl
