
# $Id$

.SILENT: all build clean

GARGS = -u $(INCLUDES) $(GFLAGS)

force:

ssl-thin: force
	echo Building ssl-thin
	$(GNATMAKE) -q $(GARGS) ssl
	$(GNATMAKE) -q $(GARGS) ssl-thin

ssl-wrappers: force
	echo Building ssl-wrappers
	$(GNATMAKE) -q $(GARGS) ssl-wrappers

libnosslaws.a:
	$(AR) cr libnosslaws.a ssl-wrappers.o
	$(RM) ssl-wrappers.o

build: ssl-thin ssl-wrappers libnosslaws.a

clean:
	-$(RM) -f *.ali *.o *.a

#############################################################################
# Configuration for GNAT Projet Files

gbuild:
	$(GNATMAKE) -Pssl -XAWS_BUILD=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}
	$(GNATMAKE) -Pnossl -XAWS_BUILD=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}

gsetup:
	-$(MKDIR) -p ../.build/debug/ssl/obj
	-$(MKDIR) -p ../.build/debug/ssl/lib
	-$(MKDIR) -p ../.build/debug/ssl/lobj
	-$(MKDIR) -p ../.build/release/ssl/obj
	-$(MKDIR) -p ../.build/release/ssl/lib
	-$(MKDIR) -p ../.build/release/ssl/lobj

gclean: clean
	$(RM) -fr ../.build/debug/ssl
	$(RM) -fr ../.build/release/ssl
