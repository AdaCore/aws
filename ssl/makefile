
# $Id$

.SILENT:

force:

build:
	$(GNATMAKE) -Pssl -XAWS_BUILD=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}
	$(GNATMAKE) -Pnossl -XAWS_BUILD=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}

setup:
	-$(MKDIR) -p ../.build/debug/ssl/obj
	-$(MKDIR) -p ../.build/debug/ssl/lib
	-$(MKDIR) -p ../.build/debug/ssl/lobj
	-$(MKDIR) -p ../.build/release/ssl/obj
	-$(MKDIR) -p ../.build/release/ssl/lib
	-$(MKDIR) -p ../.build/release/ssl/lobj

clean:
