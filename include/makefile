
# $Id$

.SILENT:

force:

build_zlib: force
	echo "Building libz.a (Zlib library)"
	${MAKE} -C zlib

build: build_zlib
	$(GNATMAKE) -Pinclude -XAWS_BUILD=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}

setup:
	-$(MKDIR) -p ../$(BDIR)/include

clean:
	${MAKE} -C zlib clean
