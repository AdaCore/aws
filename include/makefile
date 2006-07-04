
.SILENT:

force:

build_zlib: force
	echo "Building libz.a (Zlib library)"
	${MAKE} -C zlib GALL_OPTIONS=$(GALL_OPTIONS)

build: build_zlib
	$(GNATMAKE) -Pinclude -XAWS_BUILD=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}

setup:
	-$(MKDIR) -p ../$(BDIR)/include/obj
	-$(MKDIR) -p ../$(BDIR)/include/lib

clean:
	${MAKE} -C zlib clean
