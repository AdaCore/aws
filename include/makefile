
# $Id$

.SILENT:

force:

../src/aws-os_lib.adb:
	$(CP) ../config/aws-os_lib__gnat.adb ../src/aws-os_lib.adb

build_zlib: force
	echo "Building libz.a (Zlib library)"
	${MAKE} -C zlib

build: build_zlib ../src/aws-os_lib.adb
	$(GNATMAKE) -Pinclude -XAWS_BUILD=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}

setup:
	-$(MKDIR) -p ../.build/debug/include
	-$(MKDIR) -p ../.build/release/include
	${MAKE} -C zlib

clean:
	${MAKE} -C zlib clean
