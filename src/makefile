
# $Id$

.SILENT:

force:

aws-translator-conversion.adb:
	${MAKE} -C ../config setup

aws-os_lib-definitions.ads: ../config/src/check_config.c
	$(MAKE) -C ../config setup

ifdef ADASOCKETS
aws-net-std.adb: ../config/aws-net-std__adasockets.adb
	$(CP) ../config/aws-net-std__adasockets.adb aws-net-std.adb
else
aws-net-std.adb: ../config/aws-net-std__gnat.adb
	$(CP) ../config/aws-net-std__gnat.adb aws-net-std.adb
endif

adasockets:
	-$(RM) -f aws-net-std.o
	$(CP) ../config/aws-net-std__adasockets.adb aws-net-std.adb

gnatsockets:
	-$(RM) -f aws-net-std.o
	$(CP) ../config/aws-net-std__gnat.adb aws-net-std.adb

gnat_oslib:
	-$(RM) -f aws-os_lib.o
	$(CP) ../config/aws-os_lib__gnat.adb aws-os_lib.adb

posix_oslib:
	-$(RM) -f aws-os_lib.o
	$(CP) ../config/aws-os_lib__posix.adb aws-os_lib.adb

win32_oslib:
	-$(RM) -f aws-os_lib.o
	$(CP) ../config/aws-os_lib__win32.adb aws-os_lib.adb

setup: gnatsockets gnat_oslib
	-$(MKDIR) -p ../.build/debug/obj
	-$(MKDIR) -p ../.build/debug/lib
	-$(MKDIR) -p ../.build/release/obj
	-$(MKDIR) -p ../.build/release/lib

build:
	$(GNATMAKE) -Psrc -XBuild=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}

clean:
	-$(RM) aws-os_lib.adb aws-net-std.adb aws-translator-conversion.adb
	-$(RM) aws-os_lib-definitions.ads
