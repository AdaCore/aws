
# $Id$

.SILENT: all build build_ssl build_std distrib ssl_mode std_mode clean

GCC	= gcc
SSL	= ../ssl
GFLAGS	= -g -O2 -gnatwu -gnaty -I../ssl -I../include $(INCLUDES)

SOURCES	= aws-client.adb aws-messages.adb aws-mime.adb aws-os_lib.adb \
	aws-parameters.adb aws-response.adb aws-server-get_status.adb \
	aws-server.adb aws-session.adb aws-session-control.adb \
	aws-status.adb aws-translator.adb aws-url.adb aws-utils.adb \
	aws-key_value.ads aws-log.adb aws-hotplug.adb aws-communication.adb \
	aws-communication-server.adb aws-communication-client.adb \
	aws-server-hotplug.adb aws-status-set.adb aws-parameters-set.adb \
	aws-services.ads aws-services-directory.adb aws-default.ads \
	aws-config.adb aws-config-ini.adb aws-config-set.adb

all:
	echo "Targets:"
	echo ""
	echo "build:        build library"
	echo ""
	echo "ssl_mode:     set SSL mode (Secure Socket Layer)"
	echo "std_mode:     set non-SSL mode [default]"
	echo ""
	echo "gnat_oslib:   OS_Lib implementation for GNAT only [default]"
	echo "posix_oslib:  OS_Lib implementation based on POSIX"
	echo "win32_oslib:  OS_Lib implementation for Win32 only"
	echo ""
	echo "To build with SSL support:"
	echo "           $ make build_ssl build"
	echo ""
	echo "To build with SSL support and OS_Lib based on POSIX:"
	echo "           $ make ssl_mode posix_oslib build"
	echo ""

build:
	(if ! [ -e aws-net.adb ]; then cp aws-net-std.adb aws-net.adb; fi)
	(if ! [ -e aws-os_lib.adb ]; then cp ../include/aws-os_lib-gnat.adb aws-os_lib.adb; fi)
	$(GCC) -c -O2 -g -I../include $(INCLUDES) aws-session.adb
	$(GCC) -c -O2 -g -I../include $(INCLUDES) aws-key_value.ads
	$(GCC) -c -O2 -g -I../include $(INCLUDES) aws-server-get_status.adb
	$(GCC) -c -O0 -g -I../include $(INCLUDES) aws-server-push.adb
	$(GCC) -c -O0 -g -I../include $(INCLUDES) aws-services-directory.adb
# no optimization because a bug in GNAT 3.13 (no problem with 3.14 though).
	for file in $(SOURCES); do gnatmake $(GFLAGS) $$file; done

ssl_mode:
	-rm *.o *.ali
	cp aws-net-ssl.adb aws-net.adb

std_mode:
	-rm *.o *.ali
	cp aws-net-std.adb aws-net.adb

gnat_oslib:
	-rm *.o *.ali
	cp ../include/aws-os_lib-gnat.adb aws-os_lib.adb

posix_oslib:
	-rm *.o *.ali
	cp ../include/aws-os_lib-posix.adb aws-os_lib.adb

win32_oslib:
	-rm *.o *.ali
	cp ../include/aws-os_lib-win32.adb aws-os_lib.adb

clean:
	-rm *.o *.ali aws-net.adb aws-os_lib.adb *.~*.*.~
