
# $Id$

.SILENT: all build build_ssl build_std distrib ssl_mode std_mode clean

SSL	= ../ssl
GFLAGS	= -g -O2 -gnatwu -gnaty -I../ssl -I../include $(INCLUDES)

SOURCES	= aws-client.adb aws-messages.adb aws-mime.adb aws-os_lib.adb \
	aws-parameters.adb aws-response.adb \
	aws-server-get_status.adb aws-server.adb aws-session.adb \
	aws-status.adb aws-translater.adb aws-url.adb aws-utils.adb \
	aws-key_value.adb aws-log.adb aws-hotplug.adb aws-communication.adb \
	aws-communication-server.adb aws-communication-client.adb \
	aws-server-hotplug.adb

all:
	echo "Targets:"
	echo ""
	echo "build:     build library MODE=[std/ssl]"
	echo "           $ make build MODE=ssl"
	echo ""

build: build_$(MODE)
	gcc -c -O2 -g -I../include $(INCLUDES) aws-session.adb
	gcc -c -O2 -g -I../include $(INCLUDES) aws-key_value.adb
	gcc -c -O2 -g -I../include $(INCLUDES) aws-server-get_status.adb
	for file in $(SOURCES); do gnatmake $(GFLAGS) $$file; done

build_ssl:
	-rm *.o *.ali build_std
	echo > build_ssl
	cp aws-net-ssl.adb aws-net.adb

build_std:
	-rm *.o *.ali build_ssl
	echo > build_std
	cp aws-net-std.adb aws-net.adb

clean:
	-rm *.o *.ali gnat.adc build_ssl aws-net.adb
