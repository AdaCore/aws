
# $Id$

.SILENT: build ssl_mode std_mode gnat_oslib posix_oslib win32_oslib clean
.SILENT: aws-translator-conversion.adb

GCC	= gcc
GARGS	= -q $(GFLAGS) -I. -I../ssl -I../include $(INCLUDES)

SOURCES	= aws-client.adb aws-messages.adb aws-mime.adb aws-os_lib.adb \
	aws-parameters.adb aws-response.adb aws-server-get_status.adb \
	aws-server.adb aws-session.adb aws-session-control.adb \
	aws-status.adb aws-translator.adb aws-url.adb aws-utils.adb \
	aws-log.adb aws-hotplug.adb aws-communication.adb \
	aws-communication-server.adb aws-communication-client.adb \
	aws-server-hotplug.adb aws-status-set.adb aws-parameters-set.adb \
	aws-services.ads aws-services-directory.adb aws-default.ads \
	aws-config.adb aws-config-ini.adb aws-config-set.adb \
	aws-net.adb aws-net-stream_io.adb aws-net-std.adb aws-net-ssl.adb \
	aws-net-buffered.adb aws-services-page_server.adb \
	aws-templates.ads aws-dispatchers.adb aws-dispatchers-callback.adb \
	aws-services-dispatchers.ads aws-services-dispatchers-uri.adb \
	aws-services-dispatchers-method.adb \
	aws-services-dispatchers-virtual_host.adb \
	aws-server-push.adb \
	aws-smtp.adb aws-smtp-client.adb \
	aws-ldap.ads aws-ldap-thin.adb aws-ldap-client.adb \
	aws-headers.adb aws-headers-values.adb \
	aws-containers.ads aws-containers-key_value.ads \
	aws-containers-tables.adb aws-containers-tables-set.adb

ifdef XMLADA
SOURCES := $(SOURCES) aws-jabber.adb
endif

build: aws-translator-conversion.adb
# compile aws-services-directory.adb without optimisation
# work around a bug in GNAT 3.14, 3.15.
	echo Building aws-services-directory.adb
	gnatmake -u $(GARGS) -O0 aws-services-directory.adb -largs $(LFLAGS);
	for file in $(SOURCES); do \
	   echo Building $$file; \
	   gnatmake -u $(GARGS) $$file -largs $(LFLAGS); \
	done

aws-translator-conversion.adb:
	echo Setup conversion support
	gnatmake -q test_uconv
	./test_uconv

gnat_oslib:
	-rm -f *.o *.ali
	cp ../include/aws-os_lib-gnat.adb aws-os_lib.adb

posix_oslib:
	-rm -f *.o *.ali
	cp ../include/aws-os_lib-posix.adb aws-os_lib.adb

win32_oslib:
	-rm -f *.o *.ali
	cp ../include/aws-os_lib-win32.adb aws-os_lib.adb

clean:
	-rm -f *.o *.ali aws-os_lib.adb *.~*.*~
	-rm aws-translator-conversion.adb
