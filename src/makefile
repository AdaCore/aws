
# $Id$

.SILENT: build gnat_oslib posix_oslib win32_oslib adasockets gnatsockets clean
.SILENT: aws-translator-conversion.adb

GARGS	= $(GFLAGS) -I. -I../ssl -I../include $(INCLUDES)

SOURCES	= aws-client.adb aws-communication-client.adb \
	aws-communication-server.adb aws-communication.adb \
	aws-config-ini.adb aws-config-set.adb aws-config.adb \
	aws-containers-key_value.ads aws-containers-tables-set.adb \
	aws-containers-tables.adb aws-containers.ads aws-default.ads \
	aws-digest.adb aws-dispatchers-callback.adb aws-dispatchers.adb \
	aws-headers-set.adb aws-headers-values.adb aws-headers.adb \
	aws-hotplug-get_status.adb aws-hotplug.adb aws-ldap-client.adb \
	aws-ldap-thin.adb aws-ldap.ads aws-log.adb aws-messages.adb \
	aws-mime.adb aws-net-buffered.adb aws-net-ssl.adb aws-net-std.adb \
	aws-net-stream_io.adb aws-net.adb aws-os_lib.adb \
	aws-parameters-set.adb aws-parameters.adb aws-resources-embedded.adb \
	aws-resources-files.adb aws-resources-streams.adb aws-resources.adb \
	aws-response-set.adb aws-response.adb aws-server-get_status.adb \
	aws-server-hotplug.adb aws-server-push.adb aws-server.adb \
	aws-services-directory.adb aws-services-dispatchers-method.adb \
	aws-services-dispatchers-uri.adb \
	aws-services-dispatchers-virtual_host.adb \
	aws-services-dispatchers.ads aws-services-page_server.adb \
	aws-services.ads aws-session-control.adb aws-session.adb \
	aws-smtp-client.adb aws-smtp.adb aws-status-set.adb aws-status.adb \
	aws-templates.ads aws-translator.adb aws-url-raise_url_error.adb \
	aws-url.adb aws-utils.adb aws.ads aws-exceptions.ads \
	aws-status-translate_table.adb aws-server-status.adb

ifdef XMLADA
SOURCES := $(SOURCES) ../xsrc/aws-jabber.adb
endif

UNITS = $(sort $(basename $(SOURCES)))

CONF_UNITS = aws-translator-conversion.adb aws-net-std.adb

force:

$(UNITS): force
	echo Building $@
	gnatmake -u $(GARGS) $@

build: $(CONF_UNITS) $(UNITS)

aws-translator-conversion.adb:
	${MAKE} -C ../config setup

aws-net-std.adb:
ifdef ADASOCKETS
	cp ../config/aws-net-std__adasockets.adb aws-net-std.adb
else
	cp ../config/aws-net-std__gnat.adb aws-net-std.adb
endif

adasockets:
	-rm -f *.o *.ali
	cp ../config/aws-net-std__adasockets.adb aws-net-std.adb

gnatsockets:
	-rm -f *.o *.ali
	cp ../config/aws-net-std__gnat.adb aws-net-std.adb

gnat_oslib:
	-rm -f *.o *.ali
	cp ../config/aws-os_lib__gnat.adb aws-os_lib.adb

posix_oslib:
	-rm -f *.o *.ali
	cp ../config/aws-os_lib__posix.adb aws-os_lib.adb

win32_oslib:
	-rm -f *.o *.ali
	cp ../config/aws-os_lib__win32.adb aws-os_lib.adb

clean:
	-rm -f *.o *.ali aws-os_lib.adb *.~*.*~
	-rm -f aws-translator-conversion.adb
	-rm -f aws-net-std.adb
	-rm -f aws-soap.ad[sb]
