
# $Id$

.SILENT:

GARGS	= $(GFLAGS) -I. -I../ssl -I../include $(INCLUDES)

GY_tmp  = $(patsubst -gnatwe,,$(GFLAGS)) -I../include
TGARGS = $(patsubst -gnaty3abcefhiklmnoprst,-gnaty3abcefhiklmnprt,$(GY_tmp))
# We do not want to check for separate spec for the demos (s option).

TMPLT_GARGS = $(TGARGS) -I. -I../ssl -I../include $(INCLUDES)

TMPLT_SOURCES	= templates_parser.adb templates_parser-input.adb \
	templates_parser-query.adb templates_parser-utils.adb

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
        aws-os_lib-definitions.ads \
	aws-parameters-set.adb aws-parameters.adb aws-resources-embedded.adb \
	aws-resources-files.adb aws-resources-streams.adb aws-resources.adb \
	aws-resources-streams-memory.ads aws-resources-streams-zlib.adb \
	aws-resources-streams-memory-zlib.adb \
	aws-resources-streams-disk.adb aws-resources-streams-disk-once.adb \
	aws-response-set.adb aws-response.adb aws-server-get_status.adb \
	aws-server-hotplug.adb aws-server-push.adb aws-server.adb \
	aws-services-directory.adb aws-services-dispatchers-method.adb \
	aws-services-dispatchers-uri.adb \
	aws-services-dispatchers-transient_pages.adb \
	aws-services-dispatchers-virtual_host.adb \
	aws-services-dispatchers.ads aws-services-page_server.adb \
	aws-services.ads aws-session-control.adb aws-session.adb \
	aws-smtp-client.adb aws-smtp.adb aws-status-set.adb aws-status.adb \
	aws-templates.ads aws-translator.adb aws-url-raise_url_error.adb \
	aws-url.adb aws-utils.adb aws.ads aws-exceptions.ads \
	aws-status-translate_table.adb aws-server-status.adb \
	aws-server-log.adb aws-pop.adb \
	aws-services-split_pages.adb aws-services-transient_pages.adb \
	aws-services-transient_pages-control.adb \
	aws-services-web_mail.adb aws-net-sets.adb aws-net-thin.ads \
	aws-services-dispatchers-timer.adb aws-net-ssl-certificate.adb \
	aws-client-hotplug.adb

ifdef XMLADA
SOURCES       := $(SOURCES) ../xsrc/aws-jabber.adb ../xsrc/aws-client-xml.ads \
	../xsrc/aws-client-xml-input_sources.adb
TMPLT_SOURCES := $(TMPLT_SOURCES) ../xsrc/templates_parser-xml.adb
endif

UNITS = $(sort $(basename $(SOURCES)))

TMPLT_UNITS = $(sort $(basename $(TMPLT_SOURCES)))

CONF_UNITS = aws-translator-conversion.adb aws-net-std.adb \
	aws-os_lib-definitions.ads

force:

$(UNITS): force
	echo Building $@
	$(GNATMAKE) -u $(GARGS) $@

$(TMPLT_UNITS): force
	echo Building $@
	$(GNATMAKE) -u $(TMPLT_GARGS) $@

build: $(CONF_UNITS) $(UNITS) $(TMPLT_UNITS)

aws-translator-conversion.adb:
	${MAKE} -C ../config setup

aws-os_lib-definitions.ads:
	$(MAKE) -C ../config setup

ifdef ADASOCKETS
aws-net-std.adb: ../config/aws-net-std__adasockets.adb
	$(CP) ../config/aws-net-std__adasockets.adb aws-net-std.adb
else
aws-net-std.adb: ../config/aws-net-std__gnat.adb
	$(CP) ../config/aws-net-std__gnat.adb aws-net-std.adb
endif

adasockets:
	-$(RM) -f *.o *.ali
	$(CP) ../config/aws-net-std__adasockets.adb aws-net-std.adb

gnatsockets:
	-$(RM) -f *.o *.ali
	$(CP) ../config/aws-net-std__gnat.adb aws-net-std.adb

gnat_oslib:
	-$(RM) -f *.o *.ali
	$(CP) ../config/aws-os_lib__gnat.adb aws-os_lib.adb

posix_oslib:
	-$(RM) -f *.o *.ali
	$(CP) ../config/aws-os_lib__posix.adb aws-os_lib.adb

win32_oslib:
	-$(RM) -f *.o *.ali
	$(CP) ../config/aws-os_lib__win32.adb aws-os_lib.adb

clean:
	-$(RM) -f *.o *.ali aws-os_lib.adb *.~*.*~
	-$(RM) -f aws-os_lib-definitions.ads
	-$(RM) -f aws-translator-conversion.adb
	-$(RM) -f aws-net-std.adb
	-$(RM) -f aws-soap.ad[sb]

#############################################################################
# Configuration for GNAT Projet Files

gsetup: gnatsockets gnat_oslib
	-$(MKDIR) -p ../.build/debug/obj
	-$(MKDIR) -p ../.build/debug/lib
	-$(MKDIR) -p ../.build/release/obj
	-$(MKDIR) -p ../.build/release/lib

gbuild: 
	$(GNATMAKE) -Psrc -XBuild=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}

gclean: clean
	$(RM) -fr ../.build/debug/lib ../.build/debug/obj
	$(RM) -fr ../.build/release/lib ../.build/release/obj
