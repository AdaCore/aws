
# $Id$

.SILENT: all build build_ssl build_std distrib ssl_mode std_mode clean

GARGS	= -q -u $(GFLAGS) -I../ssl -I../include -I../src $(INCLUDES)

SOURCES	= soap.ads soap-types.adb soap-parameters.adb soap-message.adb \
	soap-message-xml.adb soap-message-response.adb \
	soap-message-response-error.adb soap-message-payload.adb \
	soap-client.adb soap-utils.adb soap-message-reader.adb \
	soap-wsdl.adb soap-wsdl-parser.adb soap-wsdl-parameters.adb \
	soap-generator.adb soap-dispatchers.adb soap-dispatchers-callback.adb

UNITS = $(sort $(basename $(SOURCES)))

force:

$(UNITS): force
	echo Building $@;
	$(GNATMAKE) -u $(GARGS) $@;

build: $(UNITS)

clean:
	-$(RM) -f *.o *.ali b~*.ad* *.~*.*.~
