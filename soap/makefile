
# $Id$

.SILENT: all build build_ssl build_std distrib ssl_mode std_mode clean

GARGS	= $(GFLAGS) -I../ssl -I../include -I../src $(INCLUDES)

SOURCES	= soap.ads soap-types.adb soap-parameters.adb soap-message.adb \
	soap-message-xml.adb soap-message-response.adb \
	soap-message-response-error.adb soap-message-payload.adb \
	soap-client.adb soap-utils.adb soap-message-reader.adb

build:
	for file in $(SOURCES); do gnatmake $(GARGS) $$file; done

clean:
	-rm *.o *.ali *.~*.*.~
