
# $Id$

.SILENT: all build build_ssl build_std distrib ssl_mode std_mode clean

GFLAGS	= -g -O2 -gnatwu -gnaty -I../ssl -I../include -I../src $(INCLUDES)

SOURCES	= soap.ads soap-types.adb soap-parameters.adb soap-message.adb \
	soap-message-xml.adb soap-message-response.adb \
	soap-message-response-error.adb soap-message-payload.adb \
	soap-client.adb

build:
	for file in $(SOURCES); do gnatmake $(GFLAGS) $$file; done

clean:
	-rm *.o *.ali *.~*.*.~
