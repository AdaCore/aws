
# $Id$

.SILENT: all build build_ssl build_std distrib ssl_mode std_mode clean

SSL	= ../ssl
GFLAGS	= -g -O2 -gnatwu -gnaty -I../ssl -I../include

SOURCES	= aws-client.adb aws-messages.adb aws-response.adb aws-server.adb \
	aws-status.adb aws-translater.adb aws-url.adb

all:
	echo "Targets:"
	echo ""
	echo "build:     build library MODE=[std/ssl"
	echo "           $ make build MODE=ssl"
	echo ""

build: build_$(MODE)
	gcc -c -O2 -g -I../include aws-session.adb
	gcc -c -O2 -g -I../include aws-server-get_status.adb
	for file in $(SOURCES); do gnatmake $(GFLAGS) $$file; done

build_ssl:
	-rm *.o *.ali build_std
	echo > build_ssl
	echo 'pragma Source_File_Name' > gnat.adc
	echo '   (aws.net,' >> gnat.adc
	echo '    Body_File_Name => "aws-net-ssl.adb");' >> gnat.adc

build_std:
	-rm *.o *.ali build_ssl
	echo > build_std
	echo 'pragma Source_File_Name' > gnat.adc
	echo '   (aws.net,' >> gnat.adc
	echo '    Body_File_Name => "aws-net-std.adb");' >> gnat.adc

clean:
	-rm *.o *.ali gnat.adc build_ssl
