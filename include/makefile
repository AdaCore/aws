
# $Id$

.SILENT: all build clean

GARGS	= -q -u -g -O2 -I../src

SOURCES	= table_of_dynamic_keys_and_static_values_g.adb \
	table_of_static_keys_and_dynamic_values_g \
	table_of_static_keys_and_static_values_g \
	table_of_strings_and_static_values_g.adb \
	strings_cutter.adb md5.adb \
	templates_parser.adb templates_parser-input.adb \
	sha.ads sha-process_data.adb sha-strings.adb

UNITS = $(sort $(basename $(SOURCES)))

force:

$(UNITS): force
	echo Building $@
	gnatmake -u $(GARGS) $@

all:
	echo "Targets:"
	echo ""
	echo "build:     build component"
	echo "clean:     remove generated files"
	echo ""

check_oslib:
	(if ! [ -e ../src/aws-os_lib.adb ]; then \
	   cp aws-os_lib-gnat.adb ../src/aws-os_lib.adb; fi)

build: check_oslib $(UNITS)

clean:
	-rm *.o *.ali
