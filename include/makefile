
# $Id$

.SILENT: all build clean

GARGS	= -g -O2

SOURCES	= table_of_dynamic_keys_and_static_values_g.adb \
	table_of_static_keys_and_dynamic_values_g \
	table_of_static_keys_and_static_values_g \
	table_of_strings_and_static_values_g.adb \
	strings_cutter.adb templates_parser.adb

all:
	echo "Targets:"
	echo ""
	echo "build:     build component"
	echo "clean:     remove generated files"
	echo ""

build: 
	for file in $(SOURCES); do gnatmake $(GARGS) $$file; done

clean:
	-rm *.o *.ali
