
# $Id$

.SILENT: all build clean

ifeq ($(DEBUG),1)
OFLAGS	= -g
else
OFLAGS	= -O2
endif

GARGS	= -q -u -I../src $(OFLAGS)

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

../src/aws-os_lib.adb:
	cp aws-os_lib-gnat.adb ../src/aws-os_lib.adb

build: ../src/aws-os_lib.adb $(UNITS)

clean:
	-rm -f *.o *.ali
