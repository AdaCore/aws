
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
	sha.ads sha-process_data.adb sha-strings.adb \
	memory_streams.adb zlib-thin.adb zlib.adb

UNITS = $(sort $(basename $(SOURCES)))

force:

$(UNITS): force
	echo Building $@
	$(GNATMAKE) -u $(GARGS) $@

all:
	echo "Targets:"
	echo ""
	echo "build:     build component"
	echo "clean:     remove generated files"
	echo ""

../src/aws-os_lib.adb:
	$(CP) ../config/aws-os_lib__gnat.adb ../src/aws-os_lib.adb

build_zlib: force
	echo "Building libz.a (Zlib library)"
	${MAKE} -C zlib

build: build_zlib ../src/aws-os_lib.adb $(UNITS)

clean:
	-$(RM) -f *.o *.ali
	${MAKE} -C zlib clean

#############################################################################
# Configuration for GNAT Projet Files

gbuild:
	$(GNATMAKE) -Psrc_include -XAWS_BUILD=${PRJ_BUILD} \
		-XXMLADA=${PRJ_XMLADA}

gsetup:
	-$(MKDIR) -p ../.build/debug/include
	-$(MKDIR) -p ../.build/release/include

gclean: clean
	$(RM) -fr ../.build/debug/include
	$(RM) -fr ../.build/release/include
