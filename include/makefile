
# $Id$

.SILENT:

ifeq ($(DEBUG),1)
OFLAGS	= -g
else
OFLAGS	= -O2
endif

GARGS	= -q -u -I../src $(OFLAGS)

SOURCES	= strings_cutter.adb md5.adb \
	memory_streams.adb zlib-thin.adb zlib.adb \
	sha.ads sha-process_data.adb sha-strings.adb \
	ai302-containers-hash_tables.adb \
	ai302-containers-indefinite_hashed_maps.adb \
	ai302-containers-indefinite_ordered_sets.adb \
	ai302-containers-red_black_trees.adb \
	ai302-containers-prime_numbers.adb ai302.ads \
	ai302-containers.ads ai302-strings.ads \
	ai302-strings-hash.adb strings_maps.ads \
	charles-algorithms-generic_lower_bound.adb \
	charles-algorithms.ads charles.ads \
	charles-algorithms-generic_compare.adb \
	charles-algorithms-generic_includes.adb \
	charles-algorithms-generic_set_difference.adb \
	charles-algorithms-generic_set_intersection.adb \
	charles-algorithms-generic_set_symmetric_difference.adb

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
	$(GNATMAKE) -Pinclude -XAWS_BUILD=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}

gsetup:
	-$(MKDIR) -p ../.build/debug/include
	-$(MKDIR) -p ../.build/release/include
	${MAKE} -C zlib

gclean: clean
	$(RM) -fr ../.build/debug/include
	$(RM) -fr ../.build/release/include
