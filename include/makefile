
# $Id$

.SILENT: all build clean

GFLAGS	= -g -O2

SOURCES	= avl_tree_generic.adb strings_cutter.adb templates_parser.adb

all:
	echo "Targets:"
	echo ""
	echo "build:     build component"
	echo "clean:     remove generated files"
	echo ""

build: 
	for file in $(SOURCES); do gnatmake $(GFLAGS) $$file; done

clean:
	-rm *.o *.ali
