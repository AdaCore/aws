
# $Id$

.SILENT:

# To add a new test:
#
#   1) create the appropriate Ada program (let's call it mytest.adb)
#   2) create the expected output mytest.out
#   3) add mytest to either :
#      RT_SOAP      : for SOAP tests using HTTP
#      RT_STD       : for standard tests using HTTP
#      RT_SOAP_SEC  : for SOAP tests using HTTPS
#      RT_STD_SEC   : for standard tests using HTTPS
#      RT_STD_ALONE : for standard tests, each in separate executable.
#      OTH_TESTS    : tests with specific target (not using standard procedure)
#      WSDL_TESTS   : WSDL tests
#      WSDL_ATESTS  : WSDL tests using the wsdl2aws -a option (Ada Name Style)
#      RT_SOAP_ASIS : ada2wsdl tests using a server
#      RT_SOAP_ASIS_PARSE : ada2wsdl tests using only the parser
#
#   Note, tests in the RT_SOAP, RT_STD, RT_STD_SEC, RT_SOAP_ASIS, WSDL_TESTS,
#   WSDL_ATESTS sections should not imply that name of executable file would be
#   the name of regression test main procedure. This tests would be linked
#   together to test_all executable file.

GARGS = -q -I../src -I../ssl -I../include -I../soap $(INCLUDES)
BARGS = -E

LARGS = $(LFLAGS) $(LIBS) -L../lib -lz

ifeq ($(MODE),ssl)
# With SSL support

LARGS := $(LARGS) -lssl -lcrypto

RT_SOAP_SEC = test_sec_soap
RT_STD_SEC = tsgetparam ssp ssback cert ssock2

else
# Without SSL support

LARGS := $(LARGS) -lnosslaws

endif

ifeq (${OS}, Windows_NT)
LARGS := $(LARGS) -lpoll
endif

# SOAP regression tests
RT_SOAP = soap1 soap2 soap3 soap4 workorder testxml test_soap \
	test_soap2 test_soap3 test_soap4 test_soap5 disp_soap $(RT_SOAP_SEC)

ifdef EXTRA_TESTS
RT_SOAP := test_speed $(RT_SOAP)
endif

# SOAP/WSDL test using ada2wsdl (ASIS package required)
# Names must start with wsdl_ and be followed by a single digit between 0
# and 9. Socket port is then 770<digit>.
RT_SOAP_ASIS = wsdl_1 wsdl_2 wsdl_3 wsdl_4 wsdl_5 wsdl_6 wsdl_7

RT_SOAP_ASIS_PARSE = wsdl_a wsdl_b

# Standard regression tests, could be only each in separate executable
RT_STD_ALONE = dispatch dispatch_method dispatch_vh upload2 upload3 \
	server_config

ifdef EXTRA_TESTS
RT_STD_ALONE := tlog $(RT_STD_ALONE)
endif

# Standard regression tests
RT_STD = turl tgetparam param dummy simple tclientto shutdown strm strm2 \
	tres2 upload unexph auth auth2 moved turl2 turl3 test_templates_if \
	head hval turl4 ctab file redirect hostip unexph2 sp sessions zres \
	dirop multiple tcom tmime sessions2 sessions3 huge_response sback \
	server_info append compress zapp zbig zfile zstrm split zstrm2 once \
	get_post zopen timer tmem file2 eres sock1 sock2 sessions4 wait \
	stream_response $(RT_STD_SEC)

STD_TESTS    = $(RT_STD)
ALONE_TESTS  = $(RT_STD_ALONE)
OTH_TESTS    = tres
WSDL_TESTS   =
WSDL_ATESTS  =
ASIS_TESTS   =
ASIS2_TESTS  =
WSDL2AWS     =

ifdef XMLADA
STD_TESTS   := $(STD_TESTS) $(RT_SOAP)
OTH_TESTS   := $(OTH_TESTS) check_mem
WSDL_TESTS  = test_wsdl test_wsdl2 test_wsdl4 interoplab_main googlesearch
WSDL_ATESTS = test_wsdl3
WSDL2AWS    = wsdl_parse_1 wsdl_parse_2 wsdl_parse_3 wsdl_parse_4
endif

ifdef ASIS
ASIS_TESTS   = $(RT_SOAP_ASIS)
ASIS2_TESTS  = $(RT_SOAP_ASIS_PARSE)
endif

ALL_EXEC = $(addsuffix $(EXEEXT), $(ALONE_TESTS) $(OTH_TESTS) \
	generate test_all)

TOGETHER = $(sort $(STD_TESTS) $(WSDL_TESTS) $(WSDL_ATESTS))

force:

run: init $(sort $(ALONE_TESTS) $(WSDL_TESTS) $(WSDL_ATESTS) \
	$(ASIS_TESTS) $(ASIS2_TESTS) $(WSDL2AWS) $(OTH_TESTS))
	gnatmake -m -g generate.adb $(GARGS)
	./generate $(TOGETHER) $(addsuffix _main, $(ASIS_TESTS))
	gnatmake -m -g test_all $(GARGS) -largs $(LARGS) -bargs -E
ifdef GDB_REGTESTS
	gdb --exec=test_all --command=cmd.gdb --batch
else
	./test_all
endif
	for file in $(TOGETHER); do \
	   diff -c -w $$file.out $$file.res 1>> alldifs 2>> alldifs; \
	done
ifdef ASIS
	for file in $(ASIS_TESTS); do \
	   diff -c -w $$file.out "$$file"_main.res 1>> alldifs 2>> alldifs; \
	done
endif
	cat alldifs
	diff -q -w alldifs alldifs.out

init:
	cp ../demos/cert.pem .
	touch -t 200102030505 ../icons/*.gif
	-mkdir ../icons/test-dir1 2> /dev/null
	-mkdir ../icons/test-dir2 2> /dev/null
	-mkdir ../icons/test-dir3 2> /dev/null
	touch -t 200401020404 ../icons/test-dir?
	echo "" > alldifs
	echo "### Every output after this line is a regression ###" >> alldifs
	-rm file[abc].txt* > /dev/null 2>&1
	echo "content of filea.txt" > filea.txt
	echo "content of fileb.txt" > fileb.txt
	echo "content of filec.txt" > filec.txt
	-$(GZIP) -2 fileb.txt > /dev/null 2>&1
	-$(GZIP) -9 filec.txt > /dev/null 2>&1
	echo "line 1, file 1" > file1.txt
	echo "line 2, file 1" >> file1.txt
	echo "line 1, file 2" > file2.txt
	echo "line 2, file 2" >> file2.txt
	echo "line 3, file 2" >> file2.txt
	echo "Number @_ONE_@ following by @_TWO_@." > file.tmplt
	../tools/awsres -q -r zresres file1.txt -z file2.txt file.tmplt
	rm -f *.res

$(ALONE_TESTS): force
	echo -n "Building " $@
	gnatmake -m -g $@ $(GARGS) -largs $(LARGS) -bargs -E
	echo ",  run it"
ifdef GDB_REGTESTS
	./tgdb $@ > $@.res
else
	./$@ > $@.res
endif
	-diff -c -w $@.out $@.res 1>> alldifs 2>> alldifs

$(WSDL_TESTS): force
	echo Preparing $@
	../tools/wsdl2aws -q -f -doc $@.wsdl

$(WSDL_ATESTS): force
	echo Preparing $@
	../tools/wsdl2aws -q -f -a -doc $@.wsdl

$(WSDL2AWS): force
	echo -n "Building " $@
	echo ",  run it"
	../tools/wsdl2aws -q -f -doc $@.wsdl | grep Message > $@.res
	-diff -c -w $@.out $@.res 1>> alldifs 2>> alldifs

$(ASIS_TESTS): force
	echo Preparing $@
	../tools/ada2wsdl -q -f $(INCLUDES) -I../include -I../soap -I../src \
		-a http://localhost:770`echo $@ | cut -c 6-` $@.ads -o $@.wsdl
	../tools/wsdl2aws -q -f -cb -types $@ $@.wsdl

$(ASIS2_TESTS): force
	rm -f $@.res
	echo "Running  " $@
	../tools/ada2wsdl -v -f $@.ads -o $@.wsdl > tmp 2>&1
	tail +3 tmp > $@.res
	rm tmp
	-diff -c -w $@.out $@.res 1>> alldifs 2>> alldifs

tres: force
	echo -n "Building " tres
	echo "line 1, file 1" > file1.html
	echo "line 2, file 1" >> file1.html
	echo "line 1, file 2" > file2.html
	echo "line 2, file 2" >> file2.html
	echo "line 3, file 2" >> file2.html
	echo "line 1, file 3" > file3.html
	echo "line 2, file 3" >> file3.html
	echo "line 1 : @_TAG1_@" > file.tmplt
	echo "line 2 : @_TAG2_@" >> file.tmplt
	echo "@@TABLE@@" >> file.tmplt
	echo "line 3.1 : @_TAG_V_@" >> file.tmplt
	echo "@@SECTION@@" >> file.tmplt
	echo "line 3.2 : @_TAG_V_@" >> file.tmplt
	echo "@@END_TABLE@@" >> file.tmplt
	echo "@@IF@@ @_COND_@" >> file.tmplt
	echo "   ok" >> file.tmplt
	echo "@@ELSE@@" >> file.tmplt
	echo "   nok" >> file.tmplt
	echo "@@END_IF@@" >> file.tmplt
	../tools/awsres -q -r tresres file1.html file2.html file.tmplt
	gnatmake -g tres $(GARGS) -largs $(LARGS) -bargs -E
	echo -n ",  run it"
	./tres 1245 > tres-1.res
	echo "line 3, file 1" >> file1.html
	echo "line 4, file 2" >> file2.html
	echo "line 3, file 3" >> file3.html
	echo ",  run it again"
	./tres 1246 > tres-2.res
	cat tres-1.res tres-2.res > tres.res
	-diff -c -w tres.out tres.res 1>> alldifs 2>> alldifs

# We change 2 values in aws-containers-tables.ads because its implementation
# is based on GNAT.Dynamic_Tables which uses a low-level realloc. This
# construct is not correctly handled by gnatmem. As this check is really
# target dependent what we do is to run twice the same program, each time with
# a different number of HTTP requests, then we compare the final non freed
# memory.

check_mem: force
	echo -n "Building " check_mem
	sed 's/4,/1000,/;s/8,/1000,/' ../src/aws-containers-tables.ads \
		> aws-containers-tables.ads
	gnatmake -g check_mem $(GARGS) -largs $(LARGS) -bargs -E -largs -lgmem
	rm -f aws-containers-tables.* aws*.ali aws*.o soap*.o soap*.ali
	echo -n ",  run it"
	./check_mem 2 1888 > check_mem.res
	gnatmem 3 -i gmem.out ./check_mem$(EXEEXT) 2 1248 > check_mem.run1
	grep "   Final" check_mem.run1 > run1
	echo ",  run it again"
	./check_mem 30 1889 >> check_mem.res
	gnatmem 3 -i gmem.out ./check_mem$(EXEEXT) 30 1249 > check_mem.run2
	grep "   Final" check_mem.run2 > run2
	-diff -c -w run1 run2 1>> alldifs 2>> alldifs

clean:
	-rm -f *.o *.ali b~*.ad* *~ .*~ *.res res.ad[sb] res2.ad[sb]
	-rm -f file.tmp file1.html file2.html file3.html file.tmplt tresres*
	-rm -f ?.upload.* strm-*.log cert.pem run1 run2 gmem.out $(ALL_EXEC)
	-rm -f alldifs *.log run? check_mem.run? hello_demo* *.tmp
	-rm -f stockquoteservice* stock_quote_service*
	-rm -f interoplab-* interoplab.ad* googlesearchservice*
	-rm -f wsdl_?_service* wsdl4*
	-rm -fr WSDL_6 wsdl_6.class file[abc].txt* zresres*
	-rm -f test_all.adb
	-rm -fr ../icons/test-dir1
	-rm -fr ../icons/test-dir2
	-rm -fr ../icons/test-dir3
