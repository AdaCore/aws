
# $Id$

.SILENT: checks tres check_mem

# To add a new test:
#
#   1) create the appropriate Ada program (let's call it mytest.adb)
#   2) create the expected output mytest.out
#   3) add mytest to either :
#      RT_SOAP     : for SOAP tests using HTTP
#      RT_STD      : for standard tests using HTTP
#      RT_SOAP_SEC : for SOAP tests using HTTPS
#      RT_STD_SEC  : for standard tests using HTTPS
#      OTH_TESTS   : tests with specific target (not using standard procedure)
#      WSDL_TESTS  : WSDL tests
#      WSDL_ATESTS : WSDL tests using the wsdl2aws -a option (Ada Name Style)

GARGS = -i -q -I../src -I../ssl -I../include -I../soap $(INCLUDES)
BARGS = -E

ifeq ($(MODE),ssl)
# With SSL support

ifeq (${OS}, Windows_NT)
LARGS = $(LFLAGS) $(LIBS) -L../ssl -L../win32 -lssl -lcrypto
else
LARGS = $(LFLAGS) $(LIBS) -lssl -lcrypto
endif

RT_SOAP_SEC = test_sec_soap
RT_STD_SEC = tsgetparam ssp

else
# Without SSL support

LARGS = $(LFLAGS) $(LIBS) -L../ssl -lnosslaws

endif

# SOAP regression tests
RT_SOAP = soap1 soap2 soap3 workorder testxml test_speed test_soap \
	$(RT_SOAP_SEC)

# Standard regression tests
RT_STD = turl tgetparam param dummy simple tclientto shutdown strm strm2 \
	tres2 upload unexph auth moved turl2 turl3 test_templates_if \
	head hval turl4 ctab sock1 file redirect hostip upload2 upload3 \
	tlog unexph2 sp sessions dirop $(RT_STD_SEC)

ifeq ($(XMLADA),)
STD_TESTS   = $(RT_STD)
OTH_TESTS   = tres
WSDL_TESTS  =
WSDL_ATESTS =
else
STD_TESTS   = $(RT_STD) $(RT_SOAP)
OTH_TESTS   = tres check_mem
WSDL_TESTS  = test_wsdl test_wsdl2 interoplab_main
WSDL_ATESTS = test_wsdl3 
endif

ALL_EXEC = $(addsuffix $(EXEEXT), $(STD_TESTS)) \
	$(addsuffix $(EXEEXT), $(OTH_TESTS)) \
	$(addsuffix $(EXEEXT), $(WSDL_TESTS)) \
	$(addsuffix $(EXEEXT), $(WSDL_ATESTS))

run : build checks

force:

build: init $(STD_TESTS) $(OTH_TESTS) $(WSDL_TESTS) $(WSDL_ATESTS)

init:
	cp ../demos/cert.pem .
	rm -f alldifs
	touch -t `date --date='Feb 3 04:05:00 GMT 2001' +%Y%m%d%H%M` \
	   ../icons/*.gif
	echo "" > alldifs
	echo "### Every output after this line is a regression ###" >> alldifs

$(STD_TESTS): force
	echo -n Building $@
	gnatmake -g $@ $(GARGS) -largs $(LARGS) -bargs -E
	echo ",  run it"
	rm -f $@.res
ifdef GDB_REGTESTS
	./tgdb $@ > $@.res
else
	./$@ > $@.res
endif
	-diff -c -w $@.out $@.res >> alldifs

$(WSDL_TESTS): force
	echo -n Building $@
	../tools/wsdl2aws -q -f -rpc $@.wsdl
	gnatmake -g $@ $(GARGS) -largs $(LARGS) -bargs -E
	echo ",  run it"
	rm -f $@.res
ifdef GDB_REGTESTS
	./tgdb $@ > $@.res
else
	./$@ > $@.res
endif
	-diff -c -w $@.out $@.res >> alldifs

$(WSDL_ATESTS): force
	echo -n Building $@
	../tools/wsdl2aws -q -f -a $@.wsdl
	gnatmake -g $@ $(GARGS) -largs $(LARGS) -bargs -E
	echo ",  run it"
	rm -f $@.res
ifdef GDB_REGTESTS
	./tgdb $@ > $@.res
else
	./$@ > $@.res
endif
	-diff -c -w $@.out $@.res >> alldifs

tres: force
	echo -n Building tres
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
	-diff -c -w tres.out tres.res >> alldifs

# We change 2 values in aws-containers-tables.ads because its implementation
# is based on GNAT.Dynamic_Tables which uses a low-level realloc. This
# construct is not correctly handled by gnatmem. As this check is really
# target dependent what we do is to run twice the same program, each time with
# a different number of HTTP requests, then we compare the final non freed
# memory.

check_mem: force
	echo -n Building check_mem
	sed 's/4,/1000,/;s/8,/1000,/' ../src/aws-containers-tables.ads \
		> aws-containers-tables.ads
	gnatmake -g check_mem $(GARGS) -largs $(LARGS) -bargs -E -largs -lgmem
	rm -f aws-containers-tables.*
	echo -n ",  run it"
	./check_mem 2 1248 > check_mem.res
	gnatmem 3 -i gmem.out check_mem$(EXEEXT) 2 1248 > check_mem.run1
	grep "   Final" check_mem.run1 > run1
	echo ",  run it again"
	./check_mem 30 1249 >> check_mem.res
	gnatmem 3 -i gmem.out check_mem$(EXEEXT) 30 1249 > check_mem.run2
	grep "   Final" check_mem.run2 > run2
	-diff -c -w run1 run2 >> alldifs

checks:
	cat alldifs

clean:
	-rm -f *.o *.ali b~*.ad* *.~*.*.~ *.res res.ad[sb] res2.ad[sb]
	-rm -f file.tmp file1.html file2.html file3.html file.tmplt tresres*
	-rm -f ?.upload.* strm-*.log cert.pem run1 run2 gmem.out $(ALL_EXEC)
	-rm -f alldifs *.log run? check_mem.run? hello_demo* *.tmp
	-rm -f stockquoteservice* stock_quote_service*
	-rm -f interoplab-* interoplab.ad*
