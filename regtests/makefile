
# $Id$

.SILENT: checks checks_header checks_soap checks_out checks_tres tres

# To add a new test:
#
#   1) create the appropriate Ada program (let's call it mytest.adb)
#   2) create the expected output mytest.out
#   3) add mytest to either RT_SOAP or RT_STD, RT_SOAP_SEC or RT_STD_SEC
#      RT_SOAP     : for SOAP tests using HTTP
#      RT_STD      : for standard tests using HTTP
#      RT_SOAP_SEC : for SOAP tests using HTTPS
#      RT_STD_SEC  : for standard tests using HTTPS

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
RT_STD_SEC = tsgetparam

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
	tlog unexph2 $(RT_STD_SEC)

ifeq ($(XMLADA),)
ALL_TESTS = $(RT_STD)
else
ALL_TESTS = $(RT_STD) $(RT_SOAP)
endif

ALL_EXEC = $(addsuffix $(EXEEXT), $(ALL_TESTS)) tres$(EXEEXT)

run : build checks

force:

build: init tres $(ALL_TESTS)

init:
	cp ../demos/cert.pem .
	rm -f alldifs

$(ALL_TESTS): force
	echo -n Building $@
	gnatmake -g $@ $(GARGS) -largs $(LARGS) -bargs -E
	echo ",  run it"
	rm -f $@.res
	./$@ > $@.res
	-diff -c -w $@.out $@.res >> alldifs

checks_out: checks_tres
	cat alldifs

tres: force
	echo -n Building tres;
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
	diff -c -w tres.out tres.res > alldifs

checks_tres:
	-diff -c -w tres.out tres.res

checks_header:
	echo ""
	echo "### Every output after this line means there is a regression ###"

checks: checks_header checks_out

clean:
	-rm -f *.o *.ali b~*.ad* *.~*.*.~ *.res res.ad[sb] res2.ad[sb]
	-rm -f file.tmp file1.html file2.html file3.html file.tmplt tresres*
	-rm -f ?.upload.* strm-*.log cert.pem $(ALL_EXEC)
