
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
LARGS = $(LFLAGS) $(LIBS) -L../ssl -lssl -lcrypto
else
LARGS = $(LFLAGS) $(LIBS) -lssl -lcrypto
endif

RT_SOAP_SEC = test_sec_soap
RT_STD_SEC = tsgetparam

else
# Without SSL support

LARGS = $(LFLAGS) $(LIBS) ../ssl/ssl-wrappers.o

endif

# SOAP regression tests
RT_SOAP = soap1 soap2 soap3 workorder testxml test_speed test_soap \
	$(RT_SOAP_SEC)

# Standard regression tests
RT_STD = turl tgetparam param dummy simple tclientto shutdown \
	tres2 upload unexph auth moved turl2 turl3 strm test_templates_if \
	head hval turl4 ctab $(RT_STD_SEC)

ifeq ($(XMLADA),)
ALL_TESTS = $(RT_STD)
else
ALL_TESTS = $(RT_STD) $(RT_SOAP)
endif

run : build checks

force:

build: tres
	cp ../demos/cert.pem .
	for file in $(ALL_TESTS); do \
	   echo -n Building $$file; \
	   gnatmake -g $$file $(GARGS) -largs $(LARGS) -bargs -E; \
	   echo ",  run it"; \
	   ./$$file > $$file.res; \
	done

checks_out: checks_tres
	for file in $(ALL_TESTS); do \
	   diff -c -w $$file.out $$file.res; \
	done

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
	./tres > tres-1.res
	echo "line 3, file 1" >> file1.html
	echo "line 4, file 2" >> file2.html
	echo "line 3, file 3" >> file3.html
	echo ",  run it again"
	./tres > tres-2.res
	cat tres-1.res tres-2.res > tres.res

checks_tres:
	-diff -c -w tres.out tres.res

checks_header:
	echo ""
	echo "### Every output after this line means there is a regression ###"

checks: checks_header checks_out

clean:
	-rm *.o *.ali b~*.ad* *.~*.*.~ *.res *.exe res.ad[sb] res2.ad[sb]
	-rm file file1.html file2.html file3.html file.tmplt tresres*
	-rm ?.upload.* strm-*.log cert.pem
