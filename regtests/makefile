
# $Id$

.SILENT:

# To add a new test:
#
#   1) Create the appropriate Ada program (let's call it mytest.adb)
#      Add a tag line in mytest.adb
#      --  ~ MAIN [<options>]
#      <options> can be:
#         [STD]           : test requires no external support
#         [SSL]           : test requires SSL
#         [ASIS]          : test requires ASIS
#         [SOAP]          : test requires SOAP
#         [ASIS+SOAP+SSL] : test requires ASIS, SOAP and SSL
#                           it is possible to specify only 2 options, the
#                           ordering is important though.
#
#   2) Create the expected output mytest.out
#
#   3) If the test needs to be executed in it's own executable add it to:
#      ASIS_TESTS     : test using ada2wsdl tool to create test case
#      ASIS2_TESTS    : test using ada2wsdl tool only
#      ALONE_TESTS    : if a standard test but needs to be run separately
#      OTH_TESTS      : if a test that has a specific target in makefile
#      WSDL_TESTS     : for WSDL tests
#      WSDL_ATESTS    : for WSDL tests using Ada name style
#      WSDL2AWS_TESTS : test using only wsdl2aws tool

include makefile.lst

ADA2WSDL = ../$(BDIR)/tools/ada2wsdl-main
WSDL2AWS = ../$(BDIR)/tools/wsdl2aws
AWSRES   = ../$(BDIR)/tools/awsres

# Standard regression tests that needs to be run in a separate executable

ALONE_TESTS    = dispatch1 dispatch_method dispatch_vh upload2 \
	upload3 server_config tlog

OTH_TESTS      = tres

ifeq (${XMLADA}, true)
XMLADA_UNICODE = ${shell gnat list -Pxmlada -s unicode.ads}
XMLADA_SRC     = ${subst \,/,${dir $(XMLADA_UNICODE)}}

OTH_TESTS     := $(OTH_TESTS) check_mem
WSDL_TESTS     = test_wsdl test_wsdl2 test_wsdl4 test_wsdl6 \
	interoplab_main1 googlesearch datafeed_service anytype
WSDL_ATESTS    = test_wsdl3 test_wsdl5
WSDL2AWS_TESTS = wsdl_parse_1 wsdl_parse_2 wsdl_parse_3	wsdl_parse_4
endif

ifdef ASIS
# SOAP/WSDL test using ada2wsdl (ASIS package required)
# Names must start with wsdl_ and be followed by a single digit between 0
# and 9. Socket port is then 770<digit>.

ASIS_TESTS   = wsdl_1 wsdl_2 wsdl_3 wsdl_4 wsdl_5 wsdl_6 wsdl_7 wsdl_8
ASIS2_TESTS  = wsdl_a wsdl_b register
endif

SPEC_TESTS   = $(ALONE_TESTS) $(OTH_TESTS)

ALL_EXEC = $(addsuffix $(EXEEXT), $(SPEC_TESTS) generate test_all)

SRC_WSDL_TESTS     = ${addsuffix .adb, $(WSDL_TESTS)}
SRC_WSDL_ATESTS    = ${addsuffix .adb, $(WSDL_ATESTS)}
SRC_WSDL2AWS_TESTS = ${addsuffix .adb, $(WSDL2AWS_TESTS)}
SRC_SPEC_TESTS	   = ${addsuffix .adb, $(SPEC_TESTS)}

ifeq (${EXTRA_TESTS}, false)
FILTER_OUT	= test_speed.adb
endif

S_TOGETHER	= $(sort $(TESTS) $(SRC_WSDL_TESTS) $(SRC_WSDL_ATESTS))
F_TOGETHER 	= $(filter-out $(FILTER_OUT) $(SRC_SPEC_TESTS) \
			generate.adb, $(S_TOGETHER))
TOGETHER 	= $(basename $(F_TOGETHER))

force:

gen_main: gen_sources
	echo Generating the main driver
	$(GNATMAKE) -q -Pregtests -XTest_Mode=Generator \
		-XBuild=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}
	./generate $(TOGETHER)

gen_sources: $(sort $(WSDL_TESTS) $(WSDL_ATESTS) $(ASIS_TESTS) \
	$(ASIS2_TESTS) $(WSDL2AWS_TESTS))

build_drivers: gen_main
	echo "Building drivers (takes some time)"
	$(GNATMAKE) -q -m -Pregtests -XTest_Mode=Grouped \
		-XBuild=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}

empty:=
space:= $(empty) $(empty)

REGEXP = $(subst $(space),"\\|",$(SRC_SPEC_TESTS))

build:

run: init build_drivers $(SPEC_TESTS)
ifdef GDB_REGTESTS
	gdb --exec=test_all --command=cmd.gdb --batch
else
	./test_all
endif
	-for file in $(TOGETHER); do \
	   diff -c -w $$file.out $$file.res 1>> alldifs 2>> alldifs; \
	done
	cat alldifs
	diff -q -w alldifs alldifs.out

init:
	cp ../demos/cert.pem .
	-cp -pr ../web_elements/icons . 2> /dev/null
	-mkdir icons/test-dir1 2> /dev/null
	-mkdir icons/test-dir2 2> /dev/null
	-mkdir icons/test-dir3 2> /dev/null
	(TZ=UTC; export TZ; \
	touch -t 200102030405 icons/*.gif; \
	touch -t 200411121314 icons/*.png; \
	touch -t 200401020304 icons/test-dir?)
	echo "" > alldifs
	echo "### Every output after this line is a regression ###" >> alldifs
	-rm file[abc].txt* > /dev/null 2>&1
	echo Preparing zres
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
	echo "Number @_ONE_@ following by @_TWO_@." > filez.tmplt
	$(AWSRES) -q -r zresres file1.txt -z file2.txt filez.tmplt
	echo Preparing tres
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
	$(AWSRES) -q -r tresres file1.html file2.html file.tmplt
	rm -f *.res

$(ALONE_TESTS): force
	echo "Running  " $@
ifdef GDB_REGTESTS
	./tgdb $@ > $@.res
else
	./$@ > $@.res
endif
	-diff -c -w $@.out $@.res 1>> alldifs 2>> alldifs

$(WSDL_TESTS): force
	echo Preparing $@
	$(WSDL2AWS) -q -f -doc $@.wsdl

$(WSDL_ATESTS): force
	echo Preparing $@
	$(WSDL2AWS) -q -f -a -doc $@.wsdl

$(WSDL2AWS_TESTS): force
	echo "Running  " $@
	-$(WSDL2AWS) -q -f -doc $@.wsdl > $@.res
	-diff -c -w $@.out $@.res 1>> alldifs 2>> alldifs

$(ASIS_TESTS): force
	echo Preparing $@
	$(ADA2WSDL) -q -f $(INCLUDES) -I../include -I../include/ai302 \
		-I../soap -I../src -I../xsrc -I../ssl -I$(XMLADA_SRC) \
		-a http://localhost:770`echo $@ | cut -c 6-` $@.ads -o $@.wsdl
	$(WSDL2AWS) -q -f -cb -types $@ $@.wsdl

$(ASIS2_TESTS): force
	rm -f $@.res
	echo "Running  " $@
	$(ADA2WSDL) -v -f $@.ads -o $@.wsdl > tmp 2>&1
	tail +3 tmp > $@.res
	rm tmp
	-diff -c -w $@.out $@.res 1>> alldifs 2>> alldifs

tres: force
	echo -n "Running  " tres
	./tres 1245 > tres-1.res
	echo "line 3, file 1" >> file1.html
	echo "line 4, file 2" >> file2.html
	echo "line 3, file 3" >> file3.html
	echo ",  run it again"
	./tres 1246 > tres-2.res
	cat tres-1.res tres-2.res > tres.res
	-diff -c -w tres.out tres.res 1>> alldifs 2>> alldifs

# As this check is really target dependent what we do is to run twice the same
# program, each time with a different number of HTTP requests, then we compare
# the final non freed memory.

check_mem: force
	echo -n "Running  " check_mem
	./check_mem 2 1888 > check_mem.res
	gnatmem 3 -i gmem.out ./check_mem$(EXEEXT) 2 1248 > check_mem.run1
	grep "   Final" check_mem.run1 > run1
	echo ",  run it again"
	./check_mem 30 1889 >> check_mem.res
	gnatmem 3 -i gmem.out ./check_mem$(EXEEXT) 30 1249 > check_mem.run2
	grep "   Final" check_mem.run2 > run2
	-diff -c -w run1 run2 1>> alldifs 2>> alldifs

setup:
# Get MAIN and standard sources
	-$(GREP) "\-\-  ~ MAIN" *.adb > all_mains.lst
	-$(GREP) "\[STD\]" all_mains.lst | $(GREP) -v $(REGEXP) > s_tstd.lst
	-$(CP) s_tstd.lst s_mains.lst
	-$(GREP) "\[SOAP\]" all_mains.lst > s_tsoap.lst
	-$(GREP) "\[ASIS+SOAP\]" all_mains.lst >> s_tsoap.lst
	-$(GREP) "\[SSL\]" all_mains.lst > s_tsec.lst
	-$(GREP) "\[SOAP+SSL\]" all_mains.lst > s_tsecsoap.lst
	-$(GREP) "\[ASIS+SSL\]" all_mains.lst >> s_tsecsoap.lst
ifdef ASIS
	-$(GREP) "\[ASIS\]" all_mains.lst >> s_mains.lst
endif
ifeq (${XMLADA}, true)
	-$(GREP) "\[SOAP\]" all_mains.lst >> s_mains.lst
endif
ifeq ($(SOCKET),ssl)
	-$(GREP) "\[SSL\]" all_mains.lst >> s_mains.lst
endif
ifeq (${XMLADA}, true)
 ifdef ASIS
	-$(GREP) "\[ASIS+SOAP\]" all_mains.lst >> s_mains.lst
  ifeq ($(SOCKET),ssl)
	-$(GREP) "\[ASIS+SOAP+SSL\]" all_mains.lst >> s_mains.lst
  endif
 else
  ifeq ($(SOCKET),ssl)
	-$(GREP) "\[SOAP+SSL\]" all_mains.lst >> s_mains.lst
  endif
 endif
else
 ifdef ASIS
  ifeq ($(SOCKET),ssl)
	-$(GREP) "\[ASIS+SSL\]" all_mains.lst >> s_mains.lst
  endif
 endif
endif
	echo "TESTS=\\" > makefile.lst
	$(AWK) 'BEGIN{FS=":"}{printf ("%s \\\n",$$1)}' s_mains.lst \
		>> makefile.lst
# Now write .gpr
	echo "project regtests_lst is" > regtests_lst.gpr
	echo "" >> regtests_lst.gpr
	echo "   for Source_Dirs use ();" >> regtests_lst.gpr
# T_STD
	echo "" >> regtests_lst.gpr
	echo "   T_STD := (" >> regtests_lst.gpr
	$(AWK) 'BEGIN{FS=":";n=0} \
		n==1{printf (",\n\"%s\"",$$1)} \
		n==0{printf ("\"%s\"",$$1);n=1}' s_tstd.lst >> regtests_lst.gpr
	echo "   );" >> regtests_lst.gpr
# T_SEC
	echo "" >> regtests_lst.gpr
	echo "   T_SEC := (" >> regtests_lst.gpr
	$(AWK) 'BEGIN{FS=":";n=0} \
		n==1{printf (",\n\"%s\"",$$1)} \
		n==0{printf ("\"%s\"",$$1);n=1}' s_tsec.lst >> regtests_lst.gpr
	echo "   );" >> regtests_lst.gpr
# T_SEC_SOAP
	echo "" >> regtests_lst.gpr
	echo "   T_SEC_SOAP := (" >> regtests_lst.gpr
	$(AWK) 'BEGIN{FS=":";n=0} \
		n==1{printf (",\n\"%s\"",$$1)} \
		n==0{printf ("\"%s\"",$$1);n=1}' \
			s_tsecsoap.lst >> regtests_lst.gpr
	echo "   );" >> regtests_lst.gpr
# T_SOAP
	echo "" >> regtests_lst.gpr
	echo "   T_SOAP := (" >> regtests_lst.gpr
	$(AWK) 'BEGIN{FS=":";n=0} \
		n==1{printf (",\n\"%s\"",$$1)} \
		n==0{printf ("\"%s\"",$$1);n=1}' \
			s_tsoap.lst >> regtests_lst.gpr
	echo "   );" >> regtests_lst.gpr
	echo "" >> regtests_lst.gpr
	echo "end regtests_lst;" >> regtests_lst.gpr

clean:
	-rm -f *.o *.ali b~*.ad* *~ .*~ *.res res.ad\[sb\] res2.ad\[sb\]
	-rm -f file.tmp file1.html file2.html file3.html file.tmplt tresres*
	-rm -f ?.upload.* strm-*.log cert.pem run1 run2 gmem.out $(ALL_EXEC)
	-rm -f alldifs *.log run? check_mem.run? hello_demo* *.tmp
	-rm -f anytype_service* r_hello_demo* example-com*
	-rm -f t_soapinterop* urn*
	-rm -f stockquoteservice* stock_quote_service* datafeedservice*
	-rm -f tinteroplab* googlesearchservice*
	-rm -f google_search_service*
	-rm -f wsdl_?_service* wsdl4* wsdl_parse_?-* wsdl_parse_?.ads
	-rm -fr WSDL_6 wsdl_6.class file[abc].txt* zresres*
	-rm -f test_all.adb filez.tmplt
	-rm -fr icons
	-rm -f s_mains.lst all_mains.lst
	-rm -f s_tsecsoap.lst s_tsec.lst s_tstd.lst s_tsoap.lst
