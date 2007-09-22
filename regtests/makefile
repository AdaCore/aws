############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                         Copyright (C) 2003-2007                          #
#                                 AdaCore                                  #
#                                                                          #
#  This library is free software; you can redistribute it and/or modify    #
#  it under the terms of the GNU General Public License as published by    #
#  the Free Software Foundation; either version 2 of the License, or (at   #
#  your option) any later version.                                         #
#                                                                          #
#  This library is distributed in the hope that it will be useful, but     #
#  WITHOUT ANY WARRANTY; without even the implied warranty of              #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       #
#  General Public License for more details.                                #
#                                                                          #
#  You should have received a copy of the GNU General Public License       #
#  along with this library; if not, write to the Free Software Foundation, #
#  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          #
#                                                                          #
#  As a special exception, if other files instantiate generics from this   #
#  unit, or you link this unit with other files to produce an executable,  #
#  this  unit  does not  by itself cause  the resulting executable to be   #
#  covered by the GNU General Public License. This exception does not      #
#  however invalidate any other reasons why the executable file  might be  #
#  covered by the  GNU Public License.                                     #
############################################################################

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

# Test to be run in grouped mode

ifeq (${TEST_MODE}, Grouped)
G_TESTS = $(TESTS)
else
S_TESTS = $(TESTS)
endif

FILTER_OUT	= generate.adb

ifeq (${EXTRA_TESTS}, false)
FILTER_OUT	+= test_speed.adb
endif

# Standard regression tests that needs to be run in a separate executable

OTH_TESTS      = tres

ALONE_TESTS    = server_config tlog

ifeq (${XMLADA}, true)
ifneq (${SOCKET}, std)
OTH_TESTS     := $(OTH_TESTS) check_mem
endif

WSDL_TESTS     = test_wsdl test_wsdl2 test_wsdl4 test_wsdl6 \
	interoplab_main1 googlesearch datafeed_service anytype
WSDL_ATESTS    = test_wsdl3 test_wsdl5 test_wsdl7
WSDL2AWS_TESTS = wsdl_parse_1 wsdl_parse_2 wsdl_parse_3 wsdl_parse_4
endif

PREP_WSDL_TESTS  = $(WSDL_TESTS:%=%_prep)
PREP_WSDL_ATESTS = $(WSDL_ATESTS:%=%_prep)

ifeq (${ASIS}, true)
# SOAP/WSDL test using ada2wsdl (ASIS package required)
# Names must start with wsdl_ and be followed by a single digit between 0
# and 9. Socket port is then 770<digit>.

ASIS_TESTS   = wsdl_1 wsdl_2 wsdl_3 wsdl_4 wsdl_5 wsdl_6 wsdl_7 wsdl_8 wsdl_9
ASIS2_TESTS  = wsdl_a wsdl_b wsdl_c register
endif

FILTER_OUT	+= $(addsuffix .adb, $(OTH_TESTS) $(ALONE_TESTS))
ALONE		= $(basename $(filter-out $(FILTER_OUT), $(S_TESTS)))
SPEC_TESTS      = $(ALONE_TESTS) $(OTH_TESTS) $(ALONE)

SRC_SPEC_TESTS	= ${addsuffix .adb, $(SPEC_TESTS)}

S_TOGETHER	= $(sort $(G_TESTS))
F_TOGETHER 	= $(filter-out $(FILTER_OUT) $(SRC_SPEC_TESTS), $(S_TOGETHER))
TOGETHER 	= $(basename $(F_TOGETHER))

ALL_EXEC        = $(addsuffix $(EXEEXT), $(SPEC_TESTS) generate test_all)

force:

gen_main: gen_sources
	echo Generating the main driver
	$(GNAT) make -q -Pregtests -XTest_Mode=Generator \
		-XBuild=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}
	./generate $(TOGETHER)

gen_sources: $(sort $(PREP_WSDL_TESTS) $(PREP_WSDL_ATESTS) $(ASIS_TESTS) \
	$(ASIS2_TESTS) $(WSDL2AWS_TESTS))

build_drivers: gen_main
	echo "Building drivers (takes some time)"
	$(GNAT) make -q -m -Pregtests -XTest_Mode=${TEST_MODE} \
		-XBuild=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA} test_all

empty:=
space:= $(empty) $(empty)

REGEXP = $(subst $(space),"\\|",$(FILTER_OUT))

build:

run: init build_drivers $(SPEC_TESTS)
ifdef GDB_REGTESTS
	gdb --exec=test_all --command=cmd.gdb --batch
else
	./test_all
endif
	-for file in $(TOGETHER); do \
	   if [ -f $$file.$(SOCKET).out ]; then \
	      $(DIFF) $$file.$(SOCKET).out $$file.res \
		1>> alldifs 2>> alldifs; \
	   else \
	      $(DIFF) $$file.out $$file.res 1>> alldifs 2>> alldifs; \
	   fi; \
	done
	cat alldifs
	$(DIFF) alldifs alldifs.out > /dev/null 2>&1

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

$(ALONE_TESTS) $(ALONE): force
	echo "Compiling " $@
	$(GNAT) make -q -m -Pregtests -XTest_Mode=${TEST_MODE} \
		-XBuild=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA} $@
	echo "Running   " $@
ifdef GDB_REGTESTS
	./tgdb $@ > $@.res
else
	./$@ > $@.res
endif
	$(GNAT) clean -q -Pregtests -XTest_Mode=${TEST_MODE} \
		-XBuild=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA} $@
	-$(DIFF) $@.out $@.res 1>> alldifs 2>> alldifs

$(PREP_WSDL_TESTS): force
	echo Preparing $(@:%_prep=%)
	$(WSDL2AWS) -q -f -doc $(@:%_prep=%).wsdl

$(PREP_WSDL_ATESTS): force
	echo Preparing $(@:%_prep=%)
	$(WSDL2AWS) -q -f -a -doc $(@:%_prep=%).wsdl

$(WSDL2AWS_TESTS): force
	echo "Running  " $@
	-$(WSDL2AWS) -q -f -doc $@.wsdl > $@.res
	-$(DIFF) $@.out $@.res 1>> alldifs 2>> alldifs

$(ASIS_TESTS): force
	echo Preparing $@
	$(ADA2WSDL) -q -f $(INCLUDES) -P../aws \
		-a http://localhost:770`echo $@ | cut -c 6-` $@.ads -o $@.wsdl
	$(WSDL2AWS) -q -f -cb -types $@ $@.wsdl

$(ASIS2_TESTS): force
	rm -f $@.res
	echo "Running  " $@
	$(ADA2WSDL) -v -f $@.ads -o $@.wsdl > tmp 2>&1
	tail -n +3 tmp > $@.res
	rm tmp
	-$(DIFF) $@.out $@.res 1>> alldifs 2>> alldifs

tres: force
	echo "Compiling " $@
	$(GNAT) make -q -m -Pregtests -XTest_Mode=${TEST_MODE} \
		-XBuild=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA} $@
	echo -n "Running  " tres
	./tres 1245 > tres-1.res
	echo "line 3, file 1" >> file1.html
	echo "line 4, file 2" >> file2.html
	echo "line 3, file 3" >> file3.html
	echo ",  run it again"
	./tres 1246 > tres-2.res
	cat tres-1.res tres-2.res > tres.res
	$(GNAT) clean -q -Pregtests -XTest_Mode=${TEST_MODE} \
		-XBuild=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA} $@
	-$(DIFF) tres.out tres.res 1>> alldifs 2>> alldifs

# As this check is really target dependent what we do is to run twice the same
# program, each time with a different number of HTTP requests, then we compare
# the final non freed memory.

check_mem: force
	echo "Compiling " $@
	$(GNAT) make -q -m -Pregtests -XTest_Mode=${TEST_MODE} \
		-XBuild=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA} $@
	echo -n "Running  " check_mem
	./check_mem 2 > check_mem.res
	gnatmem 3 -i gmem.out ./check_mem$(EXEEXT) > check_mem.run1
	grep "   Final" check_mem.run1 > run1
	echo ",  run it again"
	./check_mem 30 >> check_mem.res
	gnatmem 3 -i gmem.out ./check_mem$(EXEEXT) > check_mem.run2
	grep "   Final" check_mem.run2 > run2
	$(GNAT) clean -q -Pregtests -XTest_Mode=${TEST_MODE} \
		-XBuild=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA} $@
	-$(DIFF) run1 run2 1>> alldifs 2>> alldifs

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
ifeq ($(ASIS), true)
	-$(GREP) "\[ASIS\]" all_mains.lst >> s_mains.lst
endif
ifeq (${XMLADA}, true)
	-$(GREP) "\[SOAP\]" all_mains.lst >> s_mains.lst
endif
ifneq (${SOCKET}, std)
	-$(GREP) "\[SSL\]" all_mains.lst >> s_mains.lst
endif
ifeq (${XMLADA}, true)
  ifeq ($(ASIS), true)
	-$(GREP) "\[ASIS+SOAP\]" all_mains.lst >> s_mains.lst
    ifneq (${SOCKET}, std)
	-$(GREP) "\[ASIS+SOAP+SSL\]" all_mains.lst >> s_mains.lst
    endif
  endif
  ifneq (${SOCKET}, std)
	-$(GREP) "\[SOAP+SSL\]" all_mains.lst >> s_mains.lst
  endif
else
  ifeq ($(ASIS), true)
    ifneq (${SOCKET}, std)
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

check:

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
