############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2003-2008, AdaCore                     #
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

#  To add a new test:
#
#   1) Create the appropriate Ada program (let's call it mytest.adb)
#      Add a tag line in mytest.adb
#      --  ~ MAIN [<options>]
#      <options> can be:
#         [STD]             : test requires no external support
#         [SSL]             : test requires SSL
#         [ASIS]            : test requires ASIS
#         [XMLADA]          : test requires SOAP
#         [XMLADA+ASIS+SSL] : test requires ASIS, SOAP and SSL
#                             it is possible to specify only 2 options, the
#                             ordering is important though.
#
#   2) Create the expected output mytest.out
#
#   3) If the test needs to be executed in it's own executable add it to:
#      ASIS_TESTS     : test using ada2wsdl tool to create test case
#      ASIS2_TESTS    : test using ada2wsdl tool only, checking output
#      ASIS3_TESTS    : test using ada2wsdl tool only, checking generated WSDL
#      ALONE_TESTS    : if a standard test but needs to be run separately
#      OTH_TESTS      : if a test that has a specific target in makefile
#      WSDL_TESTS     : for WSDL tests
#      WSDL_ATESTS    : for WSDL tests using Ada name style
#      WSDL2AWS_TESTS : test using only wsdl2aws tool

-include makefile.lst

ADA2WSDL = ../$(BDIR)/$(LIBRARY_TYPE)/tools/ada2wsdl-main
WSDL2AWS = ../$(BDIR)/$(LIBRARY_TYPE)/tools/wsdl2aws
AWSRES   = ../$(BDIR)/$(LIBRARY_TYPE)/tools/awsres
WEBXREF  = ../$(BDIR)/$(LIBRARY_TYPE)/tools/webxref

#  Test to be run in grouped mode

ifeq (${TEST_MODE}, Grouped)
G_TESTS = $(TESTS)
else
S_TESTS = $(TESTS)
endif

FILTER_OUT	= generate.adb

#  Standard regression tests that needs to be run in a separate executable

OTH_TESTS      = wxrefs

ALONE_TESTS    =

ifeq (${XMLADA}, true)

WSDL_TESTS     =
WSDL_ATESTS    =
WSDL2AWS_TESTS = wsdl_parse_1 wsdl_parse_2 wsdl_parse_3 wsdl_parse_4
endif

PREP_WSDL_TESTS  = $(WSDL_TESTS:%=%_prep)
PREP_WSDL_ATESTS = $(WSDL_ATESTS:%=%_prep)

ifeq (${ASIS}, true)
#  SOAP/WSDL test using ada2wsdl (ASIS package required)
#  Names must start with wsdl_ and be followed by a single digit between 0
#  and 9. Socket port is then 770<digit>.

ASIS_TESTS   = wsdl_8 wsdl_9
ASIS2_TESTS  = wsdl_a wsdl_b wsdl_c register wsdl_d
ASIS3_TESTS  = callsh
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

gen_main:
	echo Generating the main driver
	$(GNAT) make -q -Pregtests -XTest_Mode=Generator
	./generate $(TOGETHER)

gen_sources: $(sort $(PREP_WSDL_TESTS) $(PREP_WSDL_ATESTS) $(ASIS_TESTS) \
	$(ASIS2_TESTS) $(ASIS3_TESTS) $(WSDL2AWS_TESTS))

build_drivers: gen_main
	echo "Building drivers (takes some time)"
	$(GNAT) make -q -m -Pregtests -XTest_Mode=${TEST_MODE} test_all

empty:=
space:= $(empty) $(empty)

REGEXP = $(subst $(space),"\\|",$(FILTER_OUT))

build:

run: init gen_sources $(SPEC_TESTS) build_drivers
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
	-$(RM) test_all$(EXEEXT)

init:
	cp ../demos/cert.pem .
	echo "" > alldifs
	echo "### Every output after this line is a regression ###" >> alldifs
	-$(RM) -f *.res

$(ALONE_TESTS) $(ALONE): force
	echo "Compiling " $@
	$(GNAT) make -q -m -Pregtests -XTest_Mode=${TEST_MODE} $@
	echo "Running   " $@
ifdef GDB_REGTESTS
	-./tgdb $@ > $@.res 2>&1
else
	-./$@ > $@.res 2>&1
endif
	-$(RM) -f ./$@$(EXEEXT)
	-$(DIFF) $@.out $@.res 1>> alldifs 2>> alldifs

$(PREP_WSDL_TESTS): force
	echo Preparing $(@:%_prep=%)
	(cd generated; ../$(WSDL2AWS) -q -f -doc ../$(@:%_prep=%).wsdl)

$(PREP_WSDL_ATESTS): force
	echo Preparing $(@:%_prep=%)
	(cd generated; ../$(WSDL2AWS) -q -f -a -doc ../$(@:%_prep=%).wsdl)

$(WSDL2AWS_TESTS): force
	echo "Running  " $@
	-(cd generated; ../$(WSDL2AWS) -q -f -doc ../$@.wsdl > ../$@.res)
	-$(DIFF) $@.out $@.res 1>> alldifs 2>> alldifs

$(ASIS_TESTS): force
	echo Preparing $@
	$(ADA2WSDL) -q -f -I. -t ../$(BDIR)/$(LIBRARY_TYPE)/obj/regtests\
		-P../aws \
		-a http://localhost:770`echo $@ | cut -c 6-` $@.ads \
		-o generated/$@.wsdl
	(cd generated; ../$(WSDL2AWS) -q -f -cb -types $@ $@.wsdl)

$(ASIS2_TESTS): force
	-$(RM) -f $@.res
	echo "Running  " $@
	$(ADA2WSDL) -v -f -I. -t ../$(BDIR)/$(LIBRARY_TYPE)/obj/regtests\
		-P../aws $@.ads -o generated/$@.wsdl > tmp 2>&1
	tail -n +3 tmp > $@.res
	rm tmp
	-$(DIFF) $@.out $@.res 1>> alldifs 2>> alldifs

$(ASIS3_TESTS): force
	-$(RM) -f $@.res
	echo "Running  " $@
	$(ADA2WSDL) -q -d -f -I. -t ../$(BDIR)/$(LIBRARY_TYPE)/obj/regtests\
		-P../aws $@.ads -o generated/$@.wsdl
	-$(DIFF) $@.out generated/$@.wsdl 1>> alldifs 2>> alldifs

wxrefs:
	echo webxrefs tests
	echo xxxxxxxxxx CROSS-REFERENCES xxxxxxxxxx > wxrefs.res
	$(WEBXREF) wx_* >> wxrefs.res
	echo xxxxxxxxxx UNUSED / UNREFERENCED xxxxxxxxxx >> wxrefs.res
	$(WEBXREF) -u wx_* >> wxrefs.res
	echo xxxxxxxxxx UNDEFINED xxxxxxxxxx >> wxrefs.res
	$(WEBXREF) -d wx_* >> wxrefs.res
	echo xxxxxxxxxx NAMING xxxxxxxxxx >> wxrefs.res
	-$(WEBXREF) -I -C -pi id wx_* >> wxrefs.res
	echo xxxxxxxxxx NAMING FILE BASED xxxxxxxxxx >> wxrefs.res
	-$(WEBXREF) -I -C -pi file_based wx_* >> wxrefs.res
	-$(DIFF) wxrefs.out wxrefs.res 1>> alldifs 2>> alldifs

setup_dir:
	-$(MKDIR) ../$(BDIR)/$(LIBRARY_TYPE)/obj/regtests
	-$(MKDIR) generated

setup: setup_dir
#  Get MAIN and standard sources
	-$(GREP) "\-\-  ~ MAIN" *.adb > all_mains.lst
	-$(GREP) "\[STD\]" all_mains.lst | $(GREP) -v $(REGEXP) > s_tstd.lst
	-$(CP) s_tstd.lst s_mains.lst
	-$(GREP) "\[XMLADA\]" all_mains.lst > s_txmlada.lst
	-$(GREP) "\[XMLADA+ASIS\]" all_mains.lst >> s_txmlada.lst
	-$(GREP) "\[SSL\]" all_mains.lst > s_tsec.lst
	-$(GREP) "\[XMLADA+SSL\]" all_mains.lst > s_tsecxmlada.lst
	-$(GREP) "\[ASIS+SSL\]" all_mains.lst >> s_tsecxmlada.lst
ifeq ($(ASIS), true)
	-$(GREP) "\[ASIS\]" all_mains.lst >> s_mains.lst
endif
ifeq (${XMLADA}, true)
	-$(GREP) "\[XMLADA\]" all_mains.lst >> s_mains.lst
endif
ifneq (${SOCKET}, std)
	-$(GREP) "\[SSL\]" all_mains.lst >> s_mains.lst
endif
ifeq (${XMLADA}, true)
  ifeq ($(ASIS), true)
	-$(GREP) "\[XMLADA+ASIS\]" all_mains.lst >> s_mains.lst
    ifneq (${SOCKET}, std)
	-$(GREP) "\[XMLADA+ASIS+SSL\]" all_mains.lst >> s_mains.lst
    endif
  endif
  ifneq (${SOCKET}, std)
	-$(GREP) "\[XMLADA+SSL\]" all_mains.lst >> s_mains.lst
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
#  Now write .gpr
	echo "project regtests_lst is" > regtests_lst.gpr
	echo "" >> regtests_lst.gpr
	echo "   for Source_Dirs use ();" >> regtests_lst.gpr
#  T_STD
	echo "" >> regtests_lst.gpr
	echo "   T_STD := (" >> regtests_lst.gpr
	$(AWK) 'BEGIN{FS=":";n=0} \
		n==1{printf (",\n\"%s\"",$$1)} \
		n==0{printf ("\"%s\"",$$1);n=1}' s_tstd.lst >> regtests_lst.gpr
	echo "   );" >> regtests_lst.gpr
#  T_SEC
	echo "" >> regtests_lst.gpr
	echo "   T_SEC := (" >> regtests_lst.gpr
	$(AWK) 'BEGIN{FS=":";n=0} \
		n==1{printf (",\n\"%s\"",$$1)} \
		n==0{printf ("\"%s\"",$$1);n=1}' s_tsec.lst >> regtests_lst.gpr
	echo "   );" >> regtests_lst.gpr
#  T_SEC_XMLADA
	echo "" >> regtests_lst.gpr
	echo "   T_SEC_XMLADA := (" >> regtests_lst.gpr
	$(AWK) 'BEGIN{FS=":";n=0} \
		n==1{printf (",\n\"%s\"",$$1)} \
		n==0{printf ("\"%s\"",$$1);n=1}' \
			s_tsecxmlada.lst >> regtests_lst.gpr
	echo "   );" >> regtests_lst.gpr
#  T_XMLADA
	echo "" >> regtests_lst.gpr
	echo "   T_XMLADA := (" >> regtests_lst.gpr
	$(AWK) 'BEGIN{FS=":";n=0} \
		n==1{printf (",\n\"%s\"",$$1)} \
		n==0{printf ("\"%s\"",$$1);n=1}' \
			s_txmlada.lst >> regtests_lst.gpr
	echo "   );" >> regtests_lst.gpr
	echo "" >> regtests_lst.gpr
	echo "end regtests_lst;" >> regtests_lst.gpr
#
	./gen_system_tags.py ../makefile.setup 2>/dev/null || \
		(echo "Can not generate system tags. The test are disabled")

check:

install:

clean:
	-$(GNAT) clean -q -Pregtests -XTest_Mode=${TEST_MODE}
	-$(RM) -f *.log *.res alldifs file[abc].txt* test_all.adb filez.tmplt
	-$(RM) -f *.tmp alldifs s_mains.lst all_mains.lst
	-$(RM) -f s_tsecxmlada.lst s_tsec.lst s_tstd.lst s_txmlada.lst
	-$(RM) -f run?
	-$(RM) -fr generated icons
	-$(RM) -f *~ .*~ res.ad\[sb\] res2.ad\[sb\]
	-$(RM) -f file.tmp file1.html file2.html file3.html file.tmplt
	-$(RM) -f ?.upload.* cert.pem gmem.out
	-$(RM) -fr WSDL_6 wsdl_6.class
