
# $Id$

.SILENT: run rt_soap rt_std turl tgetparam param dummy simple tclientto
.SILENT: checks checks_header checks_soap
.SILENT: soap1 soap2 workorder tres

#INCLUDES = -I../src -I../ssl -I../include
#LIBS     = -L../ssl -lssl -lcrypto

ifeq ($(MODE),ssl)
# With SSL support

GARGS = -i -I../src -I../ssl -I../include -I../soap $(INCLUDES)
BARGS = -E

ifeq (${OS}, Windows_NT)
LARGS = $(LFLAGS) $(LIBS) -L../ssl -lssl -lcrypto
else
LARGS = $(LFLAGS) $(LIBS) -lssl -lcrypto
endif

else
# Without SSL support

GARGS = -i -I../src -I../include -I../soap $(INCLUDES)
BARGS = -E
LARGS = $(LFLAGS) $(LIBS)
endif

# SOAP regression tests
rt_soap: soap1 soap2 workorder testxml

# Standard regression tests
rt_std: turl tgetparam param dummy simple tclientto shutdown tres tres2

ifeq ($(XMLADA),)
run : rt_std checks
else
run : rt_std rt_soap checks
endif

force:

turl: force
	gnatmake -g turl $(GARGS) -largs $(LARGS) -bargs -E
	./turl > turl.res

tgetparam: force
	gnatmake -g tgetparam  $(GARGS) -largs $(LARGS) -bargs -E
	./tgetparam > tgetparam.res

param: force
	gnatmake -g param $(GARGS) -largs $(LARGS) -bargs -E
	./param > param.res

dummy: force
	gnatmake -g dummy  $(GARGS) -largs $(LARGS) -bargs -E
	./dummy > dummy.res

simple: force
	gnatmake -g simple $(GARGS) -largs $(LARGS) -bargs -E
	./simple > simple.res

soap1: force
	gnatmake -g soap1 $(GARGS) -largs $(LARGS) -bargs -E
	./soap1 > soap1.res

soap2: force
	gnatmake -g soap2 $(GARGS) -largs $(LARGS) -bargs -E
	./soap2 > soap2.res

workorder: force
	gnatmake -g workorder $(GARGS) -largs $(LARGS) -bargs -E
	./workorder > workorder.res

testxml: force
	gnatmake -g testxml $(GARGS) -largs $(LARGS) -bargs -E
	./testxml > testxml.res

tclientto: force
	gnatmake -g tclientto $(GARGS) -largs $(LARGS) -bargs -E
	./tclientto > tclientto.res

shutdown: force
	gnatmake -g shutdown $(GARGS) -largs $(LARGS) -bargs -E
	./shutdown > shutdown.res

tres: force
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
	./tres > tres.res
	echo "line 3, file 1" >> file1.html
	echo "line 4, file 2" >> file2.html
	echo "line 3, file 3" >> file3.html
	./tres >> tres.res

tres2: force
	gnatmake -g tres2 $(GARGS) -largs $(LARGS) -bargs -E
	./tres2 > tres2.res

checks_soap:
	-diff -c -w soap1.out soap1.res
	-diff -c -w soap2.out soap2.res
	-diff -c -w workorder.out workorder.res
	-diff -c -w testxml.out testxml.res

checks_header:
	echo "### Every output after this line means there is a regression ###"

ifeq ($(XMLADA),)
checks: checks_header
else
checks: checks_header checks_soap
endif
	-diff -c -w turl.out turl.res
	-diff -c -w tgetparam.out tgetparam.res
	-diff -c -w param.out param.res
	-diff -c -w dummy.out dummy.res
	-diff -c -w simple.out simple.res
	-diff -c -w tclientto.out tclientto.res
	-diff -c -w shutdown.out shutdown.res
	-diff -c -w tres.out tres.res
	-diff -c -w tres2.out tres2.res

clean:
	-rm *.o *.ali b~*.ad* *.~*.*.~ *.res *.exe res.ad[sb] res2.ad[sb]
	-rm file file1.html file2.html file3.html file.tmplt tresres*
