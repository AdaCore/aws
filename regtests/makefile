
# $Id$

.SILENT: run rt_soap rt_std turl tgetparam param dummy simple tclientto
.SILENT: checks checks_header checks_soap
.SILENT: soap1 soap2 workorder

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
rt_soap: soap1 soap2 workorder

# Standard regression tests
rt_std: turl tgetparam param dummy simple tclientto

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

tclientto: force
	gnatmake -g tclientto $(GARGS) -largs $(LARGS) -bargs -E
	./tclientto > tclientto.res

checks_soap:
	-diff -c -w soap1.out soap1.res
	-diff -c -w soap2.out soap2.res
	-diff -c -w workorder.out workorder.res

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

clean:
	-rm *.o *.ali b~*.ad* *.~*.*.~ *.res *.exe
