
# $Id$

.SILENT: run turl tgetparam param dummy simple checks

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

run : turl tgetparam param dummy simple soap1 workorder checks

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

workorder: force
	gnatmake -g workorder $(GARGS) -largs $(LARGS) -bargs -E
	./workorder > workorder.res

checks:
	echo "### Every output after this line means there is a regression ###"
	-diff -c -w turl.out turl.res
	-diff -c -w tgetparam.out tgetparam.res
	-diff -c -w param.out param.res
	-diff -c -w dummy.out dummy.res
	-diff -c -w simple.out simple.res
	-diff -c -w soap1.out soap1.res
	-diff -c -w workorder.out workorder.res

clean:
	-rm *.o *.ali b~*.ad* *.~*.*.~ *.res *.exe
