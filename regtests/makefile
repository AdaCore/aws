
# $Id$

.SILENT: run turl tgetparam param dummy simple checks

INCLUDES = -I../src -I../ssl -I../include
LIBS     = -L../ssl -lssl -lcrypto

run : turl tgetparam param dummy simple checks

force:

turl: force
	gnatmake -g turl $(INCLUDES) -largs $(LIBS) -bargs -E
	./turl > turl.res

tgetparam: force
	gnatmake -g tgetparam  $(INCLUDES) -largs $(LIBS) -bargs -E
	./tgetparam > tgetparam.res

param: force
	gnatmake -g param  $(INCLUDES) -largs $(LIBS) -bargs -E
	./param > param.res

dummy: force
	gnatmake -g dummy  $(INCLUDES) -largs $(LIBS) -bargs -E
	./dummy > dummy.res

simple: force
	gnatmake -g simple  $(INCLUDES) -largs $(LIBS) -bargs -E
	./simple > simple.res

checks:
	echo "### Every output after this line means there is a regression ###"
	-diff -c -w turl.out turl.res
	-diff -c -w tgetparam.out tgetparam.res
	-diff -c -w param.out param.res
	-diff -c -w dummy.out dummy.res
	-diff -c -w simple.out simple.res

clean:
	-rm *.o *.ali b~*.ad* *.~*.*.~ *.res *.exe
