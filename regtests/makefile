
# $Id$

.SILENT: run turl tgetparam param dummy simple checks

INCLUDES = -I../src -I../ssl -I../include
LIBS     = -L../ssl -lssl -lcrypto

run : turl tgetparam param dummy simple checks

turl:
	gnatmake -g turl $(INCLUDES) -largs $(LIBS) -bargs -E
	./turl > turl.res

tgetparam:
	gnatmake -g tgetparam  $(INCLUDES) -largs $(LIBS) -bargs -E
	./tgetparam > tgetparam.res

param:
	gnatmake -g param  $(INCLUDES) -largs $(LIBS) -bargs -E
	./param > param.res

dummy:
	gnatmake -g dummy  $(INCLUDES) -largs $(LIBS) -bargs -E
	./dummy > dummy.res

simple:
	gnatmake -g simple  $(INCLUDES) -largs $(LIBS) -bargs -E
	./simple > simple.res

checks:
	echo "### Every output after this line means there is a regression ###"
	-diff -c turl.out turl.res
	-diff -c tgetparam.out tgetparam.res
	-diff -c param.out param.res
	-diff -c dummy.out dummy.res
	-diff -c simple.out simple.res

clean:
	-rm *.o *.ali b~*.ad* *.~*.*.~ *.res
