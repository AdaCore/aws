
# $Id$

.SILENT: run turl tgetparam checks

INCLUDES = -I../src -I../ssl -I../include
LIBS     = -L../ssl -lssl -lcrypto

run : turl tgetparam checks

turl:
	gnatmake -g turl $(INCLUDES) -largs $(LIBS) -bargs -E
	turl > turl.res

tgetparam:
	gnatmake -g tgetparam  $(INCLUDES) -largs $(LIBS) -bargs -E
	tgetparam > tgetparam.res

checks:
	echo "### Every output after this line means there is a regression ###"
	-diff -c turl.out turl.res
	-diff -c tgetparam.out tgetparam.res

clean:
	-rm *.o *.ali b~*.ad* *.~*.*.~ *.res
