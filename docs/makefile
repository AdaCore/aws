
# $Id$

.SILENT: clean build

ADA_KW    = use package is in out exception function with type constant \
		private of return range procedure begin end array record \
		subtype generic limited access all

APIFILES  = ../src/aws-response.ads.texi ../src/aws-status.ads.texi \
	../src/aws-server.ads.texi ../src/aws-config.ads.texi \
	../src/aws-session.ads.texi ../src/aws-client.ads.texi \
	../src/aws-mime.ads.texi ../src/aws-communication.ads.texi \
	../src/aws-communication-client.ads.texi \
	../src/aws-communication-server.ads.texi \
	../src/aws-server-hotplug.ads.texi ../src/aws-log.ads.texi \
	../src/aws-parameters.ads.texi

build: aws.ps aws.html aws.txt aws.info
	rm aws.texi
	echo done

ada.sed: makefile
	echo "s/-- \(.*\)$$/-- @i{\1}/" > ada.sed
	echo "s/\([^-][^-][^\"]*\)\"\([^\"]*\)\"/\1\"@i{\2}\"/g" >> ada.sed
	echo "s/@@/@@@@/g" >> ada.sed
	echo "s/_@_/_@@_/g" >> ada.sed
	for kw in $(ADA_KW); do \
		echo "s/^\([^-]* \)$$kw/\1@b{$$kw}/g" >> ada.sed; \
		echo "s/^\( *\)$$kw /\1@b{$$kw} /g" >> ada.sed; \
		echo "s/^$$kw$$/@b{$$kw}/g" >> ada.sed; \
	done

%.ads.texi: %.ads ada.sed gentexifile
	./gentexifile $< NOGROUP

%.adb.texi: %.adb ada.sed gentexifile
	./gentexifile $< NOGROUP

aws.dvi: aws.texi $(APIFILES)
	texi2dvi --clean aws.texi

aws.ps: aws.dvi
	dvips aws.dvi

aws.info: aws.texi
	makeinfo aws.texi

aws.html: aws.texi
	texi2html.pl -menu -monolithic aws.texi

aws.txt: aws.texi
	makeinfo --no-headers aws.texi > aws.txt

aws.texi: aws.texi.tmplt build.adb
	gnatmake build -I../src -I../include
	./build > aws.texi

clean:
	-rm aws.texi aws.dvi aws.html aws.info* aws.log aws.ps aws.txt \
          $(APIFILES) *~ *.o *.ali
