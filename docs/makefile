
# $Id$

ADA_KW    = use package is in out exception function with type constant \
		private of return range procedure begin end array record

APIFILES  = ../src/aws-response.ads.texi ../src/aws-status.ads.texi

build: aws.ps aws.html aws.txt aws.info

ada.sed: makefile
	echo "s/-- \(.*\)$$/-- @i{\1}/" > ada.sed
	echo "s/\([^-][^-][^\"]*\)\"\([^\"]*\)\"/\1\"@i{\2}\"/g" >> ada.sed
	echo "s/@@/@@@@/g" >> ada.sed
	for kw in $(ADA_KW); do \
		echo "s/^\([^-][^-].* \)$$kw/\1@b{$$kw}/g" >> ada.sed; \
		echo "s/^$$kw /@b{$$kw} /g" >> ada.sed; \
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

clean:
	-rm aws.dvi aws.html aws.info aws.log aws.ps aws.txt $(APIFILES)
