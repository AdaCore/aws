
# $Id$

build: aws.ps aws.html aws.txt aws.info

aws.dvi: aws.texi
	texi2dvi --clean aws.texi

aws.ps: aws.dvi
	dvips aws.dvi

aws.info: aws.texi
	makeinfo aws.texi

aws.html: aws.texi
	texi2html.pl -menu -monolithic aws.texi

aws.txt: aws.texi
	makeinfo --no-headers aws.texi > aws.txt
