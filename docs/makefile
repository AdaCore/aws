
# $Id$

.SILENT: clean build ada.sed aws.ps aws.info aws.dvi aws.txt aws.html aws.texi
.SILENT: prog check_env aws.pdf
.SILENT: check_api apiref clean_apiref adabrowse_apiref gnathtml_apiref

ADA_KW    = use package is in out exception function with type constant \
		private of return range procedure begin end array record \
		subtype generic limited access all

APIFILES = ../src/aws.ads.texi \
	../src/aws-client.ads.texi \
	../src/aws-communication-client.ads.texi \
	../src/aws-communication-server.ads.texi \
	../src/aws-communication.ads.texi \
	../src/aws-config-ini.ads.texi \
	../src/aws-config-set.ads.texi \
	../src/aws-config.ads.texi \
	../src/aws-containers-tables.ads.texi \
	../src/aws-default.ads.texi \
	../src/aws-dispatchers-callback.ads.texi \
	../src/aws-dispatchers.ads.texi \
	../src/aws-headers.ads.texi \
	../src/aws-headers-values.ads.texi \
	../src/aws-jabber.ads.texi \
	../src/aws-ldap-client.ads.texi \
	../src/aws-log.ads.texi \
	../src/aws-mime.ads.texi \
	../src/aws-parameters.ads.texi \
	../src/aws-resources-streams.ads.texi \
	../src/aws-resources.ads.texi \
	../src/aws-response.ads.texi \
	../src/aws-server-hotplug.ads.texi \
	../src/aws-server-push.ads.texi \
	../src/aws-server.ads.texi \
	../src/aws-services-directory.ads.texi \
	../src/aws-services-dispatchers-method.ads.texi \
	../src/aws-services-dispatchers-uri.ads.texi \
	../src/aws-services-dispatchers-virtual_host.ads.texi \
	../src/aws-services-dispatchers.ads.texi \
	../src/aws-services-page_server.ads.texi \
	../src/aws-session.ads.texi \
	../src/aws-smtp-client.ads.texi \
	../src/aws-smtp.ads.texi \
	../src/aws-status.ads.texi \
	../src/aws-templates.ads.texi \
	../src/aws-url.ads.texi \
	../soap/soap.ads.texi \
	../soap/soap-client.ads.texi \
	../soap/soap-message-xml.ads.texi \
	../soap/soap-message.ads.texi \
	../soap/soap-parameters.ads.texi \
	../soap/soap-types.ads.texi

check_env:
	(if ! [ -e ../src/aws-os_lib.adb ]; then \
	   cp ../include/aws-os_lib-gnat.adb ../src/aws-os_lib.adb; fi)

aws_docs : aws.ps aws.pdf aws.html aws.txt aws.info

sg_docs : style-guide.ps style-guide.pdf style-guide.html style-guide.txt style-guide.info

build: check_env aws_docs sg_docs
	echo done

ada.sed: makefile
	echo "s/\([^@]\)@\([^@]\)/\1@@\2/g" > ada.sed
	echo "s/-- \(.*\)$$/-- @i{\1}/" >> ada.sed
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

%.dvi : %.texi
	-texi2dvi --clean $<

%.ps : %.dvi
	-dvips $< -o $@

%.pdf : %.ps
	-ps2pdf $< $@

%.info : %.texi
	-makeinfo $<

%.html : %.texi
	-texi2html.pl -menu -monolithic $<

%.txt : %.texi
	-makeinfo --no-headers $< > $@

aws.dvi: aws.texi $(APIFILES)
	-texi2dvi --clean aws.texi

prog: build.adb
	gnatmake -m -q build -I../src -I../soap -I../include \
		$(INCLUDES) -largs $(LIBS)

aws.texi: prog aws.texi.tmplt
	./build | tr -d '\r' > aws.texi

check_api:
	if [ -f ../soap/soap.ali ]; then \
		rm -f ../src/test_uconv.ali; \
		exit 0; \
	else \
		echo SOAP not activated, cannot generate API documentation ; \
		exit 1; \
	fi;

apiref: gnathtml_apiref

# API cross-references using gnathtml
gnathtml_apiref: check_api
	(cd ..; fileset=; \
	for file in src/a*.ad[sb] include/templates*.ad[sb] \
	   	include/strings_cutter.ad[sb] \
		soap/soap*.ad[sb]; \
	do \
		echo -n $$file; \
		if [ -f `echo $$file | sed -e 's/\.ad[sb]/.ali/'` ]; then \
			echo "    included"; \
			fileset="$$fileset $$file"; \
		else \
			echo "    skipped"; \
		fi; \
	done; \
	gnathtml.pl -odocs/html $$fileset; \
	)

# API cross-references using AdaBrowse
adabrowse_apiref: check_api
	(cd ..; \
	for file in src/a*.ad[sb] include/templates*.ad[sb] \
	   	include/strings_cutter.ad[sb] \
		soap/soap*.ad[sb]; \
	do \
		echo -n $$file; \
		if [ -f `echo $$file | sed -e 's/\.ad[sb]/.ali/'` ]; then \
			echo "    included"; \
			adabrowse -o docs\\html\\ -f $$file -Isrc -Iinclude; \
		else \
			echo "    skipped"; \
		fi; \
	done; \
	)

clean:
	-rm aws.texi aws.dvi aws.html aws.info* aws.log aws.ps aws.txt \
          $(APIFILES) *~ *.o *.ali
	-rm style-guide.dvi style-guide.html style-guide.info* \
		style-guide.log style-guide.ps style-guide.txt

clean_apiref:
	-rm -fr html
