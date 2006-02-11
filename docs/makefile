
.SILENT:


BUILD_DOC_SCRIPT = true

MAKEINFO	= $(shell which makeinfo 2> /dev/null)
DVIPS		= $(shell which dvips 2> /dev/null)
TEXI2DVI	= $(shell which texi2dvi 2> /dev/null)
PDFTEX		= $(shell which pdftex 2> /dev/null)

ADA_KW    = use package is in out exception function with type constant \
		private of return range procedure begin end array record \
		subtype generic limited access all tagged abstract renames \
		pragma new case when null

APIFILES = ../src/aws.ads.texi \
	../src/aws-attachments.ads.texi \
	../src/aws-client.ads.texi \
	../src/aws-client-hotplug.ads.texi \
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
	../src/aws-exceptions.ads.texi \
	../src/aws-headers.ads.texi \
	../src/aws-headers-values.ads.texi \
	../src/aws-ldap-client.ads.texi \
	../src/aws-log.ads.texi \
	../src/aws-messages.ads.texi \
	../src/aws-mime.ads.texi \
	../src/aws-net.ads.texi \
	../src/aws-net-buffered.ads.texi \
	../src/aws-net-log.ads.texi \
	../src/aws-net-log-callbacks.ads.texi \
	../src/aws-net-ssl.ads.texi \
	../src/aws-net-ssl-certificate.ads.texi \
	../src/aws-parameters.ads.texi \
	../src/aws-pop.ads.texi \
	../src/aws-resources-files.ads.texi \
	../src/aws-resources-embedded.ads.texi \
	../src/aws-resources-streams.ads.texi \
	../src/aws-resources-streams-disk.ads.texi \
	../src/aws-resources-streams-disk-once.ads.texi \
	../src/aws-resources-streams-memory.ads.texi \
	../src/aws-resources-streams-memory-zlib.ads.texi \
	../src/aws-resources.ads.texi \
	../src/aws-response.ads.texi \
	../src/aws-server-hotplug.ads.texi \
	../src/aws-server-push.ads.texi \
	../src/aws-server-status.ads.texi \
	../src/aws-server-log.ads.texi \
	../src/aws-server.ads.texi \
	../src/aws-services-callbacks.ads.texi \
	../src/aws-services-directory.ads.texi \
	../src/aws-services-dispatchers-linker.ads.texi \
	../src/aws-services-dispatchers-method.ads.texi \
	../src/aws-services-dispatchers-uri.ads.texi \
	../src/aws-services-dispatchers-virtual_host.ads.texi \
	../src/aws-services-dispatchers.ads.texi \
	../src/aws-services-download.ads.texi \
	../src/aws-services-page_server.ads.texi \
	../src/aws-services-split_pages.ads.texi \
	../src/aws-services-split_pages-uniform.ads.texi \
	../src/aws-services-split_pages-uniform-alpha.ads.texi \
	../src/aws-services-split_pages-uniform-overlapping.ads.texi \
	../src/aws-services-split_pages-alpha.ads.texi \
	../src/aws-services-split_pages-alpha-bounded.ads.texi \
	../src/aws-services-transient_pages.ads.texi \
	../src/aws-session.ads.texi \
	../src/aws-smtp-client.ads.texi \
	../src/aws-smtp.ads.texi \
	../src/aws-status.ads.texi \
	../src/aws-templates.ads.texi \
	../src/aws-translator.ads.texi \
	../src/aws-url.ads.texi \
	../xsrc/aws-jabber.ads.texi \
	../soap/soap.ads.texi \
	../soap/soap-client.ads.texi \
	../soap/soap-dispatchers.ads.texi \
	../soap/soap-dispatchers-callback.ads.texi \
	../soap/soap-message-xml.ads.texi \
	../soap/soap-message.ads.texi \
	../soap/soap-parameters.ads.texi \
	../soap/soap-types.ads.texi

build_doc: $(APIFILES) aws_docs sg_docs
	echo ""
	echo AWS Documentation built with success.
	${MAKE} -C ../templates_parser doc

aws_docs: aws.texi aws.pdf aws.ps aws.html aws.txt aws.info

force:

sg_docs: style-guide.pdf style-guide.ps style-guide.html style-guide.txt \
	style-guide.info

setup:

ada.sed: makefile
	echo "s/\([^@_]\)@\([^@_]\)/\1@@\2/" > ada.sed
	echo "s/-- \(.*\)$$/-- @i{\1}/" >> ada.sed
	echo "/--/!s/\([^-][^-][^\"]*\)\"\([^\"]*\)\"/\1\"@i{\2}\"/g" \
		>> ada.sed
	echo "s/\([^@]\)@_/\1@@_/g" >> ada.sed
	echo "s/_@\([^@]\)/_@@\1/g" >> ada.sed
	echo "s/_@_/_@@_/g" >> ada.sed
	for kw in $(ADA_KW); do \
		echo "s/^\([^-]* \)$$kw/\1@b{$$kw}/g" >> ada.sed; \
		echo "s/^\([^-]* \)$$kw/\1@b{$$kw}/g" >> ada.sed; \
		echo "s/^\( *\)$$kw /\1@b{$$kw} /g" >> ada.sed; \
		echo "s/^$$kw$$/@b{$$kw}/g" >> ada.sed; \
	done

%.ads.texi: %.ads ada.sed gentexifile
	./gentexifile $< NOGROUP

%.adb.texi: %.adb ada.sed gentexifile
	./gentexifile $< NOGROUP

%.dvi: %.texi
ifneq (${TEXI2DVI},)
	echo Building $@
	-${TEXI2DVI} --expand --clean --quiet $<
else
	@echo "--------------------------------------------------------"
	@echo "texi2dvi not found, cannot build DVI or PS documentation"
	@echo "--------------------------------------------------------"
endif

%.ps: %.dvi
ifneq (${DVIPS},)
	echo Building $@
	-${DVIPS} -q $< -o $@
else
	@echo "------------------------------------------------------"
	@echo "dvips not found, cannot build POSTSCRIPT documentation"
	@echo "------------------------------------------------------"
endif

%.pdf: %.texi
ifneq (${TEXI2DVI},)
ifneq (${PDFTEX},)
	echo Building $@
	${TEXI2DVI} -p --expand --clean --quiet $<
else
	@echo "------------------------------------------------"
	@echo "pdftex not found, cannot build PDF documentation"
	@echo "------------------------------------------------"
endif
else
	@echo "--------------------------------------------------"
	@echo "texi2dvi not found, cannot build PDF documentation"
	@echo "--------------------------------------------------"
endif

%.info: %.texi
ifneq (${MAKEINFO},)
	echo Building $@
	-${MAKEINFO} $<
else
	@echo "---------------------------------------------------"
	@echo "makeinfo not found, cannot build INFO documentation"
	@echo "---------------------------------------------------"
endif

%.html: %.texi
ifneq (${MAKEINFO},)
	echo Building $@
	-${MAKEINFO} --html --no-split --css-include=aws.css --ifinfo $<
else
	@echo "---------------------------------------------------"
	@echo "makeinfo not found, cannot build HTML documentation"
	@echo "---------------------------------------------------"
endif

%.txt: %.texi
ifneq (${MAKEINFO},)
	echo Building $@
	-${MAKEINFO} --plaintext --no-headers $< --output $@
else
	@echo "---------------------------------------------------"
	@echo "makeinfo not found, cannot build TEXT documentation"
	@echo "---------------------------------------------------"
endif

# Note that we force XMLADA=Installed, this is not a problem as it is only to
# give the visibility to soap.ads to get the SOAP's version number.
prog:
	$(GNATMAKE) -Pdocs -XPRJ_Build=${PRJ_BUILD} -XPRJ_XMLADA=Installed

ifeq (${BUILD_DOC_SCRIPT},false)
aws.texi: prog aws.texi.tmplt
	echo build from ada
	./build | tr -d '\r' > aws.texi
else
aws.texi: aws.texi.tmplt
	echo build from script
	sed -f ./gen_doc.sed < aws.texi.tmplt > aws.texi
endif

check_api:
	if [ -f ../$(BDIR)/obj/soap.ali ]; then \
		exit 0; \
	else \
		echo SOAP not activated, cannot generate API documentation ; \
		exit 1; \
	fi;

# apiref can be set to gnathtml_apiref or adabrowse_apiref to select the tool
# used to generate the API cross-reference.
apiref: gnathtml_apiref

APIREFS = src/a*.ad[sb] templates_parser/src/templates*.ad[sb] xsrc/*.ad* \
	   	include/strings_cutter.ad[sb] include/zlib*.ad[sb] \
		include/memory_streams.ad[sb] \
		soap/soap*.ad[sb] templates_parser/xsrc/templates*.ad[sb]

pref1	= $(BDIR)/obj
pref2	= $(BDIR)/include

# API cross-references using gnathtml

gnathtml_apiref: check_api
	(cd ..; fileset=; \
	for file in $(APIREFS); do \
		echo -n $$file; \
		bfile=`basename $$file | sed -e 's/\.ad[sb]/.ali/'`; \
		if [ -f $(pref1)/$$bfile ] || [ -f $(pref2)/$$bfile ]; then \
			echo "    included"; \
			fileset="$$fileset `basename $$file`"; \
		else \
			echo "    skipped"; \
		fi; \
	done; \
	gnathtml.pl -Isrc -Iinclude -Isoap -Ixsrc -I$(pref1) -I$(pref2) \
		-Itemplates_parser/src -Itemplates_parser/xsrc \
		-odocs/html $$fileset; \
	)
	$(MV) html/index.htm html/aws_index.htm

# API cross-references using AdaBrowse

adabrowse_apiref: check_api
	(cd ..; \
	for file in $(APIREFS); \
	do \
		echo -n $$file; \
		if [ -f `echo $$file | sed -e 's/\.ad[sb]/.ali/'` ]; then \
			echo "    included"; \
			adabrowse -o docs\\html\\ -f $$file -Isrc -Iinclude \
			   -Itemplates_parser/src -Itemplates_parser/xsrc; \
		else \
			echo "    skipped"; \
		fi; \
	done; \
	)

clean:
	-$(RM) -f aws.texi aws.dvi aws.html aws.info* aws.log aws.ps aws.txt \
          $(APIFILES) *~ *.o *.ali genout
	-$(RM) -f style-guide.dvi style-guide.html style-guide.info* \
		style-guide.log style-guide.ps style-guide.txt

clean_apiref:
	-$(RM) -fr html
