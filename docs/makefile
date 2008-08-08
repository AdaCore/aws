############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                         Copyright (C) 2003-2008                          #
#                                 AdaCore                                  #
#                                                                          #
#  This library is free software; you can redistribute it and/or modify    #
#  it under the terms of the GNU General Public License as published by    #
#  the Free Software Foundation; either version 2 of the License, or (at   #
#  your option) any later version.                                         #
#                                                                          #
#  This library is distributed in the hope that it will be useful, but     #
#  WITHOUT ANY WARRANTY; without even the implied warranty of              #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       #
#  General Public License for more details.                                #
#                                                                          #
#  You should have received a copy of the GNU General Public License       #
#  along with this library; if not, write to the Free Software Foundation, #
#  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          #
#                                                                          #
#  As a special exception, if other files instantiate generics from this   #
#  unit, or you link this unit with other files to produce an executable,  #
#  this  unit  does not  by itself cause  the resulting executable to be   #
#  covered by the GNU General Public License. This exception does not      #
#  however invalidate any other reasons why the executable file  might be  #
#  covered by the  GNU Public License.                                     #
############################################################################

.SILENT:

BUILD_DOC_SCRIPT = true
#  When true the main documentation texinfo file will be generated from the
#  corresponding template using a script. This avoid building AWS to build
#  the documentation (see gen_doc.*).

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
	../src/aws-resources-streams-pipe.ads.texi \
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
	../src/aws-services-web_block.ads.texi \
	../src/aws-services-web_block-context.ads.texi \
	../src/aws-services-web_block-registry.ads.texi \
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

sg_docs: style-guide.pdf style-guide.ps style-guide.html style-guide.txt \
	style-guide.info

setup:

build:

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

#  Note that we force XMLADA=Installed, this is not a problem as it is only
#  to give the visibility to soap.ads to get the SOAP's version number.
prog: gen_doc.sed.tmplt
	$(GNAT) make -Pdocs -XLIBRARY_TYPE=static \
		-XPRJ_Build=${PRJ_BUILD} -XPRJ_XMLADA=Installed

ifeq (${BUILD_DOC_SCRIPT},false)
gen_texi: prog aws.texi.tmplt
	echo build from ada
	./build | tr -d '\r' > aws.texi
else
gen_texi: aws.texi.tmplt
	echo build from script
	sed -f ./gen_doc.sed < aws.texi.tmplt > aws.texi
endif

check_texi:
	if [ "`grep '[^@]@_' aws.texi`" != "" ]; then \
	   echo Wrong .texi files, some tag variables are missing; \
	   echo Please be sure to update gen_doc.sed and gen_doc.sed.tmplt; \
	   exit 1; \
	fi;

aws.texi: gen_texi check_texi

install:
	-$(CP) aws.html $(I_DOC)
	-$(CP) aws.txt $(I_DOC)
	-$(CP) *.info* $(I_DOC)

clean:
	-$(GNAT) clean -XLIBRARY_TYPE=static -Pdocs \
		-XPRJ_Build=${PRJ_BUILD} -XPRJ_XMLADA=Installed
	-$(RM) -f aws.texi aws.dvi aws.html aws.info* aws.log aws.ps aws.txt
	-$(RM) -f $(APIFILES) *~ genout
	-$(RM) -f style-guide.dvi style-guide.html style-guide.info* \
		style-guide.log style-guide.ps style-guide.txt
