############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2003-2011, AdaCore                     #
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
TEXI2DVI	= $(shell which texi2dvi 2> /dev/null)
PDFTEX		= $(shell which pdftex 2> /dev/null)

APIFILES = ../src/core/aws.ads.texi \
	../src/core/aws-attachments.ads.texi \
	../src/core/aws-client.ads.texi \
	../src/extended/aws-client-hotplug.ads.texi \
	../src/extended/aws-communication-client.ads.texi \
	../src/extended/aws-communication-server.ads.texi \
	../src/extended/aws-communication.ads.texi \
	../src/core/aws-config-ini.ads.texi \
	../src/core/aws-config-set.ads.texi \
	../src/core/aws-config.ads.texi \
	../src/core/aws-containers-tables.ads.texi \
	../src/extended/aws-cookie.ads.texi \
	../src/core/aws-default.ads.texi \
	../src/core/aws-dispatchers-callback.ads.texi \
	../src/core/aws-dispatchers.ads.texi \
	../src/core/aws-exceptions.ads.texi \
	../src/core/aws-headers.ads.texi \
	../src/core/aws-headers-values.ads.texi \
	../src/ldap/aws-ldap-client.ads.texi \
	../src/core/aws-log.ads.texi \
	../src/core/aws-messages.ads.texi \
	../src/core/aws-mime.ads.texi \
	../src/core/aws-net.ads.texi \
	../src/core/aws-net-buffered.ads.texi \
	../src/core/aws-net-log.ads.texi \
	../src/extended/aws-net-log-callbacks.ads.texi \
	../src/core/aws-net-ssl.ads.texi \
	../src/core/aws-net-ssl-certificate.ads.texi \
	../src/core/aws-parameters.ads.texi \
	../src/extended/aws-pop.ads.texi \
	../src/core/aws-resources-files.ads.texi \
	../src/core/aws-resources-embedded.ads.texi \
	../src/core/aws-resources-streams.ads.texi \
	../src/core/aws-resources-streams-disk.ads.texi \
	../src/core/aws-resources-streams-disk-once.ads.texi \
	../src/core/aws-resources-streams-memory.ads.texi \
	../src/core/aws-resources-streams-memory-zlib.ads.texi \
	../src/extended/aws-resources-streams-pipe.ads.texi \
	../src/core/aws-resources.ads.texi \
	../src/core/aws-response.ads.texi \
	../src/extended/aws-server-hotplug.ads.texi \
	../src/extended/aws-server-push.ads.texi \
	../src/core/aws-server-status.ads.texi \
	../src/core/aws-server-log.ads.texi \
	../src/core/aws-server.ads.texi \
	../src/extended/aws-services-callbacks.ads.texi \
	../src/extended/aws-services-directory.ads.texi \
	../src/extended/aws-services-dispatchers-linker.ads.texi \
	../src/extended/aws-services-dispatchers-method.ads.texi \
	../src/extended/aws-services-dispatchers-uri.ads.texi \
	../src/extended/aws-services-dispatchers-virtual_host.ads.texi \
	../src/extended/aws-services-dispatchers.ads.texi \
	../src/extended/aws-services-download.ads.texi \
	../src/extended/aws-services-page_server.ads.texi \
	../src/extended/aws-services-split_pages.ads.texi \
	../src/extended/aws-services-split_pages-uniform.ads.texi \
	../src/extended/aws-services-split_pages-uniform-alpha.ads.texi \
	../src/extended/aws-services-split_pages-uniform-overlapping.ads.texi \
	../src/extended/aws-services-split_pages-alpha.ads.texi \
	../src/extended/aws-services-split_pages-alpha-bounded.ads.texi \
	../src/core/aws-services-transient_pages.ads.texi \
	../src/extended/aws-services-web_block.ads.texi \
	../src/extended/aws-services-web_block-context.ads.texi \
	../src/extended/aws-services-web_block-registry.ads.texi \
	../src/core/aws-session.ads.texi \
	../src/extended/aws-smtp-client.ads.texi \
	../src/extended/aws-smtp.ads.texi \
	../src/core/aws-status.ads.texi \
	../src/core/aws-templates.ads.texi \
	../src/core/aws-translator.ads.texi \
	../src/core/aws-url.ads.texi \
	../src/xsrc/aws-jabber.ads.texi \
	../src/soap/soap.ads.texi \
	../src/soap/soap-client.ads.texi \
	../src/soap/soap-dispatchers.ads.texi \
	../src/soap/soap-dispatchers-callback.ads.texi \
	../src/soap/soap-message-xml.ads.texi \
	../src/soap/soap-message.ads.texi \
	../src/soap/soap-parameters.ads.texi \
	../src/soap/soap-types.ads.texi

build_doc: $(APIFILES) aws_docs sg_docs
	echo ""
	echo AWS Documentation built with success.
	${MAKE} -C ../templates_parser doc

aws_docs: aws.texi aws.pdf aws.html aws.txt aws.info

sg_docs: style-guide.pdf style-guide.html style-guide.txt \
	style-guide.info

setup:

build:

%.ads.texi: %.ads gentexifile
	./gentexifile $< NOGROUP

%.adb.texi: %.adb gentexifile
	./gentexifile $< NOGROUP

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
	$(GNAT) make -p -Pdocs -XLIBRARY_TYPE=static \
		-XPRJ_Build=${PRJ_BUILD} -XPRJ_XMLADA=Installed

ifeq (${BUILD_DOC_SCRIPT},false)
gen_texi: prog aws.texi.tmplt
	echo build from ada
	../$(BDIR)/../common/bin/build | tr -d '\r' > aws.texi
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
	-if [ -f aws.pdf ]; then \
	   $(CP) aws.pdf $(DESTDIR)$(I_DOC); \
	   $(CP) ../templates_parser/docs/templates_parser.pdf $(DESTDIR)$(I_DOC); \
	fi;
	-$(CP) aws.html $(DESTDIR)$(I_DOC)
	-$(CP) aws.txt $(DESTDIR)$(I_DOC)
	-$(CP) aws*.info* $(DESTDIR)$(I_DOC)
	-$(CP) ../templates_parser/docs/templates_parser.html $(DESTDIR)$(I_DOC)
	-$(CP) ../templates_parser/docs/templates_parser.txt $(DESTDIR)$(I_DOC)
	-$(CP) ../templates_parser/docs/templates_parser.info* $(DESTDIR)$(I_DOC)

clean:
	-$(GNAT) clean -XLIBRARY_TYPE=static -Pdocs
	-$(RM) -f aws.texi aws.html aws.info* aws.log aws.txt
	-$(RM) -f $(APIFILES) *~ genout
	-$(RM) -f style-guide.html style-guide.info* \
		style-guide.log style-guide.txt
