############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                       Copyright (C) 2015, AdaCore                        #
#                                                                          #
#  This is free software;  you can redistribute it  and/or modify it       #
#  under terms of the  GNU General Public License as published  by the     #
#  Free Software  Foundation;  either version 3,  or (at your option) any  #
#  later version.  This software is distributed in the hope  that it will  #
#  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty #
#  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     #
#  General Public License for  more details.                               #
#                                                                          #
#  You should have  received  a copy of the GNU General  Public  License   #
#  distributed  with  this  software;   see  file COPYING3.  If not, go    #
#  to http://www.gnu.org/licenses for a complete copy of the license.      #
############################################################################

-include ../../makefile.setup

#  uncomment the following to display the SOAP payload
#  DEBUG=-d
DEBUG=

#  do not activate build by default as it required gSOAP to be installed
#  manually.

ifneq (${GSOAP},true)
all:

else
ifneq (${XMLADA},false)
all: build_wsdl
	gprbuild -p -m gsoap_demo.gpr
	gcc -o c_server_rpc -Ic_rpc c_rpc/soapC.c c_rpc/soapServer.c \
		c_server_rpc.c -lgsoap
	gcc -o c_client_rpc -Ic_rpc c_rpc/soapC.c c_rpc/soapClient.c \
		c_client_rpc.c -lgsoap
	gcc -o c_server_doc -Ic_doc c_doc/soapC.c c_doc/soapServer.c \
		c_server_doc.c -lgsoap
	gcc -o c_client_doc -Ic_doc c_doc/soapC.c c_doc/soapClient.c \
		c_client_doc.c -lgsoap
else
all:
endif
endif

create-dirs:
	mkdir -p c_doc c_rpc

build_wsdl: create-dirs
	echo Generate Ada/C stub/skel from WSDL documents
	wsdl2aws $(DEBUG) -q -f -timeouts 1,2,2 hello_doc.wsdl
	wsdl2aws $(DEBUG) -q -f -timeouts 1,2,2 hello_rpc.wsdl

	wsdl2h -c hello_doc.wsdl
	soapcpp2 -c -d c_doc hello_doc.h

	wsdl2h -c hello_rpc.wsdl
	soapcpp2 -c -d c_rpc hello_rpc.h

clean:
	gprclean gsoap_demo.gpr
	rm -f hello_rpc-* hello_doc-* hello_doc.h hello_rpc.h
	rm -f c_*_rpc c_*_doc hello_rpc.ads hello_doc.ads
	rm -fr c_doc c_rpc obj
