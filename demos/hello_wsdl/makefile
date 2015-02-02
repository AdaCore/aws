############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2010-2015, AdaCore                     #
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

ifneq (${XMLADA},false)
all: build_wsdl
	gprbuild hello_wsdl.gpr
else
all:
endif

build_wsdl:
	echo Generate stub/skel from WSDL documents
	wsdl2aws -q -f -doc -timeouts 1,2,2 hello.wsdl

clean:
	gprclean hello_wsdl.gpr
	rm -f hello_demo*
