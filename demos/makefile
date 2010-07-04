############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2003-2010, AdaCore                     #
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

ifeq (${PLATFORM}, native)
AWSRES   = ../../../$(NBDIR)/static/tools/awsres
WSDL2AWS = ../../../$(NBDIR)/static/tools/wsdl2aws
else
#  On cross platforms we want to use the native tools installed with the
#  compiler.
AWSRES   = awsres
WSDL2AWS = wsdl2aws
endif

SRCDIR	= ../$(BDIR)/../../common/demos

build_res:
	echo Generate resources for res_demo
	(cd $(SRCDIR) && \
	 $(AWSRES) -q -r rdemo ../../../demos/adains.png \
		../../../demos/page3.html)

build_wsdl:
	echo Generate stub/skel from WSDL documents
	(cd $(SRCDIR) && \
	$(WSDL2AWS) -q -f -doc -timeouts 1,2,2 ../../../demos/hello.wsdl && \
	$(WSDL2AWS) -q -f -doc -noskel ../../../demos/interoplab_main.wsdl)

ifeq (${PRJ_XMLADA}, Installed)
BUILD_EXT = build_wsdl
endif

setup:
	$(MKDIR) $(SRCDIR)

install:
	$(CP) *.thtml $(I_TPL)
	$(CP) wm_login.html $(I_TPL)
	$(CP) aws_*.png $(I_IMG)
ifeq ($(SOCKET),ssl)
	-$(CP) ../$(BDIR)/static/demos/agent${EXEEXT} $(I_SBN)
endif

check:
