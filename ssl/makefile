############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                         Copyright (C) 2003-2007                          #
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

force:

ifneq (${OS}, Windows_NT)
SSL_PATH1 := $(shell ls /usr/lib/libcrypto.a 2> /dev/null)
SSL_PATH2 := $(shell ls /lib/libcrypto.a 2> /dev/null)
SSL_PATH3 := $(shell ls /usr/lib/libcrypto.so 2> /dev/null)
SSL_PATH4 := $(shell ls /lib/libcrypto.so 2> /dev/null)
endif

SSL_PATH = $(SSL_PATH1)

ifeq (${SSL_PATH},)
	SSL_PATH = $(SSL_PATH2)
endif
ifeq (${SSL_PATH},)
	SSL_PATH = $(SSL_PATH3)
endif
ifeq (${SSL_PATH},)
	SSL_PATH = $(SSL_PATH4)
endif

build:
	$(GNAT) make -Pssl -XSOCKET=${SOCKET} \
		-XAWS_BUILD=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}

check:
	$(GNAT) check -Pssl -XSOCKET=${SOCKET} \
		-XAWS_BUILD=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA} \
		-rules -from=../aws.checks

SSLGPR	=	aws_ssl_shared.gpr
ISSLGPR	=	install_aws_ssl_shared.gpr

setup_ssl:
	echo "project AWS_SSL_Shared is" > $(SSLGPR)
	echo "   for Source_Files use ();" >> $(SSLGPR)
ifeq (${SOCKET}, std)
	echo '   LIB_Path := "";' >> $(SSLGPR)
else
ifeq (${OS}, Windows_NT)
	echo '   LIB_Path := "../lib";' >> $(SSLGPR)
else
	echo '   LIB_Path := "'$(dir $(SSL_PATH))'";' >> $(SSLGPR)
endif
endif
	echo "end AWS_SSL_Shared;" >> $(SSLGPR)

setup_ssl_install:
	echo "project AWS_SSL_Shared is" > $(ISSLGPR)
	echo "   for Source_Files use ();" >> $(ISSLGPR)
ifeq (${OS}, Windows_NT)
	echo '   LIB_Path := "../../../lib";' >> $(ISSLGPR)
else
	echo '   LIB_Path := "'$(dir $(SSL_PATH))'";' >> $(ISSLGPR)
endif
	echo "end AWS_SSL_Shared;" >> $(ISSLGPR)


setup: setup_ssl setup_ssl_install
	-$(MKDIR) -p ../$(BDIR)/ssl/obj
	-$(MKDIR) -p ../$(BDIR)/ssl/lib

clean:
	-$(RM) -f $(SSLGPR)
