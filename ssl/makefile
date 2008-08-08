############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2003-2008, AdaCore                     #
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
SSL_PATH5 := $(shell ls /usr/lib/libcrypto.dylib 2> /dev/null)
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
ifeq (${SSL_PATH},)
	SSL_PATH = $(SSL_PATH5)
endif

build:
	$(GNAT) make -j$(CJOBS) -XLIBRARY_TYPE=static -Pssl
ifeq (${ENABLE_SHARED}, true)
	$(GNAT) make -j$(CJOBS) -XLIBRARY_TYPE=relocatable -Pssl
endif

check:
	$(GNAT) check -Pssl -rules -from=../aws.checks

SSLGPR	=	../$(BDIR)/../projects/aws_ssl_shared.gpr
ISSLGPR	=	../$(BDIR)/../projects/install_aws_ssl_shared.gpr

setup_ssl:
	echo "project AWS_SSL_Shared is" > $(SSLGPR)
	echo "   for Source_Files use ();" >> $(SSLGPR)
ifeq (${SOCKET}, std)
	echo '   LIB_Path := "";' >> $(SSLGPR)
else
ifeq (${OS}, Windows_NT)
	echo '   LIB_Path := "../$(BDIR)/../common/lib/win32";' >> $(SSLGPR)
else
	echo '   LIB_Path := "'$(dir $(SSL_PATH))'";' >> $(SSLGPR)
endif
endif
	echo "end AWS_SSL_Shared;" >> $(SSLGPR)

setup_ssl_install:
	echo "project AWS_SSL_Shared is" > $(ISSLGPR)
	echo "   for Source_Files use ();" >> $(ISSLGPR)
ifeq (${OS}, Windows_NT)
	echo '   LIB_Path := "../../aws/common";' >> $(ISSLGPR)
else
	echo '   LIB_Path := "'$(dir $(SSL_PATH))'";' >> $(ISSLGPR)
endif
	echo "end AWS_SSL_Shared;" >> $(ISSLGPR)

setup:
	-$(MKDIR) -p ../$(BDIR)/static/obj/ssl
	-$(MKDIR) -p ../$(BDIR)/static/lib/ssl
ifeq (${ENABLE_SHARED}, true)
	-$(MKDIR) -p ../$(BDIR)/relocatable/obj/ssl
	-$(MKDIR) -p ../$(BDIR)/relocatable/lib/ssl
endif

setup_config: setup_ssl setup_ssl_install

install:
	$(CP) *.ad[sb] $(I_INC)
	$(CP) ../$(BDIR)/static/lib/ssl/* $(I_LIB)/static
ifeq (${ENABLE_SHARED}, true)
	$(CP) ../$(BDIR)/relocatable/lib/ssl/* $(I_LIB)/relocatable
endif
	$(CP) $(ISSLGPR) $(I_AGP)/aws_ssl_shared.gpr

clean:
	$(GNAT) clean -XLIBRARY_TYPE=static -Pssl
ifeq (${ENABLE_SHARED}, true)
	$(GNAT) clean -XLIBRARY_TYPE=relocatable -Pssl
endif
