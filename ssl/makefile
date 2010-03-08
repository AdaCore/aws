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

.SILENT:

check:
	$(GNAT) check -Pssl -rules -from=../aws.checks

SSLGPR	=	../$(BDIR)/../../projects/aws_ssl_shared.gpr
ISSLGPR	=	../$(BDIR)/../../projects/install_aws_ssl_shared.gpr

setup_ssl:
	echo "project AWS_SSL_Shared is" > $(SSLGPR)
	echo "   for Source_Files use ();" >> $(SSLGPR)
ifeq (${SOCKET}, std)
	echo '   LIB_Path := "";' >> $(SSLGPR)
	echo '   S_SSL_Lib := "";' >> $(SSLGPR)
	echo '   R_SSL_Lib := "";' >> $(SSLGPR)
	echo '   S_CRY_Lib := "";' >> $(SSLGPR)
	echo '   R_CRY_Lib := "";' >> $(SSLGPR)
	echo '   S_TLS_Lib := "";' >> $(SSLGPR)
	echo '   R_TLS_Lib := "";' >> $(SSLGPR)
else
ifeq (${OS}, Windows_NT)
	echo '   LIB_Path := "../$(BDIR)/../common/lib/win32";' >> $(SSLGPR)
	echo '   S_SSL_Lib := "ssl";' >> $(SSLGPR)
	echo '   R_SSL_Lib := "ssl32";' >> $(SSLGPR)
	echo '   S_CRY_Lib := "crypto";' >> $(SSLGPR)
	echo '   R_CRY_Lib := "eay32";' >> $(SSLGPR)
	echo '   S_TLS_Lib := "gcrypt";' >> $(SSLGPR)
	echo '   R_TLS_Lib := "gcrypt";' >> $(SSLGPR)
else
	echo '   LIB_Path := ".";' >> $(SSLGPR)
	echo '   S_SSL_Lib := "ssl";' >> $(SSLGPR)
	echo '   R_SSL_Lib := "ssl";' >> $(SSLGPR)
	echo '   S_CRY_Lib := "crypto";' >> $(SSLGPR)
	echo '   R_CRY_Lib := "crypto";' >> $(SSLGPR)
	echo '   S_TLS_Lib := "gcrypt";' >> $(SSLGPR)
	echo '   R_TLS_Lib := "gcrypt";' >> $(SSLGPR)
endif
endif
	echo "end AWS_SSL_Shared;" >> $(SSLGPR)

setup_ssl_install:
	echo "project AWS_SSL_Shared is" > $(ISSLGPR)
	echo "   for Source_Files use ();" >> $(ISSLGPR)
ifeq (${OS}, Windows_NT)
	echo '   LIB_Path := "../../aws/native/ssl";' >> $(ISSLGPR)
	echo '   S_SSL_Lib := "ssl";' >> $(ISSLGPR)
	echo '   R_SSL_Lib := "ssl32";' >> $(ISSLGPR)
	echo '   S_CRY_Lib := "crypto";' >> $(ISSLGPR)
	echo '   R_CRY_Lib := "eay32";' >> $(ISSLGPR)
	echo '   S_TLS_Lib := "gcrypt";' >> $(ISSLGPR)
	echo '   R_TLS_Lib := "gcrypt";' >> $(ISSLGPR)
else
	echo '   LIB_Path := ".";' >> $(ISSLGPR)
	echo '   S_SSL_Lib := "ssl";' >> $(ISSLGPR)
	echo '   R_SSL_Lib := "ssl";' >> $(ISSLGPR)
	echo '   S_CRY_Lib := "crypto";' >> $(ISSLGPR)
	echo '   R_CRY_Lib := "crypto";' >> $(ISSLGPR)
	echo '   S_TLS_Lib := "gcrypt";' >> $(ISSLGPR)
	echo '   R_TLS_Lib := "gcrypt";' >> $(ISSLGPR)
endif
	echo "end AWS_SSL_Shared;" >> $(ISSLGPR)

setup:

setup_config: setup_ssl setup_ssl_install

install:
	$(CP) *.ad[sb] $(I_INC)
	$(CP) ../$(BDIR)/static/lib/ssl/* $(I_LIB)/static
ifeq (${ENABLE_SHARED}, true)
	$(CP) ../$(BDIR)/relocatable/lib/ssl/* $(I_LIB)/relocatable
endif
	$(CP) $(ISSLGPR) $(I_AGP)/aws_ssl_shared.gpr
