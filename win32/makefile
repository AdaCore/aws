############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2003-2009, AdaCore                     #
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

#  Build the library only if Win32 OS. On other OS these libraries must be
#  installed separately or are most of the time already installed.

LDIR = ../$(BDIR)/../common/lib/win32

LIBSSL		= $(LDIR)/libssl.dll.a
LIBCRYPTO	= $(LDIR)/libcrypto.dll.a
AWS_COFF	= $(LDIR)/aws.coff

ifeq (${OS}, Windows_NT)
   RES        = $(AWS_COFF)
   IMPORT_LIB = $(LIBSSL) $(LIBCRYPTO)
   TINSTALL   = install_win32
else
   RES        =
   IMPORT_LIB =
   TINSTALL   =
endif

$(LIBSSL): libssl32.dll ssl.def
	echo Building import libray libssl.dll.a
	$(DLLTOOL) --def ssl.def --dllname libssl32.dll \
	        --output-lib $(LIBSSL)

$(LIBCRYPTO): libeay32.dll crypto.def
	echo Building import library libcrypto.dll.a
	$(DLLTOOL) --def crypto.def --dllname libeay32.dll \
		--output-lib $(LIBCRYPTO)

$(AWS_COFF): aws.rc
	echo Building aws.coff
	$(WINDRES) -i aws.rc -o $(AWS_COFF)

setup_dir:
	-$(MKDIR) -p $(LDIR)

setup: setup_dir $(RES) $(IMPORT_LIB)

install_win32:
	-$(MKDIR) -p $(I_LIB)/ssl
	$(CP) $(LIBSSL) $(I_LIB)/ssl
	$(CP) $(LIBCRYPTO) $(I_LIB)/ssl
	$(CP) $(AWS_COFF) $(I_LIB)
ifeq (${ENABLE_SHARED}, true)
	$(CP) *.dll $(I_LIB)/ssl
	$(CP) *.dll $(I_BIN)
endif

install: $(TINSTALL)

check:
