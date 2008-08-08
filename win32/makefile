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

#  Build the library only if Win32 OS. On other OS these libraries must be
#  installed separately or are most of the time already installed.

ODIR = ../$(BDIR)/../common/obj/win32
LDIR = ../$(BDIR)/../common/lib/win32

LIBSSL		= $(LDIR)/libssl.a
LIBCRYPTO	= $(LDIR)/libcrypto.a
LIBLDAP		= $(LDIR)/libldap.a
AWS_COFF	= $(LDIR)/aws.coff

ifeq (${OS}, Windows_NT)
   RES        = $(AWS_COFF)
   IMPORT_LIB = $(LIBSSL) $(LIBCRYPTO) $(LIBLDAP)
   TBUILD     = $(LDIR)/libaws_win32.a
   TINSTALL   = install_win32
else
   RES        =
   IMPORT_LIB =
   TBUILD     =
   TINSTALL   =
endif

force:

build: $(TBUILD) $(RES) $(IMPORT_LIB)

$(LDIR)/libaws_win32.a: gai_strerror.o
	echo Building libaws_win32.a
	$(AR) cr $(LDIR)/libaws_win32.a $+

gai_strerror.o: gai_strerror.c

$(ODIR)/%.o: %.o
	$(CP) $< $(ODIR)/$<

$(LIBSSL): libssl32.dll ssl.def
	echo Building libssl.a
	$(DLLTOOL) --def ssl.def --dllname libssl32.dll \
	        --output-lib $(LIBSSL)

$(LIBCRYPTO): libeay32.dll crypto.def
	echo Building libcrypto.a
	$(DLLTOOL) --def crypto.def --dllname libeay32.dll \
		--output-lib $(LIBCRYPTO)

$(LIBLDAP): wldap32.def
	echo Building libldap.a
	$(DLLTOOL) --def wldap32.def --dllname wldap32.dll \
		--output-lib $(LIBLDAP)

$(AWS_COFF): aws.rc
	echo Building aws.coff
	$(WINDRES) -i aws.rc -o $(AWS_COFF)

clean:
	-$(RM) -f $(LIBSSL) $(LIBCRYPTO) $(LIBLDAP) $(AWS_COFF) *~

setup:
	-$(MKDIR) -p $(ODIR)
	-$(MKDIR) -p $(LDIR)

install_win32:
	-$(MKDIR) -p $(prefix)/lib/aws/common
	$(CP) $(LIBSSL) $(prefix)/lib/aws/common
	$(CP) $(LIBCRYPTO) $(prefix)/lib/aws/common
	$(CP) $(LIBLDAP) $(prefix)/lib/aws/common
	$(CP) $(LDIR)/libaws_win32.a $(prefix)/lib/aws/common
	$(CP) $(AWS_COFF) $(I_LIB)

install: $(TINSTALL)

check:
