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

setup:
	-$(MKDIR) -p ../$(BDIR)/obj/include
	-$(MKDIR) -p ../$(BDIR)/lib/include
	${MAKE} -C zlib setup GALL_OPTIONS=$(GALL_OPTIONS)

build_zlib: force
	echo Building Zlib library
	${MAKE} -C zlib GALL_OPTIONS=$(GALL_OPTIONS)

build: build_zlib
	$(GNAT) make -Pinclude

install:
	$(CP) *.ad? $(I_CPN)
	-$(CP) ../$(BDIR)/lib/include/* $(I_LIB)
	${MAKE} -C zlib install GALL_OPTIONS=$(GALL_OPTIONS)

clean:
	-$(GNAT) clean -Pinclude -XAWS_BUILD=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}
	${MAKE} -C zlib clean

check:
	$(GNAT) check -files=filenames.check -rules -from=../aws.checks
