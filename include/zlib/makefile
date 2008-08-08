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

.PHONY: all

CC = gcc

ifeq ($(DEBUG), true)
CFLAGS = -g
else
CFLAGS = -O3
endif

ODIR_S = ../../$(BDIR)/static/obj/zlib
LDIR_S = ../../$(BDIR)/static/lib/zlib

ODIR_R = ../../$(BDIR)/relocatable/obj/zlib
LDIR_R = ../../$(BDIR)/relocatable/lib/zlib

LIBZ_static = $(LDIR_S)/libz.a
LIBZ_shared = $(LDIR_R)/libz$(SOEXT)

LIB = $(LIBZ_static)

ifeq (${ENABLE_SHARED}, true)
LIB := $(LIB) $(LIBZ_shared)
ifneq (${OS}, Windows_NT)
CFLAGS := $(CFLAGS) -fPIC
endif
endif

OBJS = adler32.o compress.o crc32.o gzio.o uncompr.o deflate.o trees.o \
       zutil.o inflate.o infback.o inftrees.o inffast.o

BOBJS_S = $(addprefix $(ODIR_S)/, $(OBJS))
BOBJS_R = $(addprefix $(ODIR_R)/, $(OBJS))

all: $(LIB)

$(LIBZ_static): $(BOBJS_S)
	echo Building $(LIBZ_static)
	$(AR) rc $@ $(BOBJS_S)

$(LIBZ_shared): $(BOBJS_R)
	echo Building $(LIBZ_shared)
	$(GCC) -shared -o $@ $(BOBJS_R)

setup:
	$(MKDIR) $(ODIR_S)
	$(MKDIR) $(LDIR_S)
ifeq (${ENABLE_SHARED}, true)
	$(MKDIR) $(ODIR_R)
	$(MKDIR) $(LDIR_R)
endif

install:
	$(CP) $(LIBZ_static) $(I_LIB)/static
ifeq (${ENABLE_SHARED}, true)
	$(CP) $(LIBZ_shared) $(I_LIB)/relocatable
endif

clean:
	-$(RM) -f $(LIB) $(ODIR)/*.o *~

$(BOBJS_S):
	$(CC) $(CFLAGS) -c $(notdir $*).c -o $*.o

$(BOBJS_R):
	$(CC) $(CFLAGS) -c $(notdir $*).c -o $*.o

#  DO NOT DELETE THIS LINE -- make depend depends on it.

$(ODIR_S)/adler32.o: zlib.h zconf.h zlibdefs.h
$(ODIR_S)/compress.o: zlib.h zconf.h zlibdefs.h
$(ODIR_S)/crc32.o: crc32.h zlib.h zconf.h zlibdefs.h
$(ODIR_S)/deflate.o: deflate.h zutil.h zlib.h zconf.h zlibdefs.h
$(ODIR_S)/example.o: zlib.h zconf.h zlibdefs.h
$(ODIR_S)/gzio.o: zutil.h zlib.h zconf.h zlibdefs.h
$(ODIR_S)/inffast.o: zutil.h zlib.h zconf.h zlibdefs.h inftrees.h \
	inflate.h inffast.h
$(ODIR_S)/inflate.o: zutil.h zlib.h zconf.h zlibdefs.h inftrees.h \
	inflate.h inffast.h inffixed.h
$(ODIR_S)/infback.o: zutil.h zlib.h zconf.h zlibdefs.h inftrees.h \
	inflate.h inffast.h inffixed.h
$(ODIR_S)/inftrees.o: zutil.h zlib.h zconf.h zlibdefs.h inftrees.h
$(ODIR_S)/minigzip.o: zlib.h zconf.h zlibdefs.h
$(ODIR_S)/trees.o: deflate.h zutil.h zlib.h zconf.h zlibdefs.h trees.h
$(ODIR_S)/uncompr.o: zlib.h zconf.h zlibdefs.h
$(ODIR_S)/zutil.o: zutil.h zlib.h zconf.h zlibdefs.h

$(ODIR_R)/adler32.o: zlib.h zconf.h zlibdefs.h
$(ODIR_R)/compress.o: zlib.h zconf.h zlibdefs.h
$(ODIR_R)/crc32.o: crc32.h zlib.h zconf.h zlibdefs.h
$(ODIR_R)/deflate.o: deflate.h zutil.h zlib.h zconf.h zlibdefs.h
$(ODIR_R)/example.o: zlib.h zconf.h zlibdefs.h
$(ODIR_R)/gzio.o: zutil.h zlib.h zconf.h zlibdefs.h
$(ODIR_R)/inffast.o: zutil.h zlib.h zconf.h zlibdefs.h inftrees.h \
	inflate.h inffast.h
$(ODIR_R)/inflate.o: zutil.h zlib.h zconf.h zlibdefs.h inftrees.h \
	inflate.h inffast.h inffixed.h
$(ODIR_R)/infback.o: zutil.h zlib.h zconf.h zlibdefs.h inftrees.h \
	inflate.h inffast.h inffixed.h
$(ODIR_R)/inftrees.o: zutil.h zlib.h zconf.h zlibdefs.h inftrees.h
$(ODIR_R)/minigzip.o: zlib.h zconf.h zlibdefs.h
$(ODIR_R)/trees.o: deflate.h zutil.h zlib.h zconf.h zlibdefs.h trees.h
$(ODIR_R)/uncompr.o: zlib.h zconf.h zlibdefs.h
$(ODIR_R)/zutil.o: zutil.h zlib.h zconf.h zlibdefs.h
