# Makefile for zlib
# Copyright (C) 1995-2003 Jean-loup Gailly.
# For conditions of distribution and use, see copyright notice in zlib.h

# Modified for AWS (Ada Web Server) by Dmitriy Anisimkov and Pascal Obry

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

ifeq (${ENABLE_SHARED}, true)
	LIB = $(LIBZ_shared)
ifneq (${OS}, Windows_NT)
	CFLAGS := $(CFLAGS) -fPIC
endif
else
	LIB = $(LIBZ_static)
endif

OBJS = adler32.o compress.o crc32.o gzio.o uncompr.o deflate.o trees.o \
       zutil.o inflate.o infback.o inftrees.o inffast.o

BOBJS_S = $(addprefix $(ODIR_S)/, $(OBJS))
BOBJS_R = $(addprefix $(ODIR_R)/, $(OBJS))

all: $(LIB)

$(LIBZ_static): $(BOBJS_S)
	$(AR) rc $@ $(BOBJS_S)

$(LIBZ_shared): $(BOBJS_R)
	$(GCC) -shared -o $@ $(BOBJS_R)

setup:
	$(MKDIR) $(ODIR_S)
	$(MKDIR) $(LDIR_S)
ifeq (${ENABLE_SHARED}, true)
	$(MKDIR) $(ODIR_R)
	$(MKDIR) $(LDIR_R)
endif

install:
	$(CP) $(LIB) $(I_LIB)/relocatable

clean:
	-rm -f $(LIB) $(ODIR)/*.o *~

$(BOBJS_S):
	$(CC) $(CFLAGS) -c $(notdir $*).c -o $*.o

$(BOBJS_R):
	$(CC) $(CFLAGS) -c $(notdir $*).c -o $*.o

# DO NOT DELETE THIS LINE -- make depend depends on it.

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
