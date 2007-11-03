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

ODIR = ../../$(BDIR)/obj/zlib
LDIR = ../../$(BDIR)/lib/zlib
LIBZ_static = $(LDIR)/libz.a
LIBZ_shared = $(LDIR)/libz$(SOEXT)

ifeq (${SHARED}, true)
	LIB = $(LIBZ_shared)
ifneq (${OS}, Windows_NT)
	CFLAGS := $(CFLAGS) -fPIC
endif
else
	LIB = $(LIBZ_static)
endif

OBJS = adler32.o compress.o crc32.o gzio.o uncompr.o deflate.o trees.o \
       zutil.o inflate.o infback.o inftrees.o inffast.o

BOBJS = $(addprefix $(ODIR)/, $(OBJS))

all: $(LIB)

$(LIBZ_static): $(BOBJS)
	$(AR) rc $@ $(BOBJS)

$(LIBZ_shared): $(BOBJS)
	$(GCC) -shared -o $@ $(BOBJS)

setup:
	$(MKDIR) $(ODIR)
	$(MKDIR) $(LDIR)

install:
	$(CP) $(LIB) $(I_LIB)
clean:
	-rm -f $(LIB) $(ODIR)/*.o *~

$(BOBJS):
	$(CC) $(CFLAGS) -c $(notdir $*).c -o $*.o

# DO NOT DELETE THIS LINE -- make depend depends on it.

$(ODIR)/adler32.o: zlib.h zconf.h zlibdefs.h
$(ODIR)/compress.o: zlib.h zconf.h zlibdefs.h
$(ODIR)/crc32.o: crc32.h zlib.h zconf.h zlibdefs.h
$(ODIR)/deflate.o: deflate.h zutil.h zlib.h zconf.h zlibdefs.h
$(ODIR)/example.o: zlib.h zconf.h zlibdefs.h
$(ODIR)/gzio.o: zutil.h zlib.h zconf.h zlibdefs.h
$(ODIR)/inffast.o: zutil.h zlib.h zconf.h zlibdefs.h inftrees.h \
	inflate.h inffast.h
$(ODIR)/inflate.o: zutil.h zlib.h zconf.h zlibdefs.h inftrees.h \
	inflate.h inffast.h inffixed.h
$(ODIR)/infback.o: zutil.h zlib.h zconf.h zlibdefs.h inftrees.h \
	inflate.h inffast.h inffixed.h
$(ODIR)/inftrees.o: zutil.h zlib.h zconf.h zlibdefs.h inftrees.h
$(ODIR)/minigzip.o: zlib.h zconf.h zlibdefs.h
$(ODIR)/trees.o: deflate.h zutil.h zlib.h zconf.h zlibdefs.h trees.h
$(ODIR)/uncompr.o: zlib.h zconf.h zlibdefs.h
$(ODIR)/zutil.o: zutil.h zlib.h zconf.h zlibdefs.h
