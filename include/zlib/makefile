# Makefile for zlib
# Copyright (C) 1995-2003 Jean-loup Gailly.
# For conditions of distribution and use, see copyright notice in zlib.h

# Modified for AWS (Ada Web Server) by Dmitriy Anisimkov and Pascal Obry

.PHONY: all

CC = gcc

CFLAGS = -O3
#CFLAGS=-g -DDEBUG

LIBZ_static = ../../lib/libz.a


ifeq (${OS}, Windows_NT)
	LIBZ_shared = ../../lib/z$(SOEXT)
else
	LIBZ_shared = ../../lib/libz$(SOEXT)
endif

ifeq (${SHARED}, true)
	LIB = $(LIBZ_shared)
else
	LIB = $(LIBZ_static)
endif

OBJS = adler32.o compress.o crc32.o gzio.o uncompr.o deflate.o trees.o \
       zutil.o inflate.o infback.o inftrees.o inffast.o

all: $(LIB)

$(LIBZ_static): $(OBJS)
	$(AR) rc $@ $(OBJS)

$(LIBZ_shared): $(OBJS)
	$(GCC) -shared -o $@ $(OBJS)

clean:
	-rm -f $(LIB) *.o *~

# DO NOT DELETE THIS LINE -- make depend depends on it.

adler32.o: zlib.h zconf.h zlibdefs.h
compress.o: zlib.h zconf.h zlibdefs.h
crc32.o: crc32.h zlib.h zconf.h zlibdefs.h
deflate.o: deflate.h zutil.h zlib.h zconf.h zlibdefs.h
example.o: zlib.h zconf.h zlibdefs.h
gzio.o: zutil.h zlib.h zconf.h zlibdefs.h
inffast.o: zutil.h zlib.h zconf.h zlibdefs.h inftrees.h inflate.h inffast.h
inflate.o: zutil.h zlib.h zconf.h zlibdefs.h inftrees.h inflate.h inffast.h inffixed.h
infback.o: zutil.h zlib.h zconf.h zlibdefs.h inftrees.h inflate.h inffast.h inffixed.h
inftrees.o: zutil.h zlib.h zconf.h zlibdefs.h inftrees.h
minigzip.o: zlib.h zconf.h zlibdefs.h
trees.o: deflate.h zutil.h zlib.h zconf.h zlibdefs.h trees.h
uncompr.o: zlib.h zconf.h zlibdefs.h
zutil.o: zutil.h zlib.h zconf.h zlibdefs.h
