
# $Id$

.SILENT:

# Build the library only if Win32 OS. On other OS these libraries must be
# installed separately or are most of the time already installed.

GARGS = -m -q $(GFLAGS) -I../src $(INCLUDES)

LIBSSL		= ../lib/libssl.a
LIBCRYPTO	= ../lib/libcrypto.a
LIBLDAP		= ../lib/libldap.a
LIBPOLL         = ../lib/libpoll.a
LIBWSPIAPI      = ../lib/libwspiapi.a
AWS_COFF	= ../lib/aws.coff

ifeq (${OS}, Windows_NT)
   RES        = $(AWS_COFF)
   IMPORT_LIB = $(LIBSSL) $(LIBCRYPTO) $(LIBPOLL) $(LIBLDAP) $(LIBWSPIAPI)
else
   RES        =
   IMPORT_LIB =
endif

build: $(RES) $(IMPORT_LIB)

$(LIBSSL): libssl32.dll
	echo Building libssl.a
	dll2def libssl32.dll > ssl.def
	$(DLLTOOL) --def ssl.def --dllname libssl32.dll \
	        --output-lib $(LIBSSL)
	$(RM) ssl.def

$(LIBCRYPTO): libeay32.dll
	echo Building libcrypto.a
	$(DLL2DEF) libeay32.dll > crypto.def
	$(DLLTOOL) --def crypto.def --dllname libeay32.dll \
		--output-lib $(LIBCRYPTO)
	$(RM) crypto.def

$(LIBLDAP): wldap32.def
	echo Building libldap.a
	$(DLLTOOL) --def wldap32.def --dllname wldap32.dll \
		--output-lib $(LIBLDAP)

$(AWS_COFF): aws.rc
	echo Building aws.coff
	$(WINDRES) -i aws.rc -o $(AWS_COFF)

$(LIBPOLL): poll.ads poll.adb
	echo Building Win32 poll support
	$(GNATMAKE) -u $(GARGS) poll.adb
	$(AR) r $(LIBPOLL) poll.o

$(LIBWSPIAPI): wspiapi.h wspiapi.c
	echo Building protocol independent API support
	$(GCC) -c wspiapi.c
	$(AR) r $(LIBWSPIAPI) wspiapi.o

clean:
	-$(RM) -f $(LIBSSL) $(LIBCRYPTO) $(LIBPOLL) $(LIBLDAP) *~ aws.coff
	-$(RM) -f *.o

#############################################################################
# Configuration for GNAT Projet Files

gsetup:

gbuild: build

gclean: clean
