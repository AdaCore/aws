
# $Id$

.SILENT:

# Build the library only if Win32 OS. On other OS these libraries must be
# installed separately or are most of the time already installed.

GARGS = -m -q $(GFLAGS) -I../src $(INCLUDES)

LIBSSL		= ../lib/libssl.a
LIBCRYPTO	= ../lib/libcrypto.a
LIBLDAP		= ../lib/libldap.a
LIBWIN32	= ../lib/libaws-win32.a
AWS_COFF	= ../lib/aws.coff

ifeq (${OS}, Windows_NT)
   RES        = $(AWS_COFF)
   IMPORT_LIB = $(LIBSSL) $(LIBCRYPTO) $(LIBWIN32) $(LIBLDAP)
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

poll.o : poll.ads poll.adb
	echo Building Win32 poll support
	$(GNATMAKE) -u $(GARGS) poll.adb

$(LIBWIN32): wspiapi.o poll.o gai_strerror.o
	echo Building AWS Win32 support
	$(AR) r $(LIBWIN32) poll.o wspiapi.o gai_strerror.o

clean:
	-$(RM) -f $(LIBSSL) $(LIBCRYPTO) $(LIBWIN32) $(LIBLDAP) *~ aws.coff
	-$(RM) -f *.o

#############################################################################
# Configuration for GNAT Projet Files

gsetup:

gbuild: build

gclean: clean
