
# $Id$

.SILENT: all build build_lib clean libssl.a libcrypto.a

# Build the library only if Win32 OS. On other OS these libraries must be
# installed separately or are most of the time already installed.

LIBSSL		= ../lib/libssl.a
LIBCRYPTO	= ../lib/libcrypto.a
LIBLDAP		= ../lib/libldap.a

ifeq (${OS}, Windows_NT)
   RES        = aws.coff
   IMPORT_LIB = build_lib $(LIBLDAP)
else
   RES        =
   IMPORT_LIB =
endif

build: $(RES) $(IMPORT_LIB)

$(LIBSSL): libssl32.dll
	dll2def libssl32.dll > ssl.def
	$(DLLTOOL) --def ssl.def --dllname libssl32.dll \
	        --output-lib $(LIBSSL)
	$(RM) ssl.def

$(LIBCRYPTO): libeay32.dll
	$(DLL2DEF) libeay32.dll > crypto.def
	$(DLLTOOL) --def crypto.def --dllname libeay32.dll \
		--output-lib $(LIBCRYPTO)
	$(RM) crypto.def

$(LIBLDAP): wldap32.def
	$(DLLTOOL) --def wldap32.def --dllname wldap32.dll \
		--output-lib $(LIBLDAP)

build_lib: $(LIBSSL) $(LIBCRYPTO)

aws.coff: aws.rc
	$(WINDRES) -i aws.rc -o aws.coff
	$(CP) aws.coff ../lib

clean:
	-$(RM) -f *.a *~ aws.coff

#############################################################################
# Configuration for GNAT Projet Files

gsetup: 

gbuild: build

gclean: clean
