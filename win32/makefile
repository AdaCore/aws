
# $Id$

.SILENT: all build build_lib clean libssl.a libcrypto.a

# Build the library only if Win32 OS. On other OS these libraries must be
# installed separately or are most of the time already installed.

ifeq (${OS}, Windows_NT)
   RES        = aws.coff
   IMPORT_LIB = build_lib libldap.a
else
   RES        =
   IMPORT_LIB =
endif

build: $(RES) $(IMPORT_LIB)

libssl.a: libssl32.dll
	dll2def libssl32.dll > ssl.def
	$(DLLTOOL) --def ssl.def --dllname libssl32.dll \
	        --output-lib libssl.a
	$(RM) ssl.def

libcrypto.a: libeay32.dll
	$(DLL2DEF) libeay32.dll > crypto.def
	$(DLLTOOL) --def crypto.def --dllname libeay32.dll \
		--output-lib libcrypto.a
	$(RM) crypto.def

libldap.a: wldap32.def
	$(DLLTOOL) --def wldap32.def --dllname wldap32.dll \
		--output-lib libldap.a

build_lib: libssl.a libcrypto.a

aws.coff: aws.rc
	$(WINDRES) -i aws.rc -o aws.coff

clean:
	-$(RM) -f *.a *~ aws.coff

#############################################################################
# Configuration for GNAT Projet Files

gsetup: 

gbuild: build

gclean: clean
