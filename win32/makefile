
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
	dlltool --def ssl.def --dllname libssl32.dll \
	        --output-lib libssl.a
	rm ssl.def

libcrypto.a: libeay32.dll
	dll2def libeay32.dll > crypto.def
	dlltool --def crypto.def --dllname libeay32.dll \
		--output-lib libcrypto.a
	rm crypto.def

libldap.a: wldap32.def
	dlltool --def wldap32.def --dllname wldap32.dll --output-lib libldap.a

build_lib: libssl.a libcrypto.a

aws.coff: aws.rc
	windres -i aws.rc -o aws.coff

clean:
	-rm *.a *~ aws.coff
