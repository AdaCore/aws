# $Id$

.SILENT: all build build_lib clean libssl.a libcrypto.a

ifeq (${OS}, Windows_NT)
IMPORT_LIB = build_lib
else
IMPORT_LIB =
endif

build: $(IMPORT_LIB) clean

libssl.a: libssl32.dll
	dll2def libssl32.dll > ssl.def
	dlltool --def ssl.def --dllname ssl32.dll --output-lib libssl.a

libcrypto.a: libeay32.dll
	dll2def libeay32.dll > crypto.def
	dlltool --def crypto.def --dllname eay32.dll \
		--output-lib libcrypto.a

build_lib: libssl.a libcrypto.a

clean:
	-rm *.def
