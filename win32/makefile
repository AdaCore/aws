# $Id$

.SILENT: all build build_lib clean libssl.a libcrypto.a

ifeq (${OS}, Windows_NT)
IMPORT_LIB = build_lib
else
IMPORT_LIB =
endif

build: $(IMPORT_LIB)

libssl.a: libssl32.dll
	dll2def libssl32.dll > ssl.def
	dlltool --def ssl.def --dllname libssl32.dll --output-lib libssl.a
	rm ssl.def

libcrypto.a: libeay32.dll
	dll2def libeay32.dll > crypto.def
	dlltool --def crypto.def --dllname libeay32.dll \
		--output-lib libcrypto.a
	rm crypto.def

build_lib: libssl.a libcrypto.a

clean:
	-rm *.a
