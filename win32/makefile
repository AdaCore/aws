# $Id$

.SILENT: all build build_lib clean libssl.a libcrypto.a

# build the library only if Win32 OS and SSL mode

ifeq (${OS}, Windows_NT)

   ifeq (${MODE}, ssl)
      IMPORT_LIB = build_lib
   else
      IMPORT_LIB =
   endif

else
   IMPORT_LIB =
endif

build: $(IMPORT_LIB)

../ssl/libssl.a: libssl32.dll
	dll2def libssl32.dll > ssl.def
	dlltool --def ssl.def --dllname libssl32.dll \
	        --output-lib ../ssl/libssl.a
	rm ssl.def

../ssl/libcrypto.a: libeay32.dll
	dll2def libeay32.dll > crypto.def
	dlltool --def crypto.def --dllname libeay32.dll \
		--output-lib ../ssl/libcrypto.a
	rm crypto.def

build_lib: ../ssl/libssl.a ../ssl/libcrypto.a

clean:
	-rm ../ssl/*.a *~
