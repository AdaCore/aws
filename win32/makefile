
# $Id$

.SILENT:

# Build the library only if Win32 OS. On other OS these libraries must be
# installed separately or are most of the time already installed.

LIBSSL		= ../lib/libssl.a
LIBCRYPTO	= ../lib/libcrypto.a
LIBLDAP		= ../lib/libldap.a
AWS_COFF	= ../lib/aws.coff

ifeq (${OS}, Windows_NT)
   RES        = $(AWS_COFF)
   IMPORT_LIB = $(LIBSSL) $(LIBCRYPTO) $(LIBWIN32) $(LIBLDAP)
   SUPPORT    = build_c_support
else
   RES        =
   IMPORT_LIB =
   SUPPORT    =
endif

force:

build:
	$(GNATMAKE) -Pwin32 -XAWS_BUILD=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}

$(LIBSSL): libssl32.dll ssl.def
	echo Building libssl.a
	$(DLLTOOL) --def ssl.def --dllname libssl32.dll \
	        --output-lib $(LIBSSL)

$(LIBCRYPTO): libeay32.dll crypto.def
	echo Building libcrypto.a
	$(DLLTOOL) --def crypto.def --dllname libeay32.dll \
		--output-lib $(LIBCRYPTO)

$(LIBLDAP): wldap32.def
	echo Building libldap.a
	$(DLLTOOL) --def wldap32.def --dllname wldap32.dll \
		--output-lib $(LIBLDAP)

$(AWS_COFF): aws.rc
	echo Building aws.coff
	$(WINDRES) -i aws.rc -o $(AWS_COFF)

build_c_support: wspiapi.o gai_strerror.o
	$(CP) wspiapi.o gai_strerror.o ../.build/debug/win32/obj
	$(CP) wspiapi.o gai_strerror.o ../.build/release/win32/obj

clean:
	-$(RM) -f $(LIBSSL) $(LIBCRYPTO) $(LIBLDAP) *~ aws.coff

setup_dir:
	-$(MKDIR) -p ../.build/debug/win32/obj
	-$(MKDIR) -p ../.build/debug/win32/lib
	-$(MKDIR) -p ../.build/release/win32/obj
	-$(MKDIR) -p ../.build/release/win32/lib

setup: setup_dir $(RES) $(IMPORT_LIB) $(SUPPORT)
