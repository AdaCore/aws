
# $Id$

.SILENT:

force:

ifneq (${OS}, Windows_NT)
SSL_PATH1 := $(shell ls /usr/lib/libcrypto.a 2> /dev/null)
SSL_PATH2 := $(shell ls /lib/libcrypto.a 2> /dev/null)
endif

build:
	$(GNATMAKE) -Pssl -XSOCKET=${SOCKET} \
		-XAWS_BUILD=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}
	$(GNATMAKE) -Pnossl -XAWS_BUILD=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}

setup_ssl:
	echo "project SSL_Shared is" > ssl_shared.gpr
	echo "   for Source_Files use ();" >> ssl_shared.gpr
ifeq (${SOCKET}, std)
	echo '   LIB_Path := "";' >> ssl_shared.gpr
else
ifeq (${OS}, Windows_NT)
	echo '   LIB_Path := "../lib";' >> ssl_shared.gpr
else
ifneq (${SSL_PATH1},)
	echo '   LIB_Path := "'$(dir $(SSL_PATH1))'";' >> ssl_shared.gpr
else
	echo '   LIB_Path := "'$(dir $(SSL_PATH2))'";' >> ssl_shared.gpr
endif
endif
endif
	echo "end SSL_Shared;" >> ssl_shared.gpr

setup: setup_ssl
	-$(MKDIR) -p ../.build/debug/ssl/obj
	-$(MKDIR) -p ../.build/debug/ssl/lib
	-$(MKDIR) -p ../.build/debug/ssl/lobj
	-$(MKDIR) -p ../.build/release/ssl/obj
	-$(MKDIR) -p ../.build/release/ssl/lib
	-$(MKDIR) -p ../.build/release/ssl/lobj

clean:
	-$(RM) -f ssl_shared.gpr
