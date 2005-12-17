
.SILENT:

force:

ifneq (${OS}, Windows_NT)
SSL_PATH1 := $(shell ls /usr/lib/libcrypto.a 2> /dev/null)
SSL_PATH2 := $(shell ls /lib/libcrypto.a 2> /dev/null)
SSL_PATH3 := $(shell ls /usr/lib/libcrypto.so 2> /dev/null)
SSL_PATH4 := $(shell ls /lib/libcrypto.so 2> /dev/null)
endif

SSL_PATH = $(SSL_PATH1)

ifeq (${SSL_PATH},)
	SSL_PATH = $(SSL_PATH2)
endif
ifeq (${SSL_PATH},)
	SSL_PATH = $(SSL_PATH3)
endif
ifeq (${SSL_PATH},)
	SSL_PATH = $(SSL_PATH4)
endif

build:
	$(GNATMAKE) -Pssl -XSOCKET=${SOCKET} \
		-XAWS_BUILD=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}
	$(GNATMAKE) -Pnossl -XAWS_BUILD=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA}

SSLGPR	=	aws_ssl_shared.gpr

setup_ssl:
	echo "project AWS_SSL_Shared is" > $(SSLGPR)
	echo "   for Source_Files use ();" >> $(SSLGPR)
ifeq (${SOCKET}, std)
	echo '   LIB_Path := "";' >> $(SSLGPR)
else
ifeq (${OS}, Windows_NT)
	echo '   LIB_Path := "../lib";' >> $(SSLGPR)
else
	echo '   LIB_Path := "'$(dir $(SSL_PATH))'";' >> $(SSLGPR)
endif
endif
	echo "end AWS_SSL_Shared;" >> $(SSLGPR)

setup_ssl_install:
	echo "project AWS_SSL_Shared is" > $(SSLGPR)
	echo "   for Source_Files use ();" >> $(SSLGPR)
ifeq (${OS}, Windows_NT)
	echo '   LIB_Path := "../../../lib";' >> $(SSLGPR)
else
	echo '   LIB_Path := "'$(dir $(SSL_PATH))'";' >> $(SSLGPR)
endif
	echo "end AWS_SSL_Shared;" >> $(SSLGPR)


setup: setup_ssl
	-$(MKDIR) -p ../$(BDIR)/ssl/obj
	-$(MKDIR) -p ../$(BDIR)/ssl/lib
	-$(MKDIR) -p ../$(BDIR)/ssl/nobj
	-$(MKDIR) -p ../$(BDIR)/ssl/nlib

clean:
	-$(RM) -f $(SSLGPR)
