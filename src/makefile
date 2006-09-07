
.SILENT:

force:

aws-translator-conversion.adb:
	${MAKE} -C ../config setup

aws-os_lib-definitions.ads: ../config/src/check_config.c
	$(MAKE) -C ../config setup

setup:
	-$(MKDIR) -p ../$(BDIR)/obj
	-$(MKDIR) -p ../$(BDIR)/lib

build:
	$(GNATMAKE) -Psrc -XBuild=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA} \
		-XSOCKLIB=${PRJ_SOCKLIB}

clean:
	-$(RM) -f aws-translator-conversion.adb aws-os_lib-definitions.ads
