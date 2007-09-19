
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

check:
	echo Rule \"raised exception is not declared...\"
	echo "   Expected 3 violations in aws-server-hotplug.adb"
	echo "   Expected 3 violations in aws-server.adb"
	$(GNAT) check -rules -from=../aws.checks \
		-Psrc -XBuild=${PRJ_BUILD} -XXMLADA=${PRJ_XMLADA} \
		-XSOCKLIB=${PRJ_SOCKLIB}
