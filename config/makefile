
.SILENT:

ifeq (${OS}, Windows_NT)
EXEEXT = .exe
else
EXEEXT =
endif

setup: ../src/aws-translator-conversion.adb ../src/aws-os_lib-definitions.ads

../src/aws-translator-conversion.adb: makefile
	echo Setup conversion support
	$(GNATMAKE_FOR_HOST) -Psetup
	./test_uconv
	$(MV) aws-translator-conversion.adb ../src

../src/aws-os_lib-definitions.ads: makefile src/check_config.c
	echo Setup OS specific definitions
	$(GCC_FOR_HOST) -o check_config src/check_config.c
	./check_config ../src/aws-os_lib.ads

build:

clean:
	-$(RM) -f *.o *.ali *.~*.*~ *.exe test_uconv check_config
