
# $Id$

.SILENT:

ifeq (${OS}, Windows_NT)
EXEEXT = .exe
else
EXEEXT =
endif

setup: ../src/aws-translator-conversion.adb ../src/aws-os_lib-definitions.ads

../src/aws-translator-conversion.adb: makefile
	echo Setup conversion support
	$(GNATMAKE_FOR_HOST) -q src/test_uconv
	./test_uconv
	-$(RM) test_uconv$(EXEEXT) test_uconv.o test_uconv.ali
	$(MV) aws-translator-conversion.adb ../src

../src/aws-os_lib-definitions.ads: makefile src/check_config.c
	echo Setup OS specific definitions
	$(GCC_FOR_HOST) -o check_config src/check_config.c
	./check_config > ../src/aws-os_lib-definitions.ads

build:

clean:
	-$(RM) -f *.o *.ali *.~*.*~ *.exe test_uconv check_config
