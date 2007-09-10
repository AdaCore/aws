
.SILENT:

ifeq (${OS}, Windows_NT)
EXEEXT = .exe
else
EXEEXT =
endif

SDIR = ../.build/setup

setup_dir:
	-$(MKDIR) -p $(SDIR)/obj

setup: setup_dir ../src/aws-translator-conversion.adb ../src/aws-os_lib.ads

../src/aws-translator-conversion.adb: makefile
	echo Setup conversion support
	$(GNATMAKE_FOR_HOST) -Psetup
	$(SDIR)/test_uconv
	$(MV) aws-translator-conversion.adb ../src

../src/aws-os_lib.ads: makefile src/check_config.c
	echo Setup OS specific definitions
	$(GCC_FOR_HOST) -o $(SDIR)/check_config src/check_config.c
	$(SDIR)/check_config ../src/aws-os_lib.ads

build:

check:

clean:
	-$(RM) -f *.~*.*~
