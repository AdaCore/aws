
# $Id$

.SILENT: all setup ../src/aws-translator-conversion.adb

ifeq (${OS}, Windows_NT)
EXEEXT = .exe
else
EXEEXT =
endif

setup: ../src/aws-translator-conversion.adb

../src/aws-translator-conversion.adb:
	echo Setup conversion support
	$(GNATMAKE) -q src/test_uconv
	./test_uconv
	-$(RM) test_uconv$(EXEEXT) test_uconv.o test_uconv.ali
	$(MV) aws-translator-conversion.adb ../src

clean:
	-$(RM) -f *.o *.ali *.~*.*~ *.exe

#############################################################################
# Configuration for GNAT Projet Files

gsetup: setup

gclean: clean

gbuild:
