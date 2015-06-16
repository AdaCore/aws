############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2003-2015, AdaCore                     #
#                                                                          #
#  This is free software;  you can redistribute it  and/or modify it       #
#  under terms of the  GNU General Public License as published  by the     #
#  Free Software  Foundation;  either version 3,  or (at your option) any  #
#  later version.  This software is distributed in the hope  that it will  #
#  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty #
#  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     #
#  General Public License for  more details.                               #
#                                                                          #
#  You should have  received  a copy of the GNU General  Public  License   #
#  distributed  with  this  software;   see  file COPYING3.  If not, go    #
#  to http://www.gnu.org/licenses for a complete copy of the license.      #
############################################################################

.SILENT:

build:

testsuite.tags: ../makefile.setup gen_system_tags.py
	$(PYTHON) gen_system_tags.py ../makefile.setup 2>/dev/null || \
		(echo "Can not generate system tags. The test are disabled")

setup: testsuite.tags

aws_regtests:
	test -f testsuite.tags || (echo 'Did you run make setup ?'; exit 1)
	$(PYTHON) testsuite.py --from-build-dir

install:

clean:
	-$(RM) -f testsuite.tags testsuite.res testsuite.duration_* *.pyc
	-$(RM) -fr .outputs .build
