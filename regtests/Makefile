############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2003-2024, AdaCore                     #
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

GENERATOR := $(SRC_DIR)/regtests/gen_system_tags.py

# Add project path to generated project files
GPR_PROJECT_PATH := $(SRC_DIR)$(PSEP)$(PRJ_DIR)$(PSEP)$(GPR_PROJECT_PATH)
export GPR_PROJECT_PATH

build:

$(TGT_DIR)/testsuite.tags: $(TGT_DIR)/makefile.setup $(GENERATOR)
	$(PYTHON) $(GENERATOR) \
		$(TGT_DIR)/makefile.setup \
		$(TGT_DIR)/testsuite.tags 2>/dev/null || \
		(echo "Can not generate system tags. The test are disabled")

setup: $(TGT_DIR)/testsuite.tags

aws_regtests:
	test -f $(TGT_DIR)/testsuite.tags \
		|| (echo 'Did you run make setup ?'; exit 1)
	cd $(TGT_DIR) && \
		$(PYTHON) $(SRC_DIR)/regtests/testsuite.py \
			--from-build-dir --root-dir=$(BLD_DIR)

install:

clean:
	-$(RM) -f $(TGT_DIR)/testsuite.tags
	-$(RM) -f testsuite.res testsuite.duration_* *.pyc
	-$(RM) -fr .outputs .build
