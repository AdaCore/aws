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

BLD_DIR := $(abspath $(CURDIR)/..)
SRC_DIR := $(abspath $(CURDIR)/..)

include $(SRC_DIR)/makefile.conf

TGT_DIR := $(BLD_DIR)/$(TARGET)
PRJ_DIR := $(TGT_DIR)/projects
STP_DIR := $(TGT_DIR)/setup
CMN_DIR := $(TGT_DIR)/common

include $(BLD_DIR)/$(TARGET)/makefile.setup

# Add project path to generated project files
GPR_PROJECT_PATH := $(SRC_DIR)$(PSEP)$(PRJ_DIR)$(PSEP)$(GPR_PROJECT_PATH)
export GPR_PROJECT_PATH

# Add path to tools binaries
BKIND := $(shell echo $(PRJ_BUILD) | tr 'A-Z' 'a-z')
PATH := $(TGT_DIR)/$(BKIND)/static/tools:$(PATH)
export PATH

ALL_OPTIONS = SRC_DIR="$(SRC_DIR)" BLD_DIR="$(BLD_DIR)" TARGET="$(TARGET)" \
	TGT_DIR="$(TGT_DIR)" STP_DIR="$(STP_DIR)" KND_DIR="$(KND_DIR)" \
	PRJ_TARGET="$(PRJ_TARGET)" \
	PRJ_BUILD="$(PRJ_BUILD)" \
	PRJ_XMLADA="$(PRJ_XMLADA)" \
	PRJ_LAL="$(PRJ_LAL)" \
	PRJ_SOCKLIB="$(PRJ_SOCKLIB)" \
	PRJ_LDAP="$(PRJ_LDAP)" \
	PRJ_TARGET="$(PRJ_TARGET)" \
	TP_XMLADA="$(TP_XMLADA)" \
	GPROPTS="$(GPROPTS) -XLIBRARY_TYPE=static"

MAKEFILES = $(wildcard */Makefile)
MAKEFILES_CLEAN = ${MAKEFILES:%=%_clean}

build: $(MAKEFILES)

setup:

$(MAKEFILES): force
	$(MAKE) -C $(dir $@) $(ALL_OPTIONS)

clean: $(MAKEFILES_CLEAN)

$(MAKEFILES_CLEAN): force
	$(MAKE) -C $(dir ${@:%_clean=%}) clean $(ALL_OPTIONS)

force:

check:
