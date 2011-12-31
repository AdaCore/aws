#
#                              Ada Web Server
#
#                     Copyright (C) 2009-2012, AdaCore
#
#  This is free software;  you can redistribute it  and/or modify it
#  under terms of the  GNU General Public License as published  by the
#  Free Software  Foundation;  either version 3,  or (at your option) any
#  later version.  This software is distributed in the hope  that it will
#  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty
#  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
#  General Public License for  more details.
#
#  You should have  received  a copy of the GNU General  Public  License
#  distributed  with  this  software;   see  file COPYING3.  If not, go
#  to http://www.gnu.org/licenses for a complete copy of the license.

import sys

comment_character = '#'
kv_sep = '='

class MakeVar:
    """Simple makefile variable parser."""

    def __init__(self, config_file=None):
        self.config_file = config_file
        self.keys = []   # config keys
        self.config = {} # key/value dictionary

        config_file = open(self.config_file)
        lines = (line.strip() for line in config_file)
        # remove blanks lines
        lines = (line for line in lines if line)

        # parse key=value, skip comment lines
        for item in lines:
            if item.strip()[0] != comment_character:
                name, value = item.split(kv_sep, 1)

                # add to config dict and keys list
                self.config[name.strip()] = value.strip()
                self.keys.append(name.strip())

        config_file.close()

    def get(self, key, value="", equal="", notequal=""):
        """Returns the value associated with key if value not set.

        Returns equal if the value associated with key is value
        and notequal otherwise"""
        if key in self.keys:
            if value == "":
                return self.config[key]
            else:
                if self.config[key] == value:
                    return equal
                else:
                    return notequal
        else:
            return notequal
