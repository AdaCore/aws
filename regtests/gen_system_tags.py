#!/usr/bin/env python
"""
./gen_system_tags.py /path/to/aws/Makefile.setup

Parse AWS Makefile.setup to generate the system tags required by AWS
testsuite driver
"""

import os
import sys

# Importing gnatpython modules
CURDIR = os.getcwd()
PYTHON_SUPPORT = os.path.join(CURDIR, "python_support")
sys.path.append(PYTHON_SUPPORT)

from gnatpython.arch import Arch
from gnatpython.main import Main

def generate_tags(filename):
    _makefile = open(filename, 'r')
    tags = []
    for line in _makefile:
        # key=value
        key, value = tuple(line.rstrip().split('='))
        if value == "true":
            tags.append(key)
        else:
            tags.append("!" + key)
    _makefile.close()

    arch = Arch()
    tags.append(arch.platform)

    tags_string = ",".join(tags)
    open('testsuite.tags', 'w').write(tags_string.lower() + "\n")

    print "Generating testsuite.tags with: \n" + tags_string.lower()

def main():
    _main = Main()
    _main.parse_args()
    try:
        generate_tags(_main.args[0])
    except IndexError:
        _main.error("where is Makefile.setup ?")

if __name__ == "__main__":
    main()
