#!/usr/bin/env python
"""
./gen_system_tags.py /path/to/aws/makefile.setup /path/to/tags-file

Parse AWS makefile.setup to generate the system tags required by AWS
testsuite driver.
"""

import os
import sys

from e3.env import Env
from e3.main import Main

def generate_tags(filename, output):
    _makefile = open(filename, 'r')
    tags = []
    for line in _makefile:
        # key=value
        key, value = tuple(line.rstrip().split('='))
        if key == "SOCKET":
            if value == "std":
                tags.append("!ssl")
            else:
                tags.append("ssl")
                if value == "gnutls":
                    tags.append("gnutls")
                else:
                    tags.append("!gnutls")
        elif key == "NETLIB":
            tags.append(value)
        else:
            if value == "true":
                tags.append(key)
            else:
                tags.append("!" + key)
    _makefile.close()

    env = Env()
    tags.append(env.platform)

    tags_string = ",".join(tags)
    open(output, 'w').write(tags_string.lower() + "\n")

    print("Generating testsuite.tags with: \n" + tags_string.lower())

def main():
    _main = Main()
    _main.argument_parser.add_argument("setup_file")
    _main.argument_parser.add_argument("tags_file")
    _main.parse_args()
    try:
        generate_tags(_main.args.setup_file, _main.args.tags_file)
    except IndexError:
        _main.error("where is makefile.setup ?")

if __name__ == "__main__":
    main()
