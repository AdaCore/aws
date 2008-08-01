"""
This module contains support functions for all test.py
"""

import logging
import os
import sys

# Change directory
TEST = sys.modules['__main__']
TESTDIR = os.path.dirname(TEST.__file__)
TEST_NAME = os.path.basename(TESTDIR)
os.chdir(TESTDIR)

# Load generated configuration
import config
config.set()

# Now gnatpython modules should be visible.
from gnatpython.ex import Run

def build(prj):
    """Build a project"""
    if config.with_gprbuild:
        gprbuild(prj)
    else:
        gnatmake(prj)

def gnatmake(prj):
    """Compile a project with gnatmake"""
    cmd = ["gnatmake", "-p", "-f", "-P" + prj]
    p = Run(cmd)
    if p.status:
        # Exit with error
        logging.error(p.out)
        sys.exit(p.status)
    else:
        logging.debug(p.out)

def gprbuild(prj):
    """Compile a project with gprbuild"""
    cmd = ["gprbuild", "-p", "-f", "-P" + prj]
    if config.use_profiler:
        cmd = cmd + ["-cargs", "-pg", "-O2", "-largs", "-pg"]
    p = Run(cmd)
    if p.status:
        # Exit with error
        logging.error(p.out)
        sys.exit(p.status)
    else:
        logging.debug(p.out)

def run(bin, output_file=None):
    """Execute a binary"""
    if output_file is None:
        output_file = "test.res"
    if config.use_gdb:
        p = Run(["gdb", "--exec=" + bin, "--eval-command=run", "--batch"],
                output=output_file)
    else:
        p = Run(["./" + bin], output=output_file)
    if p.status:
        # Exit with error
        logging.error(open(output_file).read())
        sys.exit(p.status)
    else:
        logging.debug(open(output_file).read())

    if config.use_profiler:
        Run(["gprof", bin],
            output=os.path.join(config.profiles_dir,
                                "%s_%s_gprof.out" % (TEST_NAME, bin)))

def diff(left=None, right=None):
    if left is None:
        left = "test.out"
    if right is None:
        right = "test.res"
    # Print the result of diff test.out "p.out"
    p = Run(["diff", left, right])
    if p.status:
        # Exit with error
        logging.error(p.out)
        sys.exit(p.status)
    else:
        logging.debug(p.out)

def build_diff(prj):
    """Compile and run a project and check the output"""
    build(prj)
    run(prj)
    diff()


