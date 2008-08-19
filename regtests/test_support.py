"""
This module contains support functions for all test.py
"""

import logging
import os
import sys

#  Change directory

TEST = sys.modules['__main__']
TESTDIR = os.path.dirname(TEST.__file__)
TEST_NAME = os.path.basename(TESTDIR)
os.chdir(TESTDIR)

#  Load generated configuration

import config
config.set()

#  Now gnatpython modules should be visible

from gnatpython.ex import Run

def tail(infile, outfile, n):
    """Write outfile which contains infile with the top n lines removed"""
    ci=open(infile).readlines()
    fo=open(outfile, "w")
    k=0
    for line in ci:
        k = k + 1
        if k >= n:
            fo.write(line)
    fo.close()

def build(prj):
    """Build a project"""
    if config.with_gprbuild:
        gprbuild(prj)
    else:
        gnatmake(prj)

def gnatmake(prj):
    """Compile a project with gnatmake"""
    cmd = ["gnatmake", "-p", "-f", "-gnat05", "-P" + prj, "-bargs", "-E"]
    p = Run(cmd)
    if p.status:
        #  Exit with error
        logging.error(p.out)
        sys.exit(p.status)
    else:
        logging.debug(p.out)

def gprbuild(prj):
    """Compile a project with gprbuild"""
    cmd = ["gprbuild", "-p", "-f", "-cargs", "-gnat05", "-P" + prj,
           "-bargs", "-E"]
    if config.use_profiler:
        cmd = cmd + ["-cargs", "-pg", "-O2", "-largs", "-pg"]
    p = Run(cmd)
    if p.status:
        #  Exit with error
        logging.error(p.out)
        sys.exit(p.status)
    else:
        logging.debug(p.out)

def run(bin, options=[], output_file=None):
    """Run a test"""
    if output_file is None:
        output_file = "test.res"
    if config.use_gdb:
        p = Run(["gdb", "--exec=" + bin, "--eval-command=run", "--batch",
                 "--args"] + options, output=output_file)
    else:
        p = Run(["./" + bin] + options, output=output_file)
    if p.status:
        #  Exit with error
        logging.error(open(output_file).read())
        sys.exit(p.status)
    else:
        logging.debug(open(output_file).read())

    if config.use_profiler:
        Run(["gprof", bin] + options,
            output=os.path.join(config.profiles_dir,
                                "%s_%s_gprof.out" % (TEST_NAME, bin)))

def exec_cmd(bin, options=[], output_file=None, ignore_error=False):
    """Execute a binary"""
    if output_file is None:
        output_file = bin + ".res"
    p = Run([bin] + options, output=output_file)
    if p.status and not ignore_error:
        #  Exit with error
        logging.error(open(output_file).read())
        sys.exit(p.status)
    else:
        logging.debug(open(output_file).read())

def diff(left=None, right=None):
    if left is None:
        left = "test.out"
    if right is None:
        right = "test.res"
    #  Print the result of diff test.out "p.out"
    p = Run(["diff", "-w", left, right])
    if p.status:
        #  Exit with error
        logging.error(p.out)
        sys.exit(p.status)
    else:
        logging.debug(p.out)

def build_diff(prj):
    """Compile and run a project and check the output"""
    build(prj)
    run(prj)
    diff()
