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

from config import (
    set_config, PROFILES_DIR, DIFFS_DIR,
    WITH_GPROF, WITH_GDB, WITH_GPRBUILD,
    BUILD_FAILURE, DIFF_FAILURE, UNKNOWN_FAILURE
)
set_config()

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
    if WITH_GPRBUILD:
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
        sys.exit(BUILD_FAILURE)
    else:
        logging.debug(p.out)

def gprbuild(prj):
    """Compile a project with gprbuild"""
    cmd = ["gprbuild", "-p", "-f", "-cargs", "-gnat05", "-P" + prj,
           "-bargs", "-E"]
    if WITH_GPROF:
        cmd = cmd + ["-cargs", "-pg", "-O2", "-largs", "-pg"]
    p = Run(cmd)
    if p.status:
        #  Exit with error
        logging.error(p.out)
        sys.exit(BUILD_FAILURE)
    else:
        logging.debug(p.out)

def run(bin, options=[], output_file=None):
    """Run a test"""
    if "TIMEOUT" in os.environ:
        timeout = int(os.environ["TIMEOUT"])
    else:
        timeout = 300
    if output_file is None:
        output_file = "test.res"

    if WITH_GDB:
        p = Run(["gdb", "--eval-command=run", "--batch-silent",
                 "--args", bin] + options, output=output_file, timeout=timeout)
    else:
        p = Run(["./" + bin] + options, output=output_file, timeout=timeout)
    if p.status:
        #  Exit with error
        logging.error(open(output_file).read())
        sys.exit(UNKNOWN_FAILURE)
    else:
        logging.debug(open(output_file).read())

    if WITH_GPROF:
        Run(["gprof", bin] + options,
            output=os.path.join(PROFILES_DIR,
                                "%s_%s_gprof.out" % (TEST_NAME, bin)))

def exec_cmd(bin, options=[], output_file=None, ignore_error=False):
    """Execute a binary"""
    if output_file is None:
        output_file = bin + ".res"
    p = Run([bin] + options, output=output_file)
    if p.status and not ignore_error:
        #  Exit with error
        logging.error(open(output_file).read())
        sys.exit(UNKNOWN_FAILURE)
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
        save_test_diff(left,right)
        sys.exit(DIFF_FAILURE)
    else:
        logging.debug(p.out)

def save_test_diff(left, right):
    # Save diff file
    diff_filename = os.path.join(DIFFS_DIR, TEST_NAME + ".diff")
    diff_file     = open(diff_filename, 'w')

    diff_file.write("================ Bug %s\n" % TEST_NAME)

    if not os.path.exists(left):
        # No expected output. Write the first 100 line of the outputs
        lineno = 0
        right_file = open(right, 'r')
        diff_file.write('---------------- unexpected output\n')
        for line in right_file:
            if lineno >= 100:
                diff_file.write("[TRUNCATED]\n")
                break
            lineno = lineno + 1
            diff_file.write(line)
        right_file.close()
    else:
        # Output with diff
        # Limit the actual output to 2000 lines
        if os.path.exists(right):
            diff_file.write('---------------- actual output\n')
            right_file = open(right, 'r')
            lineno = 0
            for line in right_file:
                if lineno >= 2000:
                    break
                lineno = lineno + 1
                diff_file.write(line)
            right_file.close()

        diff_file.write('---------------- expected output\n')
        left_file = open(left, 'r')
        diff_file.write(left_file.read())
        left_file.close()
    diff_file.close()

def build_diff(prj):
    """Compile and run a project and check the output"""
    build(prj)
    run(prj)
    diff()
