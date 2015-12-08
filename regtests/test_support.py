"""
This module contains support functions for all test.py
"""

import logging
import os
import sys

from glob import glob
from gnatpython.env import Env
from gnatpython.fileutils import cd, mv
from gnatpython.ex import Run

Env().restore(os.environ['TEST_CONFIG'])

# Move to test directory
ROOT_DIR = os.getcwd()
TEST_DIR = os.path.dirname(sys.modules['__main__'].__file__)
TEST_NAME = os.path.basename(TEST_DIR)


def setup():
    cd(TEST_DIR)
    for prj in glob('*.gpr'):
        with open(prj) as prj_orig:
            lines = [line for line in prj_orig]
            with open(prj + '.new', 'w') as prj_new:
                for line in lines:
                    line = line.replace(
                        '../common', os.path.join(ROOT_DIR, 'common'))
                    prj_new.write(line)
        mv(prj + '.new', prj)

setup()


def tail(infile_name, outfile_name, nb_line):
    """Write outfile which contains infile with the top n lines removed

    If outfile_name is None, print to stdout
    """
    infile = open(infile_name, 'r')
    if outfile_name is not None:
        outfile = open(outfile_name, "w")
    pos = 0
    for line in infile:
        pos = pos + 1
        if pos >= nb_line:
            if outfile_name is not None:
                outfile.write(line)
            else:
                print line
    if outfile_name is not None:
        outfile.close()
    infile.close()


def build(prj):
    """Compile a project with gprbuild"""
    cmd = ["gprbuild"]
    if Env().is_cross:
        cmd.append("--target=" + Env().target.triplet)
        if Env().target.os.name.startswith('vxworks'):
            cmd.append('-XPLATFORM=vxworks')
    cmd = cmd + ["-p", "-gnat2012", "-P" + prj, "-bargs", "-E"]
    if Env().testsuite_config.with_gprof:
        cmd = cmd + ["-cargs", "-pg", "-O2", "-largs", "-pg"]
    process = Run(cmd)
    if process.status:
        #  Exit with error
        logging.error(process.out)


def run(bin, options=None, output_file=None):
    """Run a test"""
    if options is None:
        options = []
    if "TIMEOUT" in os.environ:
        timeout = int(os.environ["TIMEOUT"])
    else:
        timeout = 300

    if Env().is_cross:
        # Import gnatpython excross module only when needed
        from gnatpython.internal.excross import run_cross
        run_cross([bin + Env().target.os.exeext],
                  output=output_file, timeout=timeout,
                  copy_files_on_target=['*.txt',
                                        '*.gz',
                                        '*.dat',
                                        '*.tmplt',
                                        '*.thtml',
                                        '*.html',
                                        '*.ini',
                                        '*.types',
                                        '*.mime',
                                        '*.gif',
                                        '*.png'])
    else:
        if Env().testsuite_config.with_gdb:
            Run(["gdb", "--eval-command=run", "--batch-silent",
                 "--args", bin] + options, output=output_file, timeout=timeout)
        elif Env().testsuite_config.with_valgrind:
            Run(["valgrind", "-q", "./" + bin] + options,
                output=output_file, timeout=timeout)
        else:
            Run(["./" + bin] + options,
                output=output_file, timeout=timeout)

    if Env().testsuite_config.with_gprof:
        Run(["gprof", bin] + options,
            output=os.path.join(Env().PROFILES_DIR,
                                "%s_%s_gprof.out" % (TEST_NAME, bin)))


def exec_cmd(bin, options=None, output_file=None, ignore_error=False):
    """Execute a binary"""
    if options is None:
        options = []
    process = Run([bin] + options, output=output_file)
    if process.status and not ignore_error:
        #  Exit with error
        logging.error(open(output_file).read())


def build_and_run(prj):
    """Compile and run a project and check the output"""
    build(prj)
    run(prj)
