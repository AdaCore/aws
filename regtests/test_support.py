"""
This module contains support functions for all test.py
"""

import logging
import os
import sys
import re

from glob import glob
from e3.env import Env
from e3.os.fs import cd, mv
from e3.os.process import Run

Env().restore(os.environ["TEST_CONFIG"])

# Move to test directory
ROOT_DIR = os.getcwd()
TEST_DIR = os.path.dirname(sys.modules["__main__"].__file__)
TEST_NAME = os.path.basename(TEST_DIR)

def setup():
    cd(TEST_DIR)
    for prj in glob("*.gpr"):
        with open(prj) as prj_orig:
            lines = [line for line in prj_orig]
            with open(prj + ".new", "w") as prj_new:
                for line in lines:
                    line = line.replace("../common",
                                        os.path.join(os.environ['REGTESTS_DIR'], "common"))
                    prj_new.write(line)
        mv(prj + ".new", prj)


setup()


def tail(infile_name, outfile_name, nb_line):
    """Write outfile which contains infile with the top n lines removed

    If outfile_name is None, print to stdout
    """
    infile = open(infile_name, "r")
    if outfile_name is not None:
        outfile = open(outfile_name, "w")
    pos = 0
    for line in infile:
        pos = pos + 1
        if pos >= nb_line:
            if outfile_name is not None:
                outfile.write(line)
            else:
                print(line)
    if outfile_name is not None:
        outfile.close()
    infile.close()

def grep(infile_name, outfile_name, patterns):
    """Write outfile which contains line from infile matching pattern

    If outfile_name is None, print to stdout
    """
    infile = open(infile_name, "r")
    if outfile_name is not None:
        outfile = open(outfile_name, "w")
    pos = 0
    for line in infile:
        pos = pos + 1
        for pattern in patterns:
            r = re.search(pattern, line)
            if r != None:
                if outfile_name is not None:
                    outfile.write(line)
                else:
                    print(line[:-1])
    if outfile_name is not None:
        outfile.close()
    infile.close()

GPRBUILD_OPTIONS = []

def build(prj):
    """Compile a project with gprbuild"""
    cmd = ["gprbuild", f"-XHost_OS={Env().host.os.name}"]
    for O in os.environ.get('GPRBUILD_OPTIONS', "").split():
        cmd += [O]
    if Env().is_cross:
        cmd.append("--target=" + Env().target.triplet)
        if Env().target.os.name.startswith("vxworks"):
            cmd.append("-XPLATFORM=vxworks")
    cmd += ["-p", "-gnat2022", "-P" + prj, "-bargs", "-E"]
    if Env().options.with_gprof:
        cmd += ["-cargs", "-pg", "-O2", "-largs", "-pg"]
    process = Run(cmd)
    if process.status:
        #  Exit with error
        logging.error(process.out)


DEFAULT_RESOURCES = [
    "*.txt",
    "*.gz",
    "*.dat",
    "*.tmplt",
    "*.thtml",
    "*.html",
    "*.ini",
    "*.types",
    "*.mime",
    "*.gif",
    "*.png",
]


def run(bin, options=None, output_file=None, resources=None):
    """Run a test.

    :param resources: list of files that the executable will need to
        read, relative to the testcase directory.  This is used on
        cross configurations to send the files to the target platform
        before running BIN.

        Each item in the list can be a glob pattern.  By default,
        DEFAULT_RESOURCES will be used.
    """
    if options is None:
        options = []
    if "TIMEOUT" in os.environ:
        timeout = int(os.environ["TIMEOUT"])
    else:
        timeout = 300
    if resources is None:
        resources = DEFAULT_RESOURCES

    if Env().is_cross:
        # Import gnatpython excross module only when needed
        from pycross.runcross.main import run_cross

        run_cross(
            [bin + Env().target.os.exeext] + options,
            output=output_file,
            timeout=timeout,
            copy_files_on_target=resources,
        )
    else:
        if Env().options.with_gdb:
            Run(
                ["gdb", "--eval-command=run", "--batch-silent", "--args", bin]
                + options,
                output=output_file,
                timeout=timeout,
            )
        elif Env().options.with_valgrind:
            Run(
                ["valgrind", "-q", "./" + bin] + options,
                output=output_file,
                timeout=timeout,
            )
        else:
            Run(["./" + bin] + options, output=output_file, timeout=timeout)

    if Env().options.with_gprof:
        Run(
            ["gprof", bin] + options,
            output=os.path.join(
                Env().PROFILES_DIR, f"{TEST_NAME}_{bin}_gprof.out"
            ),
        )


def exec_cmd(bin, options=None, output_file=None, ignore_error=False):
    """Execute a binary"""
    if options is None:
        options = []
    process = Run([bin] + options, output=output_file)
    if process.status and not ignore_error:
        #  Exit with error
        logging.error(open(output_file).read())


def build_and_run(prj, resources=None):
    """Compile and run a project and check the output.

    :param resources: See :func:`run`.
    """
    build(prj)
    run(prj, resources=resources)
