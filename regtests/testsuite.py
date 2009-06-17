#!/usr/bin/env python
#                              Ada Web Server
#
#                          Copyright (C) 2003-2008
#                                  AdaCore
#
#  This library is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or (at
#  your option) any later version.
#
#  This library is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this library; if not, write to the Free Software Foundation,
#  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#
#  As a special exception, if other files instantiate generics from this
#  unit, or you link this unit with other files to produce an executable,
#  this  unit  does not  by itself cause  the resulting executable to be
#  covered by the GNU General Public License. This exception does not
#  however invalidate any other reasons why the executable file  might be
#  covered by the  GNU Public License.

"""
./testsuite.py [OPTIONS]

This module is the main driver for AWS testsuite
"""
import logging
import os
import shutil
import sys
import time
from glob import glob

# Importing gnatpython modules
CURDIR = os.getcwd()
PYTHON_SUPPORT = os.path.join(CURDIR, "python_support")
sys.path.append(PYTHON_SUPPORT)

from gnatpython.ex import Run
from gnatpython.fileutils import ln
from gnatpython.main import Main
from gnatpython.optfileparser import OptFileParse

DURATION_REPORT_NAME = "testsuite.duration"
TESTSUITE_RES = "testsuite.res"
OUTPUTS_DIR   = ".outputs"
BUILDS_DIR    = ".build"

BUILD_FAILURE   = 1
DIFF_FAILURE    = 2
UNKNOWN_FAILURE = 3


class Config(object):
    """Configure the testsuite"""
    CONFIG_TEMPLATE = """

import logging
import os
import sys
import test_support

PROFILES_DIR    = r"%(profiles_dir)s"
DIFFS_DIR       = r"%(diffs_dir)s"
WITH_GPROF      = %(with_gprof)s
WITH_GDB        = %(with_gdb)s
WITH_GPRBUILD   = %(with_gprbuild)s
BUILD_FAILURE   = %(build_failure)d
DIFF_FAILURE    = %(diff_failure)d
UNKNOWN_FAILURE = %(unknown_failure)d

def set_config():
    # Set python path
    sys.path.append("%(python_support)s")

    log_filename = os.path.basename(test_support.TESTDIR) + '.log'

    logging.basicConfig(level=logging.DEBUG,
                        datefmt='%%H:%%M:%%S',
                        filename=os.path.join(r'%(log_dir)s',
                                              log_filename),
                        mode="w")

    console = logging.StreamHandler()
    formatter = logging.Formatter('%%(levelname)-8s %%(message)s')
    console.setFormatter(formatter)
    console.setLevel(logging.%(logging_level)s)
    logging.getLogger('').addHandler(console)

"""


    def __init__(self, options):
        self.with_gdb = options.with_gdb
        if self.with_gdb:
            #  Serialize runs
            self.jobs = 1
            #  Disable gprof
            self.with_gprof = False
        else:
            self.jobs = options.jobs
            self.with_gprof = options.with_gprof

        self.delay = options.delay
        self.with_gprbuild = options.with_gprbuild

        if options.verbose:
            self.logging_level = "DEBUG"
        elif options.view_diffs:
            self.logging_level = "ERROR"
        else:
            self.logging_level = "CRITICAL"

        if options.tags is None:
            tags_file = open('testsuite.tags', 'r')
            self.tags = tags_file.read().strip()
            tags_file.close()
        else:
            self.tags = options.tags

        if options.tests is None:
            # Get all test.py
            self.tests = sorted(glob('*/test.py'), reverse=True)
        else:
            # tests parameter can be a file containing a list of tests
            if os.path.isfile(options.tests):
                list_file = open(options.tests, 'r')
                self.tests = []
                for line in list_file:
                    test_name = line.rstrip().split(':')[0]
                    self.tests.append(os.path.join(test_name, 'test.py'))
            else:
                # or a space separated string
                self.tests = [os.path.join(t, "test.py")
                              for t in options.tests.split()]

        if options.with_Z999:
            self.tests.insert(0, os.path.join("Z999_xfail", "always_fail.py"))

        self.python_support = PYTHON_SUPPORT
        self.log_dir        = os.path.join(CURDIR, OUTPUTS_DIR)
        self.profiles_dir   = os.path.join(self.log_dir, 'profiles')
        self.diffs_dir      = os.path.join(self.log_dir, "diffs")

        self.build_failure   = BUILD_FAILURE
        self.diff_failure    = DIFF_FAILURE
        self.unknown_failure = UNKNOWN_FAILURE

    def generate_config(self):
        """Generate config.py module that will be read by runtest.py"""
        conf = open("config.py", 'w')
        conf.write(self.CONFIG_TEMPLATE % self.__dict__)
        conf.close()

class Job(object):
    """Monitor a running test

    ATTRIBUTES
      name     : name of the test directory
      comment  : an optional comment
      duration : the duration of the test (or None if the job is still running)
    """
    def __init__(self, name, process, opt, start_time=None):
        """Create a new monitor

        PARAMETERS
          name       : name of the test directory
          process    : the running process
          opt        : content of test.opt file
          start_time : set to current date if not specified

        RETURN VALUE
          a Job object
        """
        if start_time is None:
            self.start_time = time.time()
        else:
            self.start_time = start_time
        self.name       = name
        self.process    = process
        self.opt        = opt
        self.duration   = None

        #  Get XFAIL value
        if self.opt is None:
            self.xfail = ""
        else:
            self.xfail = self.opt.get_value("XFAIL")

    def is_running(self):
        """Check if the job is still running

        RETURN VALUE
          True if the job is running (else False)
        """
        if self.process.poll() is not None:
            self.duration = time.time() - self.start_time
            if self.process.out is not None:
                logging.debug(self.process.out)
            return False
        return True

    def status(self):
        """Compute job final status

        RETURN VALUE
         a string (can be UOK, OK, XFAIL, DIFF or PROBLEM)
        """
        if self.process.status == 0:
            if self.xfail:
                return "UOK"
            else:
                return "OK"
        else:
            if self.xfail:
                return "XFAIL"
            else:
                if self.process.status == DIFF_FAILURE:
                    return "DIFF"
                else:
                    return "PROBLEM"

class Runner(object):
    """Run the testsuite

    Build a list of all subdirectories containing test.py then, for
    each test, parse the test.opt file (if exists) and run the test
    (by spawning a python process).
    """


    def __init__(self, config):
        """Fill the test lists"""
        self.__duration_report_rotate()
        self.jobs   = []
        self.config = config
        self.config.generate_config()

        logging.debug("Running the testsuite with the following tags: %s" %
                      self.config.tags)

        # Open report file
        self.fres    = open(TESTSUITE_RES, 'w')
        self.ftimeit = open(DURATION_REPORT_NAME + "_0", 'w')

        # Set python path
        if "PYTHONPATH" in os.environ:
            pythonpath = os.environ["PYTHONPATH"]
        else:
            pythonpath = ""
        os.environ["PYTHONPATH"] = CURDIR + os.pathsep + pythonpath

    def __duration_report_rotate(self):
        """Rotate all duration time reports"""
        for k in sorted(range(10), reverse=True):
            report_file      = DURATION_REPORT_NAME + "_%d" % k
            next_report_file = DURATION_REPORT_NAME + "_%d" % (k + 1)
            if os.path.exists(report_file):
                os.rename(report_file, next_report_file)

    def check_jobs(self):
        """Check if a job has terminated"""
        for index, job in enumerate(self.jobs):
            if not job.is_running():
                self.report(job.name, job.status(),
                            comment=job.xfail, duration=job.duration)
                del self.jobs[index]

    def report(self, name, status, comment=None, duration=None):
        """Add a test result in testsuite.res file.

        This will also log the result for interactive use of the testsuite.
        """
        self.fres.write('%s:%s:%s\n' % (name, status, comment))

        if duration is not None:
            self.ftimeit.write("%s\t%f\n" % (name, duration))
        else:
            self.ftimeit.write("%s\tNaN\n" % name)

        result = "%-60s %-9s %s" % (name, status, comment)

        test_desc_filename = os.path.join(name, "test.desc")
        if os.path.exists(test_desc_filename):
            test_desc = open(test_desc_filename, 'r')
            result = "%s [%s]" % (result, test_desc.read().strip())
            test_desc.close()
        logging.info(result)

    def start(self):
        """Start the testsuite"""
        linktree("common", os.path.join(BUILDS_DIR, "common"))

        while self.config.tests:
            # JOBS queued

            while len(self.jobs) < self.config.jobs and self.config.tests:
                # Pop a new job from the tests list and run it
                dead = False
                env  = None
                opt  = None
                test = self.config.tests.pop()
                test_dir = os.path.dirname(test)
                test_opt = os.path.join(test_dir, "test.opt")

                logging.debug("Running " + test_dir)
                if os.path.exists(test_opt):
                    opt = OptFileParse(self.config.tags, test_opt)
                    dead = opt.is_dead
                    if dead:
                        self.report(test_dir, "DEAD", opt.get_value('dead'))

                if not dead:
                    linktree(test_dir, os.path.join(BUILDS_DIR, test_dir))
                    test = os.path.join(BUILDS_DIR, test)
                    if opt is not None:
                        env = set_environment(opt.get_value("limit"))
                    process = Run(["python", test], bg=True, env=env,
                                  output=None, error=None)
                    job = Job(test_dir, process, opt)
                    self.jobs.append(job)

            # and continue loop
            time.sleep(self.config.delay)
            self.check_jobs()

        # No more test to run, wait for running jobs to terminate
        while self.jobs:
            time.sleep(self.config.delay)
            self.check_jobs()

        # Generate global diff
        global_diff = open('testsuite.diff', 'w')
        all_diffs   = glob(os.path.join(CURDIR, OUTPUTS_DIR, "diffs")
                           + '/*.diff')
        for diff_filename in all_diffs:
            global_diff.write(open(diff_filename).read())
        global_diff.close()

class ConsoleColorFormatter(logging.Formatter):
    """Output colorfull text"""
    def format(self, record):
        """If TERM supports colors, colorize output"""
        if not hasattr(self, "usecolor"):
            self.usecolor = not ((sys.platform=='win32')
                                 or ('NOCOLOR' in os.environ)
                                 or (os.environ.get('TERM', 'dumb')
                                     in ['dumb', 'emacs'])
                                 or (not sys.stderr.isatty()))

        output = logging.Formatter.format(self, record)
        if self.usecolor:
            if "PROBLEM" in output or "DIFF" in output:
                output = '\033[01;31m' + output + '\033[0m'
            elif "UOK" in output:
                output = '\033[01;33m' + output + '\033[0m'
            elif "OK" in output:
                output = '\033[01;32m' + output + '\033[0m'
            elif "XFAIL" in output:
                output = '\033[00;31m' + output + '\033[0m'
            elif "DEAD" in output:
                output = '\033[01;36m' + output + '\033[0m'
        return output

def linktree(src, dst, symlinks=0):
    """Hard link all files from src directory in dst directory"""
    names = os.listdir(src)
    os.mkdir(dst)
    for name in names:
        srcname = os.path.join(src, name)
        dstname = os.path.join(dst, name)
        try:
            if symlinks and os.path.islink(srcname):
                linkto = os.readlink(srcname)
                os.symlink(linkto, dstname)
            elif os.path.isdir(srcname):
                linktree(srcname, dstname, symlinks)
            else:
                ln(srcname, dstname)
        except (IOError, os.error), why:
            print "Can't link %s to %s: %s" % (srcname, dstname, str(why))

def set_environment(timeout):
    """Set the environment for running the test.

    Returns None if no special environment is needed.
    """
    env = None
    if timeout:
        env = os.environ.copy()
        env["TIMEOUT"] = timeout
    return env

def main():
    """Main: parse command line and run the testsuite"""

    if os.path.exists(OUTPUTS_DIR):
        shutil.rmtree(OUTPUTS_DIR)
    os.mkdir(OUTPUTS_DIR)
    os.mkdir(os.path.join(OUTPUTS_DIR, 'profiles'))
    os.mkdir(os.path.join(OUTPUTS_DIR, 'diffs'))

    if os.path.exists(BUILDS_DIR):
        shutil.rmtree(BUILDS_DIR)
    os.mkdir(BUILDS_DIR)

    # Add rlimit to PATH
    os.environ["PATH"] = os.environ["PATH"] + os.pathsep + CURDIR

    logging.basicConfig(level=logging.DEBUG,
                        filename='%s/testsuite.log' % OUTPUTS_DIR, mode='w')
    main = Main(formatter=ConsoleColorFormatter('%(message)s'))
    main.add_option("--tests", dest="tests",
                    help="list of tests to run, a space separated string or " \
                        "a filename.")
    main.add_option("--with-Z999", dest="with_Z999",
                    action="store_true", default=False,
                    help="Add a test that always fail")
    main.add_option("--view-diffs", dest="view_diffs", action="store_true",
                    default=False, help="show diffs on stdout")
    main.add_option("--jobs", dest="jobs", type="int", default=5,
                    help="Number of jobs to run in parallel")
    main.add_option("--delay", dest="delay", type="float", default=0.1,
                    help="Delay between two loops")
    main.add_option("--tags", dest="tags",
                    help="tags to use instead of testsuite.tags content")
    main.add_option("--with-gprof", dest="with_gprof", action="store_true",
                    default=False, help="Generate profiling reports")
    main.add_option("--with-gdb", dest="with_gdb", action="store_true",
                    default=False, help="Run with gdb")
    main.add_option("--with-gnatmake", dest="with_gprbuild",
                    action="store_false", default=False,
                    help="Compile with gnatmake")
    main.add_option("--with-gprbuild", dest="with_gprbuild",
                    action="store_true", default=False,
                    help="Compile with gprbuild (default is gnatmake)")
    main.parse_args()

    config = Config(main.options)
    run = Runner(config)
    run.start()

if __name__ == "__main__":
    main()
