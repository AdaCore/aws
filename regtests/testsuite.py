#!/usr/bin/env python
#                              Ada Web Server
#
#                     Copyright (C) 2003-2024, AdaCore
#
#  This library is free software; you can redistribute it and/or modify
#  This is free software;  you can redistribute it  and/or modify it
#  under terms of the  GNU General Public License as published  by the
#  Free Software  Foundation;  either version12,  or (at your option) any
#  later version.  This software is distributed in the hope  that it will
#  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty
#  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
#  General Public License for  more details.
#
#  You should have  received  a copy of the GNU General  Public  License
#  distributed  with  this  software;   see  file COPYING3.  If not, go
#  to http://www.gnu.org/licenses for a complete copy of the license.
"""
./testsuite.py [OPTIONS] [TEST_NAME]

This module is the main driver for AWS testsuite
"""
import logging
import os
import sys
import argparse
from pathlib import Path

from makevar import MakeVar
from subprocess import run

from e3.testsuite import Testsuite
from e3.testsuite.driver.adacore import AdaCoreLegacyTestDriver
from e3.testsuite.testcase_finder import ParsedTest, TestFinder

script_directory = os.path.dirname(os.path.abspath(sys.argv[0]))

class AWSTestFinder(TestFinder):
    """Look for testcase in directories which contains a test.py file.

    The Z999_xfail testcase is ignored if not required by the user.
    """
    def probe(
        self,
        testsuite,
        dirpath,
        dirnames,
        filenames,
    ):
        test_name = os.path.basename(dirpath)

        # skip Z999_xfail if needed
        if not testsuite.env.options.with_Z999 and test_name == "Z999_xfail":
            return None

        if "test.py" in filenames:
            return ParsedTest(test_name, AdaCoreLegacyTestDriver, {}, dirpath)
        else:
            return None


class AWSTestsuite(Testsuite):
    """Run the testsuite."""

    def set_up(self):
        """Setup the testsuite environment."""

        # Build the discriminants
        discs = self.env.discriminants + ["ALL"]

        if self.env.target.os.name == "vxworks6":
            discs += "vxworks6"

        self.env.discs = discs

        if self.env.options.target_dir is not None:
            build_dir = self.env.options.target_dir
        else:
            gcc_target = run("gcc -dumpmachine", capture_output=True, shell=True)
            build_dir = Path("..", gcc_target.stdout.decode().rstrip())

        # Read discriminants from testsuite.tags
        # The file testsuite.tags should have been generated by
        # AWS 'make setup'
        try:
            with open(build_dir.joinpath("testsuite.tags")) as tags_file:
                discs += tags_file.read().strip().split(",")
        except IOError:
            sys.exit("Cannot find testsuite.tags. Please run make setup")

        logging.debug(
            "Running the testsuite with the following discriminants: %s"
            % ", ".join(discs)
        )

        # Save discriminants
        with open(self.env.options.output_dir + "/discs", "w") as discs_f:
            discs_f.write(" ".join(discs))

        if self.env.options.with_gdb:
            # Serialize runs and disable gprof
            self.env.options.jobs = 1
            self.env.options.with_gprof = False

        # Add current directory in PYTHONPATH (to find test_support.py)
        self.env.add_search_path("PYTHONPATH", script_directory)

        if self.env.options.from_build_dir:
            os.environ["ADA_PROJECT_PATH"] = os.getcwd()
            # Read makefile.setup to set proper build environment
            c = MakeVar(str(build_dir.joinpath("makefile.setup")))
            os.environ["PRJ_BUILD"] = c.get("DEBUG", "true", "Debug", "Release")
            os.environ["PRJ_XMLADA"] = c.get("XMLADA", "true", "Installed", "Disabled")
            os.environ["PRJ_LAL"] = c.get("LAL", "true", "Installed", "Disabled")
            os.environ["PRJ_LDAP"] = c.get("LDAP", "true", "Installed", "Disabled")
            os.environ["PRJ_SOCKLIB"] = c.get("NETLIB")
            os.environ["SOCKET"] = c.get("SOCKET")
            os.environ["PRJ_TARGET"] = c.get("PRJ_TARGET")
            os.environ["LIBRARY_TYPE"] = "static"
            # from-build-dir only supported on native platforms
            os.environ["PLATFORM"] = "native"
            # Add current tools in from of PATH

            os.environ["GPRBUILD_OPTIONS"] = os.environ["GPROPTS"]
            os.environ["GPRBUILD_OPTIONS"] += " -XLIBRARY_TYPE=static"

            static_tools_dir = build_dir.joinpath(
                os.environ["PRJ_BUILD"].lower(), "static", "tools",
            )
            os.environ["PATH"] = os.pathsep.join(
                (str(static_tools_dir), os.environ["PATH"])
            )

        os.environ['REGTESTS_DIR'] = script_directory

        # Save the environment
        self.env.test_environ = dict(os.environ)
        self.env.test_environ["TEST_CONFIG"] = os.path.join(os.getcwd(), "env.dump")
        self.env.store(self.env.test_environ["TEST_CONFIG"])

    def add_options(self, parser: argparse.ArgumentParser) -> None:
        parser.add_argument(
            "--with-Z999",
            action="store_true",
            help="Add a test that always fail",
        )
        parser.add_argument(
            "--with-gprof",
            action="store_true",
            help="Generate profiling reports",
        )
        parser.add_argument(
            "--with-gdb",
            action="store_true",
            help="Run with gdb",
        )
        parser.add_argument(
            "--with-valgrind",
            action="store_true",
            help="Run with valgrind",
        )
        parser.add_argument(
            "--from-build-dir",
            action="store_true",
            help="Run testsuite from local build (in repository)",
        )
        parser.add_argument(
            "--target-dir",
            help=(
                "Directory where target-specific resources live"
                " (e.g. makefile.setup, testsuite.tags)."
            ),
            type=Path,
        )

    @property
    def test_finders(self):
        return [AWSTestFinder()]


if __name__ == "__main__":
    # Run the testsuite
    AWSTestsuite().testsuite_main()
