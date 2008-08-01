"""Main program initialization

This package provides a class called Main used to initialize a python script
invoked from command line. The main goal is to ensure consistency in term of
interface, documentation and logging activities for all scripts using
gnatpython.

When a script use this module it should contains a docstring formatted in
the following way. Everything before the first empty line will be part of the
usage. Everything after will be considered as part of the description.

The script will support by default the following switch:
  --target     to set the target
  --host       to set the host
  -v|--verbose to enable verbose mode (a console logger is added)
  -h|--help    display information parsed in the docstring

EXAMPLE

If you have the following script test.py
    \"\"\"test [options] [args]

    This is the description\"\"\"

    import logging
    from gnatpython.main import *

    m = Main ()
    m.add_option ("-t",
                  "--test",
                  dest = "test",
                  metavar = "STRING",
                  default = "default",
                  help    = "option example")
    m.parse_args ()
    logging.info ('Test begin')
    logging.debug ('test option value: ' + m.options.test)
    logging.debug ('target option value: ' + m.options.target)
    logging.debug ('host option value: ' + m.options.host)
    logging.info ('Test end')

Here are some invocation examples:

    $ gnatpython test.py --help
    usage: test [options] [args]

    This is the description

    options:
      -h, --help            show this help message and exit
      -v, --verbose         add some verbosity for debugging purposes
      --target=TARGET       set target
      --host=HOST           set host
      -t STRING, --test=STRING
                            option example

    $ gnatpython test.py -v
    root        : INFO     Test begin
    root        : DEBUG    test option value: default
    root        : DEBUG    target option value:
    root        : DEBUG    host option value:
    root        : INFO     Test end

    $ gnatpython test.py
    root        : INFO     Test begin
    root        : INFO     Test end

"""

from os.path import *
from optparse import OptionParser, TitledHelpFormatter
import sys
import logging
import gnatpython.logging_util

class MainError (Exception): pass

class MainHelpFormatter(TitledHelpFormatter):
    """Format help with underlined section headers.

    Do not modify description formatting.
    """

    def format_description(self, description):
        """Do not modify description"""
        return description

class Main:
    """
    ATTRIBUTES
      name       : name of the program (default is the filename with the
                   extension)
      usage      : contains the usage retrived from the main docstring
      description: contains the description retrieved from the main docstring
      options    : object containing the result of option parsing (see python
                   optparse module)
      args       : list of positionnal parameters after processing options
      add_option : this is in fact a method that can be used to add other
                   options (see documentation of the Python module optparse)
    """
    def __init__ (self, name=None, formatter=None):
        """Init Main object

        PARAMETERS
          name: name of the program (if not specified the filename without
                extension is taken)
          formatter: override the default formatter for console output

        RETURN VALUE
          an instance of Main

        REMARKS
          None
        """
        main = sys.modules['__main__']

        (self.name, ext) = splitext (basename (main.__file__))

        docstring = main.__doc__
        if docstring is None:
            raise MainError ('Doc string not found')

        usage_end = docstring.find ('\n\n')
        if usage_end == -1:
            raise MainError ('Doc string must start with a usage,'
                             'followed by an empty line')

        self.usage = docstring[0:usage_end]
        self.description = docstring[usage_end + 2:]

        self.__option_parser = OptionParser (usage = self.usage,
                                             description = self.description,
                                             formatter=MainHelpFormatter())

        # Make the add_option function directly available to Main objects
        self.add_option = self.__option_parser.add_option

        self.add_option ("-v", "--verbose",
                         dest="verbose",
                         action="store_true",
                         default=False,
                         help="add some verbosity for debugging purposes")
        self.add_option ("--target",
                         dest="target",
                         metavar="TARGET",
                         default="",
                         help="set target")
        self.add_option ("--host",
                         dest="host",
                         metavar="HOST",
                         default="",
                         help="set host")
        self.options   = None
        self.args      = None
        self.formatter = formatter

        # By default do not filter anything. What is effectively logged will
        # be defined by setting/unsetting handlers
        logging.getLogger ('').setLevel (gnatpython.logging_util.RAW)

    def parse_args (self):
        """Parse options and set console logger

        PARAMETERS
          None

        RETURN VALUE
          None

        REMARKS
          None
        """
        (self.options, self.args) = self.__option_parser.parse_args ()
        if self.options.verbose:
            level = gnatpython.logging_util.RAW
        else:
            level = logging.INFO

        (handler, rawhandler) = gnatpython.logging_util.add_handlers \
            (level, format = '%(name)-12s: %(levelname)-8s %(message)s')

        if self.formatter is not None:
            handler.setFormatter(self.formatter)

    def error (self, msg):
     """Print a usage message incorporating 'msg' to stderr and exit.

     PARAMETERS
       msg: Error message to display
     """
     self.__option_parser.error(msg)


