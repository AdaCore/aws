"""Main program initialization

This package provides a class called Main used to initialize a python script
invoked from command line. The main goal is to ensure consistency in term of
interface, documentation and logging activities for all scripts using
gnatpython.

When a script use this module it should contains a docstring formatted in
the following way. Everything before the first empty line will be part of the
usage. Everything after will be considered as part of the description.

The script will support by default the following switches::

    --target     to set the target
    --host       to set the host
    -v|--verbose to enable verbose mode (a console logger is added)
    -h|--help    display information parsed in the docstring
    --log-file FILE
                 to redirect logs to a given file (this is independant from
                 verbose option

*EXAMPLES*

If you have the following script test.py::

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

Here are some invocation examples::

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

from optparse import OptionParser, TitledHelpFormatter

import os
import sys
import re
import logging
import gnatpython.logging_util
from gnatpython.logging_util import highlight, COLOR_RED, COLOR_YELLOW, COLOR_GREEN, COLOR_CYAN
from   gnatpython.env import Env

class MainError (Exception):
    """MainError exception"""
    pass

class MainHelpFormatter(TitledHelpFormatter):
    """Format help with underlined section headers.

    Do not modify description formatting.
    """

    def format_description(self, description):
        """Do not modify description"""
        return description

color_table = {
    ' (FAILED|DIFF)'  : COLOR_RED,
    ' (UOK)'          : COLOR_YELLOW,
    ' (OK|PASSED)'    : COLOR_GREEN,
    ' (XFAIL)'        : COLOR_RED,
    ' (DEAD)'         : COLOR_CYAN}

class ConsoleColorFormatter(logging.Formatter):
    """Formatter with color support

    REMARKS
      If level is ERROR or CRITIVAL then the output color is set to red.
      Futhermore if some keyword such as PASSED,FAILED are detected then
      they are highlighted with an adequate color
    """
    def __init__(self, fmt=None, datefmt=None):
        logging.Formatter.__init__(self, fmt, datefmt)

    def format(self, record):
        output = logging.Formatter.format(self, record)
        if record.levelno >= logging.ERROR:
            output = highlight (output, fg=COLOR_RED)
        else:
            for k in color_table:
                output = re.sub (
                   k, highlight ("\\1", fg=color_table[k]), output) 
        return output

class Main(object):
    """
    ATTRIBUTES
      name       : name of the program (default is the filename with the
                   extension)
      usage      : contains the usage retrived from the main docstring
      description: contains the description retrieved from the main docstring
      options    : object containing the result of option parsing (see python
                   optparse module). Note that this object is made global by
                   putting its value in Env.main_options.
      args       : list of positionnal parameters after processing options
      add_option : this is in fact a method that can be used to add other
                   options (see documentation of the Python module optparse)
    """
    def __init__(self, name=None, formatter=None,
                 require_docstring=True, add_targets_options=True):
        """Init Main object

        PARAMETERS
          name: name of the program (if not specified the filename without
                extension is taken)
          formatter: override the default formatter for console output
          require_docstring: if True, raise MainError when the toplevel
                             docstring is not found
          add_target_options: add --target and --host options

        RETURN VALUE
          an instance of Main

        REMARKS
          None
        """
        main = sys.modules['__main__']

        if name is not None:
            self.name = name
        else:
            self.name = os.path.splitext(os.path.basename (main.__file__))[0]

        docstring = main.__doc__
        if require_docstring and docstring is None:
            raise MainError('Doc string not found')

        if docstring is not None:
            usage_end = docstring.find('\n\n')
            if usage_end == -1 and require_docstring:
                raise MainError('Doc string must start with a usage,'
                                'followed by an empty line')

        if docstring is not None:
            self.usage = docstring[0:usage_end]
            self.description = docstring[usage_end + 2:]
        else:
            self.usage = ""
            self.description = ""
        self.add_targets_options = add_targets_options

        self.__option_parser = OptionParser(
            usage=self.usage,
            description=self.description,
            formatter=MainHelpFormatter())

        # Make the add_option function directly available to Main objects
        self.add_option = self.__option_parser.add_option
        self.add_option("-v", "--verbose",
                        dest="verbose",
                        action="store_true",
                        default=False,
                        help="add some verbosity for debugging purposes."
                          + "Overrides --loglevel")
        self.add_option("--log-file",
                        dest="logfile",
                        metavar="FILE",
                        default="",
                        help="add some logs into the specified file")
        self.add_option("--enable-color",
                        dest="enable_color",
                        action="store_true",
                        default=False,
                        help="enable colors in log outputs")
        self.add_option("--loglevel", default="INFO",
                        action="store",
                        help="defines a loglevel (RAW,DEBUG,INFO,ERROR) for"
                          + " stdout")

        if add_targets_options:
            self.add_option("--target",
                            dest="target",
                            metavar="TARGET[,TARGET_VERSION]",
                            default="",
                            help="set target")
            self.add_option("--host",
                            dest="host",
                            metavar="HOST[,HOST_VERSION]",
                            default="",
                            help="set host")
        self.options   = None
        self.args      = None
        self.formatter = formatter
        self.__log_handlers_set = False

        # By default do not filter anything. What is effectively logged will
        # be defined by setting/unsetting handlers
        logging.getLogger('').setLevel(gnatpython.logging_util.RAW)

    def parse_args(self, args=None):
        """Parse options and set console logger

        PARAMETERS
          args: the list of positional parameters. If None then sys.argv[1:]
                is used

        RETURN VALUE
          None

        REMARKS
          None
        """

        levels = {'RAW'     : gnatpython.logging_util.RAW,
                  'DEBUG'   : logging.DEBUG,
                  'INFO'    : logging.INFO,
                  'ERROR'   : logging.ERROR,
                  'CRITICAL': logging.CRITICAL}

        (self.options, self.args) = self.__option_parser.parse_args(args)

        if not self.__log_handlers_set:
            # First set level of verbosity
            if self.options.verbose:
                level = gnatpython.logging_util.RAW
            else:
                level = levels.get (self.options.loglevel, logging.INFO)

            # Set logging handlers
            default_format = '%(levelname)-8s %(message)s'
            handler = gnatpython.logging_util.add_handlers(
                level=level,
                format=default_format)[0]

            if self.formatter is not None:
                default_format = self.formatter

            if self.options.enable_color:
                handler.setFormatter(ConsoleColorFormatter(default_format))
            else:
                if self.formatter is not None:
                    handler.setFormatter(logging.Formatter (self.formatter))

            # Log to a file if necessary
            if self.options.logfile != "":
                handler = gnatpython.logging_util.add_handlers(
                    level=gnatpython.logging_util.RAW,
                    format='%(asctime)s: %(name)-24s: %(levelname)-8s %(message)s',
                    filename=self.options.logfile)

            self.__log_handlers_set = True

        # Export options to env
        e = Env()
        e.main_options = self.options

        if self.add_targets_options:
            # Handle --target and --host options
            host_name      = None
            host_version   = None
            target_name    = None
            target_version = None

            if self.options.host != "":
                tmp = self.options.host.split(',')
                host_name = tmp[0]
                if len(tmp) > 1:
                    host_version = tmp[1]

            if self.options.target != "":
                tmp = self.options.target.split(',')
                target_name = tmp[0]
                if len(tmp) > 1:
                    target_version = tmp[1]

            e.set_host (host_name, host_version)
            e.set_target (target_name, target_version)

    def error (self, msg):
        """Print a usage message incorporating 'msg' to stderr and exit.

        PARAMETERS
        msg: Error message to display
        """
        self.__option_parser.error(msg)
