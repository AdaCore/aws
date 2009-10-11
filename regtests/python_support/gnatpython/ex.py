"""Subprocesses management

This package provides a single class called run which ease spawn of processes
in blocking or non blocking mode and redirection of its stdout, stderr and
stdin"""

import gnatpython.logging_util

from subprocess import Popen, STDOUT, PIPE

import logging
import os
import sys
import time

BUF_SIZE = 128

logger = logging.getLogger('gnatpython.ex')

class Run:
    """
    ATTRIBUTES
      status : exit status (meaningfull only after the end of the process)
      out    : process standard output  (if instanciated with output = PIPE)
      err    : same as out but for standard error
      pid    : PID
    """
    def __init__ (self, cmds, cwd=None, output=PIPE,
                  error=STDOUT, input=None, bg=False, timeout=None, env=None,
                  set_sigpipe=False):
        """Spawn a process

        PARAMETERS
          cmds:    two possibilities:
                   (1) a command line: a tool name and its arguments, passed
                   in a list. e.g. ['ls', '-a', '.']
                   (2) a list of command lines (as defined in (1)): the
                   different commands will be piped. This means that
                   [['ps', '-a'], ['grep', 'vxsim']] will be equivalent to
                   the system command line 'ps -a | grep vxsim'.
          cwd :    directory in which the process should be executed (string
                   or None). If None then current directory is used
          output:  can be PIPE, a filename string, a fd on an already opened
                   file, a python file object or None.
          env:     dictionary for environment variables (e.g. os.environ)
          error:   same as output or STDOUT, which indicates that the stderr
                   data from the applications should be captured into the
                   same file handle as for stdout.
          input:   same as output
          bg:      if True then run in background
          timeout: limit execution time (in seconds).

        RETURN VALUE
          Return an object of type run.

        EXCEPTIONS
          Raise OSError when trying to execute a non-existent file.

        REMARKS
          If you specify a filename for output or stderr then file content is
          reseted (equiv. to > in shell). If you prepend the filename with '+'
          then the file will be opened in append mode (equiv. to >> in shell)
          If you prepend the input with '|', then the content of input string
          will be used for process stdin.
        """
        def subprocess_setup():
            """Reset SIGPIPE hander

            Python installs a SIGPIPE handler by default. This is usually not what
            non-Python subprocesses expect.
            """
            if set_sigpipe:
                # Set sigpipe only when set_sigpipe is True
                # This should fix HC16-020 and could be activated by default
                import signal
                signal.signal(signal.SIGPIPE, signal.SIG_DFL)


        # First resolve output, error and input
        self.input_file  = File(input, 'r')
        self.output_file = File(output, 'w')
        self.error_file  = File(error, 'w')

        self.status = None
        self.out    = ''
        self.err    = ''

        if env is None:
            env = os.environ

        rlimit_args = []
        if timeout is not None:
            rlimit_args = ['rlimit', '%d' % timeout]

        try:
            if not isinstance(cmds[0], list):
                logger.debug('Run: %s' % " ".join(rlimit_args + cmds))

                popen_args = {
                    'stdin' : self.input_file.fd,
                    'stdout' : self.output_file.fd,
                    'stderr' : self.error_file.fd,
                    'cwd'    : cwd,
                    'env'    : env,
                    'universal_newlines' : True}

                if sys.platform != 'win32':
                    # preexec_fn is no supported on windows
                    popen_args['preexec_fn'] = subprocess_setup

                self.internal = Popen(rlimit_args + cmds, **popen_args)

            else:
                cmds[0] = rlimit_args + cmds[0]
                logger.debug ('Run: %s ' %
                              " | ".join([" ".join(cmd) for cmd in cmds]))
                runs = []
                for index, cmd in enumerate(cmds):
                    if index == 0:
                        stdin = self.input_file.fd
                    else:
                        stdin = runs[index - 1].stdout

                    # When connecting two processes using a Pipe don't use
                    # universal_newlines mode. Indeed commands transmitting
                    # binary data between them will crash
                    # (ex: gzip -dc toto.txt | tar -xf -)
                    if index == len(cmds) - 1:
                        stdout   = self.output_file.fd
                        txt_mode = True
                    else:
                        stdout   = PIPE
                        txt_mode = False

                    popen_args = {
                        'stdin' : stdin,
                        'stdout' : stdout,
                        'stderr' : self.error_file.fd,
                        'cwd'    : cwd,
                        'env'    : env,
                        'universal_newlines' : txt_mode}

                    if sys.platform != 'win32':
                        # preexec_fn is no supported on windows
                        popen_args['preexec_fn'] = subprocess_setup

                    runs.append(Popen(cmd, **popen_args))
                    self.internal = runs[-1]

        except Exception:
            self.__error ()
            raise

        self.pid = self.internal.pid

        if not bg:
            self.wait ()

    def __close_files (self):
        """Internal procedure"""
        self.output_file.close ()
        self.error_file.close ()
        self.input_file.close()

    def __error (self):
        """Set pid to -1 and status to 127 before closing files"""
        self.pid = -1
        self.status = 127
        self.__close_files ()

    def wait (self):
        """Wait until process ends and return its status"""
        if self.status == 127:
            return self.status

        self.status = None

        if self.input_file.fd == PIPE:
            comm = self.internal.communicate(self.input_file.get_command())
            if self.output_file.fd == PIPE:
                self.out = comm[0]

            if self.error_file.fd == PIPE:
                self.err = comm[0]

        if self.output_file.fd != PIPE and self.error_file.fd != PIPE:
            self.status = self.internal.wait ()
        else:
            size = BUF_SIZE
            while size != -1:
                if self.status is not None:
                    size = -1     # Read until EOF

                if self.output_file.fd == PIPE and self.input_file.fd != PIPE:
                    # Read at most size bytes of output pipe
                    _buffer = self.internal.stdout.read(size)
                    if _buffer != "":
                        logger.log(gnatpython.logging_util.RAW, _buffer)
                        self.out += _buffer

                if self.error_file.fd == PIPE and self.input_file.fd != PIPE:
                    # Read at most size bytes of error pipe
                    _buffer = self.internal.stderr.read(size)
                    if _buffer != "":
                        logger.log(gnatpython.logging_util.RAW, _buffer)
                        self.err += _buffer

                if self.status is None:
                    self.status = self.internal.poll ()
                    time.sleep (0.1)

        self.__close_files ()
        return self.status

    def poll (self):
        """Test if the process is still alive. If yes then return None,
        otherwise return process status"""

        if self.status != 127:
            result = self.internal.poll ()
            if result is not None:
                self.status = result
        else:
            result = 127
        return result

class File(object):
    """Can be a PIPE, a file object"""
    def __init__(self, name, mode='r'):
        """Create a new File

        PARAMETERS
          name: can be PIPE, STDOUT, a filename string,
                an opened fd, a python file object,
                or a command to pipe (if starts with |)

          mode: can be 'r' or 'w'
                if name starts with + the mode will be a+
        """
        assert mode in 'rw', 'Mode should be r or w'

        self.name     = name
        self.to_close = False
        if isinstance (name, str):
            # can be a pipe or a filename
            if mode == 'r' and name.startswith('|'):
                self.fd = PIPE
            else:
                if mode == 'w':
                    if name.startswith('+'):
                        open_mode = 'a+'
                        name = name[1:]
                    else:
                        open_mode = 'w+'
                else:
                    open_mode = 'r'

                self.fd = open(name, open_mode)
                if open_mode == 'a+':
                    self.fd.seek(0, 2)
                self.to_close = True

        else:
            # this is a file descriptor
            self.fd = name

    def get_command(self):
        """Returns the command to run to create the pipe"""
        if self.fd == PIPE:
            return self.name[1:]

    def close(self):
        """Close the file if needed"""
        if self.to_close:
            self.fd.close()

