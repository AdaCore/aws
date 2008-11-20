"""Subprocesses management

This package provides a single class called run which ease spawn of processes
in blocking or non blocking mode and redirection of its stdout, stderr and
stdin"""

import gnatpython.logging_util

from subprocess import Popen, STDOUT, PIPE

import logging
import os

BUF_SIZE=128

class Run:
    """
    ATTRIBUTES
      status : exit status (meaningfull only after the end of the process)
      out    : process standard output  (if instanciated with output = PIPE)
      err    : same as out but for standard error
      pid    : PID
    """
    def __init__ (self, cmds, cwd=None, output=PIPE,
                  error=STDOUT, input=None, bg=False, timeout=None, env=None):
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
          error:   same as output and also STDOUT (use output setting)
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
        self.input  = input
        self.stdin  = input
        self.output = output
        self.stdout = output
        self.error  = error
        self.stderr = error
        self.out    = ''
        self.err    = ''
        self.status = None
        self.comm   = None

        if env is None:
            env = os.environ

        # First resolve output, error and input
        self.__open_files ()

        rlimit_args = []
        if timeout is not None:
            rlimit_args = ['rlimit', '%d' % timeout]

        try:
            if not isinstance(cmds[0], list):
                logging.debug ('Run: ' + '%s' % (rlimit_args + cmds))
                self.internal = Popen (rlimit_args + cmds,
                                       stdin=self.stdin,
                                       stdout=self.stdout,
                                       stderr=self.stderr,
                                       cwd=cwd,
                                       env=env,
                                       universal_newlines=True)

            else:
                cmds[0] = rlimit_args + cmds[0]
                runs = []
                for index, cmd in enumerate(cmds):
                    if index == 0:
                        stdin = self.stdin
                    else:
                        stdin = runs[index - 1].stdout

                    # When connecting two processes using a Pipe don't use
                    # universal_newlines mode. Indeed commands transmitting
                    # binary data between them will crash
                    # (ex: gzip -dc toto.txt | tar -xf -)
                    if index == (len(cmds) - 1):
                        stdout = self.stdout
                        txt_mode = True
                    else:
                        stdout = PIPE
                        txt_mode = False

                    runs.append (Popen(cmd,
                                       stdin=stdin,
                                       stdout=stdout,
                                       stderr=self.stderr,
                                       cwd=cwd,
                                       env=env,
                                       universal_newlines=txt_mode))
                self.internal = runs[-1]

        except Exception:
            self.__error ()
            raise

        self.pid = self.internal.pid

        if not bg:
            self.wait ()

    def __open_files (self):
        """Internal procedure"""
        if isinstance (self.output, str):
            if self.output[0] == '+':
                self.stdout = open (self.output[1:], 'a+')
                self.stdout.seek (0, 2)
            else:
                self.stdout = open (self.output, 'w+')

        if isinstance (self.error, str):
            if self.error[0] == '+':
                self.stderr = open (self.error[1:], 'a+')
                self.stderr.seek (0, 2)
            else:
                self.stderr = open (self.error, 'w+')

        if isinstance (self.input, str):
            if self.stdin.startswith('|'):
                self.stdin = PIPE
            else:
                self.stdin  = open (self.input, 'r')

    def __close_files (self):
        """Internal procedure"""
        if isinstance (self.output, str) and isinstance (self.stdout, file):
            self.stdout.close ()

        if isinstance (self.error, str) and isinstance (self.stderr, file):
            self.stderr.close ()

        if isinstance (self.input, str) and isinstance (self.stdin, file):
            self.stdin.close ()

    def __error (self):
        """Set pid to -1 and status to 127 before closing files"""
        self.pid = -1
        self.status = 127
        self.__close_files ()

    def wait (self):
        """Wait until process ends and return its status"""

        if self.input is not None and self.input.startswith('|'):
            self.comm = self.internal.communicate(self.input[1:])

        if self.status == 127:
            return self.status

        self.status = None
        done = False

        while not done:
            if self.status is not None:
                done = True

            if self.output == PIPE:
                if self.stdin == PIPE:
                    self.out = self.comm[0]
                else:
                    if done:
                        tmp_out = self.internal.stdout.read ()
                    else:
                        tmp_out = self.internal.stdout.read (BUF_SIZE)
                    logging.log (gnatpython.logging_util.RAW, tmp_out)
                    self.out += tmp_out
            else:
                self.out = ""

            if self.error == PIPE:
                if self.stdin == PIPE:
                    self.err = self.comm[0]
                else:
                    if done:
                        tmp_err = self.internal.stderr.read ()
                    else:
                        tmp_err = self.internal.stderr.read (BUF_SIZE)
                    logging.log (gnatpython.logging_util.RAW, tmp_err)
                    self.err += tmp_err
            else:
                self.err = ""

            if self.status is None:
                self.status = self.internal.poll ()

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
