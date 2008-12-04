"""This module provides various functions to handle files and directories

All this functionalities are already present in python but they are available
in different modules with different intefaces. Here the interface of each
function tries to be as close as possible to the Unix shell commands.
"""

from gnatpython.ex import Run

from difflib import Differ

import logging
import os.path
import os
import shutil
import glob
import re

# Check whether ln is supported on this platform
# If ln is not supported, use shutil.copy2 instead
HAS_LN = hasattr(os, "link")

class FileUtilsError (Exception):
    """Exception raised by functions defined in this module
    """
    def __init__ (self, cmd, msg):
        Exception.__init__(self, cmd, msg)
        self.cmd = cmd
        self.msg = msg

    def __str__ (self):
        return "%s: %s\n" % (self.cmd, self.msg)

def cd (path):
    """Change current directory

    PARAMETERS
      path: directory name

    RETURN VALUE
      None

    REMARKS
      In case of error then FileUtilsError is raised
    """

    try:
        os.chdir (path)
    except Exception, E:
        logging.error (E)
        raise FileUtilsError ('cd', "can't chdir to %s\n" % path)

def cp (source, target, copy_attrs=True):
    """Copy files

    PARAMETERS
      source: a glob pattern
      target: target file or directory. If the source resolves as several
              files then target should be a directory
      copy_attrs: If True, also copy all the file attributes such as
              mode, timestamp, ownership, etc.

    RETURN VALUE
      None

    REMARKS
      If an error occurs then FileUtilsError is raised
    """
    switches = ''
    if copy_attrs:
        switches += ' -p'
    logging.debug ('cp %s %s->%s' % (switches, source, target))

    # Compute file list and number of file to copy
    file_list = ls (source)
    file_number = len (file_list)

    if file_number == 0:
        # If there is no source files raise an error
        raise FileUtilsError ('cp', "can't find files matching '%s'" % source)
    elif file_number > 1:
        # If we have more than one file to copy then check that target is a
        # directory
        if not os.path.isdir (target):
            raise FileUtilsError ('cp', 'target should be a directory')

    for f in file_list:
        try:
            if copy_attrs:
                shutil.copy2 (f, target)
            else:
                shutil.copy (f, target)
        except Exception, E:
            logging.error (E)
            raise FileUtilsError ('cp', 'error occured while copying %s' % f)

def ln (source, target):
    """Link files

    PARAMETERS
       source: a filename
       target: the target filename

    RETURN VALUE
       None
    """
    try:
        if HAS_LN:
            os.link (source, target)
        else:
            shutil.copy2 (source, target)
    except Exception, E:
        logging.error (E)
        raise FileUtilsError ('ln', 'can not link %s to %s' % (source, target))

def df(path):
    """Disk space available on the filesystem containing the given path

    PARAMETERS
      path: a path string

    RETURN VALUE
      An integer representing the space left in Mo

    REMARKS
      None
    """
    stats = os.statvfs(path)

    # The final value is in Mo so it can safely be converted to an integer
    return int ((stats.f_bsize * stats.f_bavail) / (1024 * 1024))

def diff (filename1, filename2, ignore=None):
    """Check if two files are different

    PARAMETERS
      filename1:  a filename
      filename2:  a filename
      ignore   :  all lines matching this pattern in both files are
                  ignored during comparison. If set to None, all lines are
                  considered.

    RETURN VALUE
      A diff string. If the string is equal to '' it means that there is no
      difference

    REMARKS
      White character at beginning and end of lines are ignored. On error,
      FileUtilsError is raised
    """
    try:
        file1_fd = open (filename1, 'r')
        file1 = file1_fd.readlines ()
        file1_fd.close ()
    except IOError:
        file1 = ""

    try:
        file2_fd = open (filename2, 'r')
        file2 = file2_fd.readlines ()
        file2_fd.close ()
    except IOError:
        file2 = ""

    def is_line_junk (line):
        """Skip non useful lines"""
        if not line.strip ():
            return True
        if ignore is not None and re.search (ignore, line):
            return True

    d = Differ(is_line_junk)

    diff_content = []
    for line in d.compare(file1, file2):
        if not line.startswith('  '):
            diff_content.append(line)
    return ''.join(diff_content)

def ls (path):
    """List files

    PARAMETERS
      path: glob pattern

    RETURN VALUE
      a list of filenames

    REMARKS
      This function do not raise an error if no file matching the glob pattern
      is encountered. The only consequence is that an empty list is returned.
    """

    logging.debug ('ls %s' % path)
    result = glob.glob (path)
    result.sort ()
    return result

def mkdir(path, mode=0750):
    """Create a directory

    PARAMETERS
      path: path to create. If intermediate directories do not exist the
            procedure create them
      mode: default is 0750

    RETURN
      None

    REMARKS
      This function behaves quite like mkdir -p command shell. So if the
      directory already exist no error is raised. If the directory cannot
      be created then FileUtilsError is raised.
    """

    if os.path.isdir (path):
        return
    else:
        logging.debug ('mkdir %s %s' % (path, mode))
        try:
            os.makedirs(path, mode)
        except Exception, E:
            logging.error (E)
            raise FileUtilsError ('mkdir', "can't create %s" % path)

def mv (source, target):
    """Move files

    PARAMETERS
      source: a glob pattern
      target: target file or directory. If the source resolves as several
              files then target should be a directory

    RETURN VALUE
      None

    REMARKS
      If an error occurs then FileUtilsError is raised
    """

    logging.debug ('mv %s->%s' % (source, target))

    # Compute file list and number of file to copy
    file_list = ls (source)
    file_number = len (file_list)

    if file_number == 0:
        # If there is no source files raise an error
        raise FileUtilsError ('mv', "can't find files matching '%s'" % source)
    elif file_number > 1:
        # If we have more than one file to move then check that target is a
        # directory
        if not os.path.isdir (target):
            raise FileUtilsError ('mv', 'target should be a directory')

    for f in file_list:
        try:
            shutil.move (f, target)
        except Exception, E:
            logging.error (E)
            raise FileUtilsError ('mv', 'error occured while moving %s' % f)


def rm (path, recursive=False):
    """Remove files

    PARAMETERS
      path:      a glob pattern
      recursive: if True do a recursive deletion. Default is False

    RETURN VALUE
      None

    REMARKS
      If an error occurs then FileUtilsError is raised. The function will not
      raise an Error is there are no file to delete.
    """
    logging.debug ('rm %s' % (path))

    file_list = ls (path)

    for f in file_list:
        try:
            if recursive:
                shutil.rmtree (f)
            else:
                os.remove (f)
        except Exception, E:
            logging.error (E)
            raise FileUtilsError ('rm', 'error occured while removing %s' % f)

def rsync (source, target, files=None, protected_files=None, delete = True):
    """Wrapper around rsync

    Under development. Do not use
    """

    rsync_args = ['rsync', '-av']
    if delete:
        rsync_args.append ('--delete-excluded')

    f = open ('/tmp/rsync.list', 'w')
    if files is not None:
        for filename in files:
            f.write ('+ /' + filename + '\n')
            while filename != '':
                (filename, tail) = os.path.split (filename)
                if filename != '':
                    f.write ('+ /' + filename + '/\n')

    if protected_files is not None:
        for filename in protected_files:
            f.write ('P /' + filename + '\n')

    f.write ('- *\n')
    f.close ()

    if protected_files is not None or files is not None:
        rsync_args.append ('--filter=. /tmp/rsync.list')

    rsync_args.append (source + '/')
    rsync_args.append (target)
    p = Run (rsync_args)
    return p.status

