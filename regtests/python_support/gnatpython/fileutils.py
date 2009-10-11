"""This module provides various functions to handle files and directories

All this functionalities are already present in python but they are available
in different modules with different interfaces. Here the interface of each
function tries to be as close as possible to the Unix shell commands.
"""

from gnatpython.ex import Run
from gnatpython.env import Env
from gnatpython.logging_util import highlight, COLOR_GREEN, COLOR_RED, COLOR_CYAN

from difflib import SequenceMatcher, unified_diff

import logging
import os.path
import os
import shutil
import stat
import glob
import re
import sys
import fnmatch

logger = logging.getLogger ('gnatpython.fileutils')

# Check whether ln is supported on this platform
# If ln is not supported, use shutil.copy2 instead
HAS_LN = hasattr(os, "link")

# When diff find a difference between two lines, we'll try to highlight
# the differences if diff_within_line is True. This is currently disabled
# because the output is not always more readable (the diff is too fine
# grained, we should probably do it at the word level)
diff_within_line = False

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
        logger.error (E)
        raise FileUtilsError ('cd', "can't chdir to %s\n" % path)

def cp (source, target, copy_attrs=True):
    """Copy files

    PARAMETERS
      source: a glob pattern
      target: target file or directory. If the source resolves as several
              files then target should be a directory
      copy_attrs: If True, also copy all the file attributes such as
              mode, timestamps, ownership, etc.

    RETURN VALUE
      None

    REMARKS
      If an error occurs then FileUtilsError is raised
    """
    switches = ''
    if copy_attrs:
        switches += ' -p'
    logger.debug ('cp %s %s->%s' % (switches, source, target))

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
            logger.error (E)
            raise FileUtilsError ('cp', 'error occurred while copying %s' % f)

def unixpath (path):
    """Convert path to Unix/Cygwin format

    PARAMETERS
      path: path string to convert

    RETURN VALUE
      None

    REMARKS
      On Unix systems this function is identity. On Win32 systems it needs
      cygpath to do the conversion.
    """
    if sys.platform == 'win32':
        p = Run (['cygpath', '-u', path])
        if p.status != 0:
            raise FileUtilsError ('unixpath',
                                  'cannot transform path %s' % path)
        return p.out.strip ()
    else:
        return path
    
def ln (source, target):
    """Create a symbolic link

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
        logger.error (E)
        raise FileUtilsError ('ln', 'can not link %s to %s' % (source, target))

def df(path):
    """Disk space available on the filesystem containing the given path

    PARAMETERS
      path: a path string

    RETURN VALUE
      An integer representing the space left in Mo
    """
    stats = os.statvfs(path)

    # The final value is in Mo so it can safely be converted to an integer
    return int ((stats.f_bsize * stats.f_bavail) / (1024 * 1024))

def colored_unified_diff (a, b, fromfile='', tofile='',
                          fromfiledate='', tofiledate='', n=3, lineterm='\n',
                          onequal=None, onreplaceA=None, onreplaceB=None):
   """Same parameters as difflib.unified_diff
      ONEQUAL is a callback: it is passed a substring matching parts of the
      input that are the same in A and B. It returns the version to be
      displayed (by default, no change). It can be used if you want to limit
      the output.
      Likewise, ONREPLACEA and ONREPLACEB are called when a substring of A is
      replaced by a substring of B. They should return the actual strings that
      will be compared to find the diffs within a line.
   """

   if not Env().main_options or not Env().main_options.enable_color:
      for line in unified_diff (
            a,b,fromfile,tofile,fromfiledate,tofiledate,n,lineterm):
         yield line
   else:   
      # Code inspired from difflib.py
      minus = highlight ('-', fg=COLOR_CYAN)
      plus  = highlight ('+', fg=COLOR_CYAN)

      if not onequal:
         onequal = lambda x: x
      if not onreplaceA:
         onreplaceA = lambda x: x
      if not onreplaceB:
         onreplaceB = lambda x: x

      started = False
      for group in SequenceMatcher(None,a,b).get_grouped_opcodes(n):
         if not started:
            yield highlight ('--- %s %s%s', fg=COLOR_CYAN) \
                  % (fromfile, fromfiledate, lineterm)
            yield highlight ('+++ %s %s%s', fg=COLOR_CYAN) \
                  % (tofile, tofiledate, lineterm)
            started = True

         i1, i2, j1, j2 = group[0][1], group[-1][2], group[0][3], group[-1][4]
         yield highlight (
            "@@ -%d,%d +%d,%d @@%s" % (i1+1, i2-i1, j1+1, j2-j1, lineterm),
            fg=COLOR_CYAN)

         for tag, i1, i2, j1, j2 in group:
            if tag == 'equal':
               for line in a[i1:i2]:
                  yield ' ' + onequal (line)
               continue

            elif tag == 'replace':
               line1 = onreplaceA (("\n" + minus).join (a[i1:i2]))
               line2 = onreplaceB (("\n" + plus).join (b[j1:j2]))

               if diff_within_line:
                  # Do a diff within the lines to highlight the difs

                  d = list (SequenceMatcher (
                     None, line1, line2).get_grouped_opcodes (
                        len (line1) + len (line2)))
                  result1 = ""
                  result2 = ""
                  for c in d:
                     for t, e1, e2, f1, f2 in c:
                        if t == 'equal':
                           result1 += "".join (onequal (line1[e1:e2]))
                           result2 += "".join (onequal (line2[f1:f2]))
                        elif t == 'replace':
                           result1 += highlight (
                              "".join (line1[e1:e2]), COLOR_RED)
                           result2 += highlight (
                              "".join (line2[f1:f2]), COLOR_GREEN)
                        elif t == 'delete':
                           result1 += highlight (
                              "".join (line1[e1:e2]), COLOR_RED)
                        elif t == 'insert':
                           result2 += highlight (
                              "".join (line2[f1:f2]), COLOR_GREEN)
                  yield minus + result1
                  yield plus + result2
               else:
                  yield minus + highlight (line1, COLOR_RED)
                  yield plus + highlight (line2, COLOR_GREEN)

            elif tag == 'delete':
               for line in a[i1:i2]:
                  if diff_within_line:
                     yield minus + line
                  else:
                     yield minus + highlight (line, COLOR_RED)
            elif tag == 'insert':
               for line in b[j1:j2]:
                  if diff_within_line:
                     yield plus + line
                  else:
                     yield plus + highlight (line, COLOR_GREEN)

def diff (item1, item2, ignore=None, item1name="expected", item2name="output"):
    """Compute diff between two files or list of strings

    PARAMETERS
      item1    :  a filename or a list of strings
      item2    :  a filename or a list of strings
      ignore   :  all lines matching this pattern in both files are
                  ignored during comparison. If set to None, all lines are
                  considered.
      item1name:  name to display for item1 in the diff
      item2name:  name to display for item2 in the diff
      color    :  whether colored diff should be displayed (even if True, this
                  will be disabled unless the user specified the --enable-color
                  switch).

    RETURN VALUE
      A diff string. If the string is equal to '' it means that there is no
      difference

    REMARKS
      White character at beginning and end of lines are ignored. On error,
      FileUtilsError is raised
    """

    tmp = [[], []]

    # Read first item
    if isinstance (item1, list):
        tmp[0] = item1
    else:
        try:
            file1_fd = open (item1, 'r')
            tmp[0] = file1_fd.readlines ()
            file1_fd.close ()
        except IOError:
            tmp[0] = []

    # Do same thing for the second one
    if isinstance (item2, list):
        tmp[1] = item2
    else:
        try:
            file2_fd = open (item2, 'r')
            tmp[1] = file2_fd.readlines ()
            file2_fd.close ()
        except IOError:
            tmp[1] = []

    def is_line_junk (line):
        """Skip non useful lines"""
        return len (line.strip ()) == 0 or \
                 (ignore is not None and re.search (ignore, line) is not None)

    # Filter empty lines in both items and lines that match ignore pattern
    for k in [0, 1]:
        tmp[k] = ["%s\n" % line.strip() \
                  for line in tmp[k] if not is_line_junk (line) ]

    diff_content = colored_unified_diff (
       tmp[0], tmp[1], n=1, fromfile=item1name, tofile=item2name)

    return ''.join(diff_content)

def ls (path):
    """List files

    PARAMETERS
      path: glob pattern or glob pattern list

    RETURN VALUE
      a list of filenames

    REMARKS
      This function do not raise an error if no file matching the glob pattern
      is encountered. The only consequence is that an empty list is returned.
    """

    if not isinstance (path, list):
        path = [path]

    result = []

    logger.debug ('ls %s' % path)

    for p in path:
        result += glob.glob (p)
        
    result.sort ()
    return result

def mkdir(path, mode=0755):
    """Create a directory

    PARAMETERS
      path: path to create. If intermediate directories do not exist the
            procedure create them
      mode: default is 0755

    RETURN VALUE
      None

    REMARKS
      This function behaves quite like mkdir -p command shell. So if the
      directory already exist no error is raised. If the directory cannot
      be created then FileUtilsError is raised.
    """

    if os.path.isdir (path):
        return
    else:
        logger.debug ('mkdir %s %s' % (path, mode))
        try:
            os.makedirs(path, mode)
        except Exception, E:
            logger.error (E)
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
    logger.debug ('mv %s->%s' % (source, target))

    try:
        # Compute file list and number of file to copy
        file_list = ls (source)
        file_number = len (file_list)
        assert file_number != 0, "can't find files matching '%s'" % source

        if len (file_list) == 1:
            f = file_list[0]
            if os.path.isdir(f) and os.path.isdir(target):
                shutil.move(f, os.path.join(target, os.path.basename(f)))
            else:
                shutil.move (f, target)
        else:
            # If we have more than one file to move then check that target is a
            # directory
            assert os.path.isdir (target), 'target should be a directory'

            for f in file_list:
                shutil.move (f, os.path.join(target, os.path.basename(f)))
    except Exception, E:
        logger.error (E)
        raise FileUtilsError ('mv', E)


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
    logger.debug ('rm %s' % (path))

    file_list = ls (path)

    def onerror(func, path, exc_info):
        """When shutil.rmtree fail, try again to delete the file"""
        if func == os.remove:
            # Cannot remove path, call chmod (IA08-008)
            os.chmod(path, 0777)
            os.remove(path)

    for f in file_list:
        try:
            if recursive:
                shutil.rmtree (f, onerror=onerror)
            else:
                os.remove (f)
        except Exception, E:
            logger.error (E)
            raise FileUtilsError ('rm', 'error occured while removing %s' % f)

def rsync (source, target, files=None, protected_files=None, delete = False):
    """Wrapper around rsync utility

    PARAMETERS
      source: source directory to sync. Note that it will be always considered
        as the 'content' of source (i.e source is passed with a trailing '/')
      target: target destination directory
      files:  if None all files from source are synchronized. Otherwise it
        should be a list of string that are patterns (rsync format) to select
        which files should be transfered.
      protected_files: type is the same as files parameters. Files that are
        matching these pattern will be protected in the destination directory
      delete: If true, files that don't exist in source will deleted in target.

    RETURN VALUE
      None

    REMARKS
      None
    """

    rsync_args = ['rsync', '-a']
    rsync_filename = ''
    
    if delete:
        rsync_args.append ('--delete-excluded')

    if files is not None or protected_files is not None:
        rsync_filename = '/tmp/rsync.list.%d' % os.getpid ()
        
        f = open (rsync_filename, 'w')
        
        if files is not None:
            for filename in files:
                # add filename to the list
                f.write ('+ /' + filename + '\n')

                # add also all its parent directories
                while filename != '':
                    (filename, _) = os.path.split (filename)
                    if filename != '':
                        f.write ('+ /' + filename + '/\n')

        if protected_files is not None:
            for filename in protected_files:
                f.write ('P /' + filename + '\n')

        # exclude files that did not match the patterns
        f.write ('- *\n')
        f.close ()

        # Update rsync arguments
        rsync_args.append ('--filter=. ' + rsync_filename)

    # Note: source and target must be in Unix format. Windows style for path
    # will not work.
    rsync_args.append (unixpath (source) + '/')
    rsync_args.append (unixpath (target))
    p = Run (rsync_args)

    # Clean temp file if necessary
    if files is not None or protected_files is not None:
        rm (rsync_filename)
        
    if p.status != 0:
        raise FileUtilsError ('rsync',
                              'rsync failed with status %d' % p.status)
    
    return

def touch (filename):
    """Update file access and modification times

    PARAMETERS
      filename: file to update

    RETURN VALUE
      None

    REMARKS
      If the file does not exist it is created.
    """
    if os.path.exists(filename):
        os.utime(filename, None)
    else:
        new_file = open(filename, 'w+')
        new_file.close()

def which (prog):
    """Locate executable

    Returns the full path of prog executable that would have been executed
    by gnatpython.ex.Run. It does this by searching for an executable in
    the directories listed in the environment variable PATH

    PARAMETERS
      prog: program to find

    RETURN VALUE
      absolute path to the program on success, '' otherwise.
    """
    pathlist = os.environ['PATH'].split(os.pathsep)
    for pathdir in pathlist:
        filename = os.path.join(pathdir, prog)
        try:
            st = os.stat(filename)
        except os.error:
            continue
        if not stat.S_ISREG(st[stat.ST_MODE]):
            logger.debug(filename + ': not a disk file')
        else:
            mode = stat.S_IMODE(st[stat.ST_MODE])
            if mode & 0111:
                return filename
            else:
                logger.debug(filename + ': not executable')
    return ""

def split_file (filename, split_line=None, keys=None, ignore_errors = False):
    """Split a file into a list or a dictionary

    PARAMETERS
      filename: file to read
      split_line: if None then the file is split by line. Otherwise lines are
        also subdivided using split_line as separator
      keys: this is a list of string. If split_line is None then this parameter
        is ignored. Otherwise, each line is subdivided using split_line
        parameter and each field associated with a key to compose a
        dictionary. If the number of keys is not sufficient additional fields
        are ignored. If the number of keys is superior to the number of fields
        then last keys will have '' as value.
        
    RETURN VALUE
      A list. If split_line if None then each element is a string (i.e a line
      of the file), otherwise each element is list of string (i.e a list split
      using split_line separator) or a dictionary (if keys are passed). If
      an I/O error occurs and ignore_errors is set to True then an empty list
      is returned.
    """

    result = []
    try:
        fd = open (filename, 'r')
    
        for line in fd:
            line = line.rstrip ()
            if split_line is not None and line != '':
                tmp = line.split (split_line)
                if keys is None:
                    line = tmp
                else:
                    line = {}
                    tmp_last = len (tmp) - 1
                    for index, k in enumerate (keys):
                        if tmp_last < index:
                            line[k] = ''
                        else:
                            line[k] = tmp[index]
                result.append (line)
            elif split_line is None:
                result.append (line)
        fd.close ()
    except IOError, E:
        if not ignore_errors:
            logger.error (E)
            raise FileUtilsError ('split_file',
                                  'cannot open file %s' % filename)
        else:
            result = []
        
    return result

def echo_to_file (filename, content, append=False):
    """Output content into a file

    PARAMETERS
      filename: file to write into
      content:  string to be written
      append:   if True append to the file. Otherwise overwrite (Default)

    RETURN VALUE
      None

    REMARKS
      This function is useful when writing few content to a file for which we
      don't want to keep a file descriptor opened . In other cases, it's more
      efficient to open a file and use the regular python I/O functions.
    """

    if append:
        fd = open (filename, 'a+')
        fd.seek (0, 2)
    else:
        fd = open (filename, 'w+')

    if isinstance (content, list):
        for l in content:
            fd.write (l + '\n')
    else:
        fd.write (content)
        
    fd.close ()

def unpack_archive (filename,
                    dest,
                    selected_files=None,
                    remove_root_dir=False):
    """Unpack an archive file (.tgz, .tar.gz, .tar or .zip)

    PARAMETERS
      filename:        archive to unpack
      dest:            destination directory (should exist)
      selected_files:  list of files to unpack (partial extraction). If None
        all files are unpacked
      remove_root_dir: if True then the root dir of the archive is suppressed.
                
    RETURN VALUE
      None

    REMARKS
      rsync and cygpath (win32) utilities might be needed when using
      remove_root_dir option
    """

    # First do some checks such as archive existence or destination directory
    # existence.
    if not os.path.isfile (filename):
        raise FileUtilsError ('unpack_archive', 'cannot find %s' % filename)

    if not os.path.isdir (dest):
        raise FileUtilsError ('unpack_archive',
                              'dest dir %s does not exist' % dest)

    if selected_files is None:
        selected_files = []
        
    logger.debug ('unpack %s in %s' % (filename, dest))

    # We need to resolve to an absolute path as the extraction related
    # processes will be run in the destination directory
    filename = os.path.abspath (filename)

    # If remove_root_dir is set then extract to a temp directory first.
    # Otherwise extract directly to the final destination
    if remove_root_dir:
        tmp_dest = '%s.%d' % (os.path.abspath (dest), os.getpid ())
        mkdir (tmp_dest)
    else:
        tmp_dest = dest

    # Handle .zip, .tar.gz and .tar archives
    if filename[-7:] == '.tar.gz' or filename[-4:] == '.tgz':
        p = Run ([['gzip', '-dc', filename ],
                  ['tar', '-xf', '-'] + selected_files], cwd=tmp_dest)
    elif filename[-4:] == '.tar':
        p = Run (['tar', '-xf', filename] + selected_files, cwd=tmp_dest)
    elif filename[-4:] == '.zip':
        p = Run (['unzip', '-o', filename] + selected_files, cwd=tmp_dest)

    if p.status != 0:
        # The extract command failed
        if remove_root_dir:
            rm (tmp_dest, True)
            
        raise FileUtilsError ('unpack_archive',
                              'extraction of %s failed' % filename)

    if remove_root_dir:
        # First check that we have only one dir in our temp destination. If not
        # raise an error.
        file_list = ls (tmp_dest + '/*')
        if len (file_list) == 0:
            # Nothing to do... just remove the tmp_dest dir and exit
            rm (tmp_dest, True)
            return
        if len (file_list) != 1:
            rm (tmp_dest, True)
            raise FileUtilsError ('unpack_archive',
                                  'archive does not have a unique root dir')
       
        # Now check if the destination directory is empty. If this is the case
        # a simple move will work, otherwise we need to do a rsync (which cost
        # more)
        dest_file_list = ls (dest + '/*')
        if len (dest_file_list) == 0:
            mv (ls (file_list[0] + '/*'), dest)
        else:    
            rsync (file_list[0], dest, delete = False)

        rm (tmp_dest, True)

def find (root, pattern=None, include_dirs=False, include_files=True):
    """Find files or directory recursively

    PARAMETERS
      root: directory from which the research start
      pattern: glob pattern that files or directories should match in order
        to be included in the final result
      include_dirs: if True include directories
      include_files: if True include regular files

    RETURN VALUE
      a list of files (strings)
    """

    result = []
    for root, dirs, files in os.walk (root):
        root = root.replace ('\\', '/')
        if include_files:
            for f in files:
                if pattern is None or fnmatch.fnmatch (f, pattern):
                    result.append (root + '/' + f)
        if include_dirs:
            for d in dirs:
                if pattern is None or fnmatch.fnmatch (d, pattern):
                    result.append (root + '/' + d)
    return result
