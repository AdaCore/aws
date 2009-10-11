"""Global environment handling

This package provide a class called Env used to store global information. Env
is a singleton so there is in fact only one instance.

Here is a description of the functionalities provided by the class:

* host, target information retrieval/setting:
    >>> from gnatpython.env import Env
    >>> e = Env ()
    >>> print e.target
    platform: x86-linux
    machine:  barcelona
    is_hie:   False
    is_host:  True
    triplet:  i686-pc-linux-gnu
    OS
        name:          linux
        version:       suse10.3
        is_bareboard:  False
    CPU
        name:   x86
        bits:   32
        endian: little
    >>> e.set_target ('ppc-vxw')
    >>> print e.target.os.name
    vxworks
    >>> print e.target.os.version
    5.5

* make some info global
    >>> e = Env ()
    >>> d = Env ()
    >>> e.example_of_global_info = 'hello'
    >>> print d.example_of_global_info
    hello

* restoring/saving complete environment, including environment variables and
  current dir

    >>> e = Env ()
    >>> e.example = 'hello'
    >>> e.store ('./saved_env')
    >>> ^D
    $ gnatpython
    >>> from gnatpython.env import Env
    >>> e = Env ()
    >>> e.restore ('./saved_env')
    >>> print e.example
    hello

"""
from gnatpython.arch import Arch

import pickle
import os

def putenv (key, value):
    """Portable version of os.putenv"""
    # When a variable is set by os.putenv, os.environ is not updated;
    # this is a problem, as Env and ex.Run use os.environ to store the
    # current environment or to spawn a process. This limitation is
    # documented in the Python Library Reference:

    # environ
    #       A mapping object representing the string environment. For
    #       example, environ['HOME'] is the pathname of your home directory
    #       (on some platforms), and is equivalent to getenv("HOME") in C.
    #
    #       This mapping is captured the first time the os module is
    #       imported, typically during Python startup as part of
    #       processing site.py. Changes to the environment made after
    #       this time are not reflected in os.environ, except for
    #       changes made by modifying os.environ directly.
    #
    #       If the platform supports the putenv() function, this
    #       mapping may be used to modify the environment as well as
    #       query the environment. putenv() will be called
    #       automatically when the mapping is modified. Note: Calling
    #       putenv() directly does not change os.environ, so it's
    #       better to modify os.environ.

    # ...For this reason, os.environ is prefered to os.putenv here.
    os.environ [key] = value

def getenv (key, default=None):
    """Portable version of os.getenv"""
    # For the reason documented in putenv, and for consistency, it is better
    # to use os.environ instead of os.getenv here.
    if key in os.environ:
        return os.environ [key]
    else:
        return default

class Env (object):
    """Environment Handling

    ATTRIBUTES
      build:  default system (autodetected)
      host:   host system
      target: target system
      is_cross: true if we are in a cross environment

    REMARKS
      build, host and target attributes are instances of Arch class. Do
      pydoc gnatpython.arch for more information
    """

    # class variable that holds the current environment
    __instance = {}

    # class variable that holds the stack of saved environments state
    __context  = []

    def __init__ (self):
        """Env constructor

        PARAMETERS
          None

        RETURN VALUE
          A Env instance

        REMARKS
          On first instantiation, build attribute will be computed and host
          and target set to the build attribute.
        """
        if 'build' not in Env.__instance:
            self.is_cross = False
            self.build    = Arch ()
            self.host     = self.build
            self.target   = self.host
            self.environ  = None
            self.cwd      = None
            self.main_options = None  # Command line switches

    def __getattr__ (self, name):
        return Env.__instance[name]

    def __setattr__ (self, name, value):
        Env.__instance[name] = value

    def set_host (self, host_name=None, host_version=None):
        """Set host platform

        PARAMETERS
          host_name: a string that identify the system to be considered as the
                     host. If None then host is set to the build one (the
                     autodetected platform).
          host_version: a string containing the system version. If set to None
                        the version is either a default or autodetected when
                        possible

        RETURN VALUE
          None

        REMARKS
          None
        """
        if host_name is not None:
            self.host = Arch (platform_name=host_name,
                              is_host=True,
                              version=host_version)
        else:
            self.host = self.build

        if self.target != self.host:
            self.is_cross = True
        else:
            self.is_cross = False

    def set_target (self, target_name=None, target_version=None):
        """Set target platform

        PARAMETERS
          host_name: a string that identify the system to be considered as the
                     host. If None then host is set to the host one.
          host_version: a string containing the system version. If set to None
                        the version is either a default or autodetected when
                        possible

        RETURN VALUE
          None

        REMARKS
          None
        """
        if target_name is not None:
            self.target = Arch (platform_name = target_name,
                                version = target_version)
        else:
            self.target = self.host

        if self.target != self.host:
            self.is_cross = True
        else:
            self.is_cross = False

    def get_attr (self, name, default_value = None, forced_value = None):
        """Return an attribute value

        PARAMETERS
          name: name of the attribute to check. Name can contain '.'
          default_value: returned value if forced_value not set and the
                attribute does not exist
          forced_value: if not None, this is the return value

        RETURN VALUE
          the attribute value

        REMARKS
          This function is useful to get the value of optional functions
          parameters whose default value might depend on the environment.
        """

        if forced_value is not None:
            return forced_value

        attributes = name.split('.')
        result = self
        for a in attributes:
            if not hasattr (result, a):
                return default_value
            else:
                result = getattr (result, a)

        if result is None or result == "":
            return default_value

        return result

    def store (self, filename=None):
        """Save environment into memory or file

        PARAMETERS
          filename: a string containing the path of the filename in which the
            environement will be saved. If set to None the environment is
            saved into memory in a stack like structure.

        RETURN VALUE
          None
        """

        # Store environment variables
        self.environ = os.environ.copy()

        # Store cwd
        self.cwd = os.getcwd ()

        if filename is None:
            Env.__context.append (pickle.dumps (Env.__instance))
        else:
            fd = open (filename, 'w+')
            pickle.dump (Env.__instance, fd)
            fd.close ()


    def restore (self, filename=None):
        """Restore environment from memory or a file

        PARAMETERS
          filename: a string containing the path of the filename from which
            the environement will be restored. If set to None the environment
            is pop the last saved one

        RETURN VALUE
          None
        """
        if filename is None:
            # We are restoring from memory.  In that case, just double-check
            # that we did store the Env object in memory beforehand (using
            # the store method).
            assert (self.environ is not None)

        if filename is None and Env.__context:
            Env.__instance = pickle.loads (Env.__context[-1])
            Env.__context = Env.__context[:-1]
        elif filename is not None:
            fd = open (filename, 'r')
            Env.__instance = pickle.load (fd)
            fd.close ()
        else:
            return

        # Restore environment variables value
        os.environ = self.environ.copy()

        # Restore current directory
        os.chdir (self.cwd)

    @classmethod
    def add_path (cls, path, append=False):
        """Set a path to PATH environment variable

        PARAMETERS
          path: path to add
          append: if True append, otherwise prepend. Default is prepend

        RETURN VALUE
          None
        REMARKS
          None
        """
        if append:
            os.environ['PATH'] += os.path.pathsep + path
        else:
            os.environ['PATH'] = path + os.path.pathsep + os.environ['PATH']

    @classmethod
    def add_search_path(cls, env_var, path, append=True):
        """Add a path to the env_var search paths

        PARAMETERS
          env_var: the environment variable name
                   (e.g. PYTHONPATH, LD_LIBRARY_PATH, ...)
          path: path to add
          append: if True append, otherwise prepend. Default is prepend

        RETURN VALUE
          None
        REMARKS
          None
        """
        if not env_var in os.environ:
            os.environ[env_var] = path
        else:
            if append:
                os.environ[env_var] += os.path.pathsep + path
            else:
                os.environ[env_var] = path + os.path.pathsep + \
                    os.environ[env_var]

    def add_dll_path (self, path, append=True):
        """Add a path to the dynamic libraries search paths

        PARAMETERS
          path: path to add
          append: if True append, otherwise prepend. Default is prepend

        RETURN VALUE
          None
        REMARKS
          None
        """
        if self.host.os.name.lower() == 'windows':
            env_var = 'PATH'
        elif self.host.os.name == 'HP-UX':
            env_var = 'SHLIB_PATH'
        elif self.host.os.name == 'Darwin':
            env_var = 'DYLD_LIBRARY_PATH'
        else:
            # On most Unixes LD_LIBRARY_PATH is used.
            env_var = 'LD_LIBRARY_PATH'

        self.add_search_path(env_var, path, append)
