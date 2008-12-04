"""This package contains a single class called Arch that allows the user to
instantiate configuration objects containing information about the system
(native or cross)
"""

import platform
import re
import os.path
from gnatpython import config

# __CPU and __OS are internal classes used only to create namespaces
# and have the possibility to declare attributes such as cpu.name in
# Arch class
class _Arch__CPU:
    """CPU attributes"""
    def __init__ (self):
        self.name = 'unknown'
        self.bits = 'unknwon'
        self.endian = 'unknwon'

class _Arch__OS:
    """OS attributes"""
    def __init__ (self):
        self.name = 'unknown'
        self.version = None
        self.is_bareboard = False

class Arch:
    """
    ATTRIBUTES
      cpu.name
      cpu.endian
      cpu.bits
      os.name
      os.version
      os.is_bareboard
      is_hie
      platform (AdaCore platform product name. Ex: x86-linux)
      triplet (GCC TARGET)
      machine (machine name)
      domain  (domain name)
      is_host (true if this is not a cross context)
    """

    def __init__ (self, platform_name=None, version=None, is_host=False):
        """Arch constructor

        PARAMETERS
          platform: if None then automatically detect current platform (native)
                    Otherwise should be a valid platform string.
          version:  if None, assume default OS version or find it automatically
                    (native case only).
                    Otherwise should be a valid version string.
          is_host:  if True the system is not a cross one. Default is False
                    except if a platform_name is specified
        RETURN VALUE
          An instantiation of Arch class

        REMARKS
          None
        """

        # Create necesarry namespaces using "dummy" classes __CPU and __OS
        self.cpu = __CPU ()
        self.os  = __OS ()

        # Initialize attributes values
        self.platform = platform_name
        self.os.version = version
        self.machine = None
        self.is_hie = False

        if self.platform is None:
            self.is_host = True
        else:
            self.is_host = is_host

        uname = platform.uname ()

        if self.is_host:
            # This is host so we can find the machine name using uname fields
            tmp = uname [1].lower ().split ('.', 1)
            self.machine = tmp[0]
            if len (tmp) > 1:
                self.domain = tmp[1]
            else:
                self.domain = ""

        if self.platform is None:
            # In this case we try to guess the host platform
            self.platform = self.__guess_platform ()

        # Fill other attributes
        self.__fill_info ()

        # Find triplet
        self.triplet = config.build_targets [self.platform]['name'] % \
            self.__get_dict ()

    def __get_dict (self):
        """Export os and cpu variables as os_{var} and cpu_{var}

        Returns a dictionary containing os and cpu exported vars
        and self.__dict__ content
        """
        str_dict = self.__dict__
        for (key, var) in self.os.__dict__.items():
            str_dict["os_" + key] = var
        for (key, var) in self.cpu.__dict__.items():
            str_dict["cpu_" + key] = var
        return str_dict

    def __str__ (self):
        """
        Return a representation string of the object
        """

        result = "platform: %(platform)s\n" \
            "machine:  %(machine)s\n" \
            "is_hie:   %(is_hie)s\n" \
            "is_host:  %(is_host)s\n" \
            "triplet:  %(triplet)s\n" \
            "OS\n" \
            "   name:          %(os_name)s\n" \
            "   version:       %(os_version)s\n" \
            "   exeext:        %(os_exeext)s\n" \
            "   is_bareboard:  %(os_is_bareboard)s\n" \
            "CPU\n" \
            "   name:   %(cpu_name)s\n" \
            "   bits:   %(cpu_bits)s\n" \
            "   endian: %(cpu_endian)s" % self.__get_dict ()
        return result

    def __fill_info (self):
        """
        Internal function that fill info related to the cpu, os, ...

        PARAMETERS
          None

        RETURN VALUE
          None

        REMARKS
          None
        """

        self.os.name  = config.platform_info[self.platform]['os']
        self.cpu.name = config.platform_info[self.platform]['cpu']
        self.is_hie   = config.platform_info[self.platform]['is_hie']

        self.cpu.bits   = config.cpu_info[self.cpu.name]['bits']
        self.cpu.endian = config.cpu_info[self.cpu.name]['endian']

        self.os.is_bareboard = config.os_info[self.os.name]['is_bareboard']
        self.os.exeext       = config.os_info[self.os.name]['exeext']

        # If version is not given by the user guess it or set it to the
        # default (cross case)
        if self.is_host:
            self.os.version = self.__guess_os_version ()
        if self.os.version is None:
            self.os.version = config.os_info[self.os.name]['version']

    def __guess_platform (self):
        """
        Internal function that guess base on uname system call the
        current platform

        PARAMETERS
          None

        RETURN VALUE
          return a string object containing the platform name

        REMARKS
          None
        """

        # First look for matching machine name
        for p in config.host_guess:
            if config.host_guess[p]['machine'] is not None:
                if re.match (config.host_guess[p]['machine'] + '$',
                             self.machine):
                    return p

        u = platform.uname ()
        for p in config.host_guess:
            if config.host_guess[p]['os'] is not None:
                if re.match (config.host_guess[p]['os'] + '$', u[0]) or \
                        re.match ('^' + config.host_guess[p]['os'], u[0]):
                    if config.host_guess[p]['cpu'] is None:
                        if p in config.host_aliases:
                            return config.host_aliases[p]
                        else:
                            return p
                    elif re.match (config.host_guess[p]['cpu'] + '$',
                                   u[4]) or \
                      re.match (config.host_guess[p]['cpu'] + '$', u[5]):
                        if p in config.host_aliases:
                            return config.host_aliases[p]
                        else:
                            return p

        return 'unknown'

    def __guess_os_version (self):
        """
        Internal function used to guess the host OS version/dist

        PARAMETERS
          None

        RETURN VALUE
          A string containing the OS version or None

        REMARKS
          None
        """

        u = platform.uname ()
        if self.os.name == 'linux':
            if os.path.isfile ('/etc/redhat-release'):
                # RedHat distributions
                return 'redhat'
            elif os.path.isfile ('/etc/SuSE-release'):
                # Suse distributions
                f = open ('/etc/SuSE-release', 'r')
                for line in f:
                    m = re.search ('VERSION = ([0-9\.]+)', line)
                    if m != None:
                        f.close ()
                        return 'suse' + m.group (1)
                f.close ()
                return 'suse'
            elif os.path.isfile ('/etc/lsb-release'):
                # /etc/lsb-release is present on the previous distrib
                # but is not usefull. On ubuntu it contains the
                # distrib number
                f = open ('/etc/lsb-release', 'r')
                distrib_name = ''
                distrib_version = ''
                for line in f:
                    m = re.search ('DISTRIB_ID=(.+)', line.rstrip ())
                    if m is not None:
                        distrib_name = m.group (1).lower ()
                    else:
                        m = re.search ('DISTRIB_RELEASE=(.*)', line.rstrip ())
                        if m != None:
                            distrib_version = m.group (1)

                f.close ()
                if not distrib_name:
                    return None
                else:
                    return distrib_name + distrib_version

            else:
                return None
        elif self.os.name == 'aix':
            return u[3] + '.' + u[2]
        elif self.os.name == 'darwin':
            return ''
        elif self.os.name == 'freebsd':
            return ''
        elif self.os.name == 'hp-ux':
            version = u[2]
            if version[0:2] == 'B.':
                version = version[2:]
            return version
        elif self.os.name == 'irix':
            return u[2]
        elif self.os.name == 'lynxos':
            return ''
        elif self.os.name == 'tru64':
            return None
        elif self.os.name == 'solaris':
            return '2' + u[2][1:]
        elif self.os.name == 'windows':
            return ''
        return None


if __name__ == "__main__":
    print Arch()
