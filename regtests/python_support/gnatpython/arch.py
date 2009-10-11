"""This package contains a single class called Arch that allows the user to
instantiate configuration objects containing information about the system
(native or cross).
"""

import platform
import re
import os.path
import sys
from gnatpython import config

UNKNOWN = 'unknown'

# __CPU and __OS are internal classes used only to create namespaces
# and have the possibility to declare attributes such as cpu.name in
# Arch class
class _Arch__CPU:
    """CPU attributes"""
    def __init__ (self):
        self.name = UNKNOWN
        self.bits = UNKNOWN
        self.endian = UNKNOWN

class _Arch__OS:
    """OS attributes"""
    def __init__ (self):
        self.name         = UNKNOWN
        self.version      = None
        self.exeext       = ''
        self.is_bareboard = False

class Arch:
    """
    ATTRIBUTES
      cpu.name: 
      cpu.endian:
      cpu.bits:
      os.name:
      os.version:
      os.is_bareboard:
      is_hie:
      platform: AdaCore platform product name. Ex: x86-linux
      triplet:  GCC target
      machine:  machine name
      domain:   domain name
      is_host:  True if this is not a cross context
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
                    except if a platform_name is specified or if the
                    platform_name is equal to the automatically detected one.
        RETURN VALUE
          An instantiation of Arch class
        """

        # Create necesarry namespaces using "dummy" classes __CPU and __OS
        self.cpu = __CPU ()
        self.os  = __OS ()

        # Initialize attributes values
        self.platform   = platform_name
        self.os.version = version
        self.machine    = None
        self.is_hie     = False

        if self.platform is None:
            self.is_host = True
        else:
            self.is_host = is_host

        uname = platform.uname ()

        if self.platform is None:
            # In this case we try to guess the host platform
            self.platform = self.__guess_platform ()
        else:
            if self.platform == self.__guess_platform():
                # This is a native platform
                self.is_host = True

        if self.is_host:
            # This is host so we can find the machine name using uname fields
            tmp = uname [1].lower ().split ('.', 1)
            self.machine = tmp[0]
            if len (tmp) > 1:
                self.domain = tmp[1]
            else:
                self.domain = ""

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
        str_dict = self.__dict__.copy ()
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
        if self.is_host and self.os.version is None:
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

        def re_contains(left, right):
            """Returns right in left (regexp aware)"""
            if re.match (left + '$', right) or \
                    re.match ('^' + left, right):
                return True
            else:
                return False

        def re_endswith(left, right):
            """Returns right.endswith(left) (regexp aware)"""
            return re.match(left + '$', right)

        def guess(os_name, p_uname):
            """Guess based on os_name"""
            for p_name in config.host_guess:
                p_config = config.host_guess[p_name]
                if p_config['os'] is not None:
                    if re_contains(p_config['os'], os_name):
                        if p_config['cpu'] is None or \
                                re_endswith(p_config['cpu'], p_uname[4]) or \
                                re_endswith(p_config['cpu'], p_uname[5]):
                            # The p_name config matched

                            if p_name in config.host_aliases:
                                return config.host_aliases[p_name]
                            else:
                                return p_name
            # wrong guess
            return None

        # First look for matching machine name
        for p_name in config.host_guess:
            if config.host_guess[p_name]['machine'] is not None:
                if re_endswith(config.host_guess[p_name]['machine'] + '$',
                               self.machine):
                    return p_name

        # Else we need to guess
        uname = platform.uname ()

        p_name = guess(uname[0], uname)
        if p_name is not None:
            return p_name

        p_name = guess(uname[2], uname)
        if p_name is not None:
            return p_name

        # Not found !
        return UNKNOWN

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
        if self.os.name in ('freebsd', 'tru64'):
            # Do not compute OS version but read config.os_info table
            return None

        uname = platform.uname ()
        if self.os.name == 'darwin':
            return uname[2]
        elif self.os.name == 'linux':
            if os.path.isfile ('/etc/redhat-release'):
                # RedHat distributions
                return 'redhat'
            elif os.path.isfile ('/etc/SuSE-release'):
                # Suse distributions
                release = open ('/etc/SuSE-release', 'r')
                for line in release:
                    version = re.search ('VERSION = ([0-9\.]+)', line)
                    if version is not None:
                        release.close ()
                        return 'suse' + version.group (1)
                release.close ()
                return 'suse'
            elif os.path.isfile ('/etc/lsb-release'):
                # /etc/lsb-release is present on the previous distrib
                # but is not usefull. On ubuntu it contains the
                # distrib number
                release = open ('/etc/lsb-release', 'r')
                distrib_name    = ''
                distrib_version = ''
                for line in release:
                    distrib_id = re.search ('DISTRIB_ID=(.+)', line.rstrip ())
                    if distrib_id is not None:
                        distrib_name = distrib_id.group (1).lower ()
                    else:
                        distrib_release = re.search ('DISTRIB_RELEASE=(.*)',
                                                     line.rstrip ())
                        if distrib_release is not None:
                            distrib_version = distrib_release.group (1)

                release.close ()
                if not distrib_name:
                    return None
                else:
                    return distrib_name + distrib_version

            else:
                return None
        elif self.os.name == 'aix':
            return uname[3] + '.' + uname[2]
        elif self.os.name == 'hp-ux':
            version = uname[2]
            if version[0:2] == 'B.':
                version = version[2:]
            return version
        elif self.os.name == 'irix':
            return uname[2]
        elif self.os.name == 'lynxos':
            return ''
        elif self.os.name == 'solaris':
            return '2' + uname[2][1:]
        elif self.os.name == 'windows':
            winver = sys.getwindowsversion ()
            if winver[3] != 2:
                # Don't try to set versions for Win 3.x or Win 9x series
                return None
            else:
                # this a WIN32_NT version
                if winver[0] == 6:
                    # versions starting with Vista. Currently we don't know
                    # how to make distinction between server 2008 and Vista
                    # or Windows 7. Indeed Server 2008 is version 6.0 as Vista
                    # and Server 2008 R2 is 6.1 as Windows 7.
                    if winver[1] == 0:
                        return 'Vista'
                    elif winver[1] == 1:
                        return '7'
                    else:
                        return None

                elif winver[0] == 5:
                    # versions from 2000 to 2008
                    if winver[1] == 0:
                        return '2000'
                    elif winver[1] == 1:
                        return 'XP'
                    elif winver[1] == 2:
                        return '2003'
                else:
                    # Don't try to guess versions of NT versions older than
                    # Windows 2000
                    return None
        return None

if __name__ == "__main__":
    print Arch()
