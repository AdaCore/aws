"""This file contains various configuration tables"""

cpu_info = {
    'sparc'     : {'endian' : 'big',     'bits' : 32},
    'sparc64'   : {'endian' : 'big',     'bits' : 64},
    'x86'       : {'endian' : 'little',  'bits' : 32},
    'x86_64'    : {'endian' : 'little',  'bits' : 64}
}

os_info = {
    'linux'     : { 'is_bareboard' : False, 'version' : 'unknown' , 'exeext': '' },
    'solaris'   : { 'is_bareboard' : False, 'version' : '2.8'     , 'exeext': '' },
    'windows'   : { 'is_bareboard' : False, 'version' : 'XP'      , 'exeext': '.exe' },
    'none'      : { 'is_bareboard' : True,  'version' : 'unknown' , 'exeext': ''}
}

platform_info = {
    'sparc64-solaris'    : { 'cpu'    : 'sparc64','os'     : 'solaris',  'is_hie' : False},
    'sparc-solaris'      : { 'cpu'    : 'sparc',  'os'     : 'solaris',  'is_hie' : False},
    'x86_64-linux'       : { 'cpu'    : 'x86_64', 'os'     : 'linux',    'is_hie' : False},
    'x86-linux'          : { 'cpu'    : 'x86',    'os'     : 'linux',    'is_hie' : False},
    'x86-windows'        : { 'cpu'    : 'x86',    'os'     : 'windows',  'is_hie' : False}
    }

build_targets = {
    'sparc64-solaris'    : { 'name'        : 'sparc64-sun-solaris%(os_version)s'},
    'sparc-solaris'      : { 'name'        : 'sparc-sun-solaris%(os_version)s'},
    'x86_64-linux'       : { 'name'        : 'x86_64-pc-linux-gnu'},
    'x86-linux'          : { 'name'        : 'i686-pc-linux-gnu'},
    'x86-windows'        : { 'name'        : 'pentium-mingw32msv'},
    }

# The following table is dictionnary which Each key value is a tuple
# of regexps. Each regexp is associated with one or several of the
# uname fields. Note that priority is given to the machine name field
# and that order in this table is not significative (so do not rely on
# it for your regexps)

host_guess = {
    # platform : OS (uname[0]), machine (uname[1]), proc (uname[4 or 5])
    'x86-linux'      : { 'os'      : 'Linux',     'machine' : None, 'cpu'     : 'i.86|pentium'},
    'x86_64-linux'   : { 'os'      : 'Linux',     'machine' : None, 'cpu'     : 'x86_64'},
    'sparc-solaris'  : { 'os'      : 'SunOS',     'machine' : None, 'cpu'     : 'sparc'},
    'x86-windows'    : { 'os'      : 'Windows',   'machine' : None, 'cpu'     : None},
    'cygwin'         : { 'os'      : 'CYGWIN_NT', 'machine' : None, 'cpu'     : None}
    }

host_aliases = {
    'cygwin' : 'x86-windows'
    }
