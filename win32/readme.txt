
* How to use OpenSSL dynamic link libraries for Win32.

   1. Copy libeay32.dll and libssl32.dll to the directory referred in the
      PATH environment variable.

   2. gnatmake should be called with a 
      -largs -L{AWS Directory}/win32/ -lssl -lcrypto
      parameters

   Note that the libssl.a and libcrypto.a import libraries will be created
   during AWS build procedure. See makefile for the build procedure.

* How to use LDAP

  During the build procedure libldap.a will be created usinf the definition
  file wldap32.def. This import library will be created to use wldap32.dll
  which is installed on Windows NT, 2000 and XP machines.

  To build applications using LDAP you need to provides the -lldap linker
  option.
