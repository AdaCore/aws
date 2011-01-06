
* To build and run applications using SSL it is required to install
  the OpenSSL DLL. See the following page for a link:

  http://www.openssl.org/related/binaries.html

  The needed DLLs are: libeay32.dll and libssl32.dll

  They must be copied into <gnat_root>/lib to be found by the
  linker and into <gnat_root>/bin to be found at runtime.

* How to use LDAP

  During the build procedure libldap.a will be created using the definition
  file wldap32.def. This import library will be created to use wldap32.dll
  which is installed on Windows NT, 2000 and XP machines.

  To build applications using LDAP you need to provides the -lldap linker
  option.
