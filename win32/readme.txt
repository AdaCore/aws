
* To build and run applications using SSL it is required to install
  the OpenSSL DLL. See the following page for a link:

  http://www.openssl.org/related/binaries.html

  The needed DLLs are: libeay32.dll and libssl32.dll

  They must be copied into <gnat_root>/lib to be found by the
  linker and into <gnat_root>/bin to be found at runtime.

* How to use LDAP

  To build applications using LDAP you need to provides the -lwldap32
  linker option.
