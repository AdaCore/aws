
How to use OpenSSL dynamic link libraries for Win32.

1. Copy libeay32.dll and libssl32.dll to the directory referred in the
   PATH environment variable.

2. gnatmake should be called with a 
   -largs -L{AWS Directory}/win32/ -lssl -lcrypto
   parameters

Note that the libssl.a and libcrypto.a import libraries will be created during
AWS build procedure. See makefile for the build procedure.
