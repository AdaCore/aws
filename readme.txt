
			    A W S - Ada Web Sever

March 7th, 2001.


Dmitriy Anisimkov and I are happy to announce the availability of the 
AWS 0.9.9 release. This version is close to the 1.0 version. We plan to
change slightly the API at this stage but it should be mostly stable.

AWS stand for Ada Web Server. It is not a real Web Server like Apache. It is
an HTTP component to embedded in any application. AWS is fully developed in
Ada with GNAT.

This new version has a lot of changes:

  - Many bugs have been fixed (as always!)

  - Add first version of a configuration file (aws.ini).

  - Add logging facility (log format is very close to the Apache one). The log
    file can be configured to change daily or monthly.

  - AWS support select HTML tags with multiple values (i.e. a parameter name
    can be given a list of values)

  - Add an API to use AWS as a communication layer between programs.

  - Add support for Hotplug module (dynamically bound module into a running
    Web  server). See documentation. This uses the communication API. Support
    for Hotplug modules is at a beta stage.

  - POSIX implementation of the OS_Lib.GMT_Clock function now returns GMT
    time. The GMT_Clock was correct only with the GNAT and Win32
    implementation of OS_Lib.

  - Client method HEAD is implemented.

  - AWS.Status is lightler because all routines to set the status have been
    moved to another unit.

  - AWS.Client now support Keep-Alive connection.

  - We have simplified the makefile and build process, but this needs
    certainly some more improvements.

  - And again, improve the documentation.

At this stage we feel that AWS is ready to build small to medium Web
server. AWS has been reported to work under Windows NT, Linux and FreeBSD 4.1.

With this new version you'll need at least version 0.1.9 of the Socket binding
from ENST. It has been tested and works fine with version 0.1.11.

You can download AWS and the Sockets binding for Win32 directly from:
http://perso.wanadoo.fr/pascal.obry/contrib.html

An online version of the documentation can be found at
http://perso.wanadoo.fr/pascal.obry/aws.html.

The OpenSSL libraries (optional) distributed are for Windows GNAT 3.13. GNAT
3.12 users must build the libraries from sources or obtain Win32 binaries from 
http://vagul.tripod.com/libssl.zip.

Under UNIX you'll have to build the libraries from sources, it is quite easy 
to do so.

See documentation for build information.


Pointers:
---------

AWS documentation: 
   http://perso.wanadoo.fr/pascal.obry/aws.html

AWS sources: 
   http://perso.wanadoo.fr/pascal.obry/contrib.html

Socket binding:

   for Win32:
      http://perso.wanadoo.fr/pascal.obry/contrib.html

   for UNIX:
      http://www.infres.enst.fr/ANC/

POSIX Binding (optional) :

   for Win32:
      http://perso.wanadoo.fr/pascal.obry/contrib.html

   for UNIX:
      http://www.cs.fsu.edu/~baker/florist.html

OpenSSL library (optional) :

   Sources for UNIX or Win32:
      http://www.openssl.org
      (we have used and we distribute version OpenSSL version 0.9.5a with AWS
      v0.9.9, we have also tested AWS with 0.9.6 without trouble)

   binaries for Win32 with GNAT 3.13 (and later):
      Included with the main AWS distribution.

   binaries for Win32 with GNAT 3.12:
      http://vagul.tripod.com/libssl.zip


Reporting bugs:
---------------

You can report bugs to:

   Dmitriy Anisimkov	anisimkov@yahoo.com
   Pascal Obry		p.obry@wanadoo.fr

It would be nice if you could also sent us a note if you are using AWS just
to know if it is used at all or not :)


Thanks to all who have reported bugs and send us patches.

Dmitriy & Pascal.
