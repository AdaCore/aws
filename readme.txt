
			    A W S - Ada Web Sever
                               0 . 9 . 10
				       
March 10th, 2001.


Dmitriy Anisimkov and I are happy to announce the availability of the 
AWS 0.9.10 release. This version is close to the 1.0 version. We plan to
change slightly the API at this stage but it should be mostly stable.

AWS stand for Ada Web Server. It is not a real Web Server like Apache. It is
an HTTP component to embedded in any applications. AWS is fully developed in
Ada with GNAT.

This is a maintenance release:

  - hotplug_cb.ads was missing from the distribution. Reported by Sune Falck.

  - can be used with latest version of Adasocket 0.1.12 and 0.1.13. With this
    release you must use the updated Win32 port (see link below). Reported
    by Sune Falck.

  - In the administrative page, if the socket is not opened a dummy value was
    reported, not the character minus is displayed. Reported by Sune Falck.

  - some minor bugs have been fixed.

At this stage we feel that AWS is ready to build small to medium Web
server. AWS has been reported to work under Windows NT, Linux and FreeBSD 4.1.

With this new version you'll need at least version 0.1.11 of the Socket binding
from ENST. It has been tested and works fine with version 0.1.13 too.

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
