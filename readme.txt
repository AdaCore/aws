
			    A W S - Ada Web Sever
                                 0 . 9 . 11
				       
April 28th, 2001.


Dmitriy Anisimkov and I are happy to announce the availability of the 
AWS 0.9.11 release. This version is close to the 1.0 version. We plan to
change slightly the API at this stage but it should be mostly stable.

AWS stand for Ada Web Server. It is not a real Web Server like Apache. It is
an HTTP component to embedded in any applications. It means that you can
communicate with your application using a standard Web browser and this
without the need for a Web Server. AWS is fully developed in Ada with GNAT.

Here are the main changes:

  - Fix bug in Keep-Alive connection handling in the server. If client does
    not ask for a non Keep-Alive connection we assume a Keep-Alive
    connection. This conform to RFC 2616.

  - New rountine AWS.Response.URL to jump to a given Web page.

  - AWS now use the new Templates_Parser API. This version should be
    stable now. Templates_Parser is used to display the status page.

  - The main demo (runme) can now be launched as a Windows NT/2000
    service. This uses Ted Dennison Ada Services library.

  - It is now possible to specify the certificate to use for the SSL 
    connection.

  - Improve (slightly) the documentation.

  - Do not use GIF images anymore, we use PNG images.

  - As always some bugs have been fixed.

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
to do so. This has been tested under Linux without trouble.

See documentation for build information.


Pointers:
---------

AWS documentation: 
   http://perso.wanadoo.fr/pascal.obry/aws.html

AWS sources: 
   http://perso.wanadoo.fr/pascal.obry/contrib.html

Templates_Parser sources: 
   Templates_Parser module is provided with AWS distribution. Latest version
   of this module and the documentation can be found at:

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
      (we have used and we distribute OpenSSL version 0.9.5a with AWS
      v0.9.11, we have also tested AWS with OpenSSL 0.9.6a without trouble)

   binaries for Win32 with GNAT 3.13 (and later):
      Included with the main AWS distribution.

   binaries for Win32 with GNAT 3.12:
      http://vagul.tripod.com/libssl.zip

Windows Services API (optional):

   To build runme demo as a Windows NT/2000 services you must download
   the services API made by Ted Dennison for his SETI@Home project.
      http://www.telepath.com/dennison/Ted/SETI/SETI_Service.html


Reporting bugs:
---------------

You can report bugs to:

   Dmitriy Anisimkov	anisimkov@yahoo.com
   Pascal Obry		p.obry@wanadoo.fr

It would be nice if you could also sent us a note if you are using AWS just
to know if it is used at all or not :)


Thanks to all who have reported bugs and have sent us patches.

Dmitriy & Pascal.
