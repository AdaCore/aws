
[ANNOUNCE] - New AWS (Ada Web Server) version (0.10)

			    A W S - Ada Web Sever
                                  0 . 10
				       
May 6th, 2001.


Dmitriy Anisimkov and I are happy to announce the availability of the 
AWS 0.10 release. This version is close to the 1.0 version. We plan to
change slightly the API at this stage but it should be mostly stable.

AWS stand for Ada Web Server. It is not a real Web Server like Apache. It is
an HTTP component to embedded in any applications. It means that you can
communicate with your application using a standard Web browser and this
without the need for a Web Server. AWS is fully developed in Ada with GNAT.

Here are the main changes:

  - Fix bug in session handling. The same session ID could have been allocated
    to differents client.

  - New Server interface (more dynamic). HTTP has only one discriminant now,
    the other setting are done through the Start procedure.

  - Default AWS.OS_Lib is now using the GNAT based implementation instead of
    the POSIX one. This should make it easier to build AWS.

  - Implement HTTP/1.0 and Keep-Alive connection (Netscape browser ask this
    kind of connection). Should fix more server hanging problems.

  - Server parameters can be handled with case sensitivity or not.

  - Fix possible memory leak in status data.

  - Improve again the way slots are aborted, this should fix more browser
    hanging problems.

  - Add Peername to the status data.

  - Improve a bit the documentation.

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

   Temlates_Parser is a very useful add-on for AWS. You should have a look at
   it if you plan to develop a Web service. Templates_Parser permits to
   completly separate the HTML design from the Ada code.

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
      (we have used and we distribute OpenSSL version 0.9.5a with this AWS
      release, we have also tested AWS with OpenSSL 0.9.6a without trouble)

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


AWS uses
--------

- SETI@Home from Ted Dennison. AWS is used as a "plugable" GUI to control the
  services status.


Thanks to all who have reported bugs and have sent us patches.

Dmitriy & Pascal.
