
			    A W S - Ada Web Server
			    1.2 release / SOAP 0.9
				       
Authors:
   Dmitriy Anisimkov
   Pascal Obry                                                April 3rd, 2002,



Dmitriy Anisimkov and I are very happy to announce the availability of the 
AWS 1.2 release. The API could change slightly at this stage but should be
fairly stable now.

AWS stand for Ada Web Server. It is not a real Web Server like Apache. It is
a small yet powerful HTTP component to embedded in any applications. It means
that you can communicate with your application using a standard Web browser
and this without the need for a Web Server. AWS is fully developed in Ada with
GNAT.

AWS support SOAP, Server Push, HTTPS/SSL, client HTTP, hotplug modules... We
have worked very hard to make this release as stable as possible. Note that
Hotplug modules are very nice but have a potentially security hole as it is
implemented today. A new secure implementation will be proposed in a future
version.

The SOAP implementation has been validated on http://validator.soapware.org/.


Here are the main changes:

   - You need GNAT 3.14 to build AWS 1.2 (GNAT 3.13 is not supported anymore).

   - Add a main procedure termination controller (AWS.Server.Wait)

   - Fix some memory leak in AWS.Response.Data and AWS.Server.Protocol_Handler
     for binary data.

   - In AWS.URL, function URI was not correctly named. It has been renamed
     Pathname. This is a backward compatibility problem. Path and File
     function has been added into AWS.URL.

   - Fix bug to close a connection when server is heavy loaded.

   - Add AWS.Services.Page_Server service. This service is a straight forward
     implementation of a simple static web page server. See WPS demo. It
     supports two template files: 404.thtml and aws_directory.thtml.

   - Fix race condition in AWS.Server implementation. This was a very nasty
     bug, sockets could be handled in two different slots. If you are
     experiencing bug with heavy loaded servers you should plan to upgrade as
     soon as possible.

   - Add dispatchers facilities which is more general than the callback
     procedure (access to procedure) for example it can transport user's
     data. This is the base of a general framework for high level services.

   - Add three high level Dispatcher facilities (AWS.Services.Dispatchers):
     1) on URI
     2) on request method
     3) on Host name (also called virtual hosting)

   - Add AWS.Templates (renaming of Templates_Parser) as this component is a
     very important one for Web development.

   - AWS can now have servers binded to different IP addresses if the 
     computer has more than on IP addresses. See AWS.Config.Server_Host.

   - New version of libssl32.dll and libeay32.dll based on OpenSSL 0.9.6c.

   - Client handle properly the HTTP continue response message.

   - Templates_Parser now integrated into AWS.Templates package. This version
     has a cache fully is thread safe.

   - Session cookie was set for first path (and sub path) used, it means that
     it was possible to have multiple session for a Web site. This behavior
     was the result of a bug. Now a single session is created for the whole
     site (starting at /).

   - Fix timeouts for client keep-alive connection.

   - SOAP handle properly zero length array.

   - SOAP handle properly Array of Record.

   - Boolean types are now directly handled on sessions.

   - Now always install AWS under directory AWS, INSTALL make variable must
     point to the AWS parent directory.

   - Plus many small fixes, enhancements and documentation work.

You can have a look at docs/TODO file to see what are the topics that we will
probably implement in future releases.

NOTE: Since we have switched to the .PNG file format we have found that
Netscape Navigator is not able to display the PNG transparent layer properly!

At this stage we feel that AWS is ready to build small to medium Web
servers. AWS has been reported to work on Windows NT/XP, Linux and FreeBSD 4.1.

With this new version you'll need at least version 1.0 of the Socket binding
from ENST. See pointers below.

The OpenSSL libraries (optional) distributed are for Windows only. On UNIX
you'll have to build the libraries from sources, it is quite easy to do
so. This has been tested under Linux without trouble.

See documentation for build information.


Pointers:
---------

AWS User's Mailing List:
   http://lists.act-europe.fr/mailman/listinfo/aws

AWS Home Page (sources and documentation): 
   http://libre.act-europe.fr/

Templates_Parser sources: 
   Templates_Parser module (sources and documentation) is provided with AWS
   distribution. Latest version of this module and the documentation can be
   found at:

   http://perso.wanadoo.fr/pascal.obry/contrib.html
   http://perso.wanadoo.fr/pascal.obry/templates_parser.html

   Templates_Parser is a very useful add-on for AWS. You should have a look at
   it if you plan to develop a Web service. Templates_Parser permits to
   completely separate the HTML design from the Ada code.

   Some other Templates engine are WebMacro, FreeMarker, PHP, ASP, JSP and
   Velocity. All of them are based on explicit iterators (#foreach with a
   variable) where Templates_Parser is based on implicit ones (you use a more
   intuitive table iterator). Be sure to check the documentation. Only
   the Velocity project has the goal to support complete separation of HTML
   design and code.

GNU/Ada - GNAT
   You need at least version 3.14 to use AWS 1.2.

   ftp://cs.nyu.edu/pub/gnat/

XMLada (optional):
   You need this library only if you want to use AWS SOAP feature. You need
   at least XMLada 0.7.1.

   http://libre.act-europe.fr/

Socket binding:
   Since AWS 1.2 you need at least version 1.0 of the Socket binding.

   for Win32:
      http://perso.wanadoo.fr/pascal.obry/contrib.html
      http://vagul.tripod.com/adasockets.tgz

   for UNIX:
      http://www.rfc1149.net/devel/adasockets

POSIX Binding (optional) :

   for Win32:
      http://perso.wanadoo.fr/pascal.obry/contrib.html

   for UNIX:
      http://www.cs.fsu.edu/~baker/florist.html

OpenSSL library (optional) :

   Sources for UNIX or Win32:
      http://www.openssl.org

   binaries for Win32:
      Included with the main AWS distribution (win32 directory).

   Note that we have used and we distribute (for Win32 platform) OpenSSL
   version 0.9.6c with this AWS release. OpenSSL have been built with GCC
   version 2.95.3 with -O3 optimization level.

   See OpenSSL license (docs/openssl.license).

Windows Services API (optional):

   To build the runme demo as a Windows NT/2000 services you must download
   the services API made by Ted Dennison for his SETI@Home project.

      http://www.telepath.com/dennison/Ted/SETI/SETI_Service.html


Reporting bugs:
---------------

You can report bugs to:

   Dmitriy Anisimkov	anisimkov@yahoo.com
   Pascal Obry		p.obry@wanadoo.fr

It would be nice if you could also sent us a note if you are using AWS just
to know if it is used at all or not :) And if you are ok, we'll add an entry
for your project in the next section.


AWS User's Mailing List:
------------------------

A good way to keep informed of AWS news and to share experiences with other
AWS users is to register to the AWS dedicated mailing list. See:

   http://lists.act-europe.fr/mailman/listinfo/aws


AWS uses
--------

- SETI@Home from Ted Dennison. AWS is used as a "plugable" GUI to retrieve
  different program status.

- DOCWEBSERVER from Wiljan Derks

  In our department we keep our documents in a directory tree. These documents
  are all project related and have a certain naming convention to be able to
  find the right document. In the past I already wrote a program that searches
  though this directory and then converts the found documents into fixed html
  pages. With AWS I was able to get a much nicer setup. I have now a server
  that can do the following:
    - browse through the projects in explorer style. The html contains info
      about the document like date and title.
    - one can check in documents through the web interface
    - it shows our download page as I have send you in the example
    - we have now all our documentation in small pieces of html as is needed
      to build .chm (w2k compiled help) files. For these we use a content
      file, that is also stored in the document archive.

  The docwebserver gives by reading all this stuff the direct view on this
  documentation. On the other hand I can run some tool and automatically
  generate the .chm files.

- OESM Server (OESM=Overall Equipment Status Monitoring) from Wiljan Derks

  I am working on a project now for our factories. ITEC mainly delivers
  equipment for discrete semiconductor assembly. Almost all of that equipment
  is now controlled by a similar Ada 95 based code with having a lot of code in
  common. One of the common things, is the way we log errors and state changes
  of our equipment.

  The OESM Server is an application which copies all this information
  continuously to its local pc by opening the proper files on the remote
  equipment. That data copied is also stored in local files. The web server
  component of the application can then, making use of that data, give reports
  that show things like the amount of products produced in a certain period,
  error paretos of equipment, mtbf, %time in production and of course many
  other things.

  The cool thing of course is that this information can easily be charted (I
  am use kavachart) and it allows simple navigation through different groups
  of equipments and different views on the equipment.

- WORM from Pascal Obry 
  (see http://www.ada-france.org/ADHERENTS/101100/05-obry.pdf).

  A Web server to share bookmarks, this server was using a standard CGI
  design. To keep session information we were using a GLADE partition. With
  AWS the design has been really simplified, there is no need for a session
  partition, there is no need to build all CGI as partitions too. GLADE is now
  used only to handle distributed objects. Indeed WORM is a multi-server
  system (using RACW) with a register/unregister mechanism.

  Also the server seems to be fastest, there is no more CGI to spawn.

- Internet Currency Trading System at http://www.actforex.com by 
  Dmitriy Anisimkov

  This is a server is used to keep historical data about currency trading to
  build charts of currency prices. The charts viewer part is written in Java
  and loaded through AWS. This server can be reach on the Internet.

  Ongoing work is done to based this development on AWS framework only and
  to remove all the Java layers. It is also interesting to note that this is
  an heavy loaded server, it handle something like 40 to 50 requests per
  seconds on a Windows 2000 Server.

- http://www.forexcoach.com site is powered by AWS. This site has been done by
  Dmitriy Anisimkov.

Thanks to all who have reported bugs and have sent us patches.

Dmitriy & Pascal.
