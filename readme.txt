
			    A W S - Ada Web Server
			    1.3 release / SOAP 1.1
				       
Authors:
   Dmitriy Anisimkov
   Pascal Obry                                             January 15th, 2003



We are very happy to announce the availability of the AWS 1.3 release. The API
could change slightly at this stage but should be fairly stable now.

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

   - Fix bug in Client module in reading chunk size returned by some
     Web servers.

   - Fix bug in Client module when reading large binary data.

   - Fix bug in Client module when reading empty file (file whose size
     Content-Length is 0).

   - Fix bug in log where client IP addresses could be wrong.

   - Add support for embedded resources. An AWS server can now be easily
     built as a stand-alone executable. All resources (images, templates,
     HTML pages...) can be embedded into the executable. See AWS.Resources
     tree and the AWSRes tools to help building the embedded resources. There
     is also a support for user defined stream.

   - Add support for client upload (see AWS.Client.Upload).

   - Send file size in the chunk transfert encoding. This make it possible for
     the browser to display the download's progress-bar.

   - Fix SOAP name space handling. It is now possible to set the name space
     for a Payload and AWS correctly set the name space as defined in incoming
     requests.

   - Add complete HTML code browsing facility using gnathtml. See
     aws/docs/html directory.

   - Add support for Digest authentication (far more secure than the
     Basic one).

   - Fix a serious file upload bug. Some files were not correctly detected in
     the MIME sections.

   - AWS is now safer, by default URL are checked for validity and an
     exception is raised if an URL try to reach a resource above the Web
     root. This was not a big problem in AWS as most URL are just "string"
     which does not reference file on disk.

   - Add support to send SMTP e-mail (MIME attachments are supported).

   - Add LDAP client binding (support read access LDAP servers).

   - Add support for user/password in URL.

   - Add fast support for string/stream_element_array conversion in
     AWS.Translator if the compiler/target support it.

   - Support HTTP header lines in multiple lines as permitted by the RFC.

   - Option added to AWS.Client.Get to automatically follow redirection.

   - Add some supports for the Jabber protocol (Message and Presence detection)

   - Improve performance when sending small files.

   - Fix bug in chunk protocol, it was working fine but was not conforming to
     the RFC.

   - Fix chunked encoding over SSL incompatibiliy with Opera browser (really
     an Opera bug)

   - Update to Templates_Parser 4.2 (add +,add,-,sub,*,mult,/,div,mod filters,
     support attributes for vectors and matrix, add not and /= operators).
     This new version completely preserve the template's format.

   - Complete rewrite of the socket handling. Sockets are now buffered for
     reading and writing making AWS faster. The AWS.Net interface also
     provide two implementations one based on GNAT.Sockets (the default) and
     one based AdaSockets.

     This is a big change and means that there is some incompatibilities. For
     example now for all socket errors the exception AWS.Net.Socket_Error is
     raised, it used to be some AdaSockets errors (either Connection_Refused
     or Socket_Error.

   - SSL support is now specified at link time and not at configuration time.
     To build an SSL/AWS application you just need to link with -lssl and
     -lcrypto. There is no specific AWS configuration.

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
   http://libre.act-europe.fr/aws

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

   You need at least version 3.15 to use AWS 1.3.

   ftp://cs.nyu.edu/pub/gnat/

Socket binding (Optional) :

   Since AWS 1.2 you need at least version 1.0 of the Socket binding. Note
   that by default AWS uses GNAT.Sockets.

   for Win32:
      http://perso.wanadoo.fr/pascal.obry/contrib.html
      http://vagul.tripod.com/adasockets.tgz

   for UNIX:
      http://www.rfc1149.net/devel/adasockets

XMLada (optional):

   You need this library only if you want to use AWS SOAP feature. You need
   at least XMLada 0.7.1.

   http://libre.act-europe.fr/

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
   version 0.9.6g with this AWS release. OpenSSL have been built with GCC
   version 3.2 with -O3 optimization level.

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
