
			    A W S - Ada Web Server
				 1.1 release
				       
Authors:
   Dmitriy Anisimkov
   Pascal Obry                                            October 14th, 2001,



Dmitriy Anisimkov and I are very happy to announce the availability of the 
AWS 1.1 release. The API could change slightly at this stage but should be
fairly stable now.

AWS stand for Ada Web Server. It is not a real Web Server like Apache. It is
a small yet powerful HTTP component to embedded in any applications. It means
that you can communicate with your application using a standard Web browser
and this without the need for a Web Server. AWS is fully developed in Ada with
GNAT.

Here are the main changes:

  - Server push implementation. Tested only with Netscape Navigator.

  - SOAP - beta implementation of SOAP. Support all SOAP types. This
    implementation has been validated through http://validator.soapware.org/
    and therefore should be quite inter-operable with other SOAP
    implementation. This implementation covers the SOAP client interface and
    as all supports to build SOAP servers.

    Versions that validate on http://validator.soapware.org/ are the AWS 
    version string (AWS.Version) catenated with the SOAP version string
    (SOAP.Version).

  - Add accept queue size parameter to help building heavy loaded servers.

  - Fix the Runme NT service demo.

  - Web Servers is started only if needed (during Server.Start) call and not
    when declaring the HTTP objects.

  - Max Connection is not anymore a discriminant. This parameter is set with
    the Start routine. This change is not upward-compatible, but it is worth 
    it since now, it is possible to change the server configuration
    dynamically. What need to be changed:

    1) Removes the discriminant on each HTTP objects

    2) Pass the number of maximum connections (was the discriminant) in the
       Server.Start call. 

    This will make the server configured the very same way.    

  - Handle User_Agent and Referer HTTP headers.

  - Add message size in the log files (last field). Now the log format is 100%
    compatible with the standard ones (Apache and Internet Information Server).

  - Add server start time in the status page.

  - Add support for user's log.

  - Properly terminate task Session.Cleaner and release associated memory. Fix
    a memory leak.

  - Properly wait for tasks termination before releasing memory. Fix memory
    leak.

  - Improves the documentation.

  - Install AWS as a library (libaws.a)

  - As always some minor bugs have been fixed but are not listed here. See
    src/ChangeLog and SOAP/ChangeLog.

  - Change the build procedure, should be easier and it is cleaner. See
    documentation.

  - First version of the regression tests suite. This will help keeping AWS
    more stable.

  - Add timeouts support for the AWS client interface. Because of this the
    AWS.Client.Create routine has its spec changed (it was a function it is
    now a procedure).

  - In AWS.Client, default retry count is set to 0 (was 1 before). Now the
    AWS.Client routines wont try more than once to get the data by
    default.

  - Properly handle textual (text/html, text/xml...) data that is chunked
    encoded.

  - Fix SSL support in AWS. The SSL layer should now be as reliable as the
    standard socket one.

  - Update distribued Win32 OpenSSL library to version 0.9.6b. Also now there
    are built as DLL.

NOTE: Since we have switched to the .PNG file format we have found that
Netscape Navigator is not able to display the PNG transparent layer properly!

At this stage we feel that AWS is ready to build small to medium Web
server. AWS has been reported to work under Windows NT, Linux and FreeBSD 4.1.

With this new version you'll need at least version 0.1.11 of the Socket binding
from ENST. It has been tested and works fine with version 0.1.13 too. See
pointers below.

The OpenSSL libraries (optional) distributed are for Windows and GNAT
3.13. GNAT 3.12 users must build the libraries from sources or obtain another
set of pre-build libraries see pointers below.

Under UNIX you'll have to build the libraries from sources, it is quite easy 
to do so. This has been tested under Linux without trouble.

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

   Temlates_Parser is a very useful add-on for AWS. You should have a look at
   it if you plan to develop a Web service. Templates_Parser permits to
   completely separate the HTML design from the Ada code.

   Some other Templates engine are WebMacro, FreeMarker, PHP, ASP, JSP and
   Velocity. All of them are based on explicit iterators (#foreach with a
   variable) where Templates_Parser is based on implicit ones (you use a more
   intuitive table iterator). Be sure to check the documentation. Only
   the Velocity project has the goal to support complete separation of HTML
   design and code.

XMLada (optional):
   You need this library only if you want to use AWS SOAP feature. You need
   at least XMLada 0.6.

   http://libre.act-europe.fr/

   XMLAda 0.6 has some memory leaks. This has been fixed now, so with future
   version of XMLAda it will be possible to build long-lived servers.

Socket binding:

   for Win32:
      http://perso.wanadoo.fr/pascal.obry/contrib.html

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
   binaries for Win32 with GNAT 3.13 (and later):
      Included with the main AWS distribution.

   Note that we have used and we distribute (for Win32 platform) OpenSSL
   version 0.9.6b with this AWS release. OpenSSL have been built with GCC
   version 2.95.2 with optimization (-O3) on.

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

A good way to keep informed of AWS news and to share experience with other AWS
users is to register to the AWS dedicated mailing list. See:

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

- Internet Currency Trading System at www.actforex.com by Dmitriy Anisimkov

  This is a server is used to keep historical data about currency trading to
  build charts of currency prices. The charts viewer part is written in Java
  and loaded through AWS. This server can be reach on the Internet.

  Ongoing work is done to based this development on AWS framework only and
  to remove all the Java layers. It is also interesting to note that this is
  an heavy loaded server, it has something like 40 to 50 requests per seconds.


Thanks to all who have reported bugs and have sent us patches.

Dmitriy & Pascal.
