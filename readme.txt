
			    A W S - Ada Web Server
			    1.4 release / SOAP 1.2
				       
Authors:
   Dmitriy Anisimkov
   Pascal Obry                                             October 14th, 2003



We are happy to announce the availability of the AWS 1.4 release. The API
could change slightly at this stage but should be fairly stable now.

AWS stand for Ada Web Server. It is not a real Web Server like Apache. It is
a small yet powerful HTTP component to embed in any applications. It means
that you can communicate with your application using a standard Web browser
and this without the need for a Web Server. AWS is fully developed in Ada
with GNAT.

AWS support SOAP, Server Push, HTTPS/SSL, client HTTP, hotplug modules... We
have worked very hard to make this release as stable as possible. Note that
Hotplug modules are very nice but have a potentially security hole as it is
implemented today. A new secure implementation will be proposed in a future
version.

The SOAP implementation has been validated on http://validator.soapware.org/.

Note that some of the changes listed below can introduce non upward
compatibility. In such a case we try to give proper advice on how to change
the code to work properly. Of course we try to avoid this as much as possible
but we really prefer to have a clean API instead of keeping awkwards
implementations.

Here are the main changes since AWS 1.3 :

   - Add WSDL parser and SOAP stub/skeleton generator. This implementation has 
     passed many tests (base and B tests) from the SOAP Interoperability Tests
     Specification (see http://www.xmethods.net/ilab/ and
     http://www.whitemesa.com/interop/proposal2.html).

   - wsdl2aws, new tool. Generate SOAP/AWS stub/skeleton from a WSDL
     document. With this tool it is very easy to build or to connect to Web
     Services. This tool use the WSDL parser and SOAP generator API. See above.
     wsdl2aws has been tested with some services on the Web, one of them was
     the Google Search Service WSDL specification which is quite complex.

   - Fix memory leak in templates parser when using a non existent template
     file.

   - Disable keep-alive connection for the last free slots. It make AWS better
     for heavy-loaded servers.

   - New configuration parameter Free_Slots_Keep_Alive_Limit to control
     keep-alive connections on heavy-loaded servers.

   - It is now possible to set the message that must be displayed when an
     authentication is cancelled.

   - Correctly free connection if a SOAP request fails. Fix memory leak.

   - LDAP API checks for non initialized Directory object.

   - Handle properly arrays of SOAP record. Only basic types were supported.

   - Fix space handling bug in SOAP parser. Multiple spaces were removed
     from the parsed string.

   - Handle properly accents in SOAP messages. Convert properly DOM Utf8
     encoded tree back to basic Ada 8 bits string.

   - SOAP strings are now properly encoded using Utf8. This is needed for
     better interoperability.

   - It is now possible to retrieve the running server from a callback
     procedure. From there it is also possible to get the server
     configuration. See AWS.Server.Get_Current routine.

   - It is now possible to have access to the server internal status. See
     AWS.Server.Status package.

   - The Log API is now thread safe.

   - Fix regression in client API on standard and proxy authentication.

   - Fix two memory leaks in templates parser engine. This version has been
     checked with gnatmem in a large project and should be memory leak free
     at this point.

   - Handle properly file upload for filenames with accents and spaces.

   - Add session cleanup callback (called when a session ID has expired).

   - Fix memory leak in the sessions container.

   - Properly handle SOAPAction in SOAP persistent connections. It was not
     possible to change the SOAPAction value for each request. The same
     SOAPAction was used for all requests over the same connection.

     => This change is not upward compatible. The SOAPAction value must be
        removed from the persistent connection creation (AWS.Client.Create)
        and passed to the SOAP.Client.Call.

   - Change AWS.Client.SOAP_Post and SOAP.Client.Call spec for the persistent
     connection cases. 

     => This is not upward compatible but easier to use. The calls were
        passing the persitent connection using an access mode. Just remove
        the 'Access attribute to pass the connection object.

   - Change the way Size of resources are computed. The size is now part of
     the objects (File, Embedded or Stream resources). This is a better design.
     AWS.Response.Stream and AWS.Response.Set.Stream does not have the stream
     size as parameter. Furthermore the AWS.Response.Set.Content_Length
     routine has been removed. The stream size is now given by overloading the
     Size stream's method (see routine AWS.Resources.Streams.Size).

     => This is not upward compatible. Remove calls to
        AWS.Response.Set.Content_Length and implement the
        AWS.Resources.Streams.Size method for the stream object.

   - Templates_Parser use lot less stack space than before while parsing a
     template file. It is now possible to parse very large template file.

   - Important Templates_Parser speed-up (between x10 and x15) for template
     files containing large vector or matrix tags.

   - Slots activity counters are updated only if an answer has been handled.

   - The directory browser's template file can now be configured using the
     AWS.Config module.

   - Using the session feature without server's session support is now
     properly detected (Constraint_Error is raised).

   - It is now possible to give back a socket, removed from the server using
     Socket_Taken, to the server. This can be used to implement waiting line
     for requests taking a long time without loosing the keep-alive status
     and without blocking server's resources.

   - Support for memory streams and compressed memory streams (deflate /
     inflate) based on ZLib library.

   - New routines to compress / decompress a Stream_Element_Array.

   - New routines to compress / decompress a file using the Gzip encoding.

   - AWS.Response.Set.Message_Body (with an access to a Stream_Element_Array)
     has been removed. There was not clean way to integrate this with the ZLib
     memory stream supprot.

     => This is not upward compatible. Add ".all" to the parameter to use the
     version with a Stream_Element_Array formal parameter.

   - Add new routines to retrieve the log filenames and the status of
     the log files. See AWS.Server.Log.

   - Plus many small fixes, enhancements, API comments, and documentation work.

You can have a look at docs/TODO file to see what are the topics that we will
probably implement in future releases.

NOTE: Since we have switched to the .PNG file format we have found that
Netscape Navigator is not able to display the PNG transparent layer properly!

The OpenSSL libraries (optional) distributed are for Windows only. On UNIX
you'll have to build the libraries from sources, it is quite easy to do
so. This has been tested on GNU/Linux without trouble.

The LDAP binding will use the LDAP dynamic library on Windows. On UNIX you
need to build and install OpenLDAP.

See documentation for build information.


Obsolescent features:
---------------------

In each new version we try to be upward compatible with previous
version. This is really important, but in some cases it seems that a
"redesign" of the API would be good in the long term. All obsolescent
features will be listed in this section. Those features will be
removed in the next version. Note that you can check usage of those
features in your application by using the GNAT's -gnatwj option as we
have tagged all obsolescent features with a pragma.

Note that pragma Obsolescent and -gnatwj option is only supported
since GNAT 3.16.

   AWS.Server.Start_Log
      use AWS.Server.Log.Start instead.

   AWS.Server.Stop_Log
      use AWS.Server.Log.Stop instead.

   AWS.Server.Start_Error_Log
      use AWS.Server.Start_Error instead.

   AWS.Server.Stop_Error_Log
      use AWS.Server.Stop_Error instead.


Validation:
-----------

AWS 1.4 has been compiled and has passed all tests on:

   Windows XP, GNAT 3.15a1, 3.16a, 3.17w and 5.01w

   Windows NT 4.0, GNAT 3.15a1

   GNU/Linux x66, GNAT 3.16a and 3.17w

   SPARC Solaris 8, GNAT 3.17w

Others platforms / compiler version combinations have not been tested, it
does not mean that it's not working.

Previous version of AWS have been build on FreeBSD 4.1 and MacOSX.


Pointers:
---------

AWS User's Mailing List:
   http://lists.act-europe.fr/mailman/listinfo/aws

AWS Home Page (sources and documentation): 
   http://libre.act-europe.fr/aws

Templates_Parser sources: 
   Templates_Parser module (sources and documentation) is provided with AWS
   distribution. Version 6.2 is distributed with AWS 1.4.

   Latest version of this module and the documentation can be found at:

   http://perso.wanadoo.fr/pascal.obry/contrib.html
   http://perso.wanadoo.fr/pascal.obry/templates_parser.html

   Templates_Parser is a very useful add-on for AWS. You should have a look at
   it if you plan to develop a Web application. Templates_Parser permits to
   completely separate the HTML design from the Ada code.

   Some other Templates engine are WebMacro, FreeMarker, PHP, ASP, JSP and
   Velocity. All of them are based on explicit iterators (#foreach with a
   variable) where Templates_Parser is based on implicit ones (you use a more
   intuitive table iterator). Be sure to check the documentation. Only
   the Velocity project has the goal to support complete separation of HTML
   design and code.

GNU/Ada - GNAT

   You need at least version 3.15 to build and use AWS 1.4.

   http://libre.act-europe.fr/GNAT/

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
   version 0.9.7c with this AWS release. OpenSSL has been built with GNAT 5.01a
   C subsystem (based on GCC 3.2.3) with -O3 optimization level.

   See OpenSSL license (docs/openssl.license).

OpenLDAP library (optional) :

   Sources for UNIX or Win32:
      http://www.openldap.org/

   binaries for Win32:
      Included with the main AWS distribution (win32 directory). The import
      library will bind to the Microsoft LDAP dynamic library.

Windows Services API (optional):

   To build the runme demo as a Windows NT/2000 services you must download
   the services API made by Ted Dennison for his SETI@Home project.

      http://www.telepath.com/~dennison/Ted/SETI/SETI_Service.html


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


Contributors:
-------------

Thanks to the contributors and peoples who send feedbacks, ideas
about AWS. In the early stage of the project this is very valuable.

So thanks goes to Ted Dennison, Wiljan Derks, Sune Falck, David C. Hoos,
Thierry Lelegard, Nicolas Lesbats, Jean-François Rameau, Maxim Reznik,
Jean-Pierre Rosen, Jerôme Roussel, Ariane Sinibardy.


AWS uses:
---------

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

- http://www.ada-ru.org (Ada in Russian)

  This Web Site is powered by AWS.

Thanks to all who have reported bugs and have sent us patches.

Dmitriy & Pascal.
