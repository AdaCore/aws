
			    A W S - Ada Web Server
			    2.1 release / SOAP 1.3

Authors:
   Dmitriy Anisimkov
   Pascal Obry                                            November 11th, 2004



We are happy to announce the availability of the AWS 2.1 release. The API
could change slightly at this stage but should be fairly stable now.

AWS stand for Ada Web Server. It is a small yet powerful HTTP component to
embed in any applications. It means that you can communicate with your
application using a standard Web browser and this without the need for a Web
Server. AWS is fully developed in Ada with GNAT.

AWS support SOAP/WSDL, Server Push, HTTPS/SSL, client HTTP, hotplug
modules...

AWS comes with SOAP/WSDL support, two tools are proposed:

   ada2wsdl   which generates a WSDL document from an Ada spec

   wsdl2aws   which generates stubs/skeletons AWS code from a WSDL document

Both tools have mapping for standard Ada types but also supports Ada
enumerations, character, records and arrays.

The SOAP implementation has been validated on http://validator.soapware.org/.


Changes
-------

Here are the main changes since AWS 2.0 :

   - Improve Nonce value generation. It is far less probable to generate
     the same Nonce value twice.

   - Completely new hotplug implementation. This is the first secure version
     and can now be used as all other AWS features.

   - Support for Content-Disposition for file and stream. This make it
     possible to properly pass the filename to be used by the client browser
     to save the file locally. See Disposition and User_Filename parameters of
     Response.File and Response.Stream constructors.

   - Uses the Ada.Containers reference implementation (known as AI302). These
     containers are based on an hash table and is therefore faster than the
     previous containers based on an AVL tree. A two times speedup has been
     experienced. Also the GNAT specific dynamic tables have been replaced
     by an Ada.Containers.Vectors.

   - A new templates engine child package provides a way to save/load a
     Translate_Table to/from an XML document. See Templates_Parser.XML.

   - A new templates engine child package provides a way to store/retrieve a
     Tag to/from a string. This can be useful to store a complete template tag
     into a session variable. See Templates_Parser.Utils.

   - Use socket timeout based on poll(), this is cleaner as it is not
     needed anymore to have a cleaner task on the client side to watch
     and close connections after the timeout.

   - Better support for project files for developers.

   - Templates Engine is again faster, it provides a new way to store
     translations (see Translate_Set) which is not a fixed structure like
     Translate_Table.

   - New templates engine with unified tags which can be nested at any
     level. Vector_Tag and Matrix_Tag are now just subtype of the new type
     Tag. Note that this new implementation is 99% upward compatible.

   - Support for certificates chain.

   - Support for streaming response in the HTTP client interface.

   - Properly support SOAP envelope nodes containing an optional header node.

   - Add support for SOAP long (xsd:long).

   - Properly handle empty SOAPAction in SOAP requests.

   - Better support for name-spaces in SOAP requests.

   - Fix a wsdl2aws bug for array's element whose name contains an underscore.

   - Add possibility to select the endpoint URL when using generated client
     stubs (only the default URL defined in the WSDL document was handled
     before).

   - Add a new option (-d) in wsdl2aws to generate debug code. This is useful
     to debug a SOAP client or server. It displays the SOAPAction, Procedure
     name, Payload and response.

   - Add exception handler in generated SOAP code for callback and server
     skeleton.

   - Add a new package AWS.Net.Generic_Sets which support user's data
     associated with each sockets.

   - New support for HTTP/SOAP messages with attachments. This support is for
     now only at the library level, there is no WSDL support nor direct
     interface with the SOAP engine. This feature has been contributed by
     Henrik Sundberg.

   - Properly fill in the cache in buffered Read, this makes file upload
     more efficient.

   - New Templates_Parser NO_CONTEXT command filter.

   - Templates_Parser table now support section blocks. This permits better
     code sharing amongst sections.

   - File upload is more secure now. All files upload to the server are
     removed after calling the callback function.

   - Add support for client's HTTPS requests through a proxy (tunneling).

   - Add support for logging the sockets activity (sent and received data)
     This API (AWS.Net.Log) can be used to debug complex applications.
     This feature has been contributed by Henrik Sundberg.

   - Add support for SOAP hotplug modules. Only standard HTTP requests were
     supported by an hotplug module.

   - Fix bug in ada2wsdl generator preventing the array component type
     definitions to be generated in some cases.

   - Fix bug in ada2wsdl generator which generated duplicate definitions for
     record components in some cases.

   - New filters ABS and NEG in the templates engive.

   - Add association iterator for a Template_Set in the templates engine.

   - Include template parameters can now be used as filter parameters.

   - A new set of page splitters has been implemented. These splitters have
     different ways to organize the pages, for example one can create
     automatically an alphabetical index. Check the AWS.Services.Split_Pages
     hierarchy for more information.
     This feature has been contributed by Jean-Pierre Rosen.

   - Fix bug in the directory browser when the URL given did not end with a
     slash. In this case it was not possible to access the files under this
     directory.

   - Fix bug in template engine preventing the use of some characters as
     filter parameters.

   - Add support for non standard SMTP/POP ports in the Web_Mail service.

   - Plus many small fixes, enhancements, API comments, and documentation work.


Non upward compatible changes
-----------------------------

Note that the changes listed below can introduce non upward compatibility.
In such a case we try to give proper advice on how to change the code
to work properly. Of course we try to avoid this as much as possible
but we really prefer to have a clean API instead of keeping awkward
implementations.

   - In templates engine, the Vector routine is not present anymore.
     Use Composite routine instead.

   - In AWS.Client interface, Timeout_Values record field types have been
     changed from Natural to Duration. It is possible to use less then one
     second timeout value now. Just change all http client timeout values to
     Duration type.

   - By default an AWS server does not accept upload files. It is required to
     activate this feature by properly setting the Upload_Directory either
     with the Start routine procedure or a config object.

   - File uploaded to the server are deleted after calling the user's
     callback function. It is up to the user to copy/rename the file to keep
     it around.


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

   - AWS.Net.Set_Blocking_Mode should not be used anymore. Use
     AWS.Net.Set_Timeout instead.


Notes
-----

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


Validation:
-----------

AWS 2.1 has been compiled and has passed all tests on:

   Windows XP, GNAT 3.15a1, 3.16a1, 5.01a and 5.02a

   GNU/Linux x86, GNAT 3.16a1 and 5.02a

   SPARC Solaris 8, GNAT 5.01a

   HP-UX 11, GNAT 5.03w

Others platforms / compiler version combinations have not been tested, it
does not mean that it's not working.

Previous version of AWS have been built on FreeBSD 4.1 and MacOSX.


Known problems:
---------------

- There is a bug in Internet Explorer which prevents to download a file when
  the connection is using the SSL encryption and there is the "Cache-Control"
  header set to "no-cache".

  See: http://support.microsoft.com/?kbid=323308


Pointers:
---------

AWS User's Mailing List:
   http://lists.act-europe.fr/mailman/listinfo/aws

AWS Home Page (sources and printable documentations in Postscript and PDF):
   http://libre.act-europe.fr/aws

Templates_Parser sources:
   Templates_Parser module (sources and documentation) is provided with AWS
   distribution. Version 8.0 is distributed with AWS 2.1.

   Latest version of this module and the documentation can be found at:

   http://www.obry.org/contrib.html
   http://www.obry.org/templates_parser.html

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

   You need at least version 3.15 to build and use AWS 2.1.

   http://libre.act-europe.fr/GNAT/

Socket binding (Optional) :

   Since AWS 2.1 you need at least version 1.8.4.4 of the Socket binding. Note
   that by default AWS uses GNAT.Sockets. To be able to use AdaSockets with
   AWS you need to copy all the .ali (cp -p) in the directory where the
   libadasockets.a is. This is needed for the GNAT Project support.

   http://www.rfc1149.net/devel/adasockets

XML/Ada (optional):

   You need this library only if you want to use AWS SOAP feature. You need
   at least XML/Ada 1.1.

   http://libre.act-europe.fr/

   Note that it should be possible to use XML/Ada 1.0 but in this case you'll
   have to create the set of project files yourself.

POSIX Binding (optional) :

   for Win32:
      http://www.obry.org/contrib.html

   for UNIX:
      http://www.cs.fsu.edu/~baker/florist.html

OpenSSL library (optional) :

   Sources for UNIX or Win32:
      http://www.openssl.org

   binaries for Win32:
      Included with the main AWS distribution (win32 directory).

   Note that we have used and we distribute (for Win32 platform) OpenSSL
   version 0.9.7d with this AWS release. OpenSSL has been built with
   Mingw GCC version 3.4.1 with -O3 optimization level.

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

License:
--------

AWS is distributed under the GMGPL (GNAT Modified GPL) license. This license
ensures that commercial applications can be built using AWS. Note that
AWS comes with a set of components. Those components are using a license
compatible with the AWS's one. For information about component's individual
licenses see include/readme.txt.


Reporting bugs:
---------------

You can report bugs to:

   Dmitriy Anisimkov	anisimkov@yahoo.com
   Pascal Obry		pascal@obry.org

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
Audran Le Baron, Thierry Lelegard, Nicolas Lesbats, Jean-François Rameau,
Maxim Reznik, Jean-Pierre Rosen, Jerôme Roussel, Ariane Sinibardy,
Henrik Sundberg.


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
