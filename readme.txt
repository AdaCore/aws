
			    A W S - Ada Web Server
			    2.0 release / SOAP 1.2
				       
Authors:
   Dmitriy Anisimkov
   Pascal Obry                                             December 20th, 2003



We are happy to announce the availability of the AWS 2.0 release. The API
could change slightly at this stage but should be fairly stable now.

AWS stand for Ada Web Server. It is a small yet powerful HTTP component to
embed in any applications. It means that you can communicate with your
application using a standard Web browser and this without the need for a Web
Server. AWS is fully developed in Ada with GNAT.

AWS support SOAP/WSDL, Server Push, HTTPS/SSL, client HTTP, hotplug
modules... We have worked very hard to make this release as stable as
possible. Note that Hotplug modules are very nice but have a potentially
security hole as it is implemented today. A new secure implementation will be
proposed in a future version.

AWS comes with SOAP/WSDL support, two tools are proposed:

   ada2wsdl   which generates a WSDL document from an Ada spec

   wsdl2aws   which generates stubs/skeletons AWS code from a WSDL document

Both tools have mapping for standard Ada types but also supports Ada
enumerations, character, records and arrays.

The SOAP implementation has been validated on http://validator.soapware.org/.


Changes
-------

Here are the main changes since AWS 1.4 :

   - ada2wsdl a new tool to generate WSDL document from an Ada spec. This
     tool will help further to build Web Services with GNAT. ada2wsdl is
     based on ASIS for GNAT. You must have GNAT and ASIS installed. ada2wsdl
     handles standard Ada types, Calendar.Time, records, arrays
     (constrained and unconstrained), derived types.

   - Add -types wsdl2aws's option to generate code compatible with an already
     present Ada spec. This is to be used with ada2wsdl tool. With -types
     wsdl2aws will not generate records or arrays, instead it will use them
     from the specified Ada spec.

   - Add -cb wsdl2aws's option to generate the SOAP callback procedure for the
     server.

   - Add -main wsdl2aws's option to generate a main procedure to build the
     server. This main procedure can be generated from a template file.

     Note that using wsdl2aws's -types, -main and -cb options together you
     can build a SOAP server for any Ada API. This is one of the simplest way
     to build a SOAP server.

   - Some improvement in the generated AWS/SOAP code from WSDL
     document. In the generated stub uses properly the
     Types.To_SOAP_Object routine.

   - Add support for enumerations in Ada and WSDL. wsdl2aws can now generate
     enumeration type. This makes ada2wsdl and wsdl2aws even more compatible.

   - wsdl2aws can now generate the SOAP callback routine for all routines
     parsed in the WSDL document. This feature is one more step into
     transparent SOAP support from Ada.

   - Better support for Notification and Request-response WSDL operations.

   - Many changes in the way SOAP and WSDL documents are generated for better
     interoperability (tested with Tomcat/Axis with some complex data
     structures). See for example wsdl_6 non-regression test (wsdl_6.java
     for the Java Axis part).

   - Add support for transient pages. These are stream objects that
     are not released by the server. Transient pages are released by a
     cleaner task after a certain amount of time. To enable transient
     pages it is required to use the transient pages dispatcher (see
     AWS.Services.Transient_Pages and
     AWS.Services.Dispatchers.Transient_Pages).

   - Add a page splitter. This can be used to split a response in
     multiple pages avoiding very large Web pages. Specific tags to
     navigate (PREVIOUS, NEXT, CURRENT_PAGE...) are generated and can
     be used in the template page. A set of vector tags are also
     available to setup a page index. (See AWS.Services.Split_Pages).

   - A new Response.File option "Once" can be used to remove a file
     after sending it. This is useful if a temporary file is prepared
     for the response as it will be removed automatically.

   - A new kind of stream (AWS.Resources.Stream.Disk) has been
     implemented. The AWS.Resources.File API is based on it.
 
   - Fix WSDL SOAP record handling (code for record in record was
     sometimes not correctly generated).

   - New package AWS.POP which implements the POP client protocol.

   - New template engine filters:

       REPLACE       Pattern matching with replacement facility.
       REPLACE_ALL   As above but replace all occurences not the first one.
       FORMAT_DATE   To format a date using GNU/Date pattern format.
       ADD_PARAM     Add a parameter to an URL.
       DEL_PARAM     Delete a parameter from an URL.

   - New template engine NOW variable tag.

   - Templates engine should be a bit faster as it uses less recursivity.

   - New version of the "&" operator to prepend data into the
     templates engine's vector tags.

   - New awsres options -u/-z to add uncompressed or compressed items into the
     resources. Compressed resources are decompressed on-the-fly if needed
     (this is completely transparent to users). It is needed for the
     templates engine for example (as it needs to parse the file) or if
     the client does not support compressed resources.

   - New high level service to handle transparently compressed or uncompressed
     files. This callback routine will either returns the compressed version
     of the file if it exists and the client support compressed encoding, the
     uncompressed file if it exists otherwise it will return a decompressed
     version of the file using a decompression stream.

   - New dispatcher based on time. It is possible to select specific callback
     routines depending on time. A time dispatcher is activated at a certain
     period in time, there is a Once period (this is unique in time) and
     Yearly, Monthly, Daily, Weekly, Hourly and Minutely periods to build
     events that repeats periodically.

   - ZLib library updated to version 1.2.1.

   - Add client and server certificate support.

   - The security options are now per server, it means that HTTPS servers
     running on the same AWS process can have different settings.

   - A a simple Web Mail service (AWS.Services.Web_Mail) using the SMTP/POP
     AWS support.

   - Plus many small fixes, enhancements, API comments, and documentation work.


Non upward compatible changes
-----------------------------

Note that the changes listed below can introduce non upward compatibility.
In such a case we try to give proper advice on how to change the code
to work properly. Of course we try to avoid this as much as possible
but we really prefer to have a clean API instead of keeping awkward
implementations.

   - Array types generated with wsdl2aws will now have a _Type suffix. This
     is needed to ensure different names for parameter and type. Some WSDL
     document use different case for parameter and type, this is not possible
     with Ada which is not case-sensitive.

     => Just add "_Type" suffix to array types and constructors references.

   - A new abstract routine named Reset has been added to the Stream
     interface.

     => Add a Reset routine in tagged types derived from
     AWS.Resources.Streams.Stream_Type.

   - AWS.Server.Set_Security is now a per server procedure. Add the Web Server
     as parameter.


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

AWS 2.0 has been compiled and has passed all tests on:

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

AWS Home Page (sources and printable documentations in Postscript and PDF):
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

   You need at least version 3.15 to build and use AWS 2.0.

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
