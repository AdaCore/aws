
			    A W S - Ada Web Server
			    2.3 release / SOAP 1.4

Authors:
   Dmitriy Anisimkov
   Pascal Obry                                              January 10th, 2007



We are happy to announce the availability of the AWS 2.3 release. The API
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
This on-line service is not available anymore but the current implementation
is validated against the Apache/AXIS SOAP implementation to ensure the
interoperability. Some users have also reported to have used AWS/SOAP with
.NET and gSOAP without trouble.


Changes
-------

Here are the main changes since AWS 2.2 :

   - Support for user defined filters in the template engine.

   - Support for extended log file format.

   - Start using Ada 2005. The main change is the switch from the temporary
     AI302 containers implementation to the standard Ada.Containers.

   - A new too templates2ada can be used to parse a template file and
     generated Ada packages containing the name of the tag and Ajax
     actions. Using this tool it is easier to keep the templates files
     and the Ada code synchronized.

   - Fix bug in handling of Templates_Parser.Null_Set

   - Fix problem in AWS/Ajax where the apply_style was raising an
     exception when the node id was not present in the DOM tree.

   - Fix a gSOAP interoperability problem. AWS was not handling the
     SOAP href properly as it expected the referenced node to have
     a specific name. Now only the node id attribute is checked.

   - Support for storing complex data types in sessions, in particular
     Ada2005 containers, through AWS.Session.Generic_Data

   - Fix URL handling security hole. With some obscure combinations
     of .. and back slash it was possible to access files above Web root.

   - Plus many small fixes, enhancements, API comments, and documentation work.


Non upward compatible changes
-----------------------------

Note that the changes listed below can introduce non upward compatibility.
In such a case we try to give proper advice on how to change the code
to work properly. Of course we try to avoid this as much as possible
but we really prefer to have a clean API instead of keeping awkward
implementations.

   * Slight Server Push non upward compatibility and improved performance.

     Stream_Output_Type generic parameter removed.

     To_Stream_Output function generic parameter renamed to To_Stream_Array and
     is now returning Ada.Streams.Stream_Element_Array instead of a
     Stream_Output_Type.

     Close_Duplicate registration Boolean parameter replaced by
     Duplicated_Age Duration parameter. It is the age of client connection
     during the time it cannot be replaced by the new arrived client
     with the same client id.

   * AWS/Ajax URL where named based on the id of the DOM element on
     which the event applied. With this scheme it was not easy to
     support multiple event (onclick, onfocus, onchange) on the same
     widget. For supporting this the URL are now : /<event>$<id>
     For example "/onclick$form_enter".


Obsolescent features
--------------------

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


Validation
----------

AWS 2.3 has been compiled and has passed all tests on:

   Windows XP, GNAT 6.0.1

   GNU/Linux x86, GNAT 6.0.1

Others platforms / compiler version combinations have not been tested, it
does not mean that it's not working.

Previous version of AWS have been built on FreeBSD 4.1 and MacOSX.


Known problems
--------------

- There is a bug in Internet Explorer which prevents to download a file when
  the connection is using the SSL encryption and there is the "Cache-Control"
  header set to "no-cache".

  See: http://support.microsoft.com/?kbid=323308


Pointers
--------

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

   You need at least GNAT 2005 GPL Edition or GNAT Pro 5.03.

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
   version 0.9.8b with this AWS release. OpenSSL has been built with
   GNAT pro 5.04a with -O3 optimization level.

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


License
-------

AWS is distributed under a dual licencing. The AWS/GPL to be used with GNAT
GPL Edition and AWS/GMGPL.

AWS/GMGPL (GNAT Modified GPL) ensures that commercial applications can
be built using AWS. Note that AWS comes with a set of
components. Those components are using a license compatible with the
AWS's one. For information about component's individual licenses see
include/readme.txt.


Reporting bugs
--------------

You can report bugs to AdaCore: report@adacore.com


AWS User's Mailing List
-----------------------

A good way to keep informed of AWS news and to share experiences with other
AWS users is to register to the AWS dedicated mailing list. See:

   http://lists.act-europe.fr/mailman/listinfo/aws


Contributors
------------

Thanks to the contributors and peoples who send feedbacks, ideas
about AWS. In the early stage of the project this is very valuable.

So thanks goes to Ted Dennison, Wiljan Derks, Sune Falck, David C. Hoos,
Audran Le Baron, Thierry Lelegard, Nicolas Lesbats, Olivier Ramonat,
Jean-Fran�ois Rameau, Maxim Reznik, Jean-Pierre Rosen, Jerme Roussel,
Ariane Sinibardy, Henrik Sundberg.


Thanks to all who have reported bugs and have sent us patches.
