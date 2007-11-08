
                            A W S - Ada Web Server
			   2.4.0 release / SOAP 1.5

Authors:
   Dmitriy Anisimkov
   Pascal Obry                                              November 8th, 2007



We are happy to announce the availability of the AWS 2.4.0 release. The API
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

New features are described into the file docs/features.

Listed below are some minor enhancements and fixes:

   - Default INSTALL location is set to the GNAT compiler found in the PATH.
     This means that by default there is no need to change INSTALL in
     makefile.conf.

   - Defaults for SOCKET and LDAP are identical on UNIX and Windows.
     Both are disabled by default now.

   - Unregistering a non registered dispatcher now raises an exception
     instead of returning silently.

   - Do not pass the uploaded files client's full pathname for
     security reasons.

   - Improve templates parser user's filters interface. A user defined
     filter has now access to the lazy tags. See non upward compatible
     changes.

   - Add support for extended HTTP protocols.

   - Fix xsd:float and xsd:double support to match the W3C recommendations.

   - The server side URL object (Status.URI returning and URL.Object)
     properly store the web parameters.

   - Drop AdaSockets support. AdaSocket is now mostly based on
     GNAT.Sockets and does not define the thin layer which was used on
     AWS.

   - Plus many small fixes, enhancements, API comments, and documentation work.


Non upward compatible changes
-----------------------------

Note that the changes listed below can introduce non upward compatibility.
In such a case we try to give proper advice on how to change the code
to work properly. Of course we try to avoid this as much as possible
but we really prefer to have a clean API instead of keeping awkward
implementations.

   - User's filter callback specs have been changed from:

      type Callback is access function
        (Value        : in String;
         Parameters   : in String;
         Translations : in Translate_Set) return String;

      type Callback_No_Param is access function
        (Value        : in String;
         Translations : in Translate_Set) return String;

     to:

      type Filter_Context is record
         Translations : Translate_Set;
         Lazy_Tag     : Dynamic.Lazy_Tag_Access;
      end record;

      type Callback is access function
        (Value      : in String;
         Parameters : in String;
         Context    : in Filter_Context) return String;

      type Callback_No_Param is access function
        (Value   : in String;
         Context : in Filter_Context) return String;

      Change the callback to this new spec, the translate set is
      now accessible through the Context parameter.

   - For hand written SOAP code the float and long_float are now respectively
     mapped to xsd:float and xsd:double. So the F constructor takes now
     a float as parameter and the D constructor a long_float. This may
     introduce compatibility problem. Either change the Ada or SOAP floating
     point type to match. There is no such problem for SOAP applications
     using ada2wsdl and wsdl2aws as those tools are now using the proper
     mapping.


Obsolescent features
--------------------

In each new version we try to be upward compatible with previous
version. This is really important, but in some cases it seems that a
"redesign" of the API would be good in the long term. All obsolescent
features will be listed in this section. Those features will be
removed in the next version. Note that you can check usage of those
features in your application by using the GNAT's -gnatwj option as we
have tagged all obsolescent features with a pragma.


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

AWS 2.4.0 has been compiled and has passed all tests on:

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
   distribution.

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

   You need at least GNAT 2007 GPL Edition or GNAT Pro 6.0.1.

   http://libre.act-europe.fr/GNAT/

XML/Ada (optional):

   You need this library only if you want to use AWS SOAP feature. You need
   at least XML/Ada version 2.2.0.

   http://libre.act-europe.fr/

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

So thanks goes to Georg Bauhaus, Ted Dennison, Wiljan Derks, Sune Falck,
David C. Hoos, Audran Le Baron, Thierry Lelegard, Nicolas Lesbats,
Olivier Ramonat, Jean-François Rameau, Maxim Reznik, Jean-Pierre Rosen,
Jerme Roussel, Ariane Sinibardy, Henrik Sundberg.


Thanks to all who have reported bugs and have sent us patches.

