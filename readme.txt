
                            A W S - Ada Web Server
                                24.0 release

Authors:
   Dmitriy Anisimkov
   Pascal Obry


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


Non upward compatible changes
-----------------------------

Note that the changes listed below can introduce non upward compatibility.
In such a case we try to give proper advice on how to change the code
to work properly. Of course we try to avoid this as much as possible
but we really prefer to have a clean API instead of keeping awkward
implementations.

- The SSL initialization API has been reworked to have support for
  certificate and host verification.

  By default AWS is now checking for certificate and host.
  The parameter Certificate_Required has been renamed
  Check_Certificate for clariry on the AWS.Net.SSL.Intialize call.

  A new parameter Check_Host has been introduced to enable host
  verification in the client API.

  Setting both Check_Certificate and Check_Host to False makes AWS
  behave as in previous version (but this is not recommanded).

- To support SMTP secure STARTTLS switching protocol the parameter to
  SMTP.Initialize has been changed from Secure to Security and
  instead of a boolean it takes a Secure_Connection enumeration which
  can be No, TLS or STARTTLS.

  If Secure was set to False, Security must be No.

  If Secure was set to True, Security should probably be TLS except if
  the STARTTLS swicthing protocol is to be used.

Obsolescent features
--------------------

In each new version we try to be upward compatible with previous
version. This is really important, but in some cases it seems that a
"redesign" of the API would be good in the long term. All obsolescent
features will be listed in this section. Those features will be
removed in the next version. Note that you can check usage of those
features in your application by using the GNAT's -gnatwj option as we
have tagged all obsolescent features with a pragma.


Pointers
--------

AWS Home Page (sources and printable documentations in Postscript and PDF):
   https://www.adacore.com/gnatpro/toolsuite/ada-web-server
   https://github.com/AdaCore/aws

Templates_Parser sources:
   https://github.com/AdaCore/templates-parser

   Templates_Parser module (sources and documentation) is provided as
   a separate download and must be unpacked into the AWS root's
   directory.

   If AWS is used from GitHub, Templates_Parser will be downloaded
   automatically as a sub-module.

GNU/Ada - GNAT
   https://www.adacore.com/gnatpro

   Or the GNAT compiler coming with your GNU/Linux distribution.

XML/Ada (optional):
   https://github.com/AdaCore/xmlada

   You need this library only for the AWS SOAP support.

Libadalang/Ada (optional):
   https://www.adacore.com/libadalang
   https://github.com/AdaCore/libadalang

   You need this library only for the AWS WSDL support.

OpenSSL (optional):

   You need to install manually the development libraries (>= 1.1.0).

LibreSSL (optional):

   You need to install manually the development libraries (>= 2.8.0).
   LibreSSL is an implementation fully compatible with OpenSSL, you
   just have to configure AWS as with OpenSSL.

GNUTLS (optional):

   You need to install manually the development libraries. AWS needs at
   least version 3.2.4 of GNUTLS.

OpenLDAP library (optional) :

   Sources for UNIX or Win32:
      http://www.openldap.org/

   Win32:
      AWS binding will use to the Microsoft LDAP dynamic library.

Windows Services API (optional):

   To build the runme demo as a Windows NT/2000 services you must download
   the services API made by Ted Dennison for his SETI@Home project.

      http://www.telepath.com/~dennison/Ted/SETI/SETI_Service.html


Reporting bugs
--------------

You can report bugs to AdaCore: report@adacore.com


Contributors
------------

Thanks to the contributors and peoples who send feedbacks, ideas
about AWS. In the early stage of the project this is very valuable.

So thanks goes to Georg Bauhaus, Ted Dennison, Wiljan Derks, Sune Falck,
David C. Hoos, Audran Le Baron, Thierry Lelegard, Nicolas Lesbats,
Olivier Ramonat, Jean-François Rameau, Maxim Reznik, Jean-Pierre Rosen,
Jerme Roussel, Ariane Sinibardy, Henrik Sundberg.


Thanks to all who have reported bugs and have sent us patches.
