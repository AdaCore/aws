.. _Building_AWS:

************
Building AWS
************

.. _Requirements:

Requirements
============

`AWS` has been mainly developed with `GNAT` on Windows.
It is built and tested regularly on `GNU/Linux` and
`Solaris`, it should be fairly portable across platforms. To
build `AWS` you need:

* GNU/Ada (GNAT compiler) ;

  .. index:: GNAT
  .. index:: GNU/Ada

  Obviously an Ada compiler is mandatory. Only GNAT is tested, the
  code should be fairly portable but has never been tested on another
  compiler. See INSTALL file distributed with AWS for specific
  versions supported.

* OpenSSL (**optional**) ;

  .. index:: OpenSSL

  OpenSSL is an Open Source toolkit implementing the *Secure Sockets Layer*
  (SSL v2 and v3 and TLS 1.1, 1.2) and much more. It is possible to
  download the OpenSSL source distribution from
  `http://www.openssl.org <http://www.openssl.org>` and  build it. A
  Windows binary distribution may also be downloaded there.

* LibreSSL (**optional**) ;

  .. index:: LibreSSL

  LibreSSL is an Open Source toolkit implementing the *Secure Sockets Layer*
  which is fully compatible with OpenSSL. It is possible to
  download the LibreSSL source distribution from
  `https://www.libressl.org/>` and  build it.

* GNUTLS (**optional**) ;

  .. index:: GNUTLS

  GNUTLS is an Open Source toolkit implementing the *Secure Sockets Layer*
  (SSL v3 and TLS 1.1, 1.2) and much more. It is necessary to install
  the developers libraries to use it in AWS.

* OpenLDAP (**optional**) ;

  .. index:: OpenLDAP

  OpenLDAP is an Open Source toolkit implementing the
  *Lightweight Directory Access Protocol*.
  If you want to use the `AWS/LDAP` API
  on UNIX based systems, you need to install properly the `OpenLDAP`
  package. On Windows you don't need to install it as the
  :file:`libldap.a` library will be built by `AWS` and will use the
  standard Windows `LDAP DLL` :file:`wldap32.dll`.

  You can download OpenLDAP from
  `http://www.openldap.org <http://www.openldap.org>`.

.. _AWS.Net.Std:

AWS.Net.Std
===========

.. index:: AWS.Net.Std

This package is the standard (non-SSL) socket implementation. It exists
different implementations of this package:

*GNAT*
  Version based on `GNAT.Sockets` from GNAT version 20 and later with IPv6
  support. This is the **default implementation** used.

*IPv6*
  Compartible with GNAT before version 20 socket implementation with IPv6
  support::

    $ make setup NETLIB=ipv6

*IPv4*
  Compartible with GNAT before version 20 socket implementation based on
  GNAT.Sockets package without IPv6 support::

    $ make setup NETLIB=ipv4

.. _Building:

Building
========

.. index:: Building

Before building be sure to edit :file:`makefile.conf`, this file
contains many settings important for the build. Note that it is
important to run `make setup` each time you edit this file.

When you have built and configured all external libraries you must set the
`ADA_PROJECT_PATH` variable to point to the GNAT Project files for
the different packages. For `XML/Ada` support, you also need to set
`XMLADA` to `true` in :file:`makefile.conf`.

At this point you can build `AWS` with::

  $ make setup build

Note that some demos require that `AWS` be built with `SSL`
support. If you want to activate `SSL` you must have installed the
necessary developers libraries. It is possible to specify the `SSL`
implementation to use with the `SOCKET` variable.

To build with `GNUTLS`::

  $ make SOCKET=gnutls setup
  $ make build

.. index:: GNUTLS build

To build with `OpenSSL` or `LibreSSL`::

  $ make SOCKET=openssl setup
  $ make build

.. index:: OpenSSL build
.. index:: LibreSSL build

It is is possible to build `AWS` in debug mode by setting
`DEBUG` make's variable::

  $ make DEBUG=true setup build

Note that by default `AWS` is configured to use the `GNAT`
compiler. So, if you use `GNAT` you can build `AWS` just with::

  $ make setup build

.. _Building_on_cross-platforms:

Building on cross-platforms
===========================

.. index:: Building
.. index:: cross-platforms

To build for a cross platform the TARGET makefile variable must be set
with the cross toolchain to be used. The value must be the triplet of
the toolchain to use.

For example, to build on VxWorks::

  $ make TARGET=powerpc-wrs-vxworks setup build

Note that on cross-environment one need to build the demos
manually. See demos/README.

.. _Demos:

Demos
=====

`AWS` comes with some ready to use demos. The demos are a good
way to learn how to use `AWS`.

Here are a short description of them:

*agent*
  A program using the `AWS` client interface. This simple tool can be used
  to retrieve Web page content. It supports passing through a proxy with
  authentication and basic authentication on the Web site.

*auth*
  A simple program to test the Web Basic and Digest authentication feature.

*autobahn*
  A demo to validate the WebSocket implementation against the autobahn
  test suite.

*cert*
  A secure server using a Certificate Authority and validating clients
  with certificate. This is the highest security level possible.

*com*
  Two simples program that uses the `AWS` communication service.

*dispatch*
  A simple demo using the dispatcher facility. see :ref:`URI_dispatcher`.

*hello_world*
  The famous Hello World program. This is a server that will always
  return a Web page saying 'Hello World!'.

*hello_wsdl*
  An hello world kind of application using a WSDL document for
  describing the messages format.

*hotplug*
  A simple test for the hotplug feature.

*https*
  A simple secure server enforcing TLS 1.2 protocol to be used by
  the Web Browser. This demo also uses a signed server's key and
  proper setup hand over the password to the secure layer.

*interoplab*
  A WSDL based demo that test most of the `SOAP` features.

*jabber_demo*
  A simple Jabber command line client to check the presence of a JID
  (Jabber ID). This uses the Jabber API, see :ref:`AWS.Jabber`.

*multiple_sessions*
  A demo of two embedded servers using different sessions.

*res_demo*
  A demo using the resource feature. This Web Server embedded a `PNG`
  image and an `HTML` page. The executable is self contained.

*runme*
  An example that test many `AWS` features.

*soap_demo*
  A simple client/server program to test the `SOAP` protocol.

*soap_disp*
  Like above but use a `SOAP` dispatcher.

*soap_vs*
  A client and server that implement seven `SOAP` procedures for
  testing purpose.

*split*
  A demo for the transient pages and page splitter `AWS`'s
  feature. Here a very big table is split on multiple pages. A set of
  links can be used to navigate to the next or previous page or to
  access directly to a given page.

*test_ldap*
  A simple `LDAP` demo which access a public `LDAP` server and
  display some information.

*test_mail*
  A simple application that send a set of `SMTP` messages with
  different kind of attachments.

*text_input*
  A simple demo which handle textarea and display the content.

*vh_demo*
  Two servers on the same machine... virtual hosting demo.
  see :ref:`Virtual_host_dispatcher`.

*web_block*
  A simple Web Bock based counter.

*web_block_ajax*
  As above but using also `Ajax`.

*web_block_ajax_templates*
  As above but using also the `templates2ada` tool which create a
  tight coupling between the web templates and the `Ada` code.

*web_elements*
  A driver to browse the Web Elements library and see some examples.

*web_mail*
  A simple Web Mail implementation that works on a `POP` mailbox.

*websockets*
  A simple WebSocket demo.

*wps*
  A very simple static Web page server based on `AWS.Services.Page_Server`.
  see :ref:`Static_Page_server`.

*ws*
  A static Web page server and push enabled server.

*ws_candy*
  A WebSocket demo using many of the WebSocket's features.

*zdemo*
  A simple demo of the Gzip content encoding feature.

For build instructions see :file:`demos/README`.

.. _Installing:

Installing
==========

.. index:: Installing

When the build is done you must install `AWS` at a specific
location. The target directory is defined with the `prefix`
:file:`makefile.conf` variable. The default value is set to the
compiler root directory. Note that the previously installed version is
automatically removed before installing the new one. To install::

  $ make install

To install `AWS` into another directory you can either edit
:file:`makefile.conf` and set `prefix` to the directory you like
to install `AWS` or just force the make `prefix` variable::

  $ make prefix=/opt install

Alternatively, with `GNAT` 5.03 and above it is possible to
install `AWS` into the GNAT Standard Library location. In this
case `AWS` is ready-to-use as there is no need to set
`ADA_PROJECT_PATH`, just set `prefix` to point to `GNAT` root
directory::

  $ make prefix=/opt/gnatpro/6.1.1 install

Now you are ready to use `AWS` !
