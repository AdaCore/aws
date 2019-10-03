.. _Introduction:

************
Introduction
************

`AWS` stands for *Ada Web Server*. It is an Ada implementation of the
`HTTP/1.1` protocol as defined in the RFC-2616 from June 1999.

The goal is not to build a full Web server but more to make it
possible to use a Web browser (like Firefox or Chrome) to control an
Ada application. As we'll see later it is also possible to have two
Ada programs exchange informations via the `HTTP` protocol. This is
possible as `AWS` also implements the client side of the HTTP
protocol.

Moreover with this library it is possible to have more than one server
in a single application. It is then possible to export different kind
of services by using different `HTTP` ports, or to have different ports
for different services priority. Client which must be served with a
very high priority can be assigned a specific port for example.

As designed, `AWS` big difference with a standard `CGI` server
is that there is only one executable. A `CGI` server has one
executable for each request or so, this becomes a pain to build and
to distribute when the project gets bigger. We will also see that it is
easier with `AWS` to deal with session data.

`AWS` support also `HTTPS` (secure `HTTP`) using
`SSL`. This is based on either `OpenSSL` or `GNUTLS` two Open Source SSL
implementations.

Major supported features are:

* HTTP implementation

* HTTPS (Secure HTTP) implementation based on SSLv3

* Template Web pages (separate the code and the design)

* Web Services - SOAP based

* WSDL support (generate stub/skeleton from WSDL documents)

* Basic and Digest authentication

* Transparent session handling (server side)

* HTTP state management (client side cookies)

* File upload

* Server push

* SMTP / POP (client API)

* LDAP (client API)

* Embedded resources (full self dependant Web server)

* Complete client API, including HTTPS

* Web server activity log
