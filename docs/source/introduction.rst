.. _Introduction:

************
Introduction
************

`AWS` stands for *Ada Web Server*. It is an Ada implementation of the
`HTTP/1.1` and `HTTP/2` protocols as defined in the RFC-2616 from June
1999 and RFC-7640 from May 2015 respectivelly.

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
`SSL`. This is based on either `OpenSSL`, `LibreSSL` or `GNUTLS` two
Open Source SSL implementations.

Major supported features are:

* HTTP/1.1 and HTTP/2 (aka h2c) implementation

* HTTPS/1.1 and HTTPS/2 (aka h2) (Secure HTTP) implementation based on SSLv3

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

.. _HTTP/2:

HTTP/2
======

The `HTTP/2` protocol has been designed with speed and security in
mind. It is a binary protocol making the exchanged frames less verbose
and has support for header compression to even more reduces the
payload size. The header compression format is called HPACK (described in
RFC-7541 from May 2015) and permits to efficiently represent HTTP
header fields by using an Huffman encoding specifically designed for
HTTP header's information.

The `HTTP/2` protocol has some specific configuration options.
See :ref:`Configuration_options`. All of them are starting with the
`HTTP2` prefix.

Finally the `HTTP/2` protocol is enabled by default and can be
disabled by setting the option `HTTP2_Activated` to `false`.

Note that disabling the `HTTP/2` protocol will not make the server
unusable, just that during the handshake with the client it won't be
selected as not advertised as supported. In this case, AWS server will
continue working using the `HTTP/1.1` protocol.

AWS also provides client side supports for the `HTTP/2` protocol. A
parameter in the client API can be used to request the `HTTP/1.1`
or the `HTTP/2` protocol to be used or to let the client and server
decide about the protocol to be used during the handshake.
