
$Id$

This directory contains a set of tests for AWS. It includes test for AWS core,
basic Web Server services and SOAP support. All tests must be passed at least
on Windows and GNU/Linux before a release. Tests must also be checked before
every check-in to detect as soon as possible regressions.

To run the test suite (from the parent directory):

  $ make run_regtests

Also before a release the SOAP server implementation must be checked against a
Java/AXIS client. For this there is a wsdl_6 Ada and Java implementation.
The Ada wsdl_6 is the server part, and the Java wsdl_6.java client is used to
check the server.

The first step is to have a working Java/AXIS environment. The follow the
procedure found in wsdl_6.java header comments.
