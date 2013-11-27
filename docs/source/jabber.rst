.. _Jabber:

******
Jabber
******

.. index:: Jabber

`AWS` support part of the Jabber protocol. At this stage only two kind
of messages are supported:

* Presence

  To check the presence status of a specific JID (Jabber ID)

* Message

  To send messages to a specific JID (Jabber ID)

Note that if you want an application to check the presence or send
message to users it is recommended to create a specific Jabber ID on the
server for this application and ask users to accept this specific user
to check their presence status.

.. _Jabber_presence:

Jabber presence
===============

.. index:: Jabber presence
.. highlight:: ada

To check for the presence of another JID you must first have the right
to do so. The jabber server won't let you see presence of another JID
unless the JID have permitted you to see its presence.

* First declare the server and status objects::

    Server : AWS.Jabber.Server;
    Status : AWS.Jabber.Presence_Status;

* Connect to the server, you must have an account created and must
  know the login and password::

    AWS.Jabber.Connect
      (Server, "jabber.domain.org", "joe", "mysuperpwd");

* Then, to check the presence of user "john"::

    AWS.Jabber.Check_Presence
      (Server, "john@jabber.domain.org", Status);

* Then, you just have to close the server::

    AWS.Jabber.Close (Server);

.. _Jabber_message:

Jabber message
==============

.. index:: Jabber message
.. highlight:: ada

To send a message to a specific JID, you must connect to
the server as above and close the server when you don't need to
communicate with it anymore. The only different part is to send the
message, here is an example::

 Send_Message
   (Server,
    JID     => "john@jabber.domain.org",
    Subject => "Hello there!",
    Content => "Are you using AWS ?");
