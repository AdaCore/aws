.. _Working_with_mails:

******************
Working with mails
******************

.. _Sending_e-mail:

.. highlight:: ada

Sending e-mail
==============

.. index:: Sending e-mail
.. index:: SMTP
.. index:: Simple Mail Transfer Protocol

`AWS` provides a complete API to send e-mail using `SMTP`
protocol. You need to have access to an SMTP server to use this
feature. The API covers sending simple mail with text message and/or
with `MIME` attachments (base64 encoded). Here are the steps to
send a simple e-mail:

* Initialize the SMTP server

  ::

   SMTP_Server : SMTP.Receiver :=
                   SMTP.Client.Initialize ("smtp.hostname");

  Here `AWS` uses the default SMTP port to create an SMTP mail server but
  it is possible to specify a different one. The hostname specified
  must be a valid SMTP server.

* Send the e-mail

  To send an e-mail there is many different API. Let's send a simple text mail::

   Status : SMTP.Status;

   SMTP.Client.Send
     (SMTP_Server,
      From    => SMTP.E_Mail ("Pascal Obry", "p.obry@wanadoo.fr"),
      To      => SMTP.E_Mail ("John Doe", "john.doe@here.com"),
      Subject => "About AWS SMTP protocol",
      Message => "AWS can now send mails",
      Status  => Status);

  Here Status will contain the SMTP returned status.

* Check that everything is ok

  Using above status data it is possible to check that the message was
  sent or not by the server. The status contain a code and an error
  message, both of them can be retrieved using specific routines,
  see :ref:`AWS.SMTP`. It is also possible to check that the call was
  successful with `SMTP.Is_Ok` routine::

   if not SMTP.Is_Ok (Status) then
      Put_Line ("Can't send message: " & SMTP.Status_Message (Status));
   end if;

In the above example, the message content was given as a string but it
is possible to specify a disk file. `AWS` can also send MIME messages
either from disk files or with in memory base64 encoded binary
data. The API provides also a way to send messages to multiple
recipients at the same time and to send messages with alternative
contents (text and HTML for example). These features are not described here,
complete documentation can be found on the spec see :ref:`AWS.SMTP` and
:ref:`AWS.SMTP.Client`.

.. _Retrieving_e-mail:

Retrieving e-mail
=================

.. index:: Retrieving e-mail
.. index:: POP
.. index:: Post Office Protocol

`AWS` provides an API to retrieve e-mails from a `POP`
mailbox. `POP` stands for *Post Office Protocol* and is the main
protocol used by Internet Service Providers around the
world. `IMAP` is another well known protocol in this area but it
is not supported by `AWS`.

We describes here the `POP` API. For a complete description see
:ref:`AWS.POP`.

* Opening the mailbox

  The first step is to authenticate using a user name and
  password. `AWS` supports two methods one called `Clear_Text`
  which is the most used and another one `APOP` which is more secure but
  almost not supported by `ISP` for the moment (and will probably
  never be supported as a more secure protocol named `SPA` -Secure
  Password Authentication- could be used instead)::

   Mailbox : POP.Mailbox :=
               POP.Initialize ("pop.hostname", "john.does", "mysuperpwd");

  The default Authentication method is `Clear_Text`.

* Getting mailbox information

  When the connection is opened it is possible to get information about
  the mailbox like the number of messages or the total number of bytes
  in the mailbox::

   N     : constant Natural := POP.Message_Count (Mailbox);

   Bytes : constant Natural := POP.Size (Mailbox);

* Retreiving individual e-mail

  Each message is numbered starting from 1. A function named `Get`
  will return a message given its mailbox's number::

   Message : constant POP.Message := POP.Get (Mailbox, 2, Remove => True);

  Remove can be set to `False` for the message to stay on the
  mailbox. The default value is `False`.

* Iterating through the mailbox content

  Another way to retreive message is by using an iterator::

   procedure Print_Subject
     (Message : in     POP.Message
      Index   : in     Positive;
      Quit    : in out Boolean) is
   begin
    Text_IO.Put_Line (POP.Subject (Message));
   end Print_Message;

   procedure Print_All_Subjects is new POP.For_Every_Message (Print_Subject);

   ...

   Print_All_Subjects (Mailbox, Remove => True);

  It exists a set of routines on a `POP.Message` object to get the subject
  the content, the date or any headers. It is also possible to work with
  attachments. See point below.

* Working with attachments

  A message can have a set of `MIME` attachments. The number of
  attachments can be retrieved using `Attachment_Count`::

   Message : constant POP.Message := ...;

   A_Count : constant Natural := POP.Attachment_Count (Message);

  As for messages it is possible to get a single attachment using its
  index in the message or by using an iterator::

   First_Attachment : constant POP.Attachment := POP.Get (Message, 1);

   procedure Write_Attachment
     (Attachment : in     POP.Attachment
      Index      : in     Positive;
      Quit       : in out Boolean) is
   begin
      POP.Write (Attachment, Directory => ".");
   end Print_Message;

   procedure Write_All_Attachments is
     new POP.For_Every_Attachment (Write_Attachment);

   ...

   Write_All_Attachments (Message);

  It is also possible to retrieve the attachment's filename, the content
  as a memory stream. See :ref:`AWS.POP`.

* Closing the connection

  ::

   POP.Close (POP_Server);
