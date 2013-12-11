.. _Using_SOAP:

**********
Using SOAP
**********

.. index:: SOAP
.. index:: Simple Object Access Protocol

`SOAP` can be used to implements Web Services. The `SOAP`
implementation uses `AWS HTTP` as the transport layer. `SOAP` is
platforms and languages independent, to ensure a good
inter-operability, `AWS/SOAP` implementation has been validated through
`http://validator.soapware.org/ <http://validator.soapware.org/>`_, the version number listed on
this server corresponds to the AWS version string
(`AWS.Version`) catenated with the `SOAP` version string
(`SOAP.Version`).

This `SOAP` implementation is certainly one with the higher level
of abstraction. No need to mess with a serializer, to know what is a
payload or be an `XML` expert. All the low level stuffs are
completely hidden as the `SOAP` type system has been binded as
much as possible to the Ada type system.

.. index:: WSDL
.. index:: Web Service Definition Language

The `SOAP` type system has been relaxed to be compatible with
`WSDL` based `SOAP` implementation. In these implementations, types
are generally (as in the Microsoft implementation) not part of the
payload and should be taken from the `WSDL` (Web Services Description
Language). `AWS/SOAP` is not `WSDL` compliant at this stage, all
such types are binded into the Ada type system as strings. It is up to
the programer to convert such strings to the desired type.

.. _SOAP_Client:

SOAP Client
===========

.. index:: SOAP Client

.. highlight:: ada

The `SOAP` client interface is quite simple. Here are the step-by-step
instructions to call a `SOAP` Web Service:

* Build the `SOAP` parameters

  As for the `SOAP` servers, the `SOAP` parameters are built using a
  `SOAP.Parameters.List` object::

   Params : constant Parameters.List := +I (10, "v1") & I (32, "v2");

* Build the `SOAP` Payload

  The Payload object is the procedure name and the associated parameters::

   declare
      Payload : Message.Payload.Object;
   begin
      Payload := Message.Payload.Build ("Add", Params);

* Call the `SOAP` Web Service

  Here we send the above Payload to the Web Server which handles the Web
  Service. Let's say that this server is named `myserver`, it is
  listening on port `8082` and the `SOAPAction` is `soapdemo`::

   Resp : constant Message.Response.Object'Class :=
            SOAP.Client.Call ("http://myserver:8082/soapdemo", Payload);

* Retrieve the result

  Let's say that the answer is sent back into the parameter named
  "myres", to get it::

   My_Res : constant Integer := SOAP.Parameters.Get (Params, "myres");

In the above example we have called a Web Service whose spec could be
described in Ada as follow::

 function Add (V1, V2 : in Integer) return Integer;
 --  Add V1 and V2 and returns the result. In SOAP the result is named "myres"

.. _SOAP_Server:

SOAP Server
===========

.. index:: SOAP Server
.. index:: SOAPAction

A `SOAP` server implementation must provides a callback procedure as for
standard Web server :ref:`Callback_procedure`. This callback must
checks for the `SOAP` Action URI to handle both standard Web requests
and `SOAP` ones. The `SOAPAction` is sent with the HTTP headers and
can be retrieved using `AWS.Status.SOAPAction`.

.. _Step_by_step_instructions:

Step by step instructions
-------------------------

Here are the step-by-step instructions to be followed in the `SOAP`
callback procedure:

* Retrieve the `SOAP` Payload

  .. index:: Payload

  The `SOAP` Payload is the `XML` message, it contains the
  procedure name to be called and the associated parameters::

   function SOAP_CB (Request : in AWS.Status.Data) return AWS.Response.Data is
      use SOAP.Types;
      use SOAP.Parameters;

      Payload : constant SOAP.Message.Payload.Object :=
                  SOAP.Message.XML.Load_Payload (AWS.Status.Payload (Request));

  `AWS.Status.Payload` returns the `XML` Payload as sent by
  the `SOAP` Client. This `XML` Payload is then parsed using
  `SOAP.Message.XML.Load_Payload` which returns a
  `SOAP.Message.Payload.Object` object.

* Retrieve the `SOAP` Parameters

  The `SOAP` procedure's parameters::

   Params : constant SOAP.Parameters.List :=
              SOAP.Message.Parameters (Payload);

  `SOAP.Parameters.List` is a structure which holds the `SOAP`
  parameters. Each parameter can be retrieved using a
  `SOAP.Parameters` API, :ref:`SOAP.Parameters`. For example to
  get the parameter named `myStruc` which is a `SOAP` struct::

   My_Struct : constant SOAP_Record :=
                 SOAP.Parameters.Get (Params, "myStruct");

  Another example, to get the parameter named `myInt` which is a
  `SOAP` integer::

   My_Int : constant Integer := SOAP.Parameters.Get (Params, "myInt");

* Implements the Web Service

  This is the real job, as for any procedure you can do whatever is
  needed to compute the result.

* Build the `SOAP` answer

  This is the procedure answer. A `SOAP` answer is built from the
  `SOAP` Payload and by setting the returned parameters::

   declare
      Resp        : SOAP.Message.Response.Object;
      Resp_Params : SOAP.Parameters.List;
   begin
      Resp := SOAP.Message.Response.From (Payload);

      Resp_Params := +I (My_Int * 2, "answer");

      SOAP.Message.Set_Parameters (Resp, Resp_Params);

  This build a response which is a single integer value named
  `answer` with the value `My_Int * 2`.

* Returns the answer back to the client

  This last step will encode the response object in `XML` and will
  returns it as the body of an `HTTP` message::

   return SOAP.Message.Response.Build (Resp);

.. _SOAP_helpers:

SOAP helpers
------------

There is two ways to help building the `SOAP`
callbacks. `AWS` provides a `SOAP` specific callback, the spec is::

 function SOAP_Callback
   (SOAPAction : in String;
    Payload    : in Message.Payload.Object;
    Request    : in AWS.Status.Data) return AWS.Response.Data;

With both solutions exposed below, `AWS` retrieve the
`SOAPAction` and the Payload from the `SOAP` request. This
is transparent to the user.

* Using Utils.SOAP_Wrapper

  .. index:: Utils.SOAP_Wrapper

  It is possible to dispatch to such callback by using the
  `SOAP.Utils.SOAP_Wrapper` generic routine::

   generic
      with function SOAP_CB
             (SOAPAction : in String;
              Payload    : in Message.Payload.Object;
              Request    : in AWS.Status.Data) return AWS.Response.Data;
   function SOAP_Wrapper
     (Request : in AWS.Status.Data) return AWS.Response.Data;
   --  From a standard HTTP callback call the SOAP callback passed as generic
   --  formal procedure. Raise Constraint_Error if Request is not a SOAP
   --  request.

  For example, from the standard HTTP callback `CB` we want to call
  `SOAP_CB` for all `SOAP` requests::

   function SOAP_CB
     (SOAPAction : in String;
      Payload    : in Message.Payload.Object;
      Request    : in AWS.Status.Data) return AWS.Response.Data is
   begin
      --  Code here
   end SOAP_CB;

   procedure SOAP_Wrapper is new SOAP.Utils.SOAP_Wrapper (SOAP_CB);

   function CB (Request : in AWS.Status.Data) return AWS.Response.Data is
      SOAPAction : constant String := Status.SOAPAction (Request);
   begin
      if SOAPAction /= "" then
         SOAP_Wrapper (Request);
      else
         ...

* Using a SOAP Dispatcher

  .. index:: SOAP Dispatcher

  `AWS` provides also a `SOAP` specific dispatcher. This
  dispatcher will automatically calls a standard `HTTP` or
  `SOAP` callback depending on the request. If `SOAPAction` is
  specified (i.e. it is a `SOAP` request), the dispatcher will call
  the `SOAP` callback otherwise it will call the standard `HTTP`
  callback. This is by far the easiest integration procedure. Using
  dispatcher the above code will be written::

   function SOAP_CB
     (SOAPAction : in String;
      Payload    : in Message.Payload.Object;
      Request    : in AWS.Status.Data) return AWS.Response.Data is
   begin
      --  Code here
   end SOAP_CB;

   function CB (Request : in AWS.Status.Data) return AWS.Response.Data is
      SOAPAction : constant String := Status.SOAPAction (Request);
   begin
      --  Code here
   end CB;

   --  In the main procedure

   begin
      AWS.Server.Start
        (WS,
         Dispatcher =>
           SOAP.Dispatchers.Callback.Create (CB'Access, SOAP_CB'Access),
         Config     =>
           AWS.Config.Default_Config);

  .. index:: SOAP.Dispatchers.Callback

  The dispacther is created using `SOAP.Dispatchers.Callback.Create`.
  This routine takes two parameters, one is the standard HTTP
  callback procedure and the other is the `SOAP` callback procedure.
