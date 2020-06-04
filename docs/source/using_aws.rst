.. _Using_AWS:

*********
Using AWS
*********

.. _Setting_up_environment:

Setting up environment
======================

.. _Using_environment_variables:

Using environment variables
---------------------------

After installing `AWS` you must set the build environment to
point the compiler to the right libraries. First let's say that
`AWS` has been installed in :file:`awsroot` directory.

Following are the instructions to set the environment yourself. Note
that the preferred solution is to use project files. In this case there
is no manual configuration.

*spec files*

  The spec files are installed in :file:`<awsroot>/include/aws`. Add this
  path into `ADA_INCLUDE_PATH` or put it on the command line
  `-aI<awsroot>/include/aws`.

*libraries*

  The GNAT library files (:file:`.ali`) and the `AWS` libraries
  (:file:`libaws.a`) are installed into :file:`<awsroot>/lib/aws`. Add
  this path into `ADA_OBJECTS_PATH` or put it on the command line
  `-aO<awsroot>/lib/aws/static`. Furthermore for `gnatlink` to find the
  libraries you must add the following library path option on the
  `gnatmake` command line `-largs -L<awsroot>/lib/aws -laws`.

  Note that to build SSL applications you need to add `-lssl -lcrypto` on
  gnatmake's `-largs` section.

*external libraries*

  You must do the same thing (setting `ADA_INCLUDE_PATH` and
  `ADA_OBJECTS_PATH`) for all external libraries that you will
  be using.

.. _Using_GNAT_Project_Files:

Using GNAT Project Files
------------------------

.. highlight:: ada

The best solution is to use the installed GNAT Project File
:file:`aws.gpr`. This is supported only for `GNAT 5.01` or
above. You must have installed `XML/Ada` with project file
support too.

If this is the case just set the `ADA_PROJECT_PATH` variable to
point to the `AWS` and `XML/Ada` install directories. From
there you just have to with the `AWS` project file in your GNAT
Project file, nothing else to set::

  with "aws";

  project Simple is

    for Main use ("prog.adb");

    for Source_Dirs use ("src");

    for Object_Dir use "obj";

  end Simple;

See the *GNAT User's Guide* for more information about GNAT Project Files.

.. _Basic_notions:

Basic notions
=============

`AWS` is not a Web Server like *IIS* or *Apache*, it is a component
to embedded HTTP protocol in an application. It means that it is possible
to build an application which can also answer to a standard browser like
*Internet Explorer* or *Netscape Navigator*. Since `AWS` provides
support client and server HTTP protocol, applications can communicate
through the HTTP channel. This give a way to build distributed
applications, see :ref:`AWS.Client`.

An application using `AWS` can open many `HTTP` channels. Each
channel will use a specific port. For example, it is possible to
embedded many `HTTP` and/or many `HTTPS` channels in the
same application.

.. _Building_an_AWS_server:

Building an AWS server
----------------------

To build a server you must:

* declare the HTTP Web Server::

    WS  : AWS.Server.HTTP;

.. index:: HTTP declaration

* Start the server

  You need to start the server before using it. This is done by calling
  `AWS.Server.Start` (see :ref:`AWS.Server`)::

   procedure Start
     (Web_Server                : in out HTTP;
      Name                      : in     String;
      Callback                  : in     Response.Callback;
      Max_Connection            : in     Positive     := Def_Max_Connect;
      Admin_URI                 : in     String       := Def_Admin_URI;
      Port                      : in     Positive     := Def_Port;
      Security                  : in     Boolean      := False;
      Session                   : in     Boolean      := False;
      Case_Sensitive_Parameters : in     Boolean      := True;
      Upload_Directory          : in     String       := Def_Upload_Dir);
   --  Start the Web server. It initialize the Max_Connection connections
   --  lines. Name is just a string used to identify the server. This is used
   --  for example in the administrative page. Admin_URI must be set to enable
   --  the administrative status page. Callback is the procedure to call for
   --  each resource requested. Port is the Web server port. If Security is
   --  set to True the server will use an HTTPS/SSL connection. If Session is
   --  set to True the server will be able to get a status for each client
   --  connected. A session ID is used for that, on the client side it is a
   --  cookie. Case_Sensitive_Parameters if set to False it means that the CGI
   --  parameters name will be handled without case sensitivity. Upload
   --  directory point to a directory where uploaded files will be stored.

.. index:: starting server

The procedure `Start` takes many parameters:

**Web_Server**

    .. index:: Web_Server

    this is the Web server to start.

**Name**

    This is a string to identify the server. This name will be used for
    example in the administrative status page.

**Callback**

    .. index:: Callback

    This is the procedure to call for each requested resources. In this
    procedure you must handle all the possible URI that you want to support.
    (see below).

**Max_Connection**

    .. index:: Max_Connection

    This is the maximum number of simultaneous connections. It means that
    Max_Connection client's browsers can gets answer at the same
    time. This parameter must be changed to match your needs. A medium Web
    server will certainly need something like 20 or 30 simultaneous
    connections.

**Admin_URI**

    .. index:: Admin_URI

    This is a special URI recognized internally by the server. If this URI
    is requested the server will return the administrative page. This page
    is built using a specific template page (default is
    '|STATUS_PAGE|'), see :ref:`Status_page`.

    The administrative page returns many information about the server. It is
    possible to configure the server via two configuration files
    see :ref:`Configuration_options`.

**Port**

    .. index:: Port

    This is the port to use for the Web server. You can use any free port on
    your computer. Note that on some OS specific range could be reserved
    or needs specials privileges (port 80 on Linux for example).

**Security**

    .. index:: Security

    If Security is set to True the server will use an HTTPS/SSL
    connection. This part uses the `OpenSSL` or `GNUTLS` library.

**Session**

    .. index:: Session

    If Session is set to true the server will keep a session ID for each
    client. The client will be able to save and get variables associated
    with this session ID.

**Case_Sensitive_Parameters**

    .. index:: Case_Sensitive_Parameters

    If set to True the CGI name parameters will be handled without using the
    case.

Note that there is other `Start` routines which support other features.
For example there is a `Start` routine which use a dispatcher routine
instead of the simple callback procedure, see :ref:`AWS.Server`. And
there is also the version using a `Config.Object` which is the most
generic one.

* provides a callback procedure

  The callback procedure has the following prototype::

   function Service (Request : in AWS.Status.Data) return AWS.Response.Data;

  This procedure receive the request status. It is possible to retrieve
  information about the request through the `AWS.Status` API
  (see :ref:`AWS.Status`.).

  For example, to know what URI has been asked::

   URI : constant String := AWS.Status.URI (Request);

   if URI = "/whatever" then
      ...
   end if;

  Then this function should return an answer using one of the constructors
  in `AWS.Response` (see :ref:`AWS.Response`.). For example, to return an
  `HTML` message::

   AWS.Response.Build (Content_Type => "text/html",
                       Message_Body => "<p>just a demo");

  It is also possible to return a file. For example, here is the way to
  return a PNG image::

   AWS.Response.File (Content_Type => "image/png",
                      Filename     => "adains.png");

Note that the main procedure should exit only when the server is terminated.
For this you can use the `AWS.Server.Wait` service.

A better solution is to use a template engine like Templates_Parser to
build the `HTML` Web Server answer. Templates_Parser module is
distributed with this version of AWS.

.. _Callback_procedure:

Callback procedure
------------------

.. index:: Callback

.. index:: Callback procedure

The callback procedure is the user's code that will be called by the AWS
component to get the right answer for the requested resource. In fact
AWS just open the HTTP message, parsing the HTTP header and it builds
an object of type `AWS.Status.Data`. At this point it calls the
user's callback procedure, passing the object. The callback procedure
must returns the right response for the requested resources. Now AWS
will just build up the HTTP response message and send it back to
user's browser.

**But what is the resource ?**

.. index:: resources

Indeed in a standard Web development a resource is either a static
object - an `HTML` page, an `XML` or `XSL` document -
or a `CGI` script. With `AWS` a resource is *just a string* to identify the
resource, it does not represent the name of a static object or `CGI` script.

So this string is just an internal representation for the
resource. The callback procedure must be implemented to handle each
internal resource and return the right response.

.. index:: Hello world

Let's have a small example. For example we want to build a Web server
that will answer 'Hello World' if we ask for the internal resource
**/hello**, and must answer 'Hum...' otherwise::

  with AWS.Response;
  with AWS.Server;
  with AWS.Status;

  procedure Hello_World is

    WS : AWS.Server.HTTP;

    function HW_CB (Request : in AWS.Status.Data)
      return AWS.Response.Data
    is
       URI : constant String := AWS.Status.URI (Request);
    begin
       if URI = "/hello" then
          return AWS.Response.Build ("text/html", "<p>Hello world !");
       else
          return AWS.Response.Build ("text/html", "<p>Hum...");
       end if;
    end HW_CB;

  begin
     AWS.Server.Start
       (WS, "Hello World", Callback => HW_CB'Unrestricted_Access);
     delay 30.0;
  end Hello_World;

Now of course the resource internal name can represent a file on
disk. It is not mandatory but it is possible. For example it is
perfectly possible to build with `AWS` a simple page server.

.. index:: Page server

.. index:: Simple server

As an example, let's build a simple page server. This server will
returns files in the current directory. Resources internal name
represent an `HTML` page or a `GIF` or `PNG` image for
example. This server will return a 404 message (Web Page Not Found) if
the file does not exist. Here is the callback procedure that implements
such simple page server::

 function Get (Request : in AWS.Status.Data) return AWS.Response.Data is
    URI      : constant String := AWS.Status.URI (Request);
    Filename : constant String := URI (2 .. URI'Last);
 begin
    if Utils.Is_Regular_File (Filename) then
       return AWS.Response.File
                (Content_Type => AWS.MIME.Content_Type (Filename),
                 Filename     => Filename);

    else
       return AWS.Response.Acknowledge
         (Messages.S404,
          "<p>Page '" & URI & "' Not found.");
    end if;
 end Get;

.. _Form_parameters:

Form parameters
---------------

.. index:: Form parameters

.. index:: Parameters

.. highlight:: xml

Form parameters are stored into a table of key/value pair. The key is the form
input tag name and the value is the content of the input field as filled by
the user::

 Enter your name

 <FORM METHOD=GET ACTION=/get-form>"
 <INPUT TYPE=TEXT NAME=name VALUE="<default>" size=15>
 <INPUT TYPE=SUBMIT NAME=go VALUE="Ok">
 </FORM>

Note that as explained above :ref:`Callback_procedure`, the resource
described in `ACTION` is just an internal string representation
for the resource.

In this example there is two form parameters:

*name*
  The value is the content of this text field as filled by the client.

*go*
  The value is "Ok".

.. highlight:: ada

There is many functions (in `AWS.Parameters`) to retrieve the tag name
or value and the number of parameters. Here are some examples::

 function Service (Request : in AWS.Status.Data) return AWS.Response.Data is
    P : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
 ...

*AWS.Parameters.Get (P, "name")*

  .. index:: Parameters Get

  Returns the value for parameter named **name**

*AWS.Parameters.Get_Name (P, 1)*

  .. index:: Parameters Get_Name

  Returns the string "name".

*AWS.Parameters.Get (P, 1)*

  Returns the value for parameter named **name**

*AWS.Parameters.Get (P, "go")*

  Returns the string "Ok".

*AWS.Parameters.Get_Name (P, 2)*

  Returns the string "go".

*AWS.Parameters.Get (P, 2)*

  Returns the string "Ok".

`Request` is the `AWS` current connection status passed to the
callback procedure. And `P` is the parameters list retrieved from the
connection status data. For a discussion about the callback procedure
see :ref:`Building_an_AWS_server`.

.. _Distribution_of_an_AWS_server:

Distribution of an AWS server
-----------------------------

.. index:: Distributing

The directory containing the server program must contain the following
files if you plan to use a status page see :ref:`Status_page`.

|STATUS_PAGE|

  The template `HTML` file for the `AWS` status page.

|LOGO_IMAGE|

  The `AWS` logo displayed on the status page.

|UP_IMAGE|

  The `AWS` hotplug table up arrow.

|DOWN_IMAGE|

  The `AWS` hotplug table down arrow.

Note that these filenames are the current `AWS` default. But it is
possible to change those defaults using the configuration files
see :ref:`Configuration_options`.

.. _Building_answers:

Building answers
================

We have already seen, in simple examples, how to build basic answers using
`AWS.Response` API. In this section we present all ways to build
answers from basic support to the more advanced support like the
compressed memory stream response.

.. _Redirection:

Redirection
-----------

A redirection is a way to redirect the client's browser to another
URL. Client's won't notice that a redirection has occurs. As soon as
the browser has received the response from the server it will retrieve
the page as pointed by the redirection::

 return Response.URL (Location => "/use-this-one");

.. _New_location_for_a_page:

New location for a page
-----------------------

User will receive a Web page saying that this page has moved and
eventually pointing to the new location::

 return Response.Moved
   (Location => "/use-this-one";
    Message  => "This page has moved, please update your reference");

.. _Authentication_required:

Authentication required
-----------------------

For protected pages you need to ask user to enter a password.
See :ref:`Authentication`.

.. _Sending_back_an_error_message:

Sending back an error message
-----------------------------

`Acknowledge` can be used to send back error messages. There is
many kind of status code, see `Message.Status_Code`
definition. Together with the status code it is possible to pass
textual error message in `Message_Body` parameter::

 return Response.Acknowledge
   (Status_Code  => Messages.S503,
    Message_Body => "Can't connect to the database, please retry later.",
    Content_Type => MIME.Text_Plain);

.. _Response_from_a_string:

Response from a string
----------------------

This is the simplest way to build a response object. There is two
constructors in `AWS.Response`, one based on a standard string
and one for Unbounded_String::

 return Response.Build (MIME.Text_HTML, "My answer");

The Build routine takes also a status code parameter to handle
errors. By default this code is `Messages.S200` which is the
standard HTTP status (no error encountered). The other parameter can
be used to control caches. See :ref:`AWS.Response`.

.. _Response_from_a_Stream_Element_Array:

Response from a Stream_Element_Array
------------------------------------

This is exactly as above but the Build routine takes a
`Stream_Element_Array` instead of a string.

.. _Response_from_a_file:

Response from a file
--------------------

To build a `File` response there is a single constructor named
`File`. This routine is very similar to the one above except that
we specify a filename as the response::

 return Response.File (MIME.Text_HTML, "index.html");

Again there parameters to control the status code and cache. No check
on the filename is done at this point, so if :file:`index.html` does
not exit no exception is raised. The server is responsible to check
for the file and to properly send back the 404 message if necessary.

Note that this routine takes an optional parameter named `Once`
that is to be used for temporary files created on the server side for
the client. With `Once` set to `True` the file will be
deleted by the server after sending it (this includes the case where
the download is suspended).

.. _Response_from_a_stream:

Response from a stream
----------------------

Sometimes it is not possible (or convenient) to build the response in
memory as a string object for example. Streams can be used to
workaround this. The constructor for such response is again very
similar to the ones above except that instead of the data we pass an
handle to a `Resources.Streams.Stream_Type` object.

The first step is to build the stream object. This is done by deriving
a new type from `Resources.Streams.Stream_Type` and implementing
three abstract procedures.

*Read*

  Must return the next chunk of data from the stream. Note that
  initialization if needed are to be done there during the first call to
  read.

*End_Of_File*

  Must return True when there is no more data on the stream.

*Close*

  Must close the stream and for example release all memory used by the
  implementation.

The second step is to build the response object::

 type SQL_Stream is new Resources.Streams.Stream_Type;

 Stream_Object : SQL_Stream;

 procedure Read (...) is ...
 function End_Of_File (...) return Boolean is ...
 procedure Close (...) is

 ...

 return Response.Stream (MIME.Text_HTML, Stream_Object);

Note that in some cases it is needed to create a file containing the
data for the client (for example a tar.gz or a zip archive). But there
is no way to properly remove this file from the file system as we
really don't know when the upload is terminated when using the
`AWS.Response.File` constructor. To solve this problem it is
possible to use a stream as the procedure `Close` is called by
the server when all data have been read. In this procedure it is
trivial to do the necessary clean-up.

.. _Response_from_a_on-disk_stream:

Response from a on-disk stream
------------------------------

An ready-to-use implementation of the stream API described above where
the stream content is read from an on-disk file.

.. _Response_from_a_on-disk_once_stream:

Response from a on-disk once stream
-----------------------------------

An ready-to-use implementation of the stream API described above where
the stream content is read from an on-disk file. When the transfer is
completed the file is removed from the file system.

.. _Response_from_a_memory_stream:

Response from a memory stream
-----------------------------

This is an implementation of the standard stream support described
above. In this case the stream is in memory and built by adding data
to it.

To create a memory stream just declare an object of type
`AWS.Resources.Streams.Memory.Stream_Type`. When created, this
memory stream is empty, using the `Streams.Memory.Append`
routines it is possible to add chunk of data to it. It is of course
possible to call `Append` as many times as needed. When done just
return this object to the server::

 Data : AWS.Resources.Streams.Memory.Stream_Type;

 Append (Data, Translator.To_Stream_Element_Array ("First chunk"));
 Append (Data, Translator.To_Stream_Element_Array ("Second chunk..."));

 ...

 return Response.Stream (MIME.Text_HTML, Data);

Note that you do not have to take care of releasing the allocated
memory, the default `Close` routine will do just that.

.. _Response_from_a_compressed_memory_stream:

Response from a compressed memory stream
----------------------------------------

This is a slight variant of the standard memory stream described
above. In this case the stream object must be declared as a
`AWS.Resources.Streams.Memory.ZLib.Stream_Type`.

The ZLib stream object must be initialized to enable the
compression and select the right parameters. This is done using the
`AWS.Resources.Streams.Memory.ZLib.Deflate_Initialize` routine which
takes many parameters to select the right options for the compression
algorithm, all of them have good default values. When initialized the
compressed stream object is used exactly as a standard stream::

 Data : AWS.Resources.Streams.Memory.ZLib.Stream_Type;

 Deflate_Initialize (Data);

 Append (Data, Translator.To_Stream_Element_Array ("First chunk"));
 Append (Data, Translator.To_Stream_Element_Array ("Second chunk..."));

 ...

 return Response.Stream (MIME.Text_HTML, Data);

Note that there is the reverse implementation to decompress a
stream. See :ref:`AWS.Resources.Streams.Memory.ZLib`. It's usage
is identical.

.. _Split_page:

Split page
----------

`AWS` has a specific high level service to split a large response
into a set of pages. For more information see :ref:`Split_pages`.

.. _Response_a_from_pipe_stream:

Response a from pipe stream
---------------------------

The response sent to the server is read from the output of an external
application. This kind of stream can be used to avoid writing a temporary
file into the hard disk. For example it is possible to return an archive
created with the `tar` tool without writing the intermediate tar
achive on the disk.

.. _Configuration_options:

Configuration options
=====================

.. index:: Configuration options

To configure an `AWS` server it is possible to use a
configuration object. This object can be set using the `AWS.Config.Set`
API or initialized using a configuration file.

Configuration files are a way to configure the server without
recompiling it. Each application can be configured using two
configurations files:

*aws.ini*

  .. index:: aws.ini
  .. index:: ini file

  This file is parsed first and corresponds to the configuration for all
  AWS server runs in the same directory.

*<program_name>.ini*

  .. index:: program_name.ini

  This file is parsed after :file:`aws.ini`. It is possible with this
  initialization file to have specific settings for some servers.
  :file:`program_name.ini` is looked first in the application's directory
  and then in the current working directory. This is only supported on
  platforms where `Ada.Command_Line` is implemented. So, on **VxWorks**
  only :file:`aws.ini` is parsed.

Furthermore, it is possible to read a specific configuration file
using the `AWS.Config.Ini.Read` routine. See :ref:`AWS.Config.Ini`.

Current supported options are:

*Accept_Queue_Size (positive)*

  .. index:: Accept_Queue_Size

  This is the size of the queue for the incoming requests. Higher this
  value will be and less "*connection refused*" will be reported to the
  client. The default value is |QUEUE_SIZE|.

*Admin_Password (string)*

  .. index:: Admin_Password

  .. highlight:: sh

  This is the password used to call the administrative page. The
  password can be generated with :file:`aws_password` (the module name
  must be `admin`)::

    $ aws_password admin <password>

*Admin_URI (string)*

  .. index:: Admin_URI

  This is the URI to call the administrative page. This can be used when
  calling `AWS.Server.Start`. The default is |ADMIN_URI|.

*Case_Sensitive_Parameters (boolean)*

  .. index:: Case_Sensitive_Parameters

  If set to `True` the `HTTP` parameters are case
  sensitive. The default value |CASE_SENSITIVE_PARAMETERS|.

*Certificate (string)*

  .. index:: Certificate (string)

  Set the certificate file to be used with the secure servers. The
  default is |DEFAULT_CERTIFICATE|. A single certificate or a
  certificate chain is supported. The certificates must be in `PEM` format
  and the chain must be sorted starting with the subject's certificate, followed
  by intermediate CA certificates if applicable and ending at the highest
  level (root) CA certificate. If the file contains only a single
  certificate, it can be followed by a private key. In this case the Key
  parameter (see below) must empty.

*Check_URL_Validity (boolean)*

  .. index:: Check_URL_Validity

  Server have to check URI for validity. For example it checks that an
  URL does not reference a resource above the Web root. The default
  is |CHECK_URL_VALIDITY|.

*Cipher_Priorities*

  .. index:: Cipher_Priorities (string)

  Values are dependent on the actual secure layer (OpenSSL or
  GNUTLS). It is used to specify the session’s handshake algorithms
  and options.

*Cleaner_Wait_For_Client_Timeout (duration)*

  .. index:: Cleaner_Wait_For_Client_Timeout

  Number of seconds to timeout on waiting for a client request. This is a
  timeout for regular cleaning task. The default is
  |CT_WAIT_FOR_CLIENT| seconds.

*Cleaner_Client_Header_Timeout (duration)*

  .. index:: Cleaner_Client_Header_Timeout

  Number of seconds to timeout on waiting for client header. This is a
  timeout for regular cleaning task. The default is |CT_CLIENT_HEADER| seconds.

*Cleaner_Client_Data_Timeout (duration)*

  .. index:: Cleaner_Client_Data_Timeout

  Number of seconds to timeout on waiting for client message body. This
  is a timeout for regular cleaning task. The default is
  |CT_CLIENT_DATA| seconds.

*Cleaner_Server_Response_Timeout (duration)*

  .. index:: Cleaner_Server_Response_Timeout

  Number of seconds to timeout on waiting for client to accept
  answer. This is a timeout for regular cleaning task. The default is
  |CT_SERVER_RESPONSE| seconds.

*Config_Directory (string)*

  .. index:: Config_Directory

  The directory in which AWS keeps some configuration parameters.
  The default is |CONFIG_DIRECTORY|.

*CRL_File (string)*

  .. index:: CRL_File, CRL

  This configuration option must point to a filename containing a CRL
  (Certificate Revocation List). This will make it possible to control
  client connecting to the server. The default values is |CRL_FILE|.

*Directory_Browser_Page (string)*

  .. index:: Directory_Browser_Page

  Specify the filename for the directory browser template page. The
  default value is |DIRECTORY_BROWSER_PAGE|.

*Disable_Program_Ini (boolean)*

  .. index:: Disable_Program_Ini

  Specify whether the configuration file :file:`program_name.ini` should be
  parsed or not. If this option is set to FALSE the program specific
  configuration file won't be parsed. This may be useful if another
  application is using such a file and cannot be shared. This setting
  is expected to be set in :file:`aws.ini` before the :file:`program_name.ini`
  file is parsed. The default value is |DISABLE_PROGRAM_INI|.

*Down_Image (string)*

  .. index:: Down_Image

  The name of the down arrow image to use in the status page. The default is
  |DOWN_IMAGE|.

*Error_Log_Activated (boolean)*

  .. index:: Error_Log_Activated

  A boolean to enable or disable the error log. By default the
  error log activation is set to |ERROR_LOG_ACTIVATED|.

*Error_Log_Filename_Prefix (string)*

  .. index:: Error_Log_Filename_Prefix

  This is to set the filename prefix for the error log file. By
  default the error log filename prefix is the program name (without
  extension) followed by "_error".

*Error_Log_Split_Mode [None/Each_Run/Daily/Monthly]*

  .. index:: Error_Log_Split_Mode

  It indicates how to split the error logs. Each_Run means that a new log file
  is used each time the process is started. Daily and Monthly will use a
  new log file each day or month. The default is |ERROR_LOG_SPLIT_MODE|.

*Exchange_Certificate (boolean)*

  .. index:: Exchange_Certificate

  If set to True it means that the client will be asked to send its
  certificate to the server. The default value is |EXCHANGE_CERTIFICATE|.

*Certificate_Required (boolean)*

  .. index:: Certificate_Required

  If set to True the server will reject all SSL connections if the
  client did not provide a certificate (be it valid or not). The
  `Exchange_Certificate` option must be set in this case. The
  default value is |CERTIFICATE_REQUIRED|.

*Force_Wait_For_Client_Timeout (duration)*

  .. index:: Force_Wait_For_Client_Timeout

  Number of seconds to timeout on waiting for a client request. This is a
  timeout for urgent request when resources are missing. The default is
  |FT_WAIT_FOR_CLIENT| seconds.

*Force_Client_Header_Timeout (duration)*

  .. index:: Force_Client_Header_Timeout

  Number of seconds to timeout on waiting for client header. This is a
  timeout for urgent request when resources are missing. The default is
  |FT_CLIENT_HEADER| seconds.

*Force_Client_Data_Timeout (duration)*

  .. index:: Force_Client_Data_Timeout

  Number of seconds to timeout on waiting for client message body. This
  is a timeout for urgent request when resources are missing. The default is
  |FT_CLIENT_DATA| seconds.

*Force_Server_Response_Timeout (duration)*

  .. index:: Force_Server_Response_Timeout

  Number of seconds to timeout on waiting for client to accept answer. This
  is a timeout for urgent request when resources are missing. The default is
  |FT_SERVER_RESPONSE| seconds.

*Free_Slots_Keep_Alive_Limit (positive)*

  .. index:: Free_Slots_Keep_Alive_Limit

  This is the minimum number of remaining free slots to enable keep-alive HTTP
  connections. For heavy-loaded HTTP servers, the Max_Connection parameter
  should be big enough, and Free_Slots_Keep_Alive_Limit should be about 1-10%
  of the Max_Connection parameter depending on the duration of the
  average server response. Longer is the average time to send back a
  response bigger Free_Slots_Keep_Alive_Limit should be.
  The default is |KEEP_ALIVE_LIMIT|.

*Hotplug_Port (positive)*

  .. index:: Hotplug_Port

  This is the hotplug communication port needed to register and
  un-register an hotplug module. The default value is |HOTPLUG_PORT|.

*Key (string)*

  .. index:: Key

  Set the RSA key file to be used with the secure servers. The
  default file is |DEFAULT_KEY|.

*Line_Stack_Size (positive)*

  .. index:: Line_Stack_Size

  The HTTP lines stack size. The stack size must be adjusted for each
  applications depending on the use of the stack done by the callback
  procedures. The default is |LINE_STACK_SIZE|.

*Log_Activated (boolean)*

  .. index:: Log_Activated

  A boolean to enable or disable the standard log. By default the
  standard log activation is set to |LOG_ACTIVATED|.

*Log_Extended_Fields (string list)*

  .. index:: Log_Extended_Fields

  Comma separated list of the extended log field names. If this parameter
  is empty, the HTTP log would be in the apache compartible format, otherwise
  log file would be in Extended format. For more information see
  :ref:`Server_Log`.

*Log_File_Directory (string)*

  .. index:: Log_File_Directory

  This is to set the directory where log file must be written. This
  parameter will be used automatically by `AWS.Log` if logging
  facility is enabled. By default log files are written in the current
  directory. The default is |LOG_FILE_DIR|.

*Log_Filename_Prefix (string)*

  .. index:: Log_Filename_Prefix

  This is to set the filename prefix for the log file. By default the
  log filename prefix is the program name (without extension).

*Log_Split_Mode [None/Each_Run/Daily/Monthly]*

  .. index:: Log_Split_Mode

  It indicates how to split the logs. Each_Run means that a new log file
  is used each time the process is started. Daily and Monthly will use a
  new log file each day or month. The default is |LOG_SPLIT_MODE|.

*Logo_Image (string).*

  .. index:: Logo_Image

  The name of the logo image to use in the status page. The default is
  |LOGO_IMAGE|.

*Max_Concurrent_Download (positive)*

  .. index:: Max_Concurrent_Download

  Control the maximum number of parallel downloads accepted by the download
  manager. The default value is |MAX_CONCURRENT_DOWNLOAD|.

*Max_Connection (positive)*

  .. index:: Max_Connection

  This is the maximum number of simultaneous connections for the
  server. This can be used when calling the `AWS.Server.Start`. The
  default is |MAX_CONNECT|.

  Note that the total number of threads used by a server is::

   N = <main server thread> + <max connections> [+ <session>]

  Note: [...] means optional value
  Add 1 only if the session feature is activated. This is
  due to the session cleaner task.

*Max_POST_Parameters (positive)*

  .. index:: Max_POST_Parameters

  The maximum number of POST parameters supported by AWS. The default
  value is |MAX_POST_PARAMETERS|.

.. _max-websocket:

*Max_WebSocket (positive)*

  .. index:: Max_WebSocket

  The maximum number of WebSocket that can be opened simultaneously
  in AWS. Above this value AWS will try to close timed-out WebSockets
  (see :ref:`WebSocket_Timeout <websocket-timeout>`). The default
  value is |MAX_WEBSOCKET|.

*Max_WebSocket_Handler (positive)*

  .. index:: Max_WebSocket_Handler

  The maximum number of message to handle simultaenously. The default
  value is |MAX_WEBSOCKET_HANDLER|.

*MIME_Types (string)*

  .. index:: MIME_Types

  The name of the file containing the MIME types associations. The default
  file name is |MIME_TYPES|.

*Receive_Timeout (duration)*

  .. index:: Receive_Timeout

  Number of seconds to timeout when receiving chunk of data. The
  default is |RECEIVE_TIMEOUT| seconds.

*Reuse_Address (boolean)*

  .. index:: Reuse_Address

  Set the socket reuse address policy. If set to True the server will be
  able to bind to a socket that has just been released without the need of
  waiting. Enabling this feature may introduce security risks on some
  platforms. The default is |REUSE_ADDRESS|.

*Security_Mode (string)*

  .. index:: Security_Mode

  Set the security mode to use for the secure connections. The default
  mode is |SECURITY_MODE|. See :ref:`AWS.Net.SSL`.

*Send_Buffer_Size (positive)*

  .. index:: Send_Buffer_Size

  This is the socket internal buffer used for sending data to the
  clients. The default is |SEND_BUFFER_SIZE|.

*Send_Timeout (duration)*

  .. index:: Send_Timeout

  Number of seconds to timeout when sending chunk of data. The default is
  |SEND_TIMEOUT| seconds.

*Server_Header (string)*

  .. index:: Server_Header

  The value to be used for the HTTP Server header. The default is
  |SERVER_HEADER|. If the value is set to the empty string, the server
  header is not sent.

*Server_Host (string)*

  .. index:: Server_Host

  The name of the host machine. This can be used if a computer has more
  than one IP address, it is possible to have two servers at the same
  port on the same machine, both being binded on different IP addresses.

*Server_Name (string)*

  .. index:: Server_Name

  The name of the server. This can be used when calling
  `AWS.Server.Start`. The default is |SERVER_NAME|.

*Server_Priority (natural)*

  .. index:: Server_Priority (natural)

  Priority of the task handling the HTTP protocol.
  The default is Default_Priority.

*Server_Port (integer)*

  .. index:: Server_Port

  The port where server will wait for incoming connections requests. This
  can be used when calling `AWS.Server.Start`. The default is
  |SERVER_PORT|.

*Service_Priority (natural)*

  .. index:: Service_Priority (natural)

  Priority of the tasks used by optional services like SMTP Server,
  Server Push, Jabber and the Transient Page cleaner.
  The default is **Default_Priority**.

*Session (boolean)*

  .. index:: Session

  Whether the session support must be activated or not. The default is
  |SESSION|.

*Session_Name (string)*

  .. index:: Session_Name

  The name of the cookie session. This can be used to support sessions
  for multiple servers embedded into the same executable. The default is
  |SESSION_NAME|.

*Session_Id_Length (positive)*

  .. index:: Session_Id_Length (positive)

  The length of the session id in characters. The default
  is |SESSION_ID_LENGTH| characters.

*Session_Lifetime (duration)*

  .. index:: Session_Lifetime (duration)

  Number of seconds to keep session information. After this period a
  session is obsoleted and will be removed at next cleanup. The default
  is |SESSION_LIFETIME| seconds.

*Session_Cleanup_Interval (duration)*

  .. index:: Session_Cleanup_Interval (duration)

  Number of seconds between each run of the session cleanup task. This
  task will remove all session data that have been obsoleted. The
  default is |SESSION_CLEANUP_INTERVAL| seconds.

*Session_Cleaner_Priority (natural)*

  .. index:: Session_Cleaner_Priority (natural)

  Priority of the task cleaning the session data.
  The default is **Default_Priority**.

*Status_Page (string)*

  .. index:: Status_Page

  The name of the status page to used. The default is |STATUS_PAGE|.

*TCP_No_Delay (boolean)*

  .. index:: TCP_No_Delay

  This control the server's socket delay/no-delay option. This option
  should be used for applications that require lower latency on every
  packet sent. The default is |TCP_NO_DELAY|.

*TLS_Ticket_Support (boolean)*

  .. index:: TLS_Ticket_Support

  Specify whether the TLS ticket support is activated or not. The
  default value is |TLS_TICKET_SUPPORT|.

*Transient_Cleanup_Interval (positive)*

  .. index:: Transient_Cleanup_Interval

  Specify the number of seconds between each run of the cleaner task to remove
  transient pages. The default value is |TRANSIENT_CLEANUP_INTERVAL| seconds.

*Transient_Lifetime (duration)*

  .. index:: Transient_Lifetime

  Specify the number of seconds to keep a transient page. After this
  period the transient page is obsoleted and will be removed during next
  cleanup. The default value is |TRANSIENT_LIFETIME| seconds.

*Trusted_CA (string)*

  .. index:: Trusted_CA

  This must point to the file containing the list of trusted
  Certificate Authorities. The CA in this file will be used to verify
  the client certificate validity. The default values is |TRUSTED_CA|.

*Up_Image (string)*

  .. index:: Up_Image

  The name of the up arrow image to use in the status page. The default is
  |UP_IMAGE|.

*Upload_Directory (string)*

  .. index:: Upload_Directory

  This is to set the directory where upload files must be stored. By
  default uploaded files are written in the current directory. The
  default is |UPLOAD_DIR|.

*User_Agent (string)*

  .. index:: User_Agent

  The value to be used for the HTTP User_Agent header. The default value
  is |USER_AGENT|. If the value is set to the empty string, the User_Agent
  header is not sent.

*WebSocket_Message_Queue_Size (positive)*

  .. index:: WebSocket_Message_Queue_Size

  This is the size of the queue containing incoming messages waiting
  to be handled by one of the task, see Max_WebSocket_Handler above. The
  default value is |WEBSOCKET_MESSAGE_QUEUE_SIZE|.

*WebSocket_Origin (string)*

  .. index:: WebSocket_Origin

  This is a regular expression which can be used to handle WebSockets
  originating from a specific domain. By default AWS handles WebSockets
  from any origins.

*WebSocket_Priority (natural)*

  .. index:: WebSocket_Priority (natural)

  Priority of the task handling the WebSockets.
  The default is **Default_Priority**.

.. _websocket-timeout:

*WebSocket_Timeout (duration)*

  .. index:: WebSocket_Timeout

  A number of seconds after which a WebSocket without activity is
  considered timed-out and can be elected to be closed if the maximum
  number of sockets opened has been reached.
  (see :ref:`Max_WebSocket <max-websocket>`). The default
  is |WEBSOCKET_TIMEOUT|.

*WWW_Root (string)*

  .. index:: WWW_Root

  This option sets the Web Server root directory. All Web resources are
  referenced from this root directory. The default value is |WWW_ROOT|.

Each option value can be retrieved using the `AWS.Config` unit or
set using `AWS.Config.Set`.

.. highlight:: ada

For example to build a server where the *port* and the maximum number of
*connection* can be changed via a configuration file (either
:file:`aws.ini` or :file:`<program_name>.ini`)::

 WS   : AWS.Server.HTTP;

 Conf : constant AWS.Config.Object := AWS.Config.Get_Current;

 Server.Start (WS, Service'Access, Conf);

.. _Session_handling:

Session handling
================

.. index:: Session

.. highlight:: ada

`AWS` provides a way to keep session data while users are
browsing. It works by creating transparently a session ID where it
is possible to insert, delete and retrieve session data using a standard
Ada API (see :ref:`AWS.Session`.). Session data are key/value pair each of
them being strings. These sessions data are kept on the server, for
client side state management see :ref:`HTTP_state_management`.

* First you declare and start an HTTP channel with session enabled::

   WS : AWS.Server.HTTP;

   Server.Start (WS,
                 Port     => 1234,
                 Callback => Service'Access,
                 Session  => True);

  Here we have built an HTTP channel with a maximum of 3 simultaneous
  connections using the port 1234. A session ID will be created and sent
  inside a cookie to the client's browser at the first request. This
  session ID will be sent back to the server each time the client will ask
  for a resource to the server.

* Next, in the Service callback procedure that you have provided you
  must retrieve the Session ID. As we have seen, the callback procedure
  has the following prototype::

   function Service (Request : in AWS.Status.Data) return AWS.Response.Data;

  The Session ID is kept in the Request object and can be retrieved using::

   Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);

* From there it is quite easy to get or set some session data using
  the provided API. For example::

   declare
      C : Integer;
   begin
      C := AWS.Session.Get (Session_ID, "counter");
      C := C + 1;
      AWS.Session.Set (Session_ID, "counter", C);
   end;

  This example first get the value (as an Integer) for session data whose
  key is "`counter`", increment this counter and then set it back to
  the new value.

It is also possible to save and restore all session data. It means that the
server can be shutdown and launched some time after and all client data are
restored as they were at shutdown time. Client will just see nothing. With this
feature it is possible to shutdown a server to update its look or because a
bug has been fixed for example. It is then possible to restart it
keeping the complete Web server context.

.. _HTTP_state_management:

HTTP state management
=====================

.. index:: HTTP state
.. index:: Cookies

.. highlight:: ada

`AWS` provides a full implementation of RFC 2109 via the `AWS.Cookie`
package. Using this package you set, get and expire client-side HTTP cookies.

First we set a cookie::

 declare
    Content : AWS.Response.Data;
 begin
    AWS.Cookie.Set (Content,
                    Key      => "hello",
                    Value    => "world",
                    Max_Age  => 86400.0);
 end;

Here we set the cookie `hello` with the value `world`, and we tell
the client to expire the cookie 86400 seconds into the future.

Getting the cookie value back is equally simple::

 declare
    Request : AWS.Status.Data
    --  Assume that this object contain an actual HTTP request.
 begin
    Put_Line (AWS.Cookie.Get (Request, "hello"));
    --  Output 'world'
 end;

Had the cookie `hello` not existed, an empty `String` would've been
returned.

In some cases it might be of value to know if a given cookie exists, and for
that we have the `Exists` function available::

 declare
    Request : AWS.Status.Data
    --  Assume that this object contain an actual HTTP request
 begin
    if AWS.Cookie.Exists ("hello") then
       Put_Line ("The 'hello' cookie exists!");
    end if;
 end;

Note that `Exists` doesn't care if the cookie contains an actual value or
not. If a cookie with no value exists, `Exists` will return `True`.

And finally we might wish to tell the client to expire a cookie::

 declare
    Content : AWS.Response.Data;
 begin
    AWS.Cookie.Expire (Content,
                       Key  => "hello");
 end;

The Cookie package provide `Get` functions and `Set` procedures for
`String`, `Integer`, `Float` and `Boolean` types, but since
cookies are inherently strings, it's important to understand what happens when
the cookie `String` value can't be converted properly to either
`Integer`, `Float` or `Boolean`.

So if either conversion fails or the cookie simply doesn't exist, the following
happens:

* For `Integer`, the value 0 is returned

* For `Float`, the value 0.0 is returned.

* For `Boolean`, the value `False` is returned. Note that only
  the string 'True' is `True`. Everything else is `False`.

For more information see :ref:`AWS.Cookie`.

.. _Authentication:

Authentication
==============

.. index:: authentication
.. index:: basic
.. index:: digest

`AWS` supports **Basic** and **Digest** authentication. The
authentication request can be sent at any time from the callback
procedure. For this the `AWS.Response.Authenticate` message must
be returned.

.. highlight:: ada

The authentication process is as follow:

* Send authentication request

  From the callback routine return an authentication request when
  needed::

   function Service (Request : in Status.Data) return Response.Data is
      URI  : constant String := Status.URI (Request);
      User : constant String := Status.Authorization_Name (Request);
   begin
      --  URI starting with "/prot/" are protected
      if URI (URI'First .. URI'First + 5) = "/prot/"
        and then User = ""
      then
         return Response.Authenticate ("AWS", Response.Basic);

  The first parameter is the **Realm**, it is just a string that will be
  displayed (on the authentication dialog box) by the browser to
  indicate for which resource the authentication is needed.

* Check authentication

  When an authentication as been done the callback's request data
  contain the user and password. Checks the values against an ACL for
  each protected resources::

   function Protected_Service
     (Request : in AWS.Status.Data) return AWS.Response.Data
   is
      User : constant String := Status.Authorization_Name (Request);
      Pwd  : constant String := Status.Authorization_Password (Request);
   begin
      if User = "xyz" and then Pwd = "azerty" then
         return ...;

Note that the **Basic** authentication is not secure at all. The password
is sent unencoded by the browser to the server. If security is an
issue it is better to use the **Digest** authentication and/or an
**SSL** server.

.. _File_upload:

File upload
===========

.. index:: File upload

.. index:: upload, server

.. highlight:: xml

File upload is the way to send a file from the client to the server. To
enable file upload on the client side the Web page must contain a **FORM**
with an **INPUT** tag of type **FILE**. The **FORM** must also contain
the **enctype** attribute set to *multipart/form-data*::

 <FORM enctype="multipart/form-data" ACTION=/whatever METHOD=POST>
   File to process: <INPUT NAME=filename TYPE=FILE>
   <INPUT TYPE=SUBMIT NAME=go VALUE="Send File">
 </FORM>

On the server side, `AWS` will retrieve the file and put it into the
upload directory. `AWS` add a prefix to the file to ensure that the
filename will be unique on the server side. The upload directory can be
changed using the configuration options. See :ref:`Configuration_options`.

The uploaded files are removed after the user's callback. This is done
for security reasons, if files were not removed it would be possible
to fill the server hard disk by uploading large files to the
server. This means that uploaded files must be specifically handled by
the users by either copying or renaming them.

`AWS` will also setup the form parameters as usual. In the above example
there is two parameters (see :ref:`Form_parameters`).

*filename*

  This variable contains two values, one with the client side name and
  one with the server side name.

*First value : Parameters.Get (P, "filename")*

    The value is the full pathname of the file on the server. (i.e. the
    upload directory catenated with the prefix and filename).

*Second value : Parameters.Get (P, "filename", 2)*

    The value is the simple filename (no path information) of the file on
    the client side.

*go*

  The value is "Send File"

.. _Communication:

Communication
=============

.. index:: Communication
.. index:: Sending message

This API is used to do communication between programs using the HTTP
GET protocol. It is a very simple API not to be compared with `GLADE`
or `SOAP`. This communication facility is to be used for simple
request or when a light communication support is needed. For more
complex communications or to achieve inter-operability with other
modules it is certainly a good idea to have a look at the
`AWS/SOAP` support, see :ref:`SOAP`.

In a communication there is a Client and a Server. Here is what is to be
done on both sides to have programs talking together.

.. _Communication_-_client_side:

Communication - client side
---------------------------

.. index:: Communication, Client

.. highlight:: ada

On the client side it is quite simple. You just have to send a message
using `AWS.Communication.Client.Send_Message`::

 function Send_Message
   (Server     : in String;
    Port       : in Positive;
    Name       : in String;
    Parameters : in Parameter_Set := Null_Parameter_Set)
    return Response.Data;

The message is sent to the specified server using the given port. A
message is composed of a name which is a string and a set of
parameters. There is a parameter set constructor in
`AWS.Communication`. This function return a response as for any
callback procedure.

.. _Communication_-_server_side:

Communication - server side
---------------------------

.. index:: Communication, Server

On the server side things are a bit more complex but not that
difficult. You must instantiate the `AWS.Communication.Server`
generic package by providing a callback procedure. This callback
procedure will must handle all kind of message that a client will send.

During instantiation you must also pass a context for the communication
server. This context will be passed back to the callback procedure::

 generic

    type T (<>) is limited private;
    type T_Access is access T;

    with function Callback
      (Server     : in String;
       Name       : in String;
       Context    : in T_Access;
       Parameters : in Parameter_Set := Null_Parameter_Set)
       return Response.Data;

 package AWS.Communication.Server is
    ...

A complete example can be found in the demos directory. Look for
:file:`com_1.adb` and :file:`com_2.adb`.

Note that this communication API is used by the Hotplug module facility,
see :ref:`Hotplug_module`.

.. _Hotplug_module:

Hotplug module
==============

.. index:: hotplug

An **Hotplug module** is a module that can by dynamically binded to a
running server. It is a Web server and the development process is very
similar to what we have seen until now :ref:`Building_an_AWS_server`.
The Hotplug module will register itself into a Web server by
sending a message using the communication API. The Hotplug module send
to the server a regular expression and an URL. The main server will
redirect all URL matching the regular expression to the Hotplug module.

Note that the main server will redirect the URL to the first matching
regular expression.

.. _Hotplug_module_-_server_activation:

Hotplug module - server activation
----------------------------------

.. highlight:: sh

The first step is to properly create the main server hotplug module
registration file. This file must list all hotplug modules that can
register into the main server. Each line have the following format::

 hotplug_module_name:password:server:port

*hotplug_module_name*

  The name of the hotplug module. You can choose any name you want. This
  name will be use during the registration process and to generate the
  password.

*password*

  The MD5 password, see below.

*server*

  The name of the server where the redirection will be made. This is for
  security reasons, main server will not permit to redirect requests to
  any other server.

*port*

  The port to use for the redirection on `server`.

You must create a password for each hotplug modules. The generated
password depends on the hotplug module name. A tool named
`aws_password` is provided with `AWS` to generate such
password. Usage is simple::

 $ aws_password <hotplug_module_name> <password>

.. highlight:: ada

Then, after starting the main server you must activate the Hotplug feature::

 AWS.Server.Hotplug.Activate (WS'Unchecked_Access, 2222, "hotplug_conf.ini");

:file:`hotplug_conf.ini` is the hotplug module registration file
described above.

.. _Hotplug_module_-_creation:

Hotplug module - creation
-------------------------

Here is how to create an Hotplug module:

* First you create a standard Web server, see :ref:`Building_an_AWS_server`::

   WS : AWS.Server.HTTP (3, 1235, False, Hotplug_CB.Hotplug'Access, False);

  Here we have a server listening to the port 1235. This server can be
  used alone if needed as any Server developed with AWS.

* Then you register the Hotplug module to the main server,
  see :ref:`AWS.Client.Hotplug`::

   Response := AWS.Client.Hotplug.Register
                 (Name     => "Hotplug_Module_Demo",
                  Password => "my_password",
                  Server   => "http://dieppe:2222",
                  Regexp   => ".*AWS.*",
                  URL      => "http://omsk:1235/");

  The hotplug module `Hotplug_Module_Demo` must have been declared
  on the main server, the password and redirection must have been
  properly recorded too for security reasons, see
  :ref:`Hotplug_module_-_server_activation`.
  This command register `Hotplug_Module_Demo` into the server running
  on the machine `dieppe` and ask it to redirect all `URL`
  containing `AWS` to the server running on machine `omsk` on
  port `1235`.

* When the Hotplug module is stopped, you must unregister it::

   Response := AWS.Client.Hotplug.Unregister
                 (Name     => "Hotplug_Module_Demo",
                  Password => "my_password",
                  Server   => "http://dieppe:2222",
                  Regexp   => ".*AWS.*");

  Here we ask to unregister `Hotplug_Module_Demo` from server
  `dieppe`. As for the registration process a proper password must
  be specified, see :ref:`Hotplug_module_-_server_activation`.

A complete example can be found in the demos directory. Look for
:file:`main.adb` and :file:`hotplug.adb`.

.. _Server_Push:

Server Push
===========

.. index:: Server Push
.. index:: Push

This protocol is obsolescent, it is hightly recommended to use the
WebSockets now. See :ref:`WebSockets`.

Server Push is a feature that let the Web Server send continuously
data to client's  Web Browser or client applications. The client does
not have to reload at periodic time (which is what is called client
pull) to have the data updated, each time the server send a piece of
data it gets displayed on the client.

To build a push server you need to build an instance of the
`AWS.Server.Push` package. This package takes a set of formal
parameters. Here are the step-by-step instructions to build a Push
Server:

* The data to be sent

  First you must create a type that will contains the data to be sent to
  client's browser except if it is a standard Ada type. See
  `Client_Output_Type` formal parameter.

* The data that will be streamed

  This is the representation of the data that will be sent to client's
  browser. This will be either a `String` for Web pages or
  `Stream_Element_Array` for binary data like pictures. See
  `Stream_Output_Type` formal parameter.

* The context

  It is often nice to be able to configure each client with different
  parameters if needed. This can be achieved with the Context data type
  that will be passed as parameter of the conversion function described
  below. See `Client_Environment` formal parameter.

* Provides a function to convert from the data type to be sent to
  the data that will be streamed.

  This is a function that will transform the data described on point 1
  above to the form described on point 2 above. See
  `To_Stream_Output` formal parameter.

* Build the Push Server

  To do so you just need to instantiate `AWS.Server.Push` with the
  above declarations.

* Registering new clients

  In the standard `AWS` procedure callback it is possible to register a
  client if requested. This is done by calling
  `AWS.Server.Push.Register`. It is possible to unregister a
  client using `AWS.Server.Push.Unregister`. Each client must be
  identified with a unique client ID. After registering a new client
  from the callback procedure you must return the
  `AWS.Response.Socket_Taken` message. This is very important, it
  tells the server to not close this socket.

* Sending the data

  At this point it is possible to send data to clients. To do so
  two routines are available.

*AWS.Server.Push.Send_To*

    .. index:: Send_To

    To send a piece of data to a specific client identified by its
    client ID.

*AWS.Server.Push.Send*

    .. index:: Send

    To send a piece of data to all clients registered on this server.

Very large Internet applications should use this feature carefully. A
push server keeps a socket reserved for each registered clients and
the number of available sockets per process is limited by the OS.

.. _Working_with_Server_sockets:

Working with Server sockets
===========================

.. index:: Working with Server sockets

With `AWS` is is possible to take out a socket from the server and give
it back later. This feature must be used carefully but it gives a lot
of flexibility. As the socket is taken away, the connection line (or slot)
is released, `AWS` can then use it to handle other requests.

This can be used to better support heavy loaded servers when some
requests need a long time to complete. Long time here means longer
than most of the other requests which should be mostly interractives
for a Web server. Of course in such a case a keep-alive connection is
kept open.

The usage in such a case is to take out the socket and put it in a
waiting line. This releases the connection for the server. When the
data to prepare the answer is ready you give back the socket to the
server.

* Take a socket from the server

  This first step is done form the callback function. A user instead of
  replying immediatly decides to take away the socket from the
  server. The first step is to record the connection socket socket by
  calling `AWS.Status.Socket`. The second step is to tell the
  server to not release this socket by returning `AWS.Response.Socket_Taken`
  from the callback function. At this point the server will continue to
  serve other clients.

  Note that this feature is used by the server push implementation,
  see :ref:`Server_Push`.

* Give back the socket to the server

  Calling `AWS.Sever.Give_Back_Socket` will register the socket for
  reuse. This socket will be placed into a spool, next time the server
  will check for incoming requests it will be picked up.

.. _Server_Log:

Server Log
==========

.. index:: logs
.. index:: Log.Start
.. index:: Log.Stop
.. index:: Log.Start_Error
.. index:: Log.Stop_Error
.. index:: Log.Flush

It is possible to have the server activity logged into the file
:file:`<progname>-Y-M-D.log`. To activate the logging you must call the
`AWS.Server.Log.Start`, and it is possible to stop logging by calling
`AWS.Server.Log.Stop`. Note that `AWS.Server.Log.Start` have
a parameter named `Auto_Flush` to control output buffering. This
parameter is False by default. If set to True, the log file will be
automatically flushed after each data. If the server logging is not
buffered, i.e. Auto_Flush is False, the log can still be flushed by
calling the `AWS.Server.Log.Flush` routine. See :ref:`AWS.Log` for
more information especially about the way rotating logs can be
setup. Using this feature it is possible to have automatic split of
the log file each day, each month or at every run. See `AWS.Log`
spec. This is very useful to avoid having very big log files.

.. highlight:: sh

The log format depend on Log_Extended_Fields configuration parameter.
If this parameter is empty, the HTTP log would have fixed apache compartible format::

 <client IP> - <auth name> - [<date and time>] "<request>" <status code> <size>

For example::

 100.99.12.1 -  - [22/Nov/2000:11:44:14] "GET /whatever HTTP/1.1" 200 1789

If the extended fields list is not empty, the log file format would have
user defined fields set::

 #Version: 1.0
 #Date: 2006-01-09 00:00:01
 #Fields: date time c-ip cs-method cs-uri cs-version sc-status sc-bytes
 2006-01-09 00:34:23 100.99.12.1 GET /foo/bar.html HTTP/1.1 200 30

Fields in the comma separated Log_Extended_Fields list could be:

*date*
  Date at which transaction completed

*time*
  Time at which transaction completed

*time-taken*
  Time taken for transaction to complete in seconds

*c-ip*
  Client side connected IP address

*c-port*
  Client side connected port

*s-ip*
  Server side connected IP address

*s-port*
  Server side connected port

*cs-method*
  HTTP request method

*cs-username*
  Client authentication username

*cs-version*
  Client supported HTTP version

*cs-uri*
  Request URI

*cs-uri-stem*
  Stem portion alone of URI (omitting query)

*cs-uri-query*
  Query portion alone of URI

*sc-status*
  Responce status code

*sc-bytes*
  Length of response message body

*cs(<header>)*
  Any header field name sent from client to server

*sc(<header>)*
  Any header field name sent from server to client

*x-<appfield>*
  Any application defined field name

`AWS` also support error log files. If activated every internal error
detected by `AWS` will gets logged into this special file.
Log file for errors would be in simple apache compartible format.
See `AWS.Server.Log.Start_Error` and `AWS.Server.Log.Stop_Error`.

For the full set of routines supporting the log facility see
:ref:`AWS.Server.Log` .

.. _Secure_server:

Secure server
=============

.. index:: Secure server
.. index:: HTTPS

It is not much difficult to use a secure server (`HTTPS`) than a
standard one. Here we describe only what is specific to an HTTPS
server.

Before going further you must check that `AWS` has been
configured with `SSL` support. See :ref:`Building`. You must also
have installed the `OpenSSL` or `GNUTLS` libraries on your system. If
this is done, you can continue reading this section.

.. _Initialization:

Initialization
--------------

.. index:: certificate

A server is configured as using the HTTPS protocol at the time it is
started. The only thing to do is to set the Start's Security parameter
to True. This will start a server and activate the `SSL` layer by
default. A secure server must use a valid certificate, the default one
is |DEFAULT_CERTIFICATE|. This certificate has been
created by the `OpenSSL` or `GNUTLS` tool and is valid until
year 2008. Yet, this certificate has not been signed. To build a
secure server user's can rely on, you must have a valid certificate
signed by one of the **Certificate Authorities**.

The certificate to be used must be specified before starting the
secure server with `AWS.Server.Set_Security`:

.. highlight:: ada

With a key and certificate files::

 AWS.Server.Set_Security
   (WS,
    Key_Filename         => "server.key",
    Certificate_Filename => "server.crt");

Or with a self-contained certificate::

 AWS.Server.Set_Security (WS, Certificate_Filename => "aws.pem");

Or using the `certificate` configuration parameter, see
:ref:`Configuration_options`.

.. _Verify_callback:

Verify callback
---------------

.. index:: Verify callback

First note that it is not necessary to use such callback to verify the
certificate validity, see :ref:`Using_a_Certificate_Authority`.

This callback will receive the client certificate as sent during SSL
handshake between the server and the client. The certificate
information can be checked for logging purpose or to impose some
restriction. Generally this callback should return the value from
`AWS.Net.SSL.Certificate.Verified`, see :ref:`AWS.Net.SSL.Certificate`.

The Verified status of the certificate is the one that has been issued
by the SSL implementation during certificate verification and can
generally be trusted.

.. _Self-signed_certificate:

Self-signed certificate
-----------------------

.. index:: Self-signed certificate

.. _Creating_a_server_certificate:

Creating a server certificate
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: server certificate

.. highlight:: sh

The goal here is not to replace the `OpenSSL` documentation but
just to present one way to create a self signed certificate for an
`HTTPS` test server. Note that `GNUTLS` offers similar tools to
generate certificates.

*Generate a RSA key*::

   $ openssl genrsa -rand <filename> -out aws-server.key

Filename must point to any file, this is used to initialized the
random seed.

*Generate the certificate*::

   $ openssl req -new -x509 -days 730 -key aws-server.key -out aws-server.cert

*Create a single self contained file (optional)*::

   $ cat aws-server.key aws-server.cert > aws.pem

A this point you can use :file:`aws.pem` with your server or the separate
:file:`server.key` and :file:`server.crt` files.

It is also possible to sign the server's key. In this case the key
won't be in plain text but will require to setup a password on the
server code for the key to be decoded. See routine Set_Password_Callback
in AWS.Net.SSL.Certificate.

*Generate a crypted RSA key*::

  $ openssl genrsa -aes128 -passout pass:<PASSWORD> -out aws-server.key

.. _Creating_a_client_certificate:

Creating a client certificate
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: client certificate

A certificate can also be used on a Web browser and passed to the
server to have a strong client authentication. A client certificate
must be `PKCS12`. The steps to generate such certificate are:

*Generate a RSA key*::

   $ openssl genrsa -des3 -out aws-client.key

Filename must point to any file, this is used to initialized the
random seed.

*Generate the certificate*::

   $ openssl req -new -x509 -days 730 -key aws-client.key -out aws-client.cert

*Create the corresponding PKCS12 certificate*::

   $ openssl pkcs12 -export -clcerts -in aws-client.cert -inkey aws-client.key -out client.p12

.. _Using_a_Certificate_Authority:

Using a Certificate Authority
-----------------------------

.. index:: CA certificate

In this section we will use a Certificate Authority to signed the
server certificates and the client certificates. Using this method is
required if the server must ensure that only clients with a valid
certificate will be able to connect to the server. The server will
verify that the client certificate received has been signed by a known
Certificate Authority.

Note that these checks are happening during the SSL handshake, so
before the user's callback.

For this to work the following configuration options must be used:

*Exchange_Certificate*
  To request that the client certificate be sent.

*Trusted_CA*
  The file containing the certificate of the Certificate Authority we
  trust. The CA which has signed the client's certificate.

*Certificate_Required*
  If no certificate has been received from the client the server will
  reject the connection. If this is not set, we can still validate the
  client's certificate in the verify callback, see :ref:`Verify_callback`
  and for example log the connecting users.

.. _Initializing_the_Certificate_Authority:

Initializing the Certificate Authority
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: CA, Certificate Authority

First the Certificate Authority must be initialized on the
computer. This is heavily dependent on the actual Operating System
used, describing this part is out of scope of this document.

On GNU/Debian the default setup (see default_ca in
:file:`/etc/ssl/openssl.cnf`) can be used to create a **demo**
Certificate Authority locally to test this feature::

 $ mkdir demoCA
 $ mkdir demoCA/newcerts
 $ touch demoCA/index.txt
 $ echo ABCC > demoCA/serial
 $ echo 01 > demoCA/crlnumber

.. _Creating_the_Certificate_Authority:

Creating the Certificate Authority
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: CA, Certificate Authority

*Generate a RSA key*::

   $ openssl genrsa -out private-ca.key 1024

*Generate the certificate signing request*::

   $ openssl req -new -key private-ca.key -out private-ca.csr

During this step you'll be asked for information about the CA
(Country, State or Province, Organization Name...).

*Create the CA certificate*::

   $ openssl x509 -req -days 365 -in private-ca.csr -signkey private-ca.key -out private-ca.crt

This certificate will be used by AWS as the trusted CA, see
:ref:`Configuration_options`.

.. _Creating_a_CA_signed_server_certificate:

Creating a CA signed server certificate
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: server certificate

*Generate a RSA key*::

   $ openssl genrsa -out aws-server.key 1024

*Generate the certificate signing request*::

   $ openssl req -new -key aws-server.key -out aws-server.csr

During this step you'll be asked for information about the server
(Country, State or Province, Common Name...). Note that the
Organization Name here must match the one from the CA and the Common
Name should be the server fully qualified domain name.

*Create the server certificate, signed it with our CA*::

   $ openssl ca -in aws-server.csr -cert private-ca.crt -keyfile private-ca.key -out aws-server.crt

*Create a single self contained file (optional)*::

   $ cat aws-server.key aws-server.cert > aws.pem

.. _Creating_a_CA_signed_client_certificate:

Creating a CA signed client certificate
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: client certificate

*Generate a RSA key*::

   $ openssl genrsa -des3 -out aws-client.key 1024

*Generate the certificate signing request*::

   $ openssl req -new -key aws-client.key -out aws-client.csr

During this step you'll be asked for information about the client
(Country, State or Province, Common Name...). Note that the
Organization Name here must match the one from the CA and the Common
Name should be the client's one.

*Create the client certificate, signed it with our CA*::

   $ openssl ca -in aws-client.csr -cert private-ca.crt -keyfile private-ca.key -out aws-client.crt

*Create the corresponding PKCS12 certificate*::

   $ openssl pkcs12 -export -clcerts -in aws-client.crt -inkey aws-client.key -out aws-client.p12

.. _Creating_a_Certificate_Revocation_List_(CRL):

Creating a Certificate Revocation List (CRL)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: CRL
.. index:: Revocation

A Certificate Revocation List is used to revoke some client's
certificates. Those clients won't be able to connect to the secure
server anymore. Using the CA created above the following commands can
be used to create a CRL.

*Revoke the certificate*::

  $ openssl ca -cert private-ca.crt -keyfile private-ca.key -revoke aws-client.crt

*Generate the CRL*::

  $ openssl ca -cert private-ca.crt -keyfile private-ca.key -gencrl -out crl.pem -crldays 30

The file :file:`crl.pem` is the one to install on the server using the
CRL_File configuration option, see :ref:`Configuration_options`. This
file contains the list of all revoked certificates for the
corresponding CA.

.. _Security_level:

Security level
--------------

.. index:: Security level

This table summarize the security level achieved with different
settings of the security oriented configuration parameters.

+------------------------------------+-----+-------------+-------------+------------+
| Security                           | SSL | Exchange    | Certificate | Trusted CA |
|                                    |     | Certificate | required    |            |
+====================================+=====+=============+=============+============+
| Data between the client and the    | Yes | No          | No          | No         |
| server are encrypted.              |     |             |             |            |
+------------------------------------+-----+-------------+-------------+------------+
| Client can be identified, it is    | Yes | Yes         | No          | No         |
| still possible to access the server|     |             |             |            |
| without having a certificate.      |     |             |             |            |
+------------------------------------+-----+-------------+-------------+------------+
| Client are identified, a           | Yes | Yes         | Yes         | No         |
| certificate is required. The       |     |             |             |            |
| verification of the validity is up |     |             |             |            |
| to the application using the       |     |             |             |            |
| verify callback.                   |     |             |             |            |
+------------------------------------+-----+-------------+-------------+------------+
| Client are identified and verified,| Yes | Yes         | Yes         | Yes        |
| the certificate must have been     |     |             |             |            |
| signed by a Certificate Authority. |     |             |             |            |
| It is not possible to access the   |     |             |             |            |
| server without a valid certificate.|     |             |             |            |
+------------------------------------+-----+-------------+-------------+------------+

.. _Protocol:

Protocol
--------

.. index:: SSL
.. index:: TLS

There are different security options, either `SSLv2`, `SSLv3` or
`TLSv1`. `SSLv2` and `SSLv3` are supported by most if
not all Web browsers. These are the default protocol used by
`AWS`.

`TLSv1` is not supported at this point.

.. _Unexpected_exception_handler:

Unexpected exception handler
============================

.. index:: exception handler

When `AWS` detects an internal problem, it calls a specific
handler. This handler can be used to log the error, send an alert
message or build the answer to be sent back to the client's browser.

.. highlight:: ada

Here is the spec for this handler::

 type Unexpected_Exception_Handler is access
   procedure (E      : in     Ada.Exceptions.Exception_Occurrence;
              Log    : in out AWS.Log.Object;
              Error  : in     Data;
              Answer : in out Response.Data);

The handler can be called in two modes:

*Non fatal error (Error.Fatal is False)*

  In this case `AWS` will continue working without problem. A
  bug has been detected but it was not fatal to the thread (slot in
  `AWS`'s terminology) handling. In this case it is possible to
  send back an application level message to the client's browser. For
  that you just have to fill the unexpected handler's `Answer` parameter
  with the right response message. The `Error` parameter receive
  information about the problem, see :ref:`AWS.Exceptions`.

*Fatal error (Error.Fatal is True)*

  In this case `AWS` will continue working but a thread (slot number
  `Error.Slot` in `AWS`'s terminology) will be killed. It means
  that `AWS` will have lost one the simultaneous connection
  handler. The server will continue working unless it was the last slot handler
  available. Note that a Fatal error means an `AWS` internal bug
  and it should be reported if possible. In this mode there is no way to
  send back an answer to the client's browser and `Error` value must
  be ignored.

The default handler for unexpected exceptions send a message to
standard error for fatal errors. For non fatal errors it log a message
(if the error log is activated for the server) and send back a message
back to the client. The message is either a built-in one or, if present
in the server's directory, the content of the :file:`500.tmplt` file.
This templates can used the following tags:

*AUTH_MODE*

  The authorization mode (Either NONE, BASIC or DIGEST).

*EXCEPTION*

  Exception information with traceback if activated.

*HTTP_VERSION*

  Either HTTP/1.0 or HTTP/1.1

*METHOD*

  The request method (Either GET, HEAD, POST or PUT)

*PAYLOAD*

  The full `XML` payload for `SOAP` request.

*PEERNAME*

  The IP address of the client

*SOAP_ACTION*

  Either True or False. Set to True for a `SOAP` request.

*URI*

  The complete URI

For more information see :ref:`AWS.Server` and :ref:`AWS.Exceptions`.

.. _Socket_log:

Socket log
==========

.. index:: Socket log

To ease `AWS` applications debugging it is possible to log all data
sent/received to/from the sockets. For this you need to call the
`AWS.Net.Log.Start` routine by passing a write procedure
callback. You have to create such procedure or use one read-to-use
provided in `AWS.Net.Log.Callbacks` package.

For more information see :ref:`AWS.Net.Log` and :ref:`AWS.Net.Log.Callbacks`.

.. _Client_side:

Client side
===========

.. index:: Client protocol
.. index:: client HTTP

`AWS` is not only a server it also implement the HTTP and HTTPS
protocol from the client side. For example with `AWS` it is
possible to get a Web page content using the `AWS.Client` API,
see :ref:`AWS.Client`.

It also support client **Keep-Alive** connections. It is then possible to
request many URI from the same server using the same connection
(i.e. the same sockets).

`AWS` client API also support proxy, proxy authentication and Web server
authentication. Only basic (and not digest) authentication is
supported at this time.

Let's say that you want to retrieve the `contrib.html` Web page from
Pascal Obry's homepage which is `http://perso.wanadoo.fr/pascal.obry <http://perso.wanadoo.fr/pascal.obry>`_. The
code to do so is::

 Data := Client.Get
           (URL => "http://perso.wanadoo.fr/pascal.obry/contrib.html");

From there you can ask for the result's content type::

 if Response.Content_Type (Data) = "text/html" then
    ...
 end if;

Or using the MIME types defined in `AWS.MIME` unit::

 if Response.Content_Type (Data) = MIME.Text_HTML then
    ...
 end if;

And display the content if it is some kind of text data::

 Text_IO.Put_Line (Response.Message_Body (Data));

If the content is some kind of binary data (executable, PNG image, Zip
archive...), then it is possible to write the result to a file for
example. Look at the `agent` program in the `demos`
directory.

If the Web page is protected and you must pass the request through an
authenticating proxy, the call will becomes::

 Data := Client.Get
           (URL        => "http://www.mydomain.net/protected/index.html"
            User       => "me",
            Pwd        => "mypwd",
            Proxy      => "192.168.67.1",
            Proxy_User => "puser",
            Proxy_Pwd  => "ppwd");

.. index:: upload, client

The client upload protocol is implemented. Using `AWS.Client.Upload` it
is possible to send a file to a server which support the file upload protocol.
