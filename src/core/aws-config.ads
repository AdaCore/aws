------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2020, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

pragma Ada_2012;

--  This package provide an easy way to handle server configuration options.
--
--  If initialization of this package is not done all functions below will
--  return the default value as declared in AWS.Default.

with System;

with GNAT.Regexp;

private with Ada.Strings.Unbounded;
private with AWS.Containers.String_Vectors;
private with AWS.Default;

package AWS.Config is

   type Object is private;

   Default_Config : constant Object;

   --  For the external configuration to be loaded either Get_Current or
   --  Load_Config must be called explicitely.

   function Get_Current return Object;
   --  Returns a configuration record. This is the properties as read in files
   --  'aws.ini' and 'progname.ini'. This configuration object holds only the
   --  per-server options.

   procedure Load_Config;
   --  Load configuration and store it into an internal object. This can be
   --  called when only some server-wide configuration are to be set from
   --  .ini files for example.

   ------------------------
   -- Per Server options --
   ------------------------

   ------------
   -- Server --
   ------------

   function Server_Name (O : Object) return String with Inline;
   --  This is the name of the server as set by AWS.Server.Start

   function Protocol_Family (O : Object) return String with Inline;
   --  Server protocol family. Family_Inet for IPv4, Family_Inet6 for IPv6 and
   --  Family_Unspec for unspecified protocol family.

   function IPv6_Only (O : Object) return Boolean with Inline;
   --  IPv6 server accepts only IPv6 connections

   function Server_Host (O : Object) return String with Inline;
   --  This is the server host. Can be used if the computer has a more than
   --  one IP address. It is possible to have two servers at the same port
   --  on the same machine, both being binded on different IP addresses.

   function Server_Port (O : Object) return Natural with Inline;
   --  This is the server port as set by the HTTP object declaration

   function Hotplug_Port (O : Object) return Positive with Inline;
   --  This is the hotplug communication port needed to register and
   --  un-register an hotplug module.

   function Session (O : Object) return Boolean with Inline;
   --  Returns True if the server session is activated

   function Case_Sensitive_Parameters (O : Object) return Boolean with Inline;
   --  HTTP parameters are case sensitive

   function Session_Name (O : Object) return String with Inline;
   --  Name of the cookie session

   function Session_Private_Name (O : Object) return String with Inline;
   --  Name of the private cookie session

   function Server_Priority (O : Object) return System.Any_Priority
     with Inline;
   --  Returns the priority used by the HTTP and WebSockets servers

   function Server_Header (O : Object) return String with Inline;
   --  Returns the Server header value

   ----------------
   -- Connection --
   ----------------

   function Max_Connection (O : Object) return Positive with Inline;
   --  This is the max simultaneous connections as set by the HTTP object
   --  declaration.

   function Send_Buffer_Size (O : Object) return Natural with Inline;
   --  This is the socket buffer size used for sending data. Increasing this
   --  value will give better performances on slow or long distances
   --  connections.

   function TCP_No_Delay (O : Object) return Boolean with Inline;
   --  Returns wether the TCP_NODELAY option is set for this server

   function Free_Slots_Keep_Alive_Limit (O : Object) return Natural
     with Inline;
   --  The minimum number of free slots where keep-alive connections are still
   --  enabled. After this limit no more keep-alive connection will be
   --  accepted by the server. This parameter must be used for heavy-loaded
   --  servers to make sure the server will never run out of slots. This limit
   --  must be less than Max_Connection.

   function Keep_Alive_Force_Limit (O : Object) return Positive with Inline;
   --  Server could have more than Max_Connection keep-alive sockets. Keep
   --  alive sockets are waiting for client input in the internal server socket
   --  set. This parameter defines the maximum number of keep alive sockets
   --  processed by the server with standard timeouts. If number of keep-alive
   --  sockets becomes more than Keep_Alive_Force_Limit the server starts to
   --  use shorter timeouts. If this parameter is not defined in the
   --  configuration, the server uses Max_Connection * 2 as value.

   function Keep_Alive_Close_Limit (O : Object) return Positive with Inline;
   --  This parameter defines the limit of keep alive sockets in the internal
   --  server socket set. If the number of sockets in socket set became more
   --  than Keep_Alive_Close_Limit, most close to timeout socket would be
   --  closed. If this parameter is not defined in the configuration,
   --  the server uses Max_Connection * 4 as value.

   function Accept_Queue_Size (O : Object) return Positive with Inline;
   --  This is the size of the queue for the incoming requests. Higher this
   --  value will be and less "connection refused" will be reported to the
   --  client.

   function Line_Stack_Size (O : Object) return Positive with Inline;
   --  HTTP lines stack size

   function Reuse_Address (O : Object) return Boolean with Inline;
   --  Returns true if bind is allowed to reuse an address (not waiting for
   --  the delay between two bind to the same port).

   ----------
   -- Data --
   ----------

   function WWW_Root (O : Object) return String with Inline;
   --  This is the root directory name for the server. This variable is not
   --  used internally by AWS. It is supposed to be used by the callback
   --  procedures who want to retrieve physical objects (images, Web pages...).
   --  The default value is the current working directory. The returned
   --  directory ends with a directory separator.

   function Upload_Directory (O : Object) return String with Inline;
   --  This point to the directory where uploaded files will be stored. The
   --  directory returned will end with a directory separator.

   function Upload_Size_Limit (O : Object) return Positive with Inline;
   --  Size limit for the client uploading data before calling the user's
   --  callback or dispatcher handler. User can call
   --  AWS.Status.Is_Body_Uploaded to check if client data is uploaded or not
   --  because of this limit. User can still approve the uploading data above
   --  this limit by using AWS.Server.Get_Message_Body.

   function Directory_Browser_Page (O : Object) return String with Inline;
   --  Filename for the directory browser template page

   function Max_POST_Parameters (O : Object) return Positive with Inline;
   --  Returns the maximum number of POST parameters handled. Past this limit
   --  the exception Too_Many_Parameters is raised.

   ---------
   -- Log --
   ---------

   function Log_Activated (O : Object) return Boolean with Inline;
   --  Whether the default log should be activated

   function Log_File_Directory (O : Object) return String with Inline;
   --  This point to the directory where log files will be written. The
   --  directory returned will end with a directory separator.

   function Log_Filename_Prefix (O : Object) return String with Inline;
   --  This is the prefix to use for the log filename

   function Log_Split_Mode (O : Object) return String with Inline;
   --  This is split mode for the log file. Possible values are : Each_Run,
   --  Daily, Monthly and None. Any other values will raise an exception.

   function Log_Size_Limit (O : Object) return Natural with Inline;

   generic
      with procedure Field_Id (Id : String);
   procedure Log_Extended_Fields_Generic_Iterate (O : Object);
   --  Calls procedure Field_Id for each extended http log field identifier

   function Log_Extended_Fields_Length (O : Object) return Natural with Inline;
   --  Returns the number of extended http log fileds identifiers.
   --  If returned value is zero then http log is not extended.

   function Error_Log_Activated (O : Object) return Boolean with Inline;
   --  Whether the error log should be activated

   function Error_Log_Filename_Prefix (O : Object) return String with Inline;
   --  This is the prefix to use for the log filename

   function Error_Log_Split_Mode (O : Object) return String with Inline;
   --  This is split mode for the log file. Possible values are : Each_Run,
   --  Daily, Monthly and None. Any other values will raise an exception.

   ------------
   -- Status --
   ------------

   function Admin_Password (O : Object) return String with Inline;
   --  The admin password

   function Admin_Realm (O : Object) return String with Inline;
   --  The admin password

   function Admin_URI (O : Object) return String with Inline;
   --  This is the name of the admin server page as set by AWS.Server.Start.
   --  It is also known as the status page.

   function Status_Page (O : Object) return String with Inline;
   --  Filename for the status template page

   function Up_Image (O : Object) return String with Inline;
   --  Filename for the up arrow image used in the status page

   function Down_Image (O : Object) return String with Inline;
   --  Filename for the down arrow image used in the status page

   function Logo_Image (O : Object) return String with Inline;
   --  Filename for the AWS logo image used in the status page

   --------------
   -- Timeouts --
   --------------

   function Cleaner_Wait_For_Client_Timeout (O : Object) return Duration
     with Inline;
   --  Number of seconds to timout on waiting for a client request.
   --  This is a timeout for regular cleaning task.

   function Cleaner_Client_Header_Timeout (O : Object) return Duration
     with Inline;
   --  Number of seconds to timout on waiting for client header.
   --  This is a timeout for regular cleaning task.

   function Cleaner_Client_Data_Timeout (O : Object) return Duration
     with Inline;
   --  Number of seconds to timout on waiting for client message body.
   --  This is a timeout for regular cleaning task.

   function Cleaner_Server_Response_Timeout (O : Object) return Duration
     with Inline;
   --  Number of seconds to timout on waiting for client to accept answer.
   --  This is a timeout for regular cleaning task.

   function Force_Wait_For_Client_Timeout (O : Object) return Duration
     with Inline;
   --  Number of seconds to timout on waiting for a client request.
   --  This is a timeout for urgent request when resources are missing.

   function Force_Client_Header_Timeout (O : Object) return Duration
     with Inline;
   --  Number of seconds to timout on waiting for client header.
   --  This is a timeout for urgent request when resources are missing.

   function Force_Client_Data_Timeout (O : Object) return Duration with Inline;
   --  Number of seconds to timout on waiting for client message body.
   --  This is a timeout for urgent request when resources are missing.

   function Force_Server_Response_Timeout (O : Object) return Duration
     with Inline;
   --  Number of seconds to timout on waiting for client to accept answer.
   --  This is a timeout for urgent request when resources are missing.

   function Send_Timeout (O : Object) return Duration with Inline;
   --  Number of seconds to timeout when sending chunck of data

   function Receive_Timeout (O : Object) return Duration with Inline;
   --  Number of seconds to timeout when receiving chunck of data

   --------------
   -- Security --
   --------------

   function Check_URL_Validity (O : Object) return Boolean with Inline;
   --  Server have to check URI for validity. For example it checks that an
   --  URL does not reference a resource above the Web root.

   function Security (O : Object) return Boolean with Inline;
   --  Is the server working through th SSL

   function Certificate (O : Object) return String with Inline;
   --  Returns the certificate to be used with the secure server. Returns the
   --  empty string if the server is not a secure one.

   function Key (O : Object) return String with Inline;
   --  Returns the key to be used with the secure server. Returns the
   --  empty string if the server is not a secure one.

   function Security_Mode (O : Object) return String with Inline;
   --  Returns the security mode to be used with the secure server. Returns the
   --  empty string if the server is not a secure one.

   function Cipher_Priorities (O : Object) return String with Inline;
   --  Returns the cipher priorities for the security communication

   function TLS_Ticket_Support (O : Object) return Boolean with Inline;
   --  Is security communication side has support stateless TLS session
   --  resumption. See RFC 5077.

   function Exchange_Certificate (O : Object) return Boolean with Inline;
   --  Returns True if the client is requested to send its certificate to the
   --  server.

   function Certificate_Required (O : Object) return Boolean with Inline;
   --  Returns True if the server must abort the connection if the
   --  client did not provide trusted certificate. If this option is set
   --  the Exchange_Certificate must also be set.

   function Trusted_CA (O : Object) return String with Inline;
   --  Returns the filename containing a list of trusted CA, this is to be used
   --  with the Exchange_Certificate option. The filename is on bundle of CAs
   --  that can be trusted. A client certificate signed with one of those CA
   --  will be accetped by the server.

   function CRL_File (O : Object) return String with Inline;
   --  Returns the filename containing the Certificate Revocation List. This
   --  list is used by the server to check for revoked certificate.

   function SSL_Session_Cache_Size (O : Object) return Natural with Inline;
   --  Returns SSL session cashe size

   -------------------------
   -- Per Process options --
   -------------------------

   function Session_Cleanup_Interval return Duration with Inline;
   --  Number of seconds between each run of the cleaner task to remove
   --  obsolete session data.

   function Session_Lifetime return Duration with Inline;
   --  Number of seconds to keep a session if not used. After this period the
   --  session data is obsoleted and will be removed during next cleanup.

   function Session_Id_Length return Positive with Inline;
   --  Returns the length (number of characters) of the session id

   function Session_Cleaner_Priority return System.Any_Priority with Inline;
   --  Returns the priority used by the session cleaner task

   function Service_Priority return System.Any_Priority with Inline;
   --  Returns the priority used by the others services (SMTP server, Jabber
   --  server, Push server...).

   function Config_Directory return String with Inline;
   --  Directory where AWS parameter files are located

   function Disable_Program_Ini return Boolean with Inline;
   --  Whether the <program_name>.ini file should be read

   function Transient_Cleanup_Interval return Duration with Inline;
   --  Number of seconds between each run of the cleaner task to remove
   --  transient pages.

   function Transient_Lifetime return Duration with Inline;
   --  Number of seconds to keep a transient page. After this period the
   --  transient page is obsoleted and will be removed during next cleanup.

   function Max_Concurrent_Download return Positive with Inline;
   --  Number of maximum concurrent download supported by the download manager
   --  service.

   function MIME_Types return String with Inline;
   --  Returns the file name of the MIME types to use

   function Input_Line_Size_Limit return Positive with Inline;
   --  Limit of the HTTP protocol text lines length

   function Context_Lifetime return Duration with Inline;
   --  Number of seconds to keep a context if not used. After this period the
   --  context data is obsoleted and will be removed during next cleanup.

   function Max_WebSocket_Handler return Positive with Inline;
   --  This is the max simultaneous connections handling WebSocket's messages

   function WebSocket_Message_Queue_Size return Positive with Inline;
   --  This is the size of the queue containing incoming messages

   function WebSocket_Send_Message_Queue_Size return Positive with Inline;
   --  This is the size of the queue containing messages to send

   function Max_WebSocket return Positive with Inline;
   --  The maximum number of simultaneous WebSocket opened. Note that that
   --  there could be more WebSocket registered when counting the closing
   --  WebSockets.

   function WebSocket_Timeout return Duration with Inline;
   --  Returns the WebSocket activity timeout. After this number of seconds
   --  without any activity the WebSocket can be closed when needed.

   function Is_WebSocket_Origin_Set return Boolean with Inline;
   --  Returns True if the Origin has been set

   function WebSocket_Origin return GNAT.Regexp.Regexp;
   --  This is regular expression to restrict WebSocket to a specific origin

   function WebSocket_Origin return String;
   --  This is the string regular expression to restrict WebSocket to a
   --  specific origin.

   function WebSocket_Priority return System.Any_Priority;
   --  Set the priority used by the WebSocket service

   function User_Agent return String with Inline;
   --  Returns the User_Agent header value

private

   use Ada.Strings.Unbounded;

   package SV renames AWS.Containers.String_Vectors;

   --  List of token (keyword) recognized by the parser. There must be one
   --  entry for every option name to be handled.

   type Parameter_Name is
      --  Per server option
     (Server_Name,
      WWW_Root,
      Admin_URI,
      Admin_Password,
      Admin_Realm,
      Protocol_Family,
      IPv6_Only,
      Server_Host,
      Server_Port,
      Server_Priority,
      Server_Header,
      Security,
      Certificate,
      Key,
      Security_Mode,
      Cipher_Priorities,
      TLS_Ticket_Support,
      Exchange_Certificate,
      Certificate_Required,
      Trusted_CA,
      CRL_File,
      SSL_Session_Cache_Size,
      Hotplug_Port,
      Max_Connection,
      Send_Buffer_Size,
      TCP_No_Delay,
      Free_Slots_Keep_Alive_Limit,
      Keep_Alive_Force_Limit,
      Keep_Alive_Close_Limit,
      Accept_Queue_Size,
      Log_File_Directory,
      Log_Filename_Prefix,
      Log_Extended_Fields,
      Log_Split_Mode,
      Log_Size_Limit,
      Log_Activated,
      Error_Log_Filename_Prefix,
      Error_Log_Split_Mode,
      Error_Log_Activated,
      Upload_Directory,
      Upload_Size_Limit,
      Session,
      Session_Name,
      Session_Private_Name,
      Cleaner_Wait_For_Client_Timeout,
      Cleaner_Client_Header_Timeout,
      Cleaner_Client_Data_Timeout,
      Cleaner_Server_Response_Timeout,
      Force_Wait_For_Client_Timeout,
      Force_Client_Header_Timeout,
      Force_Client_Data_Timeout,
      Force_Server_Response_Timeout,
      Send_Timeout,
      Receive_Timeout,
      Status_Page,
      Directory_Browser_Page,
      Up_Image,
      Down_Image,
      Logo_Image,
      Line_Stack_Size,
      Reuse_Address,
      Check_URL_Validity,
      Case_Sensitive_Parameters,
      Max_POST_Parameters,
      --  Per process options
      Session_Cleanup_Interval,
      Session_Lifetime,
      Session_Id_Length,
      Session_Cleaner_Priority,
      Service_Priority,
      Config_Directory,
      Disable_Program_Ini,
      User_Agent,
      Transient_Cleanup_Interval,
      Transient_Lifetime,
      Input_Line_Size_Limit,
      Max_Concurrent_Download,
      Max_WebSocket_Handler,
      MIME_Types,
      WebSocket_Message_Queue_Size,
      WebSocket_Send_Message_Queue_Size,
      WebSocket_Origin,
      WebSocket_Priority,
      Max_WebSocket,
      WebSocket_Timeout,
      Context_Lifetime);

   subtype Server_Parameter_Name is Parameter_Name
     range Server_Name .. Max_POST_Parameters;

   subtype Process_Parameter_Name is Parameter_Name
     range Session_Cleanup_Interval .. Context_Lifetime;

   type Value_Type  is (Str, Dir, Nat, Pos, Dur, Bool, Str_Vect, Regexp);

   type Values (Kind : Value_Type := Str) is record
      case Kind is
         when Str =>
            Str_Value : Unbounded_String;

         when Dir =>
            Dir_Value : Unbounded_String;

         when Pos =>
            Pos_Value : Positive;

         when Nat =>
            Nat_Value : Natural;

         when Dur =>
            Dur_Value : Duration;

         when Bool =>
            Bool_Value : Boolean;

         when Str_Vect =>
            Strs_Value : SV.Vector;

         when Regexp =>
            Is_Set     : Boolean;
            Pattern    : GNAT.Regexp.Regexp;
            Regexp_Str : Unbounded_String;
      end case;
   end record;

   type Parameter_Set is array (Parameter_Name range <>) of Values;

   function "+"
     (Str : String) return Unbounded_String renames To_Unbounded_String;

   Default_Parameters : constant Parameter_Set (Server_Parameter_Name) :=
                          (Cleaner_Wait_For_Client_Timeout =>
                             (Dur, Default.Cleaner_Wait_For_Client_Timeout),

                           Cleaner_Client_Header_Timeout   =>
                             (Dur, Default.Cleaner_Client_Header_Timeout),

                           Cleaner_Client_Data_Timeout     =>
                             (Dur, Default.Cleaner_Client_Data_Timeout),

                           Cleaner_Server_Response_Timeout =>
                             (Dur, Default.Cleaner_Server_Response_Timeout),

                           Force_Wait_For_Client_Timeout   =>
                             (Dur, Default.Force_Wait_For_Client_Timeout),

                           Force_Client_Header_Timeout     =>
                             (Dur, Default.Force_Client_Header_Timeout),

                           Force_Client_Data_Timeout       =>
                             (Dur, Default.Force_Client_Data_Timeout),

                           Force_Server_Response_Timeout   =>
                             (Dur, Default.Force_Server_Response_Timeout),

                           Send_Timeout                    =>
                             (Dur, Default.Send_Timeout),

                           Receive_Timeout                 =>
                             (Dur, Default.Receive_Timeout),

                           Status_Page                     =>
                             (Str, +Default.Status_Page),

                           Directory_Browser_Page          =>
                             (Str, +Default.Directory_Browser_Page),

                           Up_Image                        =>
                             (Str, +Default.Up_Image),

                           Down_Image                      =>
                             (Str, +Default.Down_Image),

                           Logo_Image                      =>
                             (Str, +Default.Logo_Image),

                           Admin_Password                  =>
                             (Str, +Default.Admin_Password),

                           Admin_Realm                     =>
                             (Str, +Default.Admin_Realm),

                           Admin_URI                       =>
                             (Str, +Default.Admin_URI),

                           Server_Name                     =>
                             (Str, +Default.Server_Name),

                           WWW_Root                        =>
                             (Dir, +Default.WWW_Root),

                           Log_File_Directory              =>
                             (Dir, +Default.Log_File_Directory),

                           Log_Filename_Prefix             =>
                             (Str, +Default.Log_Filename_Prefix),

                           Log_Extended_Fields             =>
                             (Str_Vect, SV.Empty_Vector),

                           Log_Split_Mode                  =>
                             (Str, +Default.Log_Split_Mode),

                           Log_Size_Limit                  =>
                             (Nat, Default.Log_Size_Limit),

                           Log_Activated                   =>
                             (Bool, Default.Log_Activated),

                           Error_Log_Filename_Prefix       =>
                             (Str, +Default.Error_Log_Filename_Prefix),

                           Error_Log_Split_Mode            =>
                             (Str, +Default.Error_Log_Split_Mode),

                           Error_Log_Activated             =>
                             (Bool, Default.Error_Log_Activated),

                           Upload_Directory                =>
                             (Dir, +Default.Upload_Directory),

                           Upload_Size_Limit               =>
                             (Pos, Default.Upload_Size_Limit),

                           Max_Connection                  =>
                             (Pos, Default.Max_Connection),

                           Send_Buffer_Size                =>
                             (Nat, Default.Send_Buffer_Size),

                           TCP_No_Delay                    =>
                             (Bool, Default.TCP_No_Delay),

                           Free_Slots_Keep_Alive_Limit     =>
                             (Nat, Default.Free_Slots_Keep_Alive_Limit),

                           Keep_Alive_Force_Limit          =>
                             (Nat, Default.Keep_Alive_Force_Limit),

                           Keep_Alive_Close_Limit          =>
                             (Nat, Default.Keep_Alive_Close_Limit),

                           Accept_Queue_Size               =>
                             (Pos, Default.Accept_Queue_Size),

                           Protocol_Family                 =>
                             (Str, +Default.Protocol_Family),

                           IPv6_Only                       =>
                             (Bool, Default.IPv6_Only),

                           Server_Host                     =>
                             (Str, Null_Unbounded_String),

                           Server_Port                     =>
                             (Nat, Default.Server_Port),

                           Server_Priority                 =>
                             (Nat, Default.Server_Priority),

                           Server_Header                   =>
                             (Str, +Default.Server_Header),

                           Hotplug_Port                    =>
                             (Pos, Default.Hotplug_Port),

                           Session                         =>
                             (Bool, Default.Session),

                           Session_Name                    =>
                             (Str, +Default.Session_Name),

                           Session_Private_Name            =>
                             (Str, +Default.Session_Private_Name),

                           Security                        =>
                             (Bool, Default.Security),

                           Certificate                     =>
                             (Str, +Default.Certificate),

                           Key                             =>
                             (Str, +Default.Key),

                           Security_Mode                   =>
                             (Str, +Default.Security_Mode),

                           Cipher_Priorities               =>
                             (Str, +Default.Cipher_Priorities),

                           TLS_Ticket_Support              =>
                             (Bool, Default.TLS_Ticket_Support),

                           Exchange_Certificate            =>
                             (Bool, Default.Exchange_Certificate),

                           Certificate_Required            =>
                             (Bool, Default.Certificate_Required),

                           Trusted_CA                      =>
                             (Str, +Default.Trusted_CA),

                           CRL_File                        =>
                             (Str, +Default.CRL_File),

                           SSL_Session_Cache_Size          =>
                             (Nat, Default.SSL_Session_Cache_Size),

                           Case_Sensitive_Parameters       =>
                             (Bool, Default.Case_Sensitive_Parameters),

                           Check_URL_Validity              =>
                             (Bool, Default.Check_URL_Validity),

                           Line_Stack_Size                 =>
                             (Pos, Default.Line_Stack_Size),

                           Reuse_Address                   =>
                             (Bool, Default.Reuse_Address),

                           Max_POST_Parameters             =>
                             (Pos, Default.Max_POST_Parameters));

   type Object is record
      P : Parameter_Set (Server_Parameter_Name) := Default_Parameters;
   end record;

   Default_Config : constant Object := (P => Default_Parameters);

   Process_Options : Parameter_Set (Process_Parameter_Name) :=
                       (Session_Cleanup_Interval     =>
                          (Dur, Default.Session_Cleanup_Interval),

                        Session_Lifetime             =>
                          (Dur, Default.Session_Lifetime),

                        Session_Id_Length            =>
                          (Pos, Default.Session_Id_Length),

                        Session_Cleaner_Priority     =>
                          (Nat, Default.Session_Cleaner_Priority),

                        Service_Priority             =>
                          (Nat, Default.Service_Priority),

                        Config_Directory             =>
                          (Str, +Default.Config_Directory),

                        Disable_Program_Ini          =>
                          (Bool, Default.Disable_Program_Ini),

                        User_Agent                   =>
                          (Str, +Default.User_Agent),

                        Transient_Cleanup_Interval   =>
                          (Dur, Default.Transient_Cleanup_Interval),

                        Transient_Lifetime           =>
                          (Dur, Default.Transient_Lifetime),

                        Input_Line_Size_Limit        =>
                          (Pos, Default.Input_Line_Size_Limit),

                        Max_Concurrent_Download      =>
                          (Pos, Default.Max_Concurrent_Download),

                        Max_WebSocket                =>
                          (Pos, Default.Max_WebSocket),

                        Max_WebSocket_Handler        =>
                          (Pos, Default.Max_WebSocket_Handler),

                        MIME_Types                   =>
                          (Str, +Default.MIME_Types),

                        WebSocket_Message_Queue_Size =>
                          (Pos, Default.WebSocket_Message_Queue_Size),

                        WebSocket_Send_Message_Queue_Size =>
                          (Pos, Default.WebSocket_Send_Message_Queue_Size),

                        WebSocket_Origin             =>
                          (Regexp, False, Pattern => <>,
                           Regexp_Str             => Null_Unbounded_String),

                        WebSocket_Priority           =>
                          (Nat, Default.WebSocket_Priority),

                        WebSocket_Timeout            =>
                          (Dur, Default.WebSocket_Timeout),

                        Context_Lifetime             =>
                          (Dur, Default.Context_Lifetime));

end AWS.Config;
