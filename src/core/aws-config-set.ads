------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

--  This package can be used to Set any AWS parameters

package AWS.Config.Set is

   ------------------------
   -- Per Server Options --
   ------------------------

   ------------
   -- Server --
   ------------

   procedure Server_Name (O : in out Object; Value : String);
   --  This is the name of the server as set by AWS.Server.Start

   procedure Server_Host (O : in out Object; Value : String);
   --  This is the server host as set by the HTTP object declaration

   procedure Server_Port (O : in out Object; Value : Natural);
   --  This is the server port as set by the HTTP object declaration

   procedure Hotplug_Port (O : in out Object; Value : Positive);
   --  This is the hotplug communication port needed to register and
   --  un-register an hotplug module.

   procedure Session (O : in out Object; Value : Boolean);
   --  Enable session handling is Value is True

   procedure Case_Sensitive_Parameters (O : in out Object; Value : Boolean);
   --  Parameters are handled with the case if Value is True

   procedure Line_Stack_Size (O : in out Object; Value : Positive);
   --  HTTP lines stack size

   procedure MIME_Types (O : in out Object; Value : String);
   --  The name of the file containing the MIME types associations

   procedure Reuse_Address (O : in out Object; Value : Boolean);
   --  Set the reuse address policy allowing a bind without a dealy to the same
   --  address and port.

   procedure Session_Name (O : in out Object; Value : String);
   --  Name of the cookie session

   ----------------
   -- Connection --
   ----------------

   procedure Max_Connection (O : in out Object; Value : Positive);
   --  This is the max simultaneous connections as set by the HTTP object
   --  declaration.

   procedure Send_Buffer_Size (O : in out Object; Value : Positive);
   --  This is the socket buffer size used for sending data. Increasing this
   --  value will give better performances on slow or long distances
   --  connections.

   procedure Free_Slots_Keep_Alive_Limit
     (O : in out Object; Value : Natural);
   --  The minimum number of free slots where keep-alive connections are still
   --  enabled. After this limit no more keep-alive connection will be
   --  accepted by the server. This parameter must be used for heavy-loaded
   --  servers to make sure the server will never run out of slots. This limit
   --  must be less than Max_Connection.

   procedure Keep_Alive_Force_Limit (O : in out Object; Value : Natural);
   --  Define maximum number of keep alive sockets where server process it with
   --  normal timeouts. If number of keep-alive sockets become more than
   --  Keep_Alive_Force_Limit, server start to use shorter force timeouts.
   --  If this parameter not defined in configuration or defined as 0 value
   --  server use calculated value Max_Connection * 2.

   procedure Accept_Queue_Size (O : in out Object; Value : Positive);
   --  This is the size of the queue for the incoming requests. Higher this
   --  value will be and less "connection refused" will be reported to the
   --  client.

   procedure Max_Concurrent_Download (Value : Positive);
   --  Control the maximum number of parallel downloads accepted by the
   --  download manager.

   ----------
   -- Data --
   ----------

   procedure WWW_Root (O : in out Object; Value : String);
   --  This is the root directory name for the server. This variable is not
   --  used internally by AWS. It is supposed to be used by the callback
   --  procedures who want to retrieve physical objects (images, Web
   --  pages...). The default value is the current working directory.

   procedure Upload_Directory (O : in out Object; Value : String);
   --  This point to the directory where uploaded files will be stored. The
   --  directory returned will end with a directory separator.

   procedure Upload_Size_Limit (O : in out Object; Value : Positive);
   --  Set the maximum size accepted for uploaded files

   procedure Directory_Browser_Page (O : in out Object; Value : String);
   --  Filename for the directory browser template page

   procedure Max_POST_Parameters (O : in out Object; Value : Positive);
   --  Set the maximum number of POST parameters handled. Past this limit
   --  the exception Too_Many_Parameters is raised.

   ---------
   -- Log --
   ---------

   procedure Log_File_Directory (O : in out Object; Value : String);
   --  This point to the directory where log files will be written. The
   --  directory returned will end with a directory separator.

   procedure Log_Filename_Prefix (O : in out Object; Value : String);
   --  This is the prefix to use for the log filename

   procedure Log_Size_Limit (O : in out Object; Value : Natural);
   --  If Log_Size_Limit is more than zero and size of log file
   --  become more than Log_Size_Limit, log file is be split.

   procedure Log_Split_Mode (O : in out Object; Value : String);
   --  This is split mode for the log file. Possible values are : Each_Run,
   --  Daily, Monthly and None. Any other values will raise an exception.

   procedure Log_Extended_Fields (O : in out Object; Value : String);
   --  Comma separated list of the extended log field names. If this parameter
   --  is empty, the HTTP log would have fixed apache compartible format:
   --
   --  127.0.0.1 - - [25/Apr/1998:15:37:29 +0200] "GET / HTTP/1.0" 200 1363
   --
   --  If the extended fields list is not empty, the log file format would have
   --  user defined fields set:
   --
   --  #Version: 1.0
   --  #Date: 2006-01-09 00:00:01
   --  #Fields: date time cs-method cs-uri cs-version sc-status sc-bytes
   --  2006-01-09 00:34:23 GET /foo/bar.html HTTP/1.1 200 30
   --
   --  Fields in the list could be:
   --
   --  date         Date at which transaction completed
   --  time         Time at which transaction completed
   --  c-ip         Client side connected IP address
   --  c-port       Client side connected port
   --  s-ip         Server side connected IP address
   --  s-port       Server side connected port
   --  cs-method    HTTP request method
   --  cs-username  Client authentication username
   --  cs-version   Client supported HTTP version
   --  cs-uri       Request URI
   --  cs-uri-stem  Stem portion alone of URI (omitting query)
   --  cs-uri-query Query portion alone of URI
   --  sc-status    Responce status code
   --  sc-bytes     Length of response message body
   --  cs(<header>) Any header field name sent from client to server
   --  sc(<header>) Any header field name sent from server to client
   --  x-<appfield> Any application defined field name

   procedure Error_Log_Filename_Prefix (O : in out Object; Value : String);
   --  This is the prefix to use for the log filename

   procedure Error_Log_Split_Mode (O : in out Object; Value : String);
   --  This is split mode for the log file. Possible values are : Each_Run,
   --  Daily, Monthly and None. Any other values will raise an exception.

   ------------
   -- Status --
   ------------

   procedure Admin_Password (O : in out Object; Value : String);
   --  This is the password for the admin server page as set by
   --  AWS.Server.Start. The password must be created with the aws_password
   --  tool.

   procedure Admin_URI (O : in out Object; Value : String);
   --  This is the name of the admin server page as set by AWS.Server.Start

   procedure Status_Page (O : in out Object; Value : String);
   --  Filename for the status template page

   procedure Up_Image (O : in out Object; Value : String);
   --  Filename for the up arrow image used in the status page

   procedure Down_Image (O : in out Object; Value : String);
   --  Filename for the down arrow image used in the status page

   procedure Logo_Image (O : in out Object; Value : String);
   --  Filename for the AWS logo image used in the status page

   --------------
   -- Timeouts --
   --------------

   procedure Cleaner_Wait_For_Client_Timeout
     (O     : in out Object;
      Value : Duration);
   --  Number of seconds to timout on waiting for a client request.
   --  This is a timeout for regular cleaning task.

   procedure Cleaner_Client_Header_Timeout
     (O     : in out Object;
      Value : Duration);
   --  Number of seconds to timout on waiting for client header.
   --  This is a timeout for regular cleaning task.

   procedure Cleaner_Client_Data_Timeout
     (O     : in out Object;
      Value : Duration);
   --  Number of seconds to timout on waiting for client message body.
   --  This is a timeout for regular cleaning task.

   procedure Cleaner_Server_Response_Timeout
     (O     : in out Object;
      Value : Duration);
   --  Number of seconds to timout on waiting for client to accept answer.
   --  This is a timeout for regular cleaning task.

   procedure Force_Wait_For_Client_Timeout
     (O     : in out Object;
      Value : Duration);
   --  Number of seconds to timout on waiting for a client request.
   --  This is a timeout for urgent request when resources are missing.

   procedure Force_Client_Header_Timeout
     (O     : in out Object;
      Value : Duration);
   --  Number of seconds to timout on waiting for client header.
   --  This is a timeout for urgent request when resources are missing.

   procedure Force_Client_Data_Timeout
     (O     : in out Object;
      Value : Duration);
   --  Number of seconds to timout on waiting for client message body.
   --  This is a timeout for urgent request when resources are missing.

   procedure Force_Server_Response_Timeout
     (O     : in out Object;
      Value : Duration);
   --  Number of seconds to timout on waiting for client to accept answer.
   --  This is a timeout for urgent request when resources are missing.

   procedure Send_Timeout (O  : in out Object; Value : Duration);
   --  Number of seconds to timeout when sending chunck of data

   procedure Receive_Timeout (O : in out Object; Value : Duration);
   --  Number of seconds to timeout when receiving chunck of data

   --------------
   -- Security --
   --------------

   procedure Check_URL_Validity (O : in out Object; Value : Boolean);
   --  Set the check URL validity flag. If True an URL that reference a
   --  resource above the Web root will be rejected.

   procedure Security (O : in out Object; Value : Boolean);
   --  Enable security (HTTPS/SSL) if Value is True

   procedure Certificate (O : in out Object; Filename : String);
   --  Set the certificate to be used with the secure server

   procedure Key (O : in out Object; Filename : String);
   --  Set the key to be used with the secure server

   procedure Security_Mode (O : in out Object; Mode : String);
   --  Set the security mode to be used with the secure server. Only values
   --  from AWS.Net.SSL.Method can be used.

   procedure Exchange_Certificate (O : in out Object; Value : Boolean);
   --  Set to True to request the client to send its certificate to the server

   -------------------------
   -- Per Process Options --
   -------------------------

   procedure Session_Cleanup_Interval (Value : Duration);
   --  Number of seconds between each run of the cleaner task to remove
   --  obsolete session data.

   procedure Session_Lifetime (Value : Duration);
   --  Number of seconds to keep a session if not used. After this period the
   --  session data is obsoleted and will be removed during next cleanup.

   procedure Transient_Cleanup_Interval (Value : Duration);
   --  Number of seconds between each run of the cleaner task to remove
   --  transient pages.

   procedure Transient_Lifetime (Value : Duration);
   --  Number of seconds to keep a transient page. After this period the
   --  transient page is obsoleted and will be removed during next cleanup.

   procedure Context_Lifetime (Value : Duration);
   --  Number of seconds to keep a context if not used. After this period the
   --  context data is obsoleted and will be removed during next cleanup.

   procedure Parameter
     (Config        : in out Object;
      Name          : String;
      Value         : String;
      Error_Context : String := "");
   --  Set one of the AWS HTTP per server parameters. Raises Constraint_Error
   --  in case of wrong parameter name or wrong parameter value.
   --  Error_Context may contain additional information about the parameter.
   --  This  message will be added to the Constraint_Error exception.
   --  One way to use Error_Context is to set it with information about
   --  where this parameter come form.

   procedure Parameter
     (Name          : String;
      Value         : String;
      Error_Context : String := "");
   --  Set one of the AWS HTTP per process parameters. See description above

end AWS.Config.Set;
