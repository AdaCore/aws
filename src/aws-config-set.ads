------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                               ACT-Europe                                 --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  $Id$

--  This package can be used to Set any AWS parameters.

package AWS.Config.Set is

   ------------------------
   -- Per Server Options --
   ------------------------

   procedure Server_Name (O : in out Object; Value : in String);
   --  This is the name of the server as set by AWS.Server.Start.

   procedure WWW_Root (O : in out Object; Value : in String);
   --  This is the root directory name for the server. This variable is not
   --  used internally by AWS. It is supposed to be used by the callback
   --  procedures who want to retreive physical objects (images, Web
   --  pages...). The default value is the current working directory.

   procedure Admin_URI (O : in out Object; Value : in String);
   --  This is the name of the admin server page as set by AWS.Server.Start.

   procedure Server_Port (O : in out Object; Value : in Positive);
   --  This is the server port as set by the HTTP object declaration.

   procedure Security (O : in out Object; Value : in Boolean);
   --  Enable security (HTTPS/SSL) if Value is True.

   procedure Hotplug_Port (O : in out Object; Value : in Positive);
   --  This is the hotplug communication port needed to register and
   --  un-register an hotplug module.

   procedure Max_Connection (O : in out Object; Value : in Positive);
   --  This is the max simultaneous connections as set by the HTTP object
   --  declaration.

   procedure Accept_Queue_Size (O : in out Object; Value : in Positive);
   --  This is the size of the queue for the incoming requests. Higher this
   --  value will be and less "connection refused" will be reported to the
   --  client.

   procedure Log_File_Directory (O : in out Object; Value : in String);
   --  This point to the directory where log files will be written. The
   --  directory returned will end with a directory separator.

   procedure Log_Filename_Prefix (O : in out Object; Value : in String);
   --  This is the prefix to use for the log filename.

   procedure Log_Split_Mode (O : in out Object; Value : in String);
   --  This is split mode for the log file. Possible values are : Each_Run,
   --  Daily, Monthly and None. Any other values will raise an exception.

   procedure Upload_Directory (O : in out Object; Value : in String);
   --  This point to the directory where uploaded files will be stored. The
   --  directory returned will end with a directory separator.

   procedure Session (O : in out Object; Value : in Boolean);
   --  Enable session handling is Value is True.

   procedure Cleaner_Wait_For_Client_Timeout
     (O     : in out Object;
      Value : in     Duration);
   --  Number of seconds to timout on waiting for a client request.
   --  This is a timeout for regular cleaning task.

   procedure Cleaner_Client_Header_Timeout
     (O     : in out Object;
      Value : in     Duration);
   --  Number of seconds to timout on waiting for client header.
   --  This is a timeout for regular cleaning task.

   procedure Cleaner_Client_Data_Timeout
     (O     : in out Object;
      Value : in     Duration);
   --  Number of seconds to timout on waiting for client message body.
   --  This is a timeout for regular cleaning task.

   procedure Cleaner_Server_Response_Timeout
     (O     : in out Object;
      Value : in     Duration);
   --  Number of seconds to timout on waiting for client to accept answer.
   --  This is a timeout for regular cleaning task.

   procedure Force_Wait_For_Client_Timeout
     (O     : in out Object;
      Value : in     Duration);
   --  Number of seconds to timout on waiting for a client request.
   --  This is a timeout for urgent request when resources are missing.

   procedure Force_Client_Header_Timeout
     (O     : in out Object;
      Value : in     Duration);
   --  Number of seconds to timout on waiting for client header.
   --  This is a timeout for urgent request when resources are missing.

   procedure Force_Client_Data_Timeout
     (O     : in out Object;
      Value : in     Duration);
   --  Number of seconds to timout on waiting for client message body.
   --  This is a timeout for urgent request when resources are missing.

   procedure Force_Server_Response_Timeout
     (O     : in out Object;
      Value : in     Duration);
   --  Number of seconds to timout on waiting for client to accept answer.
   --  This is a timeout for urgent request when resources are missing.

   procedure Send_Timeout
     (O  : in out Object;
      Value : in     Duration);
   --  Number of seconds to timeout when sending chunck of data.

   procedure Receive_Timeout
     (O     : in out Object;
      Value : in     Duration);
   --  Number of seconds to timeout when receiving chunck of data.

   procedure Status_Page
     (O : in out Object;
      Value     : in     String);
   --  Filename for the status page.

   procedure Up_Image (O : in out Object; Value : in String);
   --  Filename for the up arrow image used in the status page.

   procedure Down_Image (O : in out Object; Value : in String);
   --  Filename for the down arrow image used in the status page.

   procedure Logo_Image (O : in out Object; Value : in String);
   --  Filename for the AWS logo image used in the status page.

   procedure Case_Sensitive_Parameters (O : in out Object; Value : in Boolean);
   --  Parameters are handled with the case if Value is True.

   -------------------------
   -- Per Process Options --
   -------------------------

   procedure Session_Cleanup_Interval (Value : in Duration);
   --  Number of seconds between each run of the cleaner task to remove
   --  obsolete session data.

   procedure Session_Lifetime (Value : in Duration);
   --  Number of seconds to keep a session if not used. After this period the
   --  session data is obsoleted and will be removed during new cleanup.

   procedure Parameter
     (Config        : in out Object;
      Name          : in     String;
      Value         : in     String;
      Error_Context : in     String := "");
   --  Set one of the AWS HTTP per server parameters. Raises Constraint_Error
   --  in case of wrong parameter name or wrong parameter value.
   --  Error_Context may contain additional information about the parameter.
   --  This  message will be added to the Constraint_Error exception.
   --  One way to use Error_Context is to set it with information about
   --  where this parameter come form.

   procedure Parameter
     (Name          : in String;
      Value         : in String;
      Error_Context : in String := "");
   --  Set one of the AWS HTTP per process parameters. See description above.

private

   pragma Inline
     (Server_Name,
      WWW_Root,
      Admin_URI,
      Server_Port,
      Security,
      Hotplug_Port,
      Max_Connection,
      Log_File_Directory,
      Upload_Directory,
      Session,
      Session_Cleanup_Interval,
      Session_Lifetime,
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
      Up_Image,
      Down_Image,
      Logo_Image,
      Case_Sensitive_Parameters);

end AWS.Config.Set;
