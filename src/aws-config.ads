------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                               ACT-Europe                                 --
--                                                                          --
--  Authors: Dmitriy Anisimov - Pascal Obry                                 --
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

--  This package provide an easy way to handle server configuration options.
--
--  If initialization of this package is not done all functions below will
--  return the default value as declared in AWS.Default.

with Ada.Strings.Unbounded;

with AWS.Default;

package AWS.Config is

   use Ada.Strings.Unbounded;

   type Object is private;

   Default_Config : constant Object;

   function Get_Current return Object;
   --  Returns a configuration record. This is the properties as read in files
   --  'aws.ini' and 'progname.ini'. This configuration object holds only the
   --  per-server options.

   ------------------------
   -- Per Server options --
   ------------------------

   function Server_Name (O : in Object) return String;
   --  This is the name of the server as set by AWS.Server.Start.

   function WWW_Root (O : in Object) return String;
   --  This is the root directory name for the server. This variable is not
   --  used internally by AWS. It is supposed to be used by the callback
   --  procedures who want to retreive physical objects (images, Web
   --  pages...). The default value is the current working directory.

   function Admin_URI (O : in Object) return String;
   --  This is the name of the admin server page as set by AWS.Server.Start.

   function Server_Port (O : in Object) return Positive;
   --  This is the server port as set by the HTTP object declaration.

   function Hotplug_Port (O : in Object) return Positive;
   --  This is the hotplug communication port needed to register and
   --  un-register an hotplug module.

   function Max_Connection (O : in Object) return Positive;
   --  This is the max simultaneous connections as set by the HTTP object
   --  declaration.

   function Log_File_Directory (O : in Object) return String;
   --  This point to the directory where log files will be written. The
   --  directory returned will end with a directory separator.

   function Log_File_Prefix (O : in Object) return String;
   --  This is the prefix to use for the log filename.

   function Log_Split_Mode (O : in Object) return String;
   --  This is split mode for the log file. Possible values are : Each_Run,
   --  Daily, Monthly and None. Any other values will raise an exception.

   function Upload_Directory (O : in Object) return String;
   --  This point to the directory where uploaded files will be stored. The
   --  directory returned will end with a directory separator.

   function Session (O : in Object) return Boolean;
   --  Returns True if the server session is activated.

   function Cleaner_Wait_For_Client_Timeout (O : in Object) return Duration;
   --  Number of seconds to timout on waiting for a client request.
   --  This is a timeout for regular cleaning task.

   function Cleaner_Client_Header_Timeout (O : in Object) return Duration;
   --  Number of seconds to timout on waiting for client header.
   --  This is a timeout for regular cleaning task.

   function Cleaner_Client_Data_Timeout (O : in Object) return Duration;
   --  Number of seconds to timout on waiting for client message body.
   --  This is a timeout for regular cleaning task.

   function Cleaner_Server_Response_Timeout (O : in Object) return Duration;
   --  Number of seconds to timout on waiting for client to accept answer.
   --  This is a timeout for regular cleaning task.

   function Force_Wait_For_Client_Timeout (O : in Object) return Duration;
   --  Number of seconds to timout on waiting for a client request.
   --  This is a timeout for urgent request when ressources are missing.

   function Force_Client_Header_Timeout (O : in Object) return Duration;
   --  Number of seconds to timout on waiting for client header.
   --  This is a timeout for urgent request when ressources are missing.

   function Force_Client_Data_Timeout (O : in Object) return Duration;
   --  Number of seconds to timout on waiting for client message body.
   --  This is a timeout for urgent request when ressources are missing.

   function Force_Server_Response_Timeout (O : in Object) return Duration;
   --  Number of seconds to timout on waiting for client to accept answer.
   --  This is a timeout for urgent request when ressources are missing.

   function Send_Timeout (O : in Object) return Duration;
   --  Number of seconds to timeout when sending chunck of data.

   function Receive_Timeout (O : in Object) return Duration;
   --  Number of seconds to timeout when receiving chunck of data.

   function Status_Page (O : in Object) return String;
   --  Filename for the status page.

   function Up_Image (O : in Object) return String;
   --  Filename for the up arrow image used in the status page.

   function Down_Image (O : in Object) return String;
   --  Filename for the down arrow image used in the status page.

   function Logo_Image (O : in Object) return String;
   --  Filename for the AWS logo image used in the status page.

   function Security (O : in Object) return Boolean;
   --  Is the server working through th SSL

   function Case_Sensitive_Parameters (O : in Object) return Boolean;
   --  Http parameters are case sensitive.

   -------------------------
   -- Per Process options --
   -------------------------

   function Session_Cleanup_Interval return Duration;
   --  Number of seconds between each run of the cleaner task to remove
   --  obsolete session data.

   function Session_Lifetime return Duration;
   --  Number of seconds to keep a session if not used. After this period the
   --  session data is obsoleted and will be removed during new cleanup.

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

   --  List of token (keyword) recognized by the parser. There must be one
   --  entry for every option name to be handled.

   type Parameter_Name is
     (Server_Name,
      WWW_Root,
      Admin_URI,
      Server_Port,
      Security,
      Hotplug_Port,
      Max_Connection,
      Log_File_Directory,
      Log_File_Prefix,
      Log_Split_Mode,
      Upload_Directory,
      Session,
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
      Case_Sensitive_Parameters,
      Session_Cleanup_Interval,
      Session_Lifetime);

   subtype Server_Parameter_Name is Parameter_Name
     range Server_Name .. Case_Sensitive_Parameters;

   subtype Process_Parameter_Name is Parameter_Name
     range Session_Cleanup_Interval .. Session_Lifetime;

   type Value_Type  is (Str, Dir, Pos, Dur, Bool);

   type Values (Kind : Value_Type := Str) is record

      case Kind is
         when Str =>
            Str_Value : Unbounded_String;

         when Dir =>
            Dir_Value : Unbounded_String;

         when Pos =>
            Pos_Value : Positive;

         when Dur =>
            Dur_Value : Duration;

         when Bool =>
            Bool_Value : Boolean;

      end case;
   end record;

   type Parameter_Set is array (Parameter_Name range <>) of Values;

   Default_Parameters : constant Parameter_Set (Server_Parameter_Name)
     := (Cleaner_Wait_For_Client_Timeout =>
           (Dur, Default.Cleaner_Wait_For_Client_Timeout),

         Cleaner_Client_Header_Timeout =>
           (Dur, Default.Cleaner_Client_Header_Timeout),

         Cleaner_Client_Data_Timeout =>
           (Dur, Default.Cleaner_Client_Data_Timeout),

         Cleaner_Server_Response_Timeout =>
           (Dur, Default.Cleaner_Server_Response_Timeout),

         Force_Wait_For_Client_Timeout =>
           (Dur, Default.Force_Wait_For_Client_Timeout),

         Force_Client_Header_Timeout =>
           (Dur, Default.Force_Client_Header_Timeout),

         Force_Client_Data_Timeout =>
           (Dur, Default.Force_Client_Data_Timeout),

         Force_Server_Response_Timeout =>
           (Dur, Default.Force_Server_Response_Timeout),

         Send_Timeout =>
           (Dur, Default.Send_Timeout),

         Receive_Timeout =>
           (Dur, Default.Receive_Timeout),

         Status_Page =>
           (Str, To_Unbounded_String (Default.Status_Page)),

         Up_Image =>
           (Str, To_Unbounded_String (Default.Up_Image)),

         Down_Image =>
           (Str, To_Unbounded_String (Default.Down_Image)),

         Logo_Image =>
           (Str, To_Unbounded_String (Default.Logo_Image)),

         Admin_URI =>
           (Str, To_Unbounded_String (Default.Admin_URI)),

         Server_Name =>
           (Str, To_Unbounded_String (Default.Server_Name)),

         WWW_Root =>
           (Dir, To_Unbounded_String (Default.WWW_Root)),

         Log_File_Directory =>
           (Dir, To_Unbounded_String (Default.Log_File_Directory)),

         Log_File_Prefix =>
           (Str, To_Unbounded_String (Default.Log_File_Prefix)),

         Log_Split_Mode =>
           (Str, To_Unbounded_String (Default.Log_Split_Mode)),

         Upload_Directory =>
           (Dir, To_Unbounded_String (Default.Upload_Directory)),

         Max_Connection =>
           (Pos, Default.Max_Connection),

         Server_Port =>
           (Pos, Default.Server_Port),

         Hotplug_Port =>
           (Pos, Default.Hotplug_Port),

         Session =>
           (Bool, Default.Session),

         Security =>
           (Bool, Default.Security),

         Case_Sensitive_Parameters =>
           (Bool, Default.Case_Sensitive_Parameters));

   type Object is record
      P : Parameter_Set (Server_Parameter_Name) := Default_Parameters;
   end Record;

   Default_Config : constant Object
     := (P => Default_Parameters);

   Server_Config : Object;
   --  This variable will be updated with options found in 'aws.ini' and
   --  'progname.ini'.

   Process_Options : Parameter_Set (Process_Parameter_Name)
     := (Session_Cleanup_Interval =>
           (Dur, Default.Session_Cleanup_Interval),

         Session_Lifetime =>
           (Dur, Default.Session_Lifetime));

end AWS.Config;
