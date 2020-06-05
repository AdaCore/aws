------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2020, AdaCore                     --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with AWS.Default;
with SOAP;

procedure Build is

   use Ada;
   use AWS;

   procedure Add (Var, Value : String);
   procedure Add (Var : String; Value : Natural);
   procedure Add (Var : String; Value : Duration);
   procedure Add (Var : String; Value : Boolean);
   --  Add a new var/value pair

   ---------
   -- Add --
   ---------

   procedure Add (Var, Value : String) is
   begin
      Text_IO.Put (".. |" & Var & "| replace:: **");
      if Value'Length = 0 then
         Text_IO.Put ("<not-defined>");
      else
         Text_IO.Put (Value);
      end if;
      Text_IO.Put_Line ("**");
   end Add;

   procedure Add (Var : String; Value : Natural) is
      N_Img : constant String := Natural'Image (Value);
   begin
      Add (Var, N_Img (N_Img'First + 1 .. N_Img'Last));
   end Add;

   procedure Add (Var : String; Value : Duration) is
      D_S : constant String   := Duration'Image (Value);
      I   : constant Positive := Strings.Fixed.Index (D_S, ".");
   begin
      Add (Var, D_S (D_S'First + 1 .. I + 1));
   end Add;

   procedure Add (Var : String; Value : Boolean) is
   begin
      Add (Var, Boolean'Image (Value));
   end Add;

begin
   if Command_Line.Argument_Count = 0 then
      Text_IO.Put_Line ("AWS_VERSION='" & AWS.Version & ''');
   else
      Text_IO.Put_Line ("AWS_VERSION='" & Command_Line.Argument (1) & ''');
   end if;

   Text_IO.Put_Line ("rst_epilog = """"""");

   --  Either the version from AWS or the version passed in argument

   if Command_Line.Argument_Count = 0 then
      Add ("AWS_VERSION", AWS.Version);
   else
      Add ("AWS_VERSION", Command_Line.Argument (1));
   end if;

   Add ("SOAP_VERSION", SOAP.Version);
   Add ("MAX_CONCURRENT_DOWNLOAD", Default.Max_Concurrent_Download);
   Add ("MAX_WEBSOCKET_HANDLER", Default.Max_WebSocket_Handler);
   Add ("MAX_WEBSOCKET", Default.Max_WebSocket);
   Add ("WEBSOCKET_MESSAGE_QUEUE_SIZE", Default.WebSocket_Message_Queue_Size);
   Add ("MAX_CONNECT", Default.Max_Connection);
   Add ("MIME_TYPES", Default.MIME_Types);
   Add ("KEEP_ALIVE_LIMIT", Default.Free_Slots_Keep_Alive_Limit);
   Add ("QUEUE_SIZE", Default.Accept_Queue_Size);
   Add ("SERVER_NAME", Default.Server_Name);
   Add ("SERVER_PORT", Default.Server_Port);
   Add ("HOTPLUG_PORT", Default.Hotplug_Port);
   Add ("ERROR_LOG_ACTIVATED", Default.Error_Log_Activated);
   Add ("LOG_ACTIVATED", Default.Log_Activated);
   Add ("LOG_FILE_DIR", Default.Log_File_Directory);
   Add ("LOG_SPLIT_MODE", Default.Log_Split_Mode);
   Add ("ERROR_LOG_SPLIT_MODE", Default.Error_Log_Split_Mode);
   Add ("DIRECTORY_BROWSER_PAGE", Default.Directory_Browser_Page);
   Add ("UPLOAD_DIR", Default.Upload_Directory);
   Add ("LINE_STACK_SIZE", Default.Line_Stack_Size);
   Add ("REUSE_ADDRESS", Default.Reuse_Address);
   Add ("CHECK_URL_VALIDITY", Default.Check_URL_Validity);
   Add ("DEFAULT_CERTIFICATE", Default.Certificate);
   Add ("DEFAULT_KEY", Default.Key);
   Add ("SECURITY_MODE", Default.Security_Mode);
   Add ("CIPHER_PRIORITIES", Default.Cipher_Priorities);
   Add ("EXCHANGE_CERTIFICATE", Default.Exchange_Certificate);
   Add ("CASE_SENSITIVE_PARAMETERS", Default.Case_Sensitive_Parameters);
   Add ("ADMIN_URI", Default.Admin_URI);
   Add ("CT_WAIT_FOR_CLIENT", Default.Cleaner_Wait_For_Client_Timeout);
   Add ("CT_CLIENT_HEADER", Default.Cleaner_Client_Header_Timeout);
   Add ("CT_CLIENT_DATA", Default.Cleaner_Client_Data_Timeout);
   Add ("CT_SERVER_RESPONSE", Default.Cleaner_Server_Response_Timeout);
   Add ("FT_WAIT_FOR_CLIENT", Default.Force_Wait_For_Client_Timeout);
   Add ("FT_CLIENT_HEADER", Default.Force_Client_Header_Timeout);
   Add ("FT_CLIENT_DATA", Default.Force_Client_Data_Timeout);
   Add ("FT_SERVER_RESPONSE", Default.Force_Server_Response_Timeout);
   Add ("SEND_BUFFER_SIZE", Default.Send_Buffer_Size);
   Add ("TCP_NO_DELAY", Default.TCP_No_Delay);
   Add ("SEND_TIMEOUT", Default.Send_Timeout);
   Add ("RECEIVE_TIMEOUT", Default.Receive_Timeout);
   Add ("LOGO_IMAGE", Default.Logo_Image);
   Add ("DOWN_IMAGE", Default.Down_Image);
   Add ("UP_IMAGE", Default.Up_Image);
   Add ("STATUS_PAGE", Default.Status_Page);
   Add ("SESSION", Default.Session);
   Add ("SESSION_NAME", Default.Session_Name);
   Add ("SESSION_LIFETIME", Default.Session_Lifetime);
   Add ("SESSION_CLEANUP_INTERVAL", Default.Session_Cleanup_Interval);
   Add ("TRANSIENT_LIFETIME", Default.Transient_Lifetime);
   Add ("TRANSIENT_CLEANUP_INTERVAL", Default.Transient_Cleanup_Interval);
   Add ("WebSocket_Timeout", Default.Websocket_Timeout);
   Add ("WWW_ROOT", Default.WWW_Root);
   Add ("MAX_POST_PARAMETERS", Default.Max_POST_Parameters);
   Add ("CERTIFICATE_REQUIRED", Default.Certificate_Required);
   Add ("TRUSTED_CA", Default.Trusted_CA);
   Add ("CRL_FILE", Default.CRL_File);
   Add ("SESSION_ID_LENGTH", Default.Session_Id_Length);
   Add ("TLS_TICKET_SUPPORT", Default.TLS_Ticket_Support);
   Add ("CONFIG_DIRECTORY", Default.Config_Directory);
   Add ("DISABLE_PROGRAM_INI", Default.Disable_Program_Ini);
   Add ("USER_AGENT", Default.User_Agent);
   Add ("SERVER_HEADER", Default.Server_Header);
   Text_IO.Put_Line ("""""""");
end Build;
