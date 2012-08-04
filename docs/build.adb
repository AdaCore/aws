------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

with Ada.Strings.Fixed;
with Ada.Text_IO;

with AWS.Default;
with AWS.Utils;
with SOAP;

with Templates_Parser;

procedure Build is

   use Ada;
   use AWS;
   use AWS.Utils;
   use Templates_Parser;

   File : Text_IO.File_Type;

   function Image (D : Duration) return String;
   --  D string representation

   -----------
   -- Image --
   -----------

   function Image (D : Duration) return String is
      D_S : constant String   := Duration'Image (D);
      I   : constant Positive := Strings.Fixed.Index (D_S, ".");
   begin
      return D_S (D_S'First + 1 .. I + 1);
   end Image;

   T : Translate_Set;

begin
   --  If a tag is added into this table make sure to update gen_doc.sed.tmplt

   Insert (T, Assoc ("UNDERSCORE", "_"));
   --  Tag to avoid templates parser confusion when output template tags

   Insert (T, Assoc ("AWS_VERSION", AWS.Version));
   Insert (T, Assoc ("SOAP_VERSION", SOAP.Version));
   Insert (T, Assoc ("MAX_CONCURRENT_DOWNLOAD",
                     Image (Natural'(Default.Max_Concurrent_Download))));
   Insert (T, Assoc ("MAX_WEBSOCKET_HANDLER",
                     Image (Natural'(Default.Max_WebSocket_Handler))));
   Insert (T, Assoc ("WEBSOCKET_MESSAGE_QUEUE_SIZE",
                     Image (Natural'(Default.WebSocket_Message_Queue_Size))));
   Insert (T, Assoc ("MAX_CONNECT", Image (Natural'(Default.Max_Connection))));
   Insert (T, Assoc ("MIME_TYPES", Default.MIME_Types));
   Insert (T, Assoc ("KEEP_ALIVE_LIMIT",
                     Image (Natural'(Default.Free_Slots_Keep_Alive_Limit))));
   Insert (T, Assoc ("QUEUE_SIZE", Default.Accept_Queue_Size));
   Insert (T, Assoc ("SERVER_NAME", Default.Server_Name));
   Insert (T, Assoc ("SERVER_PORT", Image (Natural'(Default.Server_Port))));
   Insert (T, Assoc ("HOTPLUG_PORT", Image (Natural'(Default.Hotplug_Port))));
   Insert (T, Assoc ("LOG_FILE_DIR", Default.Log_File_Directory));
   Insert (T, Assoc ("LOG_SPLIT_MODE", Default.Log_Split_Mode));
   Insert (T, Assoc ("ERROR_LOG_SPLIT_MODE", Default.Error_Log_Split_Mode));
   Insert (T, Assoc ("DIRECTORY_BROWSER_PAGE",
                     Default.Directory_Browser_Page));
   Insert (T, Assoc ("UPLOAD_DIR", Default.Upload_Directory));
   Insert (T, Assoc ("LINE_STACK_SIZE", Default.Line_Stack_Size));
   Insert (T, Assoc ("REUSE_ADDRESS", Default.Reuse_Address));
   Insert (T, Assoc ("CHECK_URL_VALIDITY", Default.Check_URL_Validity));
   Insert (T, Assoc ("DEFAULT_CERTIFICATE", Default.Certificate));
   Insert (T, Assoc ("DEFAULT_KEY", Default.Key));
   Insert (T, Assoc ("SECURITY_MODE", Default.Security_Mode));
   Insert (T, Assoc ("EXCHANGE_CERTIFICATE", Default.Exchange_Certificate));
   Insert (T, Assoc ("CASE_SENSITIVE_PARAMETERS",
                     Default.Case_Sensitive_Parameters));
   Insert (T, Assoc ("ADMIN_URI", Default.Admin_URI));
   Insert (T, Assoc ("CT_WAIT_FOR_CLIENT",
                     Image (Default.Cleaner_Wait_For_Client_Timeout)));
   Insert (T, Assoc ("CT_CLIENT_HEADER",
                     Image (Default.Cleaner_Client_Header_Timeout)));
   Insert (T, Assoc ("CT_CLIENT_DATA",
                     Image (Default.Cleaner_Client_Data_Timeout)));
   Insert (T, Assoc ("CT_SERVER_RESPONSE",
                     Image (Default.Cleaner_Server_Response_Timeout)));
   Insert (T, Assoc ("FT_WAIT_FOR_CLIENT",
                     Image (Default.Force_Wait_For_Client_Timeout)));
   Insert (T, Assoc ("FT_CLIENT_HEADER",
                     Image (Default.Force_Client_Header_Timeout)));
   Insert (T, Assoc ("FT_CLIENT_DATA",
                     Image (Default.Force_Client_Data_Timeout)));
   Insert (T, Assoc ("FT_SERVER_RESPONSE",
                     Image (Default.Force_Server_Response_Timeout)));
   Insert (T, Assoc ("SEND_BUFFER_SIZE", Default.Send_Buffer_Size));
   Insert (T, Assoc ("SEND_TIMEOUT", Image (Default.Send_Timeout)));
   Insert (T, Assoc ("RECEIVE_TIMEOUT", Image (Default.Receive_Timeout)));
   Insert (T, Assoc ("LOGO_IMAGE", Default.Logo_Image));
   Insert (T, Assoc ("DOWN_IMAGE", Default.Down_Image));
   Insert (T, Assoc ("UP_IMAGE", Default.Up_Image));
   Insert (T, Assoc ("STATUS_PAGE", Default.Status_Page));
   Insert (T, Assoc ("SESSION", Default.Session));
   Insert (T, Assoc ("SESSION_NAME", Default.Session_Name));
   Insert (T, Assoc ("SESSION_LIFETIME",
                     Image (Default.Session_Lifetime)));
   Insert (T, Assoc ("SESSION_CLEANUP_INTERVAL",
                     Image (Default.Session_Cleanup_Interval)));
   Insert (T, Assoc ("TRANSIENT_LIFETIME",
                     Image (Default.Transient_Lifetime)));
   Insert (T, Assoc ("TRANSIENT_CLEANUP_INTERVAL",
                     Image (Default.Transient_Cleanup_Interval)));
   Insert (T, Assoc ("WWW_ROOT", Default.WWW_Root));
   Insert (T, Assoc ("MAX_POST_PARAMETERS", Default.Max_POST_Parameters));
   Insert (T, Assoc ("CERTIFICATE_REQUIRED", Default.Certificate_Required));
   Insert (T, Assoc ("TRUSTED_CA", Default.Trusted_CA));

   --  Generates the documentation

   Text_IO.Put_Line
     (Parse ("aws.texi.tmplt", T, Keep_Unknown_Tags => True));

   --  Generates a script that can be used to create the documentation from
   --  the document template.

   Text_IO.Create (File, Text_IO.Out_File, "gen_doc.sed");

   declare
      use Strings.Fixed;
      Script : String :=
        Parse ("gen_doc.sed.tmplt", T, Keep_Unknown_Tags => True);
      I      : Natural;
   begin
      loop
         I := Index (Script, "x_");
         exit when I = 0;
         Replace_Slice (Script, I, I + 1, "@_");
      end loop;

      loop
         I := Index (Script, "_x");
         exit when I = 0;
         Replace_Slice (Script, I, I + 1, "_@");
      end loop;

      Text_IO.Put_Line (File, Script);
   end;
   Text_IO.Close (File);
end Build;
