------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2004                          --
--                                ACT-Europe                                --
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

with Ada.Text_IO;
with Ada.Strings.Fixed;

with AWS.Utils;
with AWS.Default;
with SOAP;

with Templates_Parser;

procedure Build is

   use Ada;
   use AWS;
   use AWS.Utils;
   use Templates_Parser;

   function Image (D : in Duration) return String;
   --  D string representation

   -----------
   -- Image --
   -----------

   function Image (D : in Duration) return String is
      D_S : constant String   := Duration'Image (D);
      I   : constant Positive := Strings.Fixed.Index (D_S, ".");
   begin
      return D_S (D_S'First + 1 .. I + 1);
   end Image;


   T : Translate_Table
     := (Assoc ("AWS_VERSION", AWS.Version),
         Assoc ("SOAP_VERSION", SOAP.Version),
         Assoc ("MAX_CONNECT", Image (Default.Max_Connection)),
         Assoc ("KEEP_ALIVE_LIMIT",
                Image (Default.Free_Slots_Keep_Alive_Limit)),
         Assoc ("QUEUE_SIZE", Default.Accept_Queue_Size),
         Assoc ("SERVER_NAME", Default.Server_Name),
         Assoc ("SERVER_PORT", Image (Default.Server_Port)),
         Assoc ("HOTPLUG_PORT", Image (Default.Hotplug_Port)),
         Assoc ("LOG_FILE_DIR", Default.Log_File_Directory),
         Assoc ("LOG_SPLIT_MODE", Default.Log_Split_Mode),
         Assoc ("ERROR_LOG_SPLIT_MODE", Default.Error_Log_Split_Mode),
         Assoc ("DIRECTORY_BROWSER_PAGE", Default.Directory_Browser_Page),
         Assoc ("UPLOAD_DIR", Default.Upload_Directory),
         Assoc ("LINE_STACK_SIZE", Default.Line_Stack_Size),
         Assoc ("CHECK_URL_VALIDITY", Default.Check_URL_Validity),
         Assoc ("DEFAULT_CERTIFICATE", Default.Certificate),
         Assoc ("DEFAULT_KEY", Default.Key),
         Assoc ("SECURITY_MODE", Default.Security_Mode),
         Assoc ("EXCHANGE_CERTIFICATE", Default.Exchange_Certificate),
         Assoc ("CASE_SENSITIVE_PARAMETERS",
                Default.Case_Sensitive_Parameters),
         Assoc ("ADMIN_URI", Default.Admin_URI),
         Assoc ("CT_WAIT_FOR_CLIENT",
                Image (Default.Cleaner_Wait_For_Client_Timeout)),
         Assoc ("CT_CLIENT_HEADER",
                Image (Default.Cleaner_Client_Header_Timeout)),
         Assoc ("CT_CLIENT_DATA",
                Image (Default.Cleaner_Client_Data_Timeout)),
         Assoc ("CT_SERVER_RESPONSE",
                Image (Default.Cleaner_Server_Response_Timeout)),
         Assoc ("FT_WAIT_FOR_CLIENT",
                Image (Default.Force_Wait_For_Client_Timeout)),
         Assoc ("FT_CLIENT_HEADER",
                Image (Default.Force_Client_Header_Timeout)),
         Assoc ("FT_CLIENT_DATA",
                Image (Default.Force_Client_Data_Timeout)),
         Assoc ("FT_SERVER_RESPONSE",
                Image (Default.Force_Server_Response_Timeout)),
         Assoc ("SEND_TIMEOUT",
                Image (Default.Send_Timeout)),
         Assoc ("RECEIVE_TIMEOUT",
                Image (Default.Receive_Timeout)),
         Assoc ("LOGO_IMAGE",
                Default.Logo_Image),
         Assoc ("DOWN_IMAGE",
                Default.Down_Image),
         Assoc ("UP_IMAGE",
                Default.Up_Image),
         Assoc ("STATUS_PAGE",
                Default.Status_Page),
         Assoc ("SESSION_LIFETIME",
                Image (Default.Session_Lifetime)),
         Assoc ("SESSION_CLEANUP_INTERVAL",
                Image (Default.Session_Cleanup_Interval)),
         Assoc ("TRANSIENT_LIFETIME",
                Image (Default.Transient_Lifetime)),
         Assoc ("TRANSIENT_CLEANUP_INTERVAL",
                Image (Default.Transient_Cleanup_Interval)),
         Assoc ("WWW_ROOT", Default.WWW_Root)
        );

begin
   Text_IO.Put_Line (Parse ("aws.texi.tmplt", T, Keep_Unknown_Tags => True));
end Build;
