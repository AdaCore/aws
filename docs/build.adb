
with Ada.Text_IO;
with Ada.Strings.Fixed;

with AWS.Utils;
with AWS.Config;

with Templates_Parser;

procedure Build is

   use Ada;
   use AWS;
   use AWS.Utils;
   use Templates_Parser;

   function Image (D : in Duration) return String is
      D_S : constant String   := Duration'Image (D);
      I   : constant Positive := Strings.Fixed.Index (D_S, ".");
   begin
      return D_S (D_S'First + 1 .. I + 1);
   end Image;


   T : Translate_Table
     := (Assoc ("MAX_CONNECT", Image (Config.Default_Max_Connection)),
         Assoc ("SERVER_NAME", Config.Default_Server_Name),
         Assoc ("SERVER_PORT", Image (Config.Default_Server_Port)),
         Assoc ("LOG_FILE_DIR", Config.Default_Log_File_Directory),
         Assoc ("UPLOAD_DIR", Config.Default_Upload_Directory),
         Assoc ("CT_WAIT_FOR_CLIENT",
                Image (Config.Default_Cleaner_Wait_For_Client_Timeout)),
         Assoc ("CT_CLIENT_HEADER",
                Image (Config.Default_Cleaner_Client_Header_Timeout)),
         Assoc ("CT_CLIENT_DATA",
                Image (Config.Default_Cleaner_Client_Data_Timeout)),
         Assoc ("CT_SERVER_RESPONSE",
                Image (Config.Default_Cleaner_Server_Response_Timeout)),
         Assoc ("FT_WAIT_FOR_CLIENT",
                Image (Config.Default_Force_Wait_For_Client_Timeout)),
         Assoc ("FT_CLIENT_HEADER",
                Image (Config.Default_Force_Client_Header_Timeout)),
         Assoc ("FT_CLIENT_DATA",
                Image (Config.Default_Force_Client_Data_Timeout)),
         Assoc ("FT_SERVER_RESPONSE",
                Image (Config.Default_Force_Server_Response_Timeout)),
         Assoc ("SEND_TIMEOUT",
                Image (Config.Default_Send_Timeout)),
         Assoc ("RECEIVE_TIMEOUT",
                Image (Config.Default_Receive_Timeout)),
         Assoc ("LOGO_IMAGE",
                Config.Default_Logo_Image),
         Assoc ("DOWN_IMAGE",
                Config.Default_Down_Image),
         Assoc ("UP_IMAGE",
                Config.Default_Up_Image),
         Assoc ("STATUS_PAGE",
                Config.Default_Status_Page),
         Assoc ("SESSION_LIFETIME",
                Image (Config.Default_Session_Lifetime)),
         Assoc ("SESSION_CLEANUP_INTERVAL",
                Image (Config.Default_Session_Cleanup_Interval))
        );

begin
   Text_IO.Put_Line (Parse ("aws.texi.tmplt", T));
end Build;
