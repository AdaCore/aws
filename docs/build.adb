
with Ada.Text_IO;
with Ada.Strings.Fixed;

with AWS.Utils;
with AWS.Default;

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
     := (Assoc ("VERSION", AWS.Version),
         Assoc ("MAX_CONNECT", Image (Default.Max_Connection)),
         Assoc ("SERVER_NAME", Default.Server_Name),
         Assoc ("SERVER_PORT", Image (Default.Server_Port)),
         Assoc ("LOG_FILE_DIR", Default.Log_File_Directory),
         Assoc ("LOG_SPLIT_MODE", Default.Log_Split_Mode),
         Assoc ("UPLOAD_DIR", Default.Upload_Directory),
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
                Image (Default.Session_Cleanup_Interval))
        );

begin
   Text_IO.Put_Line (Parse ("aws.texi.tmplt", T));
end Build;
