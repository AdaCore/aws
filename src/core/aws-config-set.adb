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

with AWS.Config.Utils;
with AWS.Utils;

package body AWS.Config.Set is

   -----------------------
   -- Accept_Queue_Size --
   -----------------------

   procedure Accept_Queue_Size (O : in out Object; Value : Positive) is
   begin
      O.P (Accept_Queue_Size).Pos_Value := Value;
   end Accept_Queue_Size;

   --------------------
   -- Admin_Password --
   --------------------

   procedure Admin_Password (O : in out Object; Value : String) is
   begin
      O.P (Admin_Password).Str_Value := To_Unbounded_String (Value);
   end Admin_Password;

   ---------------
   -- Admin_URI --
   ---------------

   procedure Admin_URI (O : in out Object; Value : String) is
   begin
      O.P (Admin_URI).Str_Value := To_Unbounded_String (Value);
   end Admin_URI;

   -------------------------------
   -- Case_Sensitive_Parameters --
   -------------------------------

   procedure Case_Sensitive_Parameters
     (O     : in out Object;
      Value : Boolean) is
   begin
      O.P (Case_Sensitive_Parameters).Bool_Value := Value;
   end Case_Sensitive_Parameters;

   -----------------
   -- Certificate --
   -----------------

   procedure Certificate (O : in out Object; Filename : String) is
   begin
      O.P (Certificate).Str_Value := To_Unbounded_String (Filename);
   end Certificate;

   ------------------------
   -- Check_URL_Validity --
   ------------------------

   procedure Check_URL_Validity (O : in out Object; Value : Boolean) is
   begin
      O.P (Check_URL_Validity).Bool_Value := Value;
   end Check_URL_Validity;

   ---------------------------------
   -- Cleaner_Client_Data_Timeout --
   ---------------------------------

   procedure Cleaner_Client_Data_Timeout
     (O     : in out Object;
      Value : Duration) is
   begin
      O.P (Cleaner_Client_Data_Timeout).Dur_Value := Value;
   end Cleaner_Client_Data_Timeout;

   -----------------------------------
   -- Cleaner_Client_Header_Timeout --
   -----------------------------------

   procedure Cleaner_Client_Header_Timeout
     (O     : in out Object;
      Value : Duration) is
   begin
      O.P (Cleaner_Client_Header_Timeout).Dur_Value := Value;
   end Cleaner_Client_Header_Timeout;

   -------------------------------------
   -- Cleaner_Server_Response_Timeout --
   -------------------------------------

   procedure Cleaner_Server_Response_Timeout
     (O     : in out Object;
      Value : Duration) is
   begin
      O.P (Cleaner_Server_Response_Timeout).Dur_Value := Value;
   end Cleaner_Server_Response_Timeout;

   -------------------------------------
   -- Cleaner_Wait_For_Client_Timeout --
   -------------------------------------

   procedure Cleaner_Wait_For_Client_Timeout
     (O     : in out Object;
      Value : Duration) is
   begin
      O.P (Cleaner_Wait_For_Client_Timeout).Dur_Value := Value;
   end Cleaner_Wait_For_Client_Timeout;

   ----------------------
   -- Context_Lifetime --
   ----------------------

   procedure Context_Lifetime (Value : Duration) is
   begin
      Process_Options (Context_Lifetime).Dur_Value := Value;
   end Context_Lifetime;

   ----------------------------
   -- Directory_Browser_Page --
   ----------------------------

   procedure Directory_Browser_Page (O : in out Object; Value : String) is
   begin
      O.P (Directory_Browser_Page).Str_Value := To_Unbounded_String (Value);
   end Directory_Browser_Page;

   ----------------
   -- Down_Image --
   ----------------

   procedure Down_Image (O : in out Object; Value : String) is
   begin
      O.P (Down_Image).Str_Value := To_Unbounded_String (Value);
   end Down_Image;

   -------------------------------
   -- Error_Log_Filename_Prefix --
   -------------------------------

   procedure Error_Log_Filename_Prefix
     (O : in out Object; Value : String) is
   begin
      O.P (Error_Log_Filename_Prefix).Str_Value := To_Unbounded_String (Value);
   end Error_Log_Filename_Prefix;

   --------------------------
   -- Error_Log_Split_Mode --
   --------------------------

   procedure Error_Log_Split_Mode (O : in out Object; Value : String) is
   begin
      O.P (Error_Log_Split_Mode).Str_Value := To_Unbounded_String (Value);
   end Error_Log_Split_Mode;

   --------------------------
   -- Exchange_Certificate --
   --------------------------

   procedure Exchange_Certificate (O : in out Object; Value : Boolean) is
   begin
      O.P (Exchange_Certificate).Bool_Value := Value;
   end Exchange_Certificate;

   -------------------------------
   -- Force_Client_Data_Timeout --
   -------------------------------

   procedure Force_Client_Data_Timeout
     (O     : in out Object;
      Value : Duration) is
   begin
      O.P (Force_Client_Data_Timeout).Dur_Value := Value;
   end Force_Client_Data_Timeout;

   ---------------------------------
   -- Force_Client_Header_Timeout --
   ---------------------------------

   procedure Force_Client_Header_Timeout
     (O     : in out Object;
      Value : Duration) is
   begin
      O.P (Force_Client_Header_Timeout).Dur_Value := Value;
   end Force_Client_Header_Timeout;

   -----------------------------------
   -- Force_Server_Response_Timeout --
   -----------------------------------

   procedure Force_Server_Response_Timeout
     (O     : in out Object;
      Value : Duration) is
   begin
      O.P (Force_Server_Response_Timeout).Dur_Value := Value;
   end Force_Server_Response_Timeout;

   -----------------------------------
   -- Force_Wait_For_Client_Timeout --
   -----------------------------------

   procedure Force_Wait_For_Client_Timeout
     (O     : in out Object;
      Value : Duration) is
   begin
      O.P (Force_Wait_For_Client_Timeout).Dur_Value := Value;
   end Force_Wait_For_Client_Timeout;

   ---------------------------------
   -- Free_Slots_Keep_Alive_Limit --
   ---------------------------------

   procedure Free_Slots_Keep_Alive_Limit
     (O     : in out Object;
      Value : Natural) is
   begin
      O.P (Free_Slots_Keep_Alive_Limit).Nat_Value := Value;
   end Free_Slots_Keep_Alive_Limit;

   ------------------
   -- Hotplug_Port --
   ------------------

   procedure Hotplug_Port (O : in out Object; Value : Positive) is
   begin
      O.P (Hotplug_Port).Pos_Value := Value;
   end Hotplug_Port;

   ----------------------------
   -- Keep_Alive_Force_Limit --
   ----------------------------

   procedure Keep_Alive_Force_Limit (O : in out Object; Value : Natural) is
   begin
      O.P (Keep_Alive_Force_Limit).Nat_Value := Value;
   end Keep_Alive_Force_Limit;

   ---------
   -- Key --
   ---------

   procedure Key (O : in out Object; Filename : String) is
   begin
      O.P (Key).Str_Value := To_Unbounded_String (Filename);
   end Key;

   ---------------------
   -- Line_Stack_Size --
   ---------------------

   procedure Line_Stack_Size (O : in out Object; Value : Positive) is
   begin
      O.P (Line_Stack_Size).Pos_Value := Value;
   end Line_Stack_Size;

   -------------------------
   -- Log_Extended_Fields --
   -------------------------

   procedure Log_Extended_Fields (O : in out Object; Value : String) is
   begin
      Utils.Parse_Strings (O.P (Log_Extended_Fields).Strs_Value, Value);
   end Log_Extended_Fields;

   ------------------------
   -- Log_File_Directory --
   ------------------------

   procedure Log_File_Directory (O : in out Object; Value : String) is
   begin
      O.P (Log_File_Directory).Dir_Value := To_Unbounded_String (Value);
   end Log_File_Directory;

   -------------------------
   -- Log_Filename_Prefix --
   -------------------------

   procedure Log_Filename_Prefix (O : in out Object; Value : String) is
   begin
      O.P (Log_Filename_Prefix).Str_Value := To_Unbounded_String (Value);
   end Log_Filename_Prefix;

   --------------------
   -- Log_Size_Limit --
   --------------------

   procedure Log_Size_Limit (O : in out Object; Value : Natural) is
   begin
      O.P (Log_Size_Limit).Nat_Value := Value;
   end Log_Size_Limit;

   --------------------
   -- Log_Split_Mode --
   --------------------

   procedure Log_Split_Mode (O : in out Object; Value : String) is
   begin
      O.P (Log_Split_Mode).Str_Value := To_Unbounded_String (Value);
   end Log_Split_Mode;

   ----------------
   -- Logo_Image --
   ----------------

   procedure Logo_Image (O : in out Object; Value : String) is
   begin
      O.P (Logo_Image).Str_Value := To_Unbounded_String (Value);
   end Logo_Image;

   -------------------------------
   --  Max_Concurrent_Download  --
   -------------------------------

   procedure Max_Concurrent_Download (Value : Positive) is
   begin
      Process_Options (Max_Concurrent_Download).Pos_Value := Value;
   end Max_Concurrent_Download;

   --------------------
   -- Max_Connection --
   --------------------

   procedure Max_Connection (O : in out Object; Value : Positive) is
   begin
      O.P (Max_Connection).Pos_Value := Value;
   end Max_Connection;

   -------------------------
   -- Max_POST_Parameters --
   -------------------------

   procedure Max_POST_Parameters (O : in out Object; Value : Positive) is
   begin
      O.P (Max_POST_Parameters).Pos_Value := Value;
   end Max_POST_Parameters;

   ---------------------------
   -- Max_WebSocket_Handler --
   ---------------------------

   procedure Max_WebSocket_Handler (Value : Positive) is
   begin
      Process_Options (Max_WebSocket_Handler).Pos_Value := Value;
   end Max_WebSocket_Handler;

   ----------------
   -- MIME_Types --
   ----------------

   procedure MIME_Types (O : in out Object; Value : String) is
   begin
      O.P (MIME_Types).Str_Value := To_Unbounded_String (Value);
   end MIME_Types;

   ---------------
   -- Parameter --
   ---------------

   procedure Parameter
     (Config        : in out Object;
      Name          : String;
      Value         : String;
      Error_Context : String := "") is
   begin
      Utils.Set_Parameter
        (Config.P, Utils.Value (Name, Error_Context), Value, Error_Context);
   end Parameter;

   procedure Parameter
     (Name          : String;
      Value         : String;
      Error_Context : String := "") is
   begin
      Utils.Set_Parameter
        (Process_Options,
         Utils.Value (Name, Error_Context),
         Value,
         Error_Context);
   end Parameter;

   ---------------------
   -- Protocol_Family --
   ---------------------

   procedure Protocol_Family (O : in out Object; Value : String) is
   begin
      O.P (Protocol_Family).Str_Value := To_Unbounded_String (Value);
   end Protocol_Family;

   ---------------------
   -- Receive_Timeout --
   ---------------------

   procedure Receive_Timeout (O : in out Object; Value : Duration) is
   begin
      O.P (Receive_Timeout).Dur_Value := Value;
   end Receive_Timeout;

   -------------------
   -- Reuse_Address --
   -------------------

   procedure Reuse_Address (O : in out Object; Value : Boolean) is
   begin
      O.P (Reuse_Address).Bool_Value := Value;
   end Reuse_Address;

   --------------
   -- Security --
   --------------

   procedure Security (O : in out Object; Value : Boolean) is
   begin
      O.P (Security).Bool_Value := Value;
   end Security;

   -------------------
   -- Security_Mode --
   -------------------

   procedure Security_Mode (O : in out Object; Mode : String) is
   begin
      O.P (Security_Mode).Str_Value := To_Unbounded_String (Mode);
   end Security_Mode;

   ----------------------
   -- Send_Buffer_Size --
   ----------------------

   procedure Send_Buffer_Size (O : in out Object; Value : Positive) is
   begin
      O.P (Send_Buffer_Size).Nat_Value := Value;
   end Send_Buffer_Size;

   ------------------
   -- Send_Timeout --
   ------------------

   procedure Send_Timeout (O : in out Object; Value : Duration) is
   begin
      O.P (Send_Timeout).Dur_Value := Value;
   end Send_Timeout;

   -----------------
   -- Server_Host --
   -----------------

   procedure Server_Host (O : in out Object; Value : String) is
   begin
      O.P (Server_Host).Str_Value := To_Unbounded_String (Value);
   end Server_Host;

   -----------------
   -- Server_Name --
   -----------------

   procedure Server_Name (O : in out Object; Value : String) is
   begin
      O.P (Server_Name).Str_Value := To_Unbounded_String (Value);
   end Server_Name;

   -----------------
   -- Server_Port --
   -----------------

   procedure Server_Port (O : in out Object; Value : Natural) is
   begin
      O.P (Server_Port).Nat_Value := Value;
   end Server_Port;

   -------------
   -- Session --
   -------------

   procedure Session (O : in out Object; Value : Boolean) is
   begin
      O.P (Session).Bool_Value := Value;
   end Session;

   ------------------------------
   -- Session_Cleanup_Interval --
   ------------------------------

   procedure Session_Cleanup_Interval (Value : Duration) is
   begin
      Process_Options (Session_Cleanup_Interval).Dur_Value := Value;
   end Session_Cleanup_Interval;

   ----------------------
   -- Session_Lifetime --
   ----------------------

   procedure Session_Lifetime (Value : Duration) is
   begin
      Process_Options (Session_Lifetime).Dur_Value := Value;
   end Session_Lifetime;

   ------------------
   -- Session_Name --
   ------------------

   procedure Session_Name (O : in out Object; Value : String) is
   begin
      O.P (Session_Name).Str_Value := To_Unbounded_String (Value);
   end Session_Name;

   -----------------
   -- Status_Page --
   -----------------

   procedure Status_Page (O : in out Object; Value : String) is
   begin
      O.P (Status_Page).Str_Value := To_Unbounded_String (Value);
   end Status_Page;

   --------------------------------
   -- Transient_Cleanup_Interval --
   --------------------------------

   procedure Transient_Cleanup_Interval (Value : Duration) is
   begin
      Process_Options (Transient_Cleanup_Interval).Dur_Value := Value;
   end Transient_Cleanup_Interval;

   ------------------------
   -- Transient_Lifetime --
   ------------------------

   procedure Transient_Lifetime (Value : Duration) is
   begin
      Process_Options (Transient_Lifetime).Dur_Value := Value;
   end Transient_Lifetime;

   --------------
   -- Up_Image --
   --------------

   procedure Up_Image (O : in out Object; Value : String) is
   begin
      O.P (Up_Image).Str_Value := To_Unbounded_String (Value);
   end Up_Image;

   ----------------------
   -- Upload_Directory --
   ----------------------

   procedure Upload_Directory (O : in out Object; Value : String) is
   begin
      O.P (Upload_Directory).Dir_Value :=
        To_Unbounded_String (AWS.Utils.Normalized_Directory (Value));
   end Upload_Directory;

   -----------------------
   -- Upload_Size_Limit --
   -----------------------

   procedure Upload_Size_Limit (O : in out Object; Value : Positive) is
   begin
      O.P (Upload_Size_Limit).Pos_Value := Value;
   end Upload_Size_Limit;

   ----------------------------------
   -- WebSocket_Message_Queue_Size --
   ----------------------------------

   procedure WebSocket_Message_Queue_Size (Value : Positive) is
   begin
      Process_Options (WebSocket_Message_Queue_Size).Pos_Value := Value;
   end WebSocket_Message_Queue_Size;

   --------------
   -- WWW_Root --
   --------------

   procedure WWW_Root (O : in out Object; Value : String) is
   begin
      O.P (WWW_Root).Dir_Value :=
        To_Unbounded_String (AWS.Utils.Normalized_Directory (Value));
   end WWW_Root;

end AWS.Config.Set;
