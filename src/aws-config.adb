------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2009, AdaCore                     --
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

with Ada.Text_IO;

with AWS.Config.Ini;

package body AWS.Config is

   Server_Config : Object;
   --  This variable will be updated with options found in 'aws.ini' and
   --  '<progname>.ini'.

   Ini_Loaded : Boolean := False;
   --  Set to True when initialization (.ini) files have been loaded

   procedure Read_If_Present (Filename : String);
   --  Read and parse Filename, does not raise an exception if the file does
   --  not exists.

   -----------------------
   -- Accept_Queue_Size --
   -----------------------

   function Accept_Queue_Size (O : Object) return Positive is
   begin
      return O.P (Accept_Queue_Size).Pos_Value;
   end Accept_Queue_Size;

   --------------------
   -- Admin_Password --
   --------------------

   function Admin_Password (O : Object) return String is
   begin
      return To_String (O.P (Admin_Password).Str_Value);
   end Admin_Password;

   -----------------
   -- Admin_Realm --
   -----------------

   function Admin_Realm (O : Object) return String is
   begin
      return To_String (O.P (Admin_Realm).Str_Value);
   end Admin_Realm;

   ---------------
   -- Admin_URI --
   ---------------

   function Admin_URI (O : Object) return String is
   begin
      return To_String (O.P (Admin_URI).Str_Value);
   end Admin_URI;

   -------------------------------
   -- Case_Sensitive_Parameters --
   -------------------------------

   function Case_Sensitive_Parameters (O : Object) return Boolean is
   begin
      return O.P (Case_Sensitive_Parameters).Bool_Value;
   end Case_Sensitive_Parameters;

   -----------------
   -- Certificate --
   -----------------

   function Certificate (O : Object) return String is
   begin
      return To_String (O.P (Certificate).Str_Value);
   end Certificate;

   ------------------------
   -- Check_URL_Validity --
   ------------------------

   function Check_URL_Validity (O : Object) return Boolean is
   begin
      return O.P (Check_URL_Validity).Bool_Value;
   end Check_URL_Validity;

   ---------------------------------
   -- Cleaner_Client_Data_Timeout --
   ---------------------------------

   function Cleaner_Client_Data_Timeout (O : Object) return Duration is
   begin
      return O.P (Cleaner_Client_Data_Timeout).Dur_Value;
   end Cleaner_Client_Data_Timeout;

   -----------------------------------
   -- Cleaner_Client_Header_Timeout --
   -----------------------------------

   function Cleaner_Client_Header_Timeout (O : Object) return Duration is
   begin
      return O.P (Cleaner_Client_Header_Timeout).Dur_Value;
   end Cleaner_Client_Header_Timeout;

   -------------------------------------
   -- Cleaner_Server_Response_Timeout --
   -------------------------------------

   function Cleaner_Server_Response_Timeout (O : Object) return Duration is
   begin
      return O.P (Cleaner_Server_Response_Timeout).Dur_Value;
   end Cleaner_Server_Response_Timeout;

   -------------------------------------
   -- Cleaner_Wait_For_Client_Timeout --
   -------------------------------------

   function Cleaner_Wait_For_Client_Timeout (O : Object) return Duration is
   begin
      return O.P (Cleaner_Wait_For_Client_Timeout).Dur_Value;
   end Cleaner_Wait_For_Client_Timeout;

   ----------------------------
   -- Directory_Browser_Page --
   ----------------------------

   function Directory_Browser_Page (O : Object) return String is
   begin
      return To_String (O.P (Directory_Browser_Page).Str_Value);
   end Directory_Browser_Page;

   ----------------
   -- Down_Image --
   ----------------

   function Down_Image (O : Object) return String is
   begin
      return To_String (O.P (Down_Image).Str_Value);
   end Down_Image;

   -------------------------------
   -- Error_Log_Filename_Prefix --
   -------------------------------

   function Error_Log_Filename_Prefix (O : Object) return String is
   begin
      return To_String (O.P (Error_Log_Filename_Prefix).Str_Value);
   end Error_Log_Filename_Prefix;

   --------------------------
   -- Error_Log_Split_Mode --
   --------------------------

   function Error_Log_Split_Mode (O : Object) return String is
   begin
      return To_String (O.P (Error_Log_Split_Mode).Str_Value);
   end Error_Log_Split_Mode;

   --------------------------
   -- Exchange_Certificate --
   --------------------------

   function Exchange_Certificate (O : Object) return Boolean is
   begin
      return O.P (Exchange_Certificate).Bool_Value;
   end Exchange_Certificate;

   -------------------------------
   -- Force_Client_Data_Timeout --
   -------------------------------

   function Force_Client_Data_Timeout (O : Object) return Duration is
   begin
      return O.P (Force_Client_Data_Timeout).Dur_Value;
   end Force_Client_Data_Timeout;

   ---------------------------------
   -- Force_Client_Header_Timeout --
   ---------------------------------

   function Force_Client_Header_Timeout (O : Object) return Duration is
   begin
      return O.P (Force_Client_Header_Timeout).Dur_Value;
   end Force_Client_Header_Timeout;

   -----------------------------------
   -- Force_Server_Response_Timeout --
   -----------------------------------

   function Force_Server_Response_Timeout (O : Object) return Duration is
   begin
      return O.P (Force_Server_Response_Timeout).Dur_Value;
   end Force_Server_Response_Timeout;

   -----------------------------------
   -- Force_Wait_For_Client_Timeout --
   -----------------------------------

   function Force_Wait_For_Client_Timeout (O : Object) return Duration is
   begin
      return O.P (Force_Wait_For_Client_Timeout).Dur_Value;
   end Force_Wait_For_Client_Timeout;

   ---------------------------------
   -- Free_Slots_Keep_Alive_Limit --
   ---------------------------------

   function Free_Slots_Keep_Alive_Limit (O : Object) return Natural is
   begin
      return O.P (Free_Slots_Keep_Alive_Limit).Nat_Value;
   end Free_Slots_Keep_Alive_Limit;

   -----------------
   -- Get_Current --
   -----------------

   function Get_Current return Object is
   begin
      if not Ini_Loaded then
         Ini_Loaded := True;

         Read_If_Present ("aws.ini");
         Read_If_Present (Ini.Program_Ini_File (Full_Path => True));
         Read_If_Present (Ini.Program_Ini_File (Full_Path => False));
      end if;

      return Server_Config;
   end Get_Current;

   ------------------
   -- Hotplug_Port --
   ------------------

   function Hotplug_Port (O : Object) return Positive is
   begin
      return O.P (Hotplug_Port).Pos_Value;
   end Hotplug_Port;

   ---------------------------
   -- Input_Line_Size_Limit --
   ---------------------------

   function Input_Line_Size_Limit return Positive is
   begin
      return Process_Options (Input_Line_Size_Limit).Pos_Value;
   end Input_Line_Size_Limit;

   ----------------------------
   -- Keep_Alive_Close_Limit --
   ----------------------------

   function Keep_Alive_Close_Limit (O : Object) return Positive is
   begin
      if O.P (Keep_Alive_Close_Limit).Nat_Value = 0 then
         return Max_Connection (O) * 4;
      else
         return O.P (Keep_Alive_Close_Limit).Nat_Value;
      end if;
   end Keep_Alive_Close_Limit;

   ----------------------------
   -- Keep_Alive_Force_Limit --
   ----------------------------

   function Keep_Alive_Force_Limit (O : Object) return Positive is
   begin
      if O.P (Keep_Alive_Force_Limit).Nat_Value = 0 then
         return Max_Connection (O) * 2;
      else
         return O.P (Keep_Alive_Force_Limit).Nat_Value;
      end if;
   end Keep_Alive_Force_Limit;

   ---------
   -- Key --
   ---------

   function Key (O : Object) return String is
   begin
      return To_String (O.P (Key).Str_Value);
   end Key;

   ---------------------
   -- Line_Stack_Size --
   ---------------------

   function Line_Stack_Size (O : Object) return Positive is
   begin
      return O.P (Line_Stack_Size).Pos_Value;
   end Line_Stack_Size;

   -----------------------------------------
   -- Log_Extended_Fields_Generic_Iterate --
   -----------------------------------------

   procedure Log_Extended_Fields_Generic_Iterate (O : Object) is
   begin
      for J in 1 .. Log_Extended_Fields_Length (O) loop
         Field_Id (SV.Element (O.P (Log_Extended_Fields).Strs_Value, J));
      end loop;
   end Log_Extended_Fields_Generic_Iterate;

   --------------------------------
   -- Log_Extended_Fields_Length --
   --------------------------------

   function Log_Extended_Fields_Length (O : Object) return Natural is
   begin
      return Natural (SV.Length (O.P (Log_Extended_Fields).Strs_Value));
   end Log_Extended_Fields_Length;

   ------------------------
   -- Log_File_Directory --
   ------------------------

   function Log_File_Directory (O : Object) return String is
   begin
      return To_String (O.P (Log_File_Directory).Dir_Value);
   end Log_File_Directory;

   -------------------------
   -- Log_Filename_Prefix --
   -------------------------

   function Log_Filename_Prefix (O : Object) return String is
   begin
      return To_String (O.P (Log_Filename_Prefix).Str_Value);
   end Log_Filename_Prefix;

   --------------------
   -- Log_Split_Mode --
   --------------------

   function Log_Split_Mode (O : Object) return String is
   begin
      return To_String (O.P (Log_Split_Mode).Str_Value);
   end Log_Split_Mode;

   ----------------
   -- Logo_Image --
   ----------------

   function Logo_Image (O : Object) return String is
   begin
      return To_String (O.P (Logo_Image).Str_Value);
   end Logo_Image;

   -----------------------------
   -- Max_Concurrent_Download --
   -----------------------------

   function Max_Concurrent_Download return Positive is
   begin
      return Process_Options (Max_Concurrent_Download).Pos_Value;
   end Max_Concurrent_Download;

   --------------------
   -- Max_Connection --
   --------------------

   function Max_Connection (O : Object) return Positive is
   begin
      return O.P (Max_Connection).Pos_Value;
   end Max_Connection;

   ---------------------
   -- Read_If_Present --
   ---------------------

   procedure Read_If_Present (Filename : String) is
   begin
      Ini.Read (Server_Config, Filename);
   exception
      when Ada.Text_IO.Name_Error =>
         null;
   end Read_If_Present;

   ---------------------
   -- Receive_Timeout --
   ---------------------

   function Receive_Timeout (O : Object) return Duration is
   begin
      return O.P (Receive_Timeout).Dur_Value;
   end Receive_Timeout;

   -------------------
   -- Reuse_Address --
   -------------------

   function Reuse_Address (O : Object) return Boolean is
   begin
      return O.P (Reuse_Address).Bool_Value;
   end Reuse_Address;

   --------------
   -- Security --
   --------------

   function Security (O : Object) return Boolean is
   begin
      return O.P (Security).Bool_Value;
   end Security;

   -------------------
   -- Security_Mode --
   -------------------

   function Security_Mode (O : Object) return String is
   begin
      return To_String (O.P (Security_Mode).Str_Value);
   end Security_Mode;

   ------------------
   -- Send_Timeout --
   ------------------

   function Send_Timeout (O : Object) return Duration is
   begin
      return O.P (Send_Timeout).Dur_Value;
   end Send_Timeout;

   -----------------
   -- Server_Host --
   -----------------

   function Server_Host (O : Object) return String is
   begin
      return To_String (O.P (Server_Host).Str_Value);
   end Server_Host;

   -----------------
   -- Server_Name --
   -----------------

   function Server_Name (O : Object) return String is
   begin
      return To_String (O.P (Server_Name).Str_Value);
   end Server_Name;

   -----------------
   -- Server_Port --
   -----------------

   function Server_Port (O : Object) return Natural is
   begin
      return O.P (Server_Port).Nat_Value;
   end Server_Port;

   -------------
   -- Session --
   -------------

   function Session (O : Object) return Boolean is
   begin
      return O.P (Session).Bool_Value;
   end Session;

   ------------------------------
   -- Session_Cleanup_Interval --
   ------------------------------

   function Session_Cleanup_Interval return Duration is
   begin
      return Process_Options (Session_Cleanup_Interval).Dur_Value;
   end Session_Cleanup_Interval;

   ----------------------
   -- Session_Lifetime --
   ----------------------

   function Session_Lifetime return Duration is
   begin
      return Process_Options (Session_Lifetime).Dur_Value;
   end Session_Lifetime;

   ------------------
   -- Session_Name --
   ------------------

   function Session_Name (O : Object) return String is
   begin
      return To_String (O.P (Session_Name).Str_Value);
   end Session_Name;

   -----------------
   -- Status_Page --
   -----------------

   function Status_Page (O : Object) return String is
   begin
      return To_String (O.P (Status_Page).Str_Value);
   end Status_Page;

   --------------------------------
   -- Transient_Cleanup_Interval --
   --------------------------------

   function Transient_Cleanup_Interval return Duration is
   begin
      return Process_Options (Transient_Cleanup_Interval).Dur_Value;
   end Transient_Cleanup_Interval;

   ------------------------
   -- Transient_Lifetime --
   ------------------------

   function Transient_Lifetime return Duration is
   begin
      return Process_Options (Transient_Lifetime).Dur_Value;
   end Transient_Lifetime;

   --------------
   -- Up_Image --
   --------------

   function Up_Image (O : Object) return String is
   begin
      return To_String (O.P (Up_Image).Str_Value);
   end Up_Image;

   ----------------------
   -- Upload_Directory --
   ----------------------

   function Upload_Directory (O : Object) return String is
   begin
      return To_String (O.P (Upload_Directory).Dir_Value);
   end Upload_Directory;

   -----------------------
   -- Upload_Size_Limit --
   -----------------------

   function Upload_Size_Limit (O : Object) return Positive is
   begin
      return O.P (Upload_Size_Limit).Pos_Value;
   end Upload_Size_Limit;

   --------------
   -- WWW_Root --
   --------------

   function WWW_Root (O : Object) return String is
   begin
      return To_String (O.P (WWW_Root).Dir_Value);
   end WWW_Root;

end AWS.Config;
