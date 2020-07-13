------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2020, AdaCore                     --
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

with Ada.Characters.Handling;
with Ada.Environment_Variables;

with AWS.Config.Ini;
with AWS.OS_Lib;
with AWS.Utils;

package body AWS.Config is

   use Ada;

   Server_Config : Object;
   --  This variable will be updated with options found in 'aws.ini' and
   --  '<progname>.ini'.

   Ini_Loaded : Boolean := False;
   --  Set to True when initialization (.ini) files have been loaded

   procedure Read_Or_Ignore (Filename : String);
   --  Read and parse Filename, does not raise an exception if the file does
   --  not exists or can't be read.

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

   --------------------------
   -- Certificate_Required --
   --------------------------

   function Certificate_Required (O : Object) return Boolean is
   begin
      return O.P (Certificate_Required).Bool_Value;
   end Certificate_Required;

   ------------------------
   -- Check_URL_Validity --
   ------------------------

   function Check_URL_Validity (O : Object) return Boolean is
   begin
      return O.P (Check_URL_Validity).Bool_Value;
   end Check_URL_Validity;

   -----------------------
   -- Cipher_Priorities --
   -----------------------

   function Cipher_Priorities (O : Object) return String is
   begin
      return To_String (O.P (Cipher_Priorities).Str_Value);
   end Cipher_Priorities;

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

   ----------------------
   -- Config_Directory --
   ----------------------

   function Config_Directory return String is

      use Ada.Characters.Handling;

      function Home_Path return String;

      ---------------
      -- Home_Path --
      ---------------

      function Home_Path return String is
         use Ada.Environment_Variables;
         Home : constant String := "HOME";        -- Unix
         User : constant String := "USERPROFILE"; -- Windows
      begin
         if Exists (Home) then
            return Value (Home);
         elsif Exists (User) then
            return Value (User);
         else
            return "~";
         end if;
      end Home_Path;

      Result : constant String :=
                 To_String (Process_Options (Config_Directory).Str_Value);

   begin
      if Result'Length = 0
        or else Result (Result'First) in '/' | '\'
        or else (Result'Length > 2
                 and then To_Lower (Result (Result'First)) in 'a' .. 'z'
                 and then Result (Result'First + 1) = ':')
      then
         return Result;
      end if;

      return Home_Path & OS_Lib.Directory_Separator & Result;
   end Config_Directory;

   ----------------------
   -- Context_Lifetime --
   ----------------------

   function Context_Lifetime return Duration is
   begin
      return Process_Options (Context_Lifetime).Dur_Value;
   end Context_Lifetime;

   --------------
   -- CRL_File --
   --------------

   function CRL_File (O : Object) return String is
   begin
      return To_String (O.P (CRL_File).Str_Value);
   end CRL_File;

   ----------------------------
   -- Directory_Browser_Page --
   ----------------------------

   function Directory_Browser_Page (O : Object) return String is
   begin
      return To_String (O.P (Directory_Browser_Page).Str_Value);
   end Directory_Browser_Page;

   -------------------------
   -- Disable_Program_Ini --
   -------------------------

   function Disable_Program_Ini return Boolean is
   begin
      return Process_Options (Disable_Program_Ini).Bool_Value;
   end Disable_Program_Ini;

   ----------------
   -- Down_Image --
   ----------------

   function Down_Image (O : Object) return String is
   begin
      return To_String (O.P (Down_Image).Str_Value);
   end Down_Image;

   -------------------------
   -- Error_Log_Activated --
   -------------------------

   function Error_Log_Activated (O : Object) return Boolean is
   begin
      return O.P (Error_Log_Activated).Bool_Value;
   end Error_Log_Activated;

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
         Load_Config;
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

   ---------------
   -- IPv6_Only --
   ---------------

   function IPv6_Only (O : Object) return Boolean is
   begin
      return O.P (IPv6_Only).Bool_Value;
   end IPv6_Only;

   -----------------------------
   -- Is_WebSocket_Origin_Set --
   -----------------------------

   function Is_WebSocket_Origin_Set return Boolean is
   begin
      return Process_Options (Parameter_Name'(WebSocket_Origin)).Is_Set;
   end Is_WebSocket_Origin_Set;

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

   -----------------
   -- Load_Config --
   -----------------

   procedure Load_Config is
   begin
      Read_Or_Ignore
        (Config_Directory & OS_Lib.Directory_Separator & "aws.ini");
      Read_Or_Ignore ("aws.ini");

      if not Disable_Program_Ini then
         Read_Or_Ignore (Ini.Program_Ini_File (Full_Path => True));
         Read_Or_Ignore (Ini.Program_Ini_File (Full_Path => False));
      end if;
   end Load_Config;

   -------------------
   -- Log_Activated --
   -------------------

   function Log_Activated (O : Object) return Boolean is
   begin
      return O.P (Log_Activated).Bool_Value;
   end Log_Activated;

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
   -- Log_Size_Limit --
   --------------------

   function Log_Size_Limit (O : Object) return Natural is
   begin
      return O.P (Log_Size_Limit).Nat_Value;
   end Log_Size_Limit;

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

   -------------------------
   -- Max_POST_Parameters --
   -------------------------

   function Max_POST_Parameters (O : Object) return Positive is
   begin
      return O.P (Max_POST_Parameters).Pos_Value;
   end Max_POST_Parameters;

   -------------------
   -- Max_WebSocket --
   -------------------

   function Max_WebSocket return Positive is
   begin
      return Process_Options (Max_WebSocket).Pos_Value;
   end Max_WebSocket;

   ---------------------------
   -- Max_WebSocket_Handler --
   ---------------------------

   function Max_WebSocket_Handler return Positive is
   begin
      return Process_Options (Max_WebSocket_Handler).Pos_Value;
   end Max_WebSocket_Handler;

   ----------------
   -- MIME_Types --
   ----------------

   function MIME_Types return String is
   begin
      return To_String (Process_Options (MIME_Types).Str_Value);
   end MIME_Types;

   ---------------------
   -- Protocol_Family --
   ---------------------

   function Protocol_Family (O : Object) return String is
   begin
      return To_String (O.P (Protocol_Family).Str_Value);
   end Protocol_Family;

   --------------------
   -- Read_Or_Ignore --
   --------------------

   procedure Read_Or_Ignore (Filename : String) is
   begin
      if Utils.Is_Regular_File (Filename) then
         Ini.Read (Server_Config, Filename);
      end if;

   exception
      when others =>
         null;
   end Read_Or_Ignore;

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

   ----------------------
   -- Send_Buffer_Size --
   ----------------------

   function Send_Buffer_Size (O : Object) return Natural is
   begin
      return O.P (Send_Buffer_Size).Nat_Value;
   end Send_Buffer_Size;

   ------------------
   -- Send_Timeout --
   ------------------

   function Send_Timeout (O : Object) return Duration is
   begin
      return O.P (Send_Timeout).Dur_Value;
   end Send_Timeout;

   -------------------
   -- Server_Header --
   -------------------

   function Server_Header (O : Object) return String is
   begin
      return To_String (O.P (Server_Header).Str_Value);
   end Server_Header;

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

   ---------------------
   -- Server_Priority --
   ---------------------

   function Server_Priority (O : Object) return System.Any_Priority is
   begin
      return O.P (Server_Priority).Nat_Value;
   end Server_Priority;

   ----------------------
   -- Service_Priority --
   ----------------------

   function Service_Priority return System.Any_Priority is
   begin
      return Process_Options (Service_Priority).Nat_Value;
   end Service_Priority;

   -------------
   -- Session --
   -------------

   function Session (O : Object) return Boolean is
   begin
      return O.P (Session).Bool_Value;
   end Session;

   ------------------------------
   -- Session_Cleaner_Priority --
   ------------------------------

   function Session_Cleaner_Priority return System.Any_Priority is
   begin
      return Process_Options (Session_Cleaner_Priority).Nat_Value;
   end Session_Cleaner_Priority;

   ------------------------------
   -- Session_Cleanup_Interval --
   ------------------------------

   function Session_Cleanup_Interval return Duration is
   begin
      return Process_Options (Session_Cleanup_Interval).Dur_Value;
   end Session_Cleanup_Interval;

   -----------------------
   -- Session_Id_Length --
   -----------------------

   function Session_Id_Length return Positive is
   begin
      return Process_Options (Session_Id_Length).Pos_Value;
   end Session_Id_Length;

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

   --------------------------
   -- Session_Private_Name --
   --------------------------

   function Session_Private_Name (O : Object) return String is
   begin
      return To_String (O.P (Session_Private_Name).Str_Value);
   end Session_Private_Name;

   ----------------------------
   -- SSL_Session_Cache_Size --
   ----------------------------

   function SSL_Session_Cache_Size (O : Object) return Natural is
   begin
      return O.P (SSL_Session_Cache_Size).Nat_Value;
   end SSL_Session_Cache_Size;

   -----------------
   -- Status_Page --
   -----------------

   function Status_Page (O : Object) return String is
   begin
      return To_String (O.P (Status_Page).Str_Value);
   end Status_Page;

   ------------------
   -- TCP_No_Delay --
   ------------------

   function TCP_No_Delay (O : Object) return Boolean is
   begin
      return O.P (TCP_No_Delay).Bool_Value;
   end TCP_No_Delay;

   ------------------------
   -- TLS_Ticket_Support --
   ------------------------

   function TLS_Ticket_Support (O : Object) return Boolean is
   begin
      return O.P (TLS_Ticket_Support).Bool_Value;
   end TLS_Ticket_Support;

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

   ----------------
   -- Trusted_CA --
   ----------------

   function Trusted_CA (O : Object) return String is
   begin
      return To_String (O.P (Trusted_CA).Str_Value);
   end Trusted_CA;

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

   ----------------
   -- User_Agent --
   ----------------

   function User_Agent return String is
   begin
      return To_String (Process_Options (User_Agent).Str_Value);
   end User_Agent;

   ----------------------------------
   -- WebSocket_Message_Queue_Size --
   ----------------------------------

   function WebSocket_Message_Queue_Size return Positive is
   begin
      return Process_Options (WebSocket_Message_Queue_Size).Pos_Value;
   end WebSocket_Message_Queue_Size;

   ----------------------
   -- WebSocket_Origin --
   ----------------------

   function WebSocket_Origin return GNAT.Regexp.Regexp is
   begin
      return Process_Options (WebSocket_Origin).Pattern;
   end WebSocket_Origin;

   ----------------------
   -- WebSocket_Origin --
   ----------------------

   function WebSocket_Origin return String is
   begin
      return To_String (Process_Options (WebSocket_Origin).Regexp_Str);
   end WebSocket_Origin;

   ------------------------
   -- WebSocket_Priority --
   ------------------------

   function WebSocket_Priority return System.Any_Priority is
   begin
      return Process_Options (WebSocket_Priority).Nat_Value;
   end WebSocket_Priority;

   ---------------------------------------
   -- WebSocket_Send_Message_Queue_Size --
   ---------------------------------------

   function WebSocket_Send_Message_Queue_Size return Positive is
   begin
      return Process_Options (WebSocket_Send_Message_Queue_Size).Pos_Value;
   end WebSocket_Send_Message_Queue_Size;

   -----------------------
   -- WebSocket_Timeout --
   -----------------------

   function WebSocket_Timeout return Duration is
   begin
      return Process_Options (WebSocket_Timeout).Dur_Value;
   end WebSocket_Timeout;

   --------------
   -- WWW_Root --
   --------------

   function WWW_Root (O : Object) return String is
   begin
      return To_String (O.P (WWW_Root).Dir_Value);
   end WWW_Root;

end AWS.Config;
