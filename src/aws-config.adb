------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2003                          --
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

with AWS.Config.Ini;
--  This package is not used, but it is loaded here to initialize the
--  Server_Config variable.
pragma Warnings (Off, AWS.Config.Ini);

package body AWS.Config is

   -----------------------
   -- Accept_Queue_Size --
   -----------------------

   function Accept_Queue_Size (O : in Object) return Positive is
   begin
      return O.P (Accept_Queue_Size).Pos_Value;
   end Accept_Queue_Size;

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

   function Certificate return String is
   begin
      return To_String (Process_Options (Certificate).Str_Value);
   end Certificate;

   ------------------------
   -- Check_URL_Validity --
   ------------------------

   function Check_URL_Validity (O : in Object) return Boolean is
   begin
      return O.P (Check_URL_Validity).Bool_Value;
   end Check_URL_Validity;

   ---------------------------------
   -- Cleaner_Client_Data_Timeout --
   ---------------------------------

   function Cleaner_Client_Data_Timeout (O : in Object) return Duration is
   begin
      return O.P (Cleaner_Client_Data_Timeout).Dur_Value;
   end Cleaner_Client_Data_Timeout;

   -----------------------------------
   -- Cleaner_Client_Header_Timeout --
   -----------------------------------

   function Cleaner_Client_Header_Timeout (O : in Object) return Duration is
   begin
      return O.P (Cleaner_Client_Header_Timeout).Dur_Value;
   end Cleaner_Client_Header_Timeout;

   -------------------------------------
   -- Cleaner_Server_Response_Timeout --
   -------------------------------------

   function Cleaner_Server_Response_Timeout (O : in Object) return Duration is
   begin
      return O.P (Cleaner_Server_Response_Timeout).Dur_Value;
   end Cleaner_Server_Response_Timeout;

   -------------------------------------
   -- Cleaner_Wait_For_Client_Timeout --
   -------------------------------------

   function Cleaner_Wait_For_Client_Timeout (O : in Object) return Duration is
   begin
      return O.P (Cleaner_Wait_For_Client_Timeout).Dur_Value;
   end Cleaner_Wait_For_Client_Timeout;

   ----------------
   -- Down_Image --
   ----------------

   function Down_Image (O : in Object) return String is
   begin
      return To_String (O.P (Down_Image).Str_Value);
   end Down_Image;

   -------------------------------
   -- Error_Log_Filename_Prefix --
   -------------------------------

   function Error_Log_Filename_Prefix (O : in Object) return String is
   begin
      return To_String (O.P (Error_Log_Filename_Prefix).Str_Value);
   end Error_Log_Filename_Prefix;

   --------------------------
   -- Error_Log_Split_Mode --
   --------------------------

   function Error_Log_Split_Mode (O : in Object) return String is
   begin
      return To_String (O.P (Error_Log_Split_Mode).Str_Value);
   end Error_Log_Split_Mode;

   -------------------------------
   -- Force_Client_Data_Timeout --
   -------------------------------

   function Force_Client_Data_Timeout (O : in Object) return Duration is
   begin
      return O.P (Force_Client_Data_Timeout).Dur_Value;
   end Force_Client_Data_Timeout;

   ---------------------------------
   -- Force_Client_Header_Timeout --
   ---------------------------------

   function Force_Client_Header_Timeout (O : in Object) return Duration is
   begin
      return O.P (Force_Client_Header_Timeout).Dur_Value;
   end Force_Client_Header_Timeout;

   -----------------------------------
   -- Force_Server_Response_Timeout --
   -----------------------------------

   function Force_Server_Response_Timeout (O : in Object) return Duration is
   begin
      return O.P (Force_Server_Response_Timeout).Dur_Value;
   end Force_Server_Response_Timeout;

   -----------------------------------
   -- Force_Wait_For_Client_Timeout --
   -----------------------------------

   function Force_Wait_For_Client_Timeout (O : in Object) return Duration is
   begin
      return O.P (Force_Wait_For_Client_Timeout).Dur_Value;
   end Force_Wait_For_Client_Timeout;

   -----------------
   -- Get_Current --
   -----------------

   function Get_Current return Object is
   begin
      return Server_Config;
   end Get_Current;

   ------------------
   -- Hotplug_Port --
   ------------------

   function Hotplug_Port (O : in Object) return Positive is
   begin
      return O.P (Hotplug_Port).Pos_Value;
   end Hotplug_Port;

   ---------------------
   -- Line_Stack_Size --
   ---------------------

   function Line_Stack_Size (O : in Object) return Positive is
   begin
      return O.P (Line_Stack_Size).Pos_Value;
   end Line_Stack_Size;

   ------------------------
   -- Log_File_Directory --
   ------------------------

   function Log_File_Directory (O : in Object) return String is
   begin
      return To_String (O.P (Log_File_Directory).Dir_Value);
   end Log_File_Directory;

   -------------------------
   -- Log_Filename_Prefix --
   -------------------------

   function Log_Filename_Prefix (O : in Object) return String is
   begin
      return To_String (O.P (Log_Filename_Prefix).Str_Value);
   end Log_Filename_Prefix;

   --------------------
   -- Log_Split_Mode --
   --------------------

   function Log_Split_Mode (O : in Object) return String is
   begin
      return To_String (O.P (Log_Split_Mode).Str_Value);
   end Log_Split_Mode;

   ----------------
   -- Logo_Image --
   ----------------

   function Logo_Image (O : in Object) return String is
   begin
      return To_String (O.P (Logo_Image).Str_Value);
   end Logo_Image;

   --------------------
   -- Max_Connection --
   --------------------

   function Max_Connection (O : in Object) return Positive is
   begin
      return O.P (Max_Connection).Pos_Value;
   end Max_Connection;

   ---------------------
   -- Receive_Timeout --
   ---------------------

   function Receive_Timeout (O : in Object) return Duration is
   begin
      return O.P (Receive_Timeout).Dur_Value;
   end Receive_Timeout;

   --------------
   -- Security --
   --------------

   function Security (O : in Object) return Boolean is
   begin
      return O.P (Security).Bool_Value;
   end Security;

   ------------------
   -- Send_Timeout --
   ------------------

   function Send_Timeout (O : in Object) return Duration is
   begin
      return O.P (Send_Timeout).Dur_Value;
   end Send_Timeout;

   -----------------
   -- Server_Host --
   -----------------

   function Server_Host (O : in Object) return String is
   begin
      return To_String (O.P (Server_Host).Str_Value);
   end Server_Host;

   -----------------
   -- Server_Name --
   -----------------

   function Server_Name (O : in Object) return String is
   begin
      return To_String (O.P (Server_Name).Str_Value);
   end Server_Name;

   -----------------
   -- Server_Port --
   -----------------

   function Server_Port (O : in Object) return Positive is
   begin
      return O.P (Server_Port).Pos_Value;
   end Server_Port;

   -------------
   -- Session --
   -------------

   function Session (O : in Object) return Boolean is
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

   -----------------
   -- Status_Page --
   -----------------

   function Status_Page (O : in Object) return String is
   begin
      return To_String (O.P (Status_Page).Str_Value);
   end Status_Page;

   --------------
   -- Up_Image --
   --------------

   function Up_Image (O : in Object) return String is
   begin
      return To_String (O.P (Up_Image).Str_Value);
   end Up_Image;

   ----------------------
   -- Upload_Directory --
   ----------------------

   function Upload_Directory (O : in Object) return String is
   begin
      return To_String (O.P (Upload_Directory).Dir_Value);
   end Upload_Directory;

   --------------
   -- WWW_Root --
   --------------

   function WWW_Root (O : in Object) return String is
   begin
      return To_String (O.P (WWW_Root).Dir_Value);
   end WWW_Root;

end AWS.Config;
