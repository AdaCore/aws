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

package body AWS.Config.Set is

   ---------------
   -- Admin_URI --
   ---------------

   procedure Admin_URI (O : in out Object; Value : in String) is
   begin
      O (Admin_URI).Str_Value := To_Unbounded_String (Value);
   end Admin_URI;

   -------------------------------
   -- Case_Sensitive_Parameters --
   -------------------------------

   procedure Case_Sensitive_Parameters
     (O     : in out Object;
      Value : in     Boolean) is
   begin
      O (Case_Sensitive_Parameters).Bool_Value := Value;
   end Case_Sensitive_Parameters;

   ---------------------------------
   -- Cleaner_Client_Data_Timeout --
   ---------------------------------

   procedure Cleaner_Client_Data_Timeout
     (O     : in out Object;
      Value : in     Duration) is
   begin
      O (Cleaner_Client_Data_Timeout).Dur_Value := Value;
   end Cleaner_Client_Data_Timeout;

   -----------------------------------
   -- Cleaner_Client_Header_Timeout --
   -----------------------------------

   procedure Cleaner_Client_Header_Timeout
     (O     : in out Object;
      Value : in     Duration) is
   begin
      O (Cleaner_Client_Header_Timeout).Dur_Value := Value;
   end Cleaner_Client_Header_Timeout;

   -------------------------------------
   -- Cleaner_Server_Response_Timeout --
   -------------------------------------

   procedure Cleaner_Server_Response_Timeout
     (O     : in out Object;
      Value : in     Duration) is
   begin
      O (Cleaner_Server_Response_Timeout).Dur_Value := Value;
   end Cleaner_Server_Response_Timeout;

   -------------------------------------
   -- Cleaner_Wait_For_Client_Timeout --
   -------------------------------------

   procedure Cleaner_Wait_For_Client_Timeout
     (O     : in out Object;
      Value : in     Duration) is
   begin
      O (Cleaner_Wait_For_Client_Timeout).Dur_Value := Value;
   end Cleaner_Wait_For_Client_Timeout;

   ----------------
   -- Down_Image --
   ----------------

   procedure Down_Image (O : in out Object; Value : in String) is
   begin
      O (Down_Image).Str_Value := To_Unbounded_String (Value);
   end Down_Image;

   -------------------------------
   -- Force_Client_Data_Timeout --
   -------------------------------

   procedure Force_Client_Data_Timeout
     (O     : in out Object;
      Value : in     Duration) is
   begin
      O (Force_Client_Data_Timeout).Dur_Value := Value;
   end Force_Client_Data_Timeout;

   ---------------------------------
   -- Force_Client_Header_Timeout --
   ---------------------------------

   procedure Force_Client_Header_Timeout
     (O     : in out Object;
      Value : in     Duration) is
   begin
      O (Force_Client_Header_Timeout).Dur_Value := Value;
   end Force_Client_Header_Timeout;

   -----------------------------------
   -- Force_Server_Response_Timeout --
   -----------------------------------

   procedure Force_Server_Response_Timeout
     (O     : in out Object;
      Value : in     Duration) is
   begin
      O (Force_Server_Response_Timeout).Dur_Value := Value;
   end Force_Server_Response_Timeout;

   -----------------------------------
   -- Force_Wait_For_Client_Timeout --
   -----------------------------------

   procedure Force_Wait_For_Client_Timeout
     (O     : in out Object;
      Value : in     Duration) is
   begin
      O (Force_Wait_For_Client_Timeout).Dur_Value := Value;
   end Force_Wait_For_Client_Timeout;

   ------------------
   -- Hotplug_Port --
   ------------------

   procedure Hotplug_Port (O : in out Object; Value : in Positive) is
   begin
      O (Hotplug_Port).Pos_Value := Value;
   end Hotplug_Port;

   ------------------------
   -- Log_File_Directory --
   ------------------------

   procedure Log_File_Directory (O : in out Object; Value : in String) is
   begin
      O (Log_File_Directory).Dir_Value := To_Unbounded_String (Value);
   end Log_File_Directory;

   ---------------------
   -- Log_File_Prefix --
   ---------------------

   procedure Log_File_Prefix (O : in out Object; Value : in String) is
   begin
      O (Log_File_Prefix).Str_Value := To_Unbounded_String (Value);
   end Log_File_Prefix;

   --------------------
   -- Log_Split_Mode --
   --------------------

   procedure Log_Split_Mode (O : in out Object; Value : in String) is
   begin
      O (Log_Split_Mode).Str_Value := To_Unbounded_String (Value);
   end Log_Split_Mode;

   ----------------
   -- Logo_Image --
   ----------------

   procedure Logo_Image (O : in out Object; Value : in String) is
   begin
      O (Logo_Image).Str_Value := To_Unbounded_String (Value);
   end Logo_Image;

   --------------------
   -- Max_Connection --
   --------------------

   procedure Max_Connection (O : in out Object; Value : in Positive) is
   begin
      O (Max_Connection).Pos_Value := Value;
   end Max_Connection;

   ---------------------
   -- Receive_Timeout --
   ---------------------

   procedure Receive_Timeout (O : in out Object; Value : in Duration) is
   begin
      O (Receive_Timeout).Dur_Value := Value;
   end Receive_Timeout;

   --------------
   -- Security --
   --------------

   procedure Security (O : in out Object; Value : in Boolean) is
   begin
      O (Security).Bool_Value := Value;
   end Security;

   ------------------
   -- Send_Timeout --
   ------------------

   procedure Send_Timeout (O : in out Object; Value : in Duration) is
   begin
      O (Send_Timeout).Dur_Value := Value;
   end Send_Timeout;

   -----------------
   -- Server_Name --
   -----------------

   procedure Server_Name (O : in out Object; Value : in String) is
   begin
      O (Server_Name).Str_Value := To_Unbounded_String (Value);
   end Server_Name;

   -----------------
   -- Server_Port --
   -----------------

   procedure Server_Port (O : in out Object; Value : in Positive) is
   begin
      O (Server_Port).Pos_Value := Value;
   end Server_Port;

   -------------
   -- Session --
   -------------

   procedure Session (O : in out Object; Value : in Boolean) is
   begin
      O (Session).Bool_Value := Value;
   end Session;

   ------------------------------
   -- Session_Cleanup_Interval --
   ------------------------------

   procedure Session_Cleanup_Interval
     (Value : in Duration) is
   begin
      Process_Options (Session_Cleanup_Interval).Dur_Value := Value;
   end Session_Cleanup_Interval;

   ----------------------
   -- Session_Lifetime --
   ----------------------

   procedure Session_Lifetime (Value : in Duration) is
   begin
      Process_Options (Session_Lifetime).Dur_Value := Value;
   end Session_Lifetime;

   -----------------
   -- Status_Page --
   -----------------

   procedure Status_Page (O : in out Object; Value : in String) is
   begin
      O (Status_Page).Str_Value := To_Unbounded_String (Value);
   end Status_Page;

   --------------
   -- Up_Image --
   --------------

   procedure Up_Image (O : in out Object; Value : in String) is
   begin
      O (Up_Image).Str_Value := To_Unbounded_String (Value);
   end Up_Image;

   ----------------------
   -- Upload_Directory --
   ----------------------

   procedure Upload_Directory (O : in out Object; Value : in String) is
   begin
      O (Upload_Directory).Dir_Value := To_Unbounded_String (Value);
   end Upload_Directory;

   --------------
   -- WWW_Root --
   --------------

   procedure WWW_Root (O : in out Object; Value : in String) is
   begin
      O (WWW_Root).Dir_Value := To_Unbounded_String (Value);
   end WWW_Root;

end AWS.Config.Set;
