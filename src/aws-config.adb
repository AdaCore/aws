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

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Utils;

package body AWS.Config is

   use Ada.Strings.Unbounded;

   --  List of token (keyword) recognized by the parser. There must be one
   --  entry for every option name to be handled.

   Admin_URI_Token          : aliased constant String := "admin_uri";
   Server_Name_Token        : aliased constant String := "server_name";
   Log_File_Directory_Token : aliased constant String := "log_file_directory";
   Upload_Directory_Token   : aliased constant String := "upload_directory";
   Max_Connection_Token     : aliased constant String := "max_connection";
   Server_Port_Token        : aliased constant String := "server_port";
   Hotplug_Port_Token       : aliased constant String := "hotplug_port";
   Send_Timeout_Token       : aliased constant String := "send_timeout";
   Receive_Timeout_Token    : aliased constant String := "receive_timeout";
   Status_Page_Token        : aliased constant String := "status_page";
   Up_Image_Token           : aliased constant String := "up_image";
   Down_Image_Token         : aliased constant String := "down_image";
   Logo_Image_Token         : aliased constant String := "logo_image";

   Session_Lifetime_Token                : aliased constant String
     := "session_lifetime";

   Session_Cleanup_Interval_Token        : aliased constant String
     := "session_cleanup_interval";

   Cleaner_Wait_For_Client_Timeout_Token : aliased constant String
     := "cleaner_wait_for_client_timeout";

   Cleaner_Client_Header_Timeout_Token   : aliased constant String
     := "cleaner_client_header_timeout";

   Cleaner_Client_Data_Timeout_Token     : aliased constant String
     := "cleaner_client_data_timeout";

   Cleaner_Server_Response_Timeout_Token : aliased constant String
     := "cleaner_server_response_timeout";

   Force_Wait_For_Client_Timeout_Token   : aliased constant String
     := "force_wait_for_client_timeout";

   Force_Client_Header_Timeout_Token     : aliased constant String
     := "force_client_header_timeout";

   Force_Client_Data_Timeout_Token       : aliased constant String
     := "force_client_data_timeout";

   Force_Server_Response_Timeout_Token   : aliased constant String
     := "force_server_response_timeout";

   --  List of values, one for each option declared above.

   Admin_URI_Value          : aliased Unbounded_String
     := To_Unbounded_String (Default_Admin_URI);

   Server_Name_Value        : aliased Unbounded_String
     := To_Unbounded_String (Default_Server_Name);

   Log_File_Directory_Value : aliased Unbounded_String
     := To_Unbounded_String (Default_Log_File_Directory);

   Upload_Directory_Value   : aliased Unbounded_String
     := To_Unbounded_String (Default_Upload_Directory);

   Max_Connection_Value     : aliased Positive := Default_Max_Connection;
   Server_Port_Value        : aliased Positive := Default_Server_Port;
   Hotplug_Port_Value       : aliased Positive := Default_Hotplug_Port;

   Session_Cleanup_Interval_Value : aliased Duration
     := Default_Session_Cleanup_Interval;

   Session_Lifetime_Value : aliased Duration
     := Default_Session_Lifetime;

   Cleaner_Wait_For_Client_Timeout_Value : aliased Duration
     := Default_Cleaner_Wait_For_Client_Timeout;

   Cleaner_Client_Header_Timeout_Value : aliased Duration
     := Default_Cleaner_Client_Header_Timeout;

   Cleaner_Client_Data_Timeout_Value : aliased Duration
     := Default_Cleaner_Client_Data_Timeout;

   Cleaner_Server_Response_Timeout_Value : aliased Duration
     := Default_Cleaner_Server_Response_Timeout;

   Force_Wait_For_Client_Timeout_Value : aliased Duration
     := Default_Force_Wait_For_Client_Timeout;

   Force_Client_Header_Timeout_Value : aliased Duration
     := Default_Force_Client_Header_Timeout;

   Force_Client_Data_Timeout_Value : aliased Duration
     := Default_Force_Client_Data_Timeout;

   Force_Server_Response_Timeout_Value : aliased Duration
     := Default_Force_Server_Response_Timeout;

   Send_Timeout_Value : aliased Duration
     := Default_Send_Timeout;

   Receive_Timeout_Value : aliased Duration
     := Default_Receive_Timeout;

   Status_Page_Value : aliased Unbounded_String
     := To_Unbounded_String (Default_Status_Page);

   Up_Image_Value    : aliased Unbounded_String
     := To_Unbounded_String (Default_Up_Image);

   Down_Image_Value  : aliased Unbounded_String
     := To_Unbounded_String (Default_Down_Image);

   Logo_Image_Value  : aliased Unbounded_String
     := To_Unbounded_String (Default_Logo_Image);

   -----------------------------
   -- Describe all parameters --
   -----------------------------

   type Value_Type        is (Str, Dir, Pos, Dur);

   type String_Pointer    is access constant String;
   type UString_Pointer   is access all Unbounded_String;
   type Positive_Pointer  is access all Positive;
   type Duration_Pointer  is access all Duration;

   type Values (Kind : Value_Type := Str) is record
      Key : String_Pointer;

      case Kind is
         when Str =>
            Str_Value : UString_Pointer;

         when Dir =>
            Dir_Value : UString_Pointer;

         when Pos =>
            Pos_Value : Positive_Pointer;

         when Dur =>
            Dur_Value : Duration_Pointer;
      end case;
   end record;

   type Parameter_List is array (Positive range <>) of Values;

   Parameters : constant Parameter_List :=

     ((Str, Server_Name_Token'Access,
       Server_Name_Value'Access),

      (Str, Admin_URI_Token'Access,
       Admin_URI_Value'Access),

      (Dir, Log_File_Directory_Token'Access,
       Log_File_Directory_Value'Access),

      (Dir, Upload_Directory_Token'Access,
       Upload_Directory_Value'Access),

      (Pos, Max_Connection_Token'Access,
       Max_Connection_Value'Access),

      (Pos, Server_Port_Token'Access,
       Server_Port_Value'Access),

      (Pos, Hotplug_Port_Token'Access,
       Hotplug_Port_Value'Access),

      (Dur, Session_Cleanup_Interval_Token'Access,
       Session_Cleanup_Interval_Value'Access),

      (Dur, Session_Lifetime_Token'Access,
       Session_Lifetime_Value'Access),

      (Dur, Cleaner_Wait_For_Client_Timeout_Token'Access,
       Cleaner_Wait_For_Client_Timeout_Value'Access),

      (Dur, Cleaner_Client_Header_Timeout_Token'Access,
       Cleaner_Client_Header_Timeout_Value'Access),

      (Dur, Cleaner_Client_Data_Timeout_Token'Access,
       Cleaner_Client_Data_Timeout_Value'Access),

      (Dur, Cleaner_Server_Response_Timeout_Token'Access,
       Cleaner_Server_Response_Timeout_Value'Access),

      (Dur, Force_Wait_For_Client_Timeout_Token'Access,
       Force_Wait_For_Client_Timeout_Value'Access),

      (Dur, Force_Client_Header_Timeout_Token'Access,
       Force_Client_Header_Timeout_Value'Access),

      (Dur, Force_Client_Data_Timeout_Token'Access,
       Force_Client_Data_Timeout_Value'Access),

      (Dur, Force_Server_Response_Timeout_Token'Access,
       Force_Server_Response_Timeout_Value'Access),

      (Dur, Send_Timeout_Token'Access,
       Send_Timeout_Value'Access),

      (Dur, Receive_Timeout_Token'Access,
       Receive_Timeout_Value'Access));

   procedure Initialize;
   --  Read aws.ini and <program name>.ini files if present and initialize
   --  this package accordingly. File aws.ini is parsed before <program
   --  name>.ini and the later overrides values set by the former.

   ---------------
   -- Admin_URI --
   ---------------

   function Admin_URI return String is
   begin
      return To_String (Admin_URI_Value);
   end Admin_URI;

   -------------------------------------
   -- Cleaner_Wait_For_Client_Timeout --
   -------------------------------------

   function Cleaner_Wait_For_Client_Timeout return Duration is
   begin
      return Cleaner_Wait_For_Client_Timeout_Value;
   end Cleaner_Wait_For_Client_Timeout;

   -----------------------------------
   -- Cleaner_Client_Header_Timeout --
   -----------------------------------

   function Cleaner_Client_Header_Timeout return Duration is
   begin
      return Cleaner_Client_Header_Timeout_Value;
   end Cleaner_Client_Header_Timeout;

   -------------------------
   -- Cleaner_Client_Data --
   -------------------------

   function Cleaner_Client_Data_Timeout return Duration is
   begin
      return Cleaner_Client_Data_Timeout_Value;
   end Cleaner_Client_Data_Timeout;

   -------------------------------------
   -- Cleaner_Server_Response_Timeout --
   -------------------------------------

   function Cleaner_Server_Response_Timeout return Duration is
   begin
      return Cleaner_Server_Response_Timeout_Value;
   end Cleaner_Server_Response_Timeout;

   ----------------
   -- Down_Image --
   ----------------

   function Down_Image return String is
   begin
      return To_String (Down_Image_Value);
   end Down_Image;

   -----------------------------------
   -- Force_Wait_For_Client_Timeout --
   -----------------------------------

   function Force_Wait_For_Client_Timeout return Duration is
   begin
      return Force_Wait_For_Client_Timeout_Value;
   end Force_Wait_For_Client_Timeout;

   ---------------------------------
   -- Force_Client_Header_Timeout --
   ---------------------------------

   function Force_Client_Header_Timeout return Duration is
   begin
      return Force_Client_Header_Timeout_Value;
   end Force_Client_Header_Timeout;

   -----------------------
   -- Force_Client_Data --
   -----------------------

   function Force_Client_Data_Timeout return Duration is
   begin
      return Force_Client_Data_Timeout_Value;
   end Force_Client_Data_Timeout;

   -----------------------------------
   -- Force_Server_Response_Timeout --
   -----------------------------------

   function Force_Server_Response_Timeout return Duration is
   begin
      return Force_Server_Response_Timeout_Value;
   end Force_Server_Response_Timeout;

   ------------------
   -- Hotplug_Port --
   ------------------

   function Hotplug_Port return Positive is
   begin
      return Hotplug_Port_Value;
   end Hotplug_Port;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

      use Ada;

      procedure Error_Message (Filename : in String; Message : in String);
      --  Output error message with filename and line number.

      procedure Process_Ini (Filename : in String);
      --  Read init file and set variables accordingly.

      function Program_Ini_File return String;
      --  Returns initialization filename for current server (using the
      --  executable name and adding .ini)

      procedure Find_Key_And_Set_Value
        (Params   : in     Parameter_List;
         Filename : in     String;
         Key      : in     String;
         Value    : in     String;
         Found    :    out Boolean);
      --  Look for parameter whose name is Key in the parameter list. If found
      --  the Found is set to True and the parameter is set with Value. If not
      --  found it returns with Found set to False.

      Line : Natural;
      --  current line number parsed

      -------------------
      -- Error_Message --
      -------------------

      procedure Error_Message (Filename : in String; Message : in String) is
      begin
         Text_IO.Put ('(' & Filename & ':');
         Text_IO.Put (AWS.Utils.Image (Line));
         Text_IO.Put_Line (") " & Message & '.');
      end Error_Message;

      ----------------------------
      -- Find_Key_And_Set_Value --
      ----------------------------

      procedure Find_Key_And_Set_Value
        (Params   : in     Parameter_List;
         Filename : in     String;
         Key      : in     String;
         Value    : in     String;
         Found    :    out Boolean)
      is
         use Ada.Strings.Unbounded;

         function "+" (S : in String)
           return Unbounded_String
           renames To_Unbounded_String;

         Key_Value : constant String := Ada.Characters.Handling.To_Lower (Key);

         Expected_Type : Unbounded_String;

      begin
         Found := False;

         Look_For_Key : for I in Params'Range loop

            if Key_Value = Params (I).Key.all then
               Found := True;

               begin
                  case Params (I).Kind is
                     when Str =>
                        Expected_Type := +"string";
                        Params (I).Str_Value.all := +Value;

                     when Dir =>
                        Expected_Type := +"string";
                        if Value (Value'Last) = '/'
                          or else Value (Value'Last) = '\'
                        then
                           Params (I).Dir_Value.all := +Value;
                        else
                           Params (I).Dir_Value.all := +(Value & '/');
                        end if;

                     when Pos =>
                        Expected_Type := +"positive";
                        Params (I).Pos_Value.all := Positive'Value (Value);

                     when Dur =>
                        Expected_Type := +"duration";
                        Params (I).Dur_Value.all := Duration'Value (Value);
                  end case;

               exception
                  when others =>
                     Error_Message
                       (Filename,
                        "wrong value for " & Key
                        & " " & To_String (Expected_Type) & " expected");
               end;

               exit Look_For_Key;
            end if;

         end loop Look_For_Key;
      end Find_Key_And_Set_Value;

      -----------------
      -- Process_Ini --
      -----------------

      procedure Process_Ini (Filename : in String) is

         Separators : constant Strings.Maps.Character_Set
           := Strings.Maps.To_Set (' ' & ASCII.HT);

         File    : Text_IO.File_Type;
         Buffer  : String (1 .. 1024);
         Last    : Natural;

         K_First : Natural;
         K_Last  : Natural;

         V_First : Natural;
         V_Last  : Natural;

      begin
         Text_IO.Open (Name => Filename,
                       File => File,
                       Mode => Text_IO.In_File);
         Line := 0;

         while not Text_IO.End_Of_File (File) loop
            Text_IO.Get_Line (File, Buffer, Last);
            Line := Line + 1;

            --  Remove comments

            for I in 1 .. Last loop
               if Buffer (I) = '#' then
                  Last := I - 1;
                  exit;
               end if;
            end loop;

            if Last /= 0 then

               --  Looks for Key token

               Strings.Fixed.Find_Token
                 (Buffer (1 .. Last), Separators, Strings.Outside,
                  K_First, K_Last);

               --  Looks for associated value

               Strings.Fixed.Find_Token
                 (Buffer (K_Last + 1 .. Last), Separators, Strings.Outside,
                  V_First, V_Last);

               if K_Last /= 0 and then V_Last /= 0 then

                  declare
                     Key   : constant String := Buffer (K_First .. K_Last);

                     Value : constant String := Buffer (V_First .. V_Last);

                     Found : Boolean;
                  begin
                     Find_Key_And_Set_Value (Parameters, Filename,
                                             Key, Value, Found);

                     if not Found then
                        Error_Message (Filename, "unrecognized option " & Key);
                     end if;
                  end;
               else
                  Error_Message (Filename, "wrong format");
               end if;
            end if;
         end loop;

         Text_IO.Close (File);
      exception
         when Text_IO.Name_Error =>
            null;
      end Process_Ini;

      ----------------------
      -- Program_Ini_File --
      ----------------------

      function Program_Ini_File return String is
         Exec_Name : constant String := Ada.Command_Line.Command_Name;
         Last      : Natural;
      begin
         Last := Strings.Fixed.Index (Exec_Name, ".", Strings.Backward);

         if Last = 0 then
            return Exec_Name & ".ini";
         else
            return Exec_Name (Exec_Name'First .. Last) & "ini";
         end if;
      end Program_Ini_File;

   begin
      Process_Ini ("aws.ini");
      Process_Ini (Program_Ini_File);
   end Initialize;

   ------------------------
   -- Log_File_Directory --
   ------------------------

   function Log_File_Directory return String is
   begin
      return To_String (Log_File_Directory_Value);
   end Log_File_Directory;

   ----------------
   -- Logo_Image --
   ----------------

   function Logo_Image return String is
   begin
      return To_String (Logo_Image_Value);
   end Logo_Image;

   --------------------
   -- Max_Connection --
   --------------------

   function Max_Connection return Positive is
   begin
      return Max_Connection_Value;
   end Max_Connection;

   ---------------------
   -- Receive_Timeout --
   ---------------------

   function Receive_Timeout return Duration is
   begin
      return Receive_Timeout_Value;
   end Receive_Timeout;

   ------------------
   -- Send_Timeout --
   ------------------

   function Send_Timeout return Duration is
   begin
      return Send_Timeout_Value;
   end Send_Timeout;
   -----------------
   -- Server_Name --
   -----------------

   function Server_Name return String is
   begin
      return To_String (Server_Name_Value);
   end Server_Name;

   -----------------
   -- Server_Port --
   -----------------

   function Server_Port return Positive is
   begin
      return Server_Port_Value;
   end Server_Port;

   ------------------------------
   -- Session_Cleanup_Interval --
   ------------------------------

   function Session_Cleanup_Interval return Duration is
   begin
      return Session_Cleanup_Interval_Value;
   end Session_Cleanup_Interval;

   ----------------------
   -- Session_Lifetime --
   ----------------------

   function Session_Lifetime return Duration is
   begin
      return Session_Lifetime_Value;
   end Session_Lifetime;

   -----------------
   -- Status_Page --
   -----------------

   function Status_Page return String is
   begin
      return To_String (Status_Page_Value);
   end Status_Page;

   --------------
   -- Up_Image --
   --------------

   function Up_Image return String is
   begin
      return To_String (Up_Image_Value);
   end Up_Image;

   ----------------------
   -- Upload_Directory --
   ----------------------

   function Upload_Directory return String is
   begin
      return To_String (Upload_Directory_Value);
   end Upload_Directory;

begin
   Initialize;
end AWS.Config;
