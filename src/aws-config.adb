------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                      Dmitriy Anisimkov & Pascal Obry                     --
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

with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Regpat;

with AWS.Utils;

package body AWS.Config is

   use Ada.Strings.Unbounded;

   Admin_URI_Value          : Unbounded_String
     := To_Unbounded_String (Default_Admin_URI);

   Server_Name_Value        : Unbounded_String
     := To_Unbounded_String (Default_Server_Name);

   Log_File_Directory_Value : Unbounded_String
     := To_Unbounded_String (Default_Log_File_Directory);

   Upload_Directory_Value   : Unbounded_String
     := To_Unbounded_String (Default_Upload_Directory);

   Max_Connection_Value     : Positive := Default_Max_Connection;
   Server_Port_Value        : Positive := Default_Server_Port;

   Cleaner_Wait_For_Client_Timeout_Value : Duration
     := Default_Cleaner_Wait_For_Client_Timeout;

   Cleaner_Client_Header_Timeout_Value : Duration
     := Default_Cleaner_Client_Header_Timeout;

   Cleaner_Client_Data_Timeout_Value : Duration
     := Default_Cleaner_Client_Data_Timeout;

   Cleaner_Server_Response_Timeout_Value : Duration
     := Default_Cleaner_Server_Response_Timeout;

   Force_Wait_For_Client_Timeout_Value : Duration
     := Default_Force_Wait_For_Client_Timeout;

   Force_Client_Header_Timeout_Value : Duration
     := Default_Force_Client_Header_Timeout;

   Force_Client_Data_Timeout_Value : Duration
     := Default_Force_Client_Data_Timeout;

   Force_Server_Response_Timeout_Value : Duration
     := Default_Force_Server_Response_Timeout;

   Send_Timeout_Value : Duration
     := Default_Send_Timeout;

   Receive_Timeout_Value : Duration
     := Default_Receive_Timeout;

   Status_Page_Value : Unbounded_String
     := To_Unbounded_String (Default_Status_Page);

   Up_Image_Value    : Unbounded_String
     := To_Unbounded_String (Default_Up_Image);

   Down_Image_Value  : Unbounded_String
     := To_Unbounded_String (Default_Down_Image);

   Logo_Image_Value  : Unbounded_String
     := To_Unbounded_String (Default_Logo_Image);

   procedure Initialize;
   --  Read aws.ini file if present and initialize this package accordingly.

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

      use GNAT;
      use Ada;

      procedure Error_Message (Filename : in String; Message : in String);
      --  Output error message with filename and line number.

      procedure Process_Ini (Filename : in String);
      --  Read init file and set variables accordingly.

      function Program_Ini_File return String;
      --  Returns initialization filename for current server (using the
      --  executable name and adding .ini)

      Line : Natural;
      --  current line number parsed

      -------------------
      -- Error_Message --
      -------------------

      procedure Error_Message (Filename : in String; Message : in String) is
      begin
         Text_IO.Put ('(' & Filename & ':');
         Text_IO.Put (AWS.Utils.Image (Line));
         Text_IO.Put_Line (") " & Message);
      end Error_Message;

      -----------------
      -- Process_Ini --
      -----------------

      procedure Process_Ini (Filename : in String) is

         use type GNAT.Regpat.Match_Location;

         Regexp  : constant String := "^ *([#a-zA-Z_]+) +([0-9a-zA-Z/\-:.]+)";
         Matcher : constant Regpat.Pattern_Matcher := Regpat.Compile (Regexp);
         Matches : Regpat.Match_Array (1 .. 2);

         File    : Text_IO.File_Type;
         Buffer  : String (1 .. 1024);
         Last    : Natural;
      begin
         Text_IO.Open (Name => Filename,
                       File => File,
                       Mode => Text_IO.In_File);

         Line := 0;

         while not Text_IO.End_Of_File (File) loop
            Text_IO.Get_Line (File, Buffer, Last);
            Line := Line + 1;

            -- Remove comments
            for I in 1 .. Last loop
               if Buffer (I) = '#' then
                  Last := I - 1;
                  exit;
               end if;
            end loop;

            if Last /= 0 then
               Regpat.Match (Matcher, Buffer (1 .. Last), Matches);

               if Matches (2) /= Regpat.No_Match then
                  declare
                     Key   : constant String
                       := Buffer (Matches (1).First .. Matches (1).Last);
                     Value : constant String
                       := Buffer (Matches (2).First .. Matches (2).Last);
                  begin
                     if Key = "Server_Name" then
                        Server_Name_Value := To_Unbounded_String (Value);

                     elsif Key = "Admin_URI" then
                        Admin_URI_Value := To_Unbounded_String (Value);

                     elsif Key = "Log_File_Directory" then
                        if Value (Value'Last) = '/' then
                           Log_File_Directory_Value
                             := To_Unbounded_String (Value);
                        else
                           Log_File_Directory_Value
                             := To_Unbounded_String (Value & '/');
                        end if;

                     elsif Key = "Upload_Directory" then
                        if Value (Value'Last) = '/' then
                           Upload_Directory_Value
                             := To_Unbounded_String (Value);
                        else
                           Upload_Directory_Value
                             := To_Unbounded_String (Value & '/');
                        end if;

                     elsif Key = "Max_Connection" then
                        begin
                           Max_Connection_Value := Positive'Value (Value);
                        exception
                           when others =>
                              Error_Message
                                (Filename, "wrong value for " & Key);
                        end;

                     elsif Key = "Server_Port" then
                        begin
                           Server_Port_Value := Positive'Value (Value);
                        exception
                           when others =>
                              Error_Message
                                (Filename, "wrong value for " & Key);
                        end;

                     elsif Key = "Cleaner_Wait_For_Client_Timeout" then
                        begin
                           Cleaner_Wait_For_Client_Timeout_Value
                             := Duration'Value (Value);
                        exception
                           when others =>
                              Error_Message
                                (Filename, "wrong value for " & Key);
                        end;

                     elsif Key = "Cleaner_Wait_For_Client_Timeout" then
                        begin
                           Cleaner_Wait_For_Client_Timeout_Value
                             := Duration'Value (Value);
                        exception
                           when others =>
                              Error_Message
                                (Filename, "wrong value for " & Key);
                        end;

                     elsif Key = "Cleaner_Client_Header_Timeout" then
                        begin
                           Cleaner_Client_Header_Timeout_Value
                             := Duration'Value (Value);
                        exception
                           when others =>
                              Error_Message
                                (Filename, "wrong value for " & Key);
                        end;

                     elsif Key = "Cleaner_Client_Data_Timeout" then
                        begin
                           Cleaner_Client_Data_Timeout_Value
                             := Duration'Value (Value);
                        exception
                           when others =>
                              Error_Message
                                (Filename, "wrong value for " & Key);
                        end;

                     elsif Key = "Cleaner_Server_Response_Timeout" then
                        begin
                           Cleaner_Server_Response_Timeout_Value
                             := Duration'Value (Value);
                        exception
                           when others =>
                              Error_Message
                                (Filename, "wrong value for " & Key);
                        end;

                     elsif Key = "Force_Wait_For_Client_Timeout" then
                        begin
                           Force_Wait_For_Client_Timeout_Value
                             := Duration'Value (Value);
                        exception
                           when others =>
                              Error_Message
                                (Filename, "wrong value for " & Key);
                        end;

                     elsif Key = "Force_Client_Header_Timeout" then
                        begin
                           Force_Client_Header_Timeout_Value
                             := Duration'Value (Value);
                        exception
                           when others =>
                              Error_Message
                                (Filename, "wrong value for " & Key);
                        end;

                     elsif Key = "Force_Client_Data_Timeout" then
                        begin
                           Force_Client_Data_Timeout_Value
                             := Duration'Value (Value);
                        exception
                           when others =>
                              Error_Message
                                (Filename, "wrong value for " & Key);
                        end;

                     elsif Key = "Force_Server_Response_Timeout" then
                        begin
                           Force_Server_Response_Timeout_Value
                             := Duration'Value (Value);
                        exception
                           when others =>
                              Error_Message
                                (Filename, "wrong value for " & Key);
                        end;

                     elsif Key = "Send_Timeout" then
                        begin
                           Send_Timeout_Value
                             := Duration'Value (Value);
                        exception
                           when others =>
                              Error_Message
                                (Filename, "wrong value for " & Key);
                        end;

                     elsif Key = "Receive_Timeout" then
                        begin
                           Receive_Timeout_Value
                             := Duration'Value (Value);
                        exception
                           when others =>
                              Error_Message
                                (Filename, "wrong value for " & Key);
                        end;

                     else
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
      Process_Ini ("awi.ini");
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
