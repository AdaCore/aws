------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
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

with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Utils;

package body AWS.Config.Ini is

   use Ada;
   use Ada.Strings.Unbounded;

   function Program_Ini_File return String;
   --  Returns initialization filename for current server (using the
   --  executable name and adding .ini)

   ----------------------
   -- Program_Ini_File --
   ----------------------

   function Program_Ini_File return String is
      Exec_Name : constant String := Ada.Command_Line.Command_Name;
      Last      : Natural;
      First     : Natural;
   begin
      First := Strings.Fixed.Index
        (Exec_Name, Strings.Maps.To_Set ("/\"), Going => Strings.Backward);

      if First = 0 then
         First := Exec_Name'First;
      end if;

      Last := Strings.Fixed.Index
        (Exec_Name (First .. Exec_Name'Last), ".", Strings.Backward);

      if Last = 0 then
         return Exec_Name & ".ini";
      else
         return Exec_Name (Exec_Name'First .. Last) & "ini";
      end if;
   end Program_Ini_File;

   ----------
   -- Read --
   ----------

   procedure Read
     (Config   : in out Object;
      Filename : in     String := "")
   is

      procedure Error_Message (Filename : in String; Message : in String);
      --  Output error message with filename and line number.

      procedure Set_Value
        (Filename : in String;
         Key      : in String;
         Value    : in String);

      Line : Natural;
      --  current line number parsed

      Process_Mode : Boolean := True;
      --  Set to True when parsing a file that can support per process
      --  options.

      -------------------
      -- Error_Message --
      -------------------

      procedure Error_Message (Filename : in String; Message : in String) is
      begin
         Text_IO.Put (Text_IO.Current_Error, '(' & Filename & ':');
         Text_IO.Put (Text_IO.Current_Error, AWS.Utils.Image (Line));
         Text_IO.Put_Line (Text_IO.Current_Error, ") " & Message & '.');
      end Error_Message;

      ---------------
      -- Set_Value --
      ---------------

      procedure Set_Value
        (Filename : in String;
         Key      : in String;
         Value    : in String)
      is
         use Ada.Strings.Unbounded;

         function "+" (S : in String)
           return Unbounded_String
           renames To_Unbounded_String;

         Expected_Type : Unbounded_String;

         P : Parameter_Name;

      begin

         begin
            P := Parameter_Name'Value (Key);
         exception
            when others =>
               Error_Message (Filename, "unrecognized option " & Key);
               return;
         end;

         if P in Server_Parameter_Name then

            case Config.P (P).Kind is
               when Str =>
                  Expected_Type := +"string";
                  Config.P (P).Str_Value := +Value;

               when Dir =>
                  Expected_Type := +"string";

                  if Value (Value'Last) = '/'
                    or else Value (Value'Last) = '\'
                  then
                     Config.P (P).Dir_Value := +Value;
                  else
                     Config.P (P).Dir_Value := +(Value & '/');
                  end if;

               when Pos =>
                  Expected_Type := +"positive";
                  Config.P (P).Pos_Value := Positive'Value (Value);

               when Dur =>
                  Expected_Type := +"duration";
                  Config.P (P).Dur_Value := Duration'Value (Value);

               when Bool =>
                  Expected_Type := +"boolean";
                  Config.P (P).Bool_Value := Boolean'Value (Value);

            end case;

         else

            if not Process_Mode then
               Error_Message
                 (Filename,
                  "Per process option (" & Key
                  & ") not supported for this file");
            end if;

            case Process_Options (P).Kind is

               when Str =>
                  Expected_Type := +"string";
                  Process_Options (P).Str_Value := +Value;

               when Dir =>
                  Expected_Type := +"string";

                  if Value (Value'Last) = '/'
                    or else Value (Value'Last) = '\'
                  then
                     Process_Options (P).Dir_Value := +Value;
                  else
                     Process_Options (P).Dir_Value := +(Value & '/');
                  end if;

               when Pos =>
                  Expected_Type := +"positive";
                  Process_Options (P).Pos_Value := Positive'Value (Value);

               when Dur =>
                  Expected_Type := +"duration";
                  Process_Options (P).Dur_Value := Duration'Value (Value);

               when Bool =>
                  Expected_Type := +"boolean";
                  Process_Options (P).Bool_Value := Boolean'Value (Value);
            end case;

         end if;

      exception
         when others =>
            Error_Message
              (Filename,
               "wrong value for " & Key
               & " " & To_String (Expected_Type) & " expected");

      end Set_Value;

      Separators : constant Strings.Maps.Character_Set
        := Strings.Maps.To_Set (' ' & ASCII.HT);

      File    : Text_IO.File_Type;
      Buffer  : String (1 .. 1024);
      Last    : Natural;

      K_First : Natural;
      K_Last  : Natural;

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

            if K_Last /= 0 then

               declare
                  Key   : constant String := Buffer (K_First .. K_Last);
                  Value : constant String := Strings.Fixed.Trim
                    (Buffer (K_Last + 1 .. Last), Separators, Separators);
               begin
                  if Value = "" then
                     Error_Message (Filename, "No value for " & Key);
                  else
                     Set_Value (Filename, Key, Value);
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
   end Read;

begin
   Read (Server_Config, "aws.ini");
   Read (Server_Config, Program_Ini_File);
end AWS.Config.Ini;
