------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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

with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;

with AWS.Config.Utils;
with AWS.Utils;

package body AWS.Config.Ini is

   use Ada;

   ----------------------
   -- Program_Ini_File --
   ----------------------

   function Program_Ini_File (Full_Path : Boolean) return String is
      Exec_Name : constant String := Command_Line.Command_Name;
      Last      : Natural;
      First     : Natural;
   begin
      First := Strings.Fixed.Index
        (Exec_Name, Strings.Maps.To_Set ("/\"), Going => Strings.Backward);

      if First = 0 then
         First := Exec_Name'First;
      else
         First := First + 1;
      end if;

      Last := Strings.Fixed.Index
        (Exec_Name (First .. Exec_Name'Last), ".", Strings.Backward);

      if Last = 0 then
         Last := Exec_Name'Last;
      else
         Last := Last - 1;
      end if;

      if Full_Path then
         return AWS.Utils.Get_Program_Directory
           & Exec_Name (First .. Last) & ".ini";
      else
         return Exec_Name (First .. Last) & ".ini";
      end if;
   end Program_Ini_File;

   ----------
   -- Read --
   ----------

   procedure Read
     (Config   : in out Object;
      Filename : String)
   is
      procedure Raise_Error (Message : String);
      --  Raise error message with filename and line number

      function Error_Context return String;
      --  Return the string with filename and line number

      procedure Set_Value (Key : String; Value : String);

      Line : Natural;
      --  Current line number parsed

      -------------------
      -- Error_Message --
      -------------------

      function Error_Context return String is
      begin
         return '(' & Filename & ':' & AWS.Utils.Image (Line) & ") ";
      end Error_Context;

      -----------------
      -- Raise_Error --
      -----------------

      procedure Raise_Error (Message : String) is
      begin
         raise Constraint_Error with Error_Context & Message;
      end Raise_Error;

      ---------------
      -- Set_Value --
      ---------------

      procedure Set_Value (Key : String; Value : String) is
         P : constant Parameter_Name := Utils.Value (Key, Error_Context);
      begin
         if P in Server_Parameter_Name then
            Utils.Set_Parameter (Config.P, P, Value, Error_Context);
         else
            Utils.Set_Parameter (Process_Options, P, Value, Error_Context);
         end if;
      end Set_Value;

      Separators : constant Strings.Maps.Character_Set :=
                     Strings.Maps.To_Set (' ' & ASCII.HT);

      File    : Text_IO.File_Type;
      Buffer  : String (1 .. 1024);
      Last    : Natural;

      K_First : Natural;
      K_Last  : Natural;

   begin
      Text_IO.Open (Name => Filename,
                    File => File,
                    Mode => Text_IO.In_File,
                    Form => "shared=no");
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
                  Value : constant String :=
                            Strings.Fixed.Trim
                              (Buffer (K_Last + 1 .. Last),
                               Separators, Separators);
               begin
                  if Value = "" then
                     Raise_Error ("No value for " & Key);
                  else
                     Set_Value (Key, Value);
                  end if;
               end;

            else
               Raise_Error ("wrong format");
            end if;

         end if;
      end loop;

      Text_IO.Close (File);

   exception
      when others =>
         if Text_IO.Is_Open (File) then
            Text_IO.Close (File);
         end if;

         raise;
   end Read;

end AWS.Config.Ini;
