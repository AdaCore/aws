------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2002                          --
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

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;
with AWS.Headers.Values;
with Ada.Strings.Unbounded;

procedure HVal is

   use AWS.Headers.Values;
   use Ada.Strings.Unbounded;

   procedure Test (Data : in String);

   Test_1 : constant String := " Token_1 name_1=value_1,";
   Test_2 : constant String := Test_1 & "name_2=""value_2"";" & ASCII.LF;
   Test_3 : constant String := Test_2 & "name_3=""value_3""," & ASCII.HT;
   Test_4 : constant String := Test_3 & "name_4=""value_4""; " & ASCII.CR;
   Test_5 : constant String := Test_4 & "Token_5; ";
   Test_6 : constant String := Test_5 & "Token_6; ";
   Test_7 : constant String := Test_6 & "name_7=value_7;";

   ----------
   -- Test --
   ----------

   procedure Test (Data : in String) is

      S  : constant Data_Set := Split (Data);

      Dummy : Boolean := False;

      procedure Named_Value (Name, Value : in String; Quit : in out Boolean);

      procedure Value (Item : in String; Quit : in out Boolean);

      procedure Named_Value (Name, Value : in String; Quit : in out Boolean) is
      begin
         Put_Line (Name & "=""" & Value & '"');
      end Named_Value;

      procedure Value (Item : in String; Quit : in out Boolean) is
      begin
         Put_Line (Item);
      end Value;

      procedure Test_Parse is new Parse (Value, Named_Value);

   begin
      Put_Line ("===============");

      for I in S'Range loop
         if S (I).Named_Value then
            Named_Value
               (To_String (S (I).Name),
                To_String (S (I).Value),
                Dummy);
         else
            Value (To_String (S (I).Value), Dummy);
         end if;
      end loop;

      Put_Line ("----------");

      --  call the instantiated routine.
      Test_Parse (Data);

      New_Line;

   end Test;

begin
   Test (Test_1);
   Test (Test_2);
   Test (Test_3);
   Test (Test_4);
   Test (Test_5);
   Test (Test_6);
   Test (Test_7);
   Test (Test_1 & Test_2 & Test_3 & Test_4 & Test_5 & Test_6 & Test_7);
   Put_Line (Search (Test_7, "name_1"));
   Put_Line (Search (Test_7, "name_2"));
   Put_Line (Search (Test_7, "name_3"));
   Put_Line (Search (Test_7, "NAME_4", False));
   Put_Line (Search (Test_7, "NAME_4")); -- should be empty line.
   Put_Line (Get_Unnamed_Value (Test_7, 1));
   Put_Line (Get_Unnamed_Value (Test_7, 2));
   Put_Line (Get_Unnamed_Value (Test_7, 3));
   Put_Line (Get_Unnamed_Value (Test_7, 4)); -- should be empty line
   Put_Line ("Done.");
end HVal;
