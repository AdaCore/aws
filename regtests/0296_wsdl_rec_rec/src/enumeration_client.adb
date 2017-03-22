------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Text_IO;

with Enumeration_Test_Service.Client;
with Enumeration_Types;

procedure Enumeration_Client is

   use Ada.Text_IO;
   use Enumeration_Test_Service.Client;

   use all type Enumeration_Types.Enumeration_Type;

begin

   begin
      Put_Line ("Test 0...");
      Test_0 (Item => True);
      Put_Line ("Success.");
   end;

   begin
      Put_Line ("Test 1...");
      Test_1 ((Sub_Rec => (Value => Second), Value => First));
      Put_Line ("Success.");
   end;

   begin
      Put_Line ("Test 2...");
      Test_2 (Second);
      Put_Line ("Success.");
   end;

   begin
      Put_Line ("Test 3...");
      Test_3 (Value_Param  => First,
              Value_Record => (Sub_Rec => (Value => Third), Value => Second));
      Put_Line ("Success.");
   end;

   declare
      Result : Enumeration_Types.Enumeration_Type;
   begin
      Put_Line ("Test 4...");
      Result := Test_4 (Item => (Sub_Rec => (Value => First), Value => Third));
      Put_Line
        ("Got " & Enumeration_Types.Enumeration_Type'Image (result)
         & " back.");
   end;

   declare
      Result : Enumeration_Types.Enumeration_Record_Type;
   begin
      Put_Line ("Test 5...");
      Result := Test_5 (Item => First);
      Put_Line
        ("Got "
         & Enumeration_Types.Enumeration_Type'Image (Result.Value)
         & " back.");
   end;

   begin
      Put_Line ("Test 6...");
      Test_6 ('A');
      Put_Line ("Success.");
   end;

   declare
      Result : Character;
   begin
      Put_Line ("Test 7...");
      Result := Test_7 ('B');
      Put_Line ("Got '" & Result & "' back.");
   end;
end Enumeration_Client;
