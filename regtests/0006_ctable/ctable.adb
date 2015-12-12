------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2015, AdaCore                     --
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

--  Simple table test, see ctab for a more complete one

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Containers.Tables;

procedure Ctable is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   use AWS.Containers;
   use AWS.Containers.Tables;

   T : Table_Type;

begin
   T.Reset;
   T.Add ("one", "un");
   T.Add ("two", "deux");

   Put_Line ("All keys:");

   for K in 1 .. Tables.Count (T) loop
      Put_Line (Integer'Image (K) & "] "
                  & Get_Name (T, K) & " = " & Get_Value (T, k));
   end loop;

   Put_Line ("Exist:");

   Put_Line ("one " & Boolean'Image (Exist (T, "one")));
   Put_Line ("xne " & Boolean'Image (Exist (T, "xne")));

   Put_Line ("All values 1:");

   declare
      V : VString_Array := Get_Values (T, "one");
   begin
      for K in V'Range loop
         Put_Line (Positive'Image (K) & " - " & To_String (V (K)));
      end loop;
   end;

   Put_Line ("All values 2:");

   T.Add ("one", "1 1 1");

   declare
      V : VString_Array := Get_Values (T, "one");
   begin
      for K in V'Range loop
         Put_Line (Positive'Image (K) & " - " & To_String (V (K)));
      end loop;
   end;
end Ctable;
