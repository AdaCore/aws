------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2004-2008, AdaCore                    --
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

--  Simple table test, see ctab for a more complete one

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Containers.Tables.Set;

procedure Ctable is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   use AWS.Containers;
   use AWS.Containers.Tables;

   T : Table_Type;

begin
   Set.Reset (T);
   Set.Add (T, "one", "un");
   Set.Add (T, "two", "deux");

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

   Set.Add (T, "one", "1 1 1");

   declare
      V : VString_Array := Get_Values (T, "one");
   begin
      for K in V'Range loop
         Put_Line (Positive'Image (K) & " - " & To_String (V (K)));
      end loop;
   end;
end Ctable;
