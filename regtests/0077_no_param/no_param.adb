------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2008, AdaCore                     --
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Parameters;
with AWS.Parameters.Set;

procedure No_Param is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use AWS;

   pragma Warnings (Off);
   Empty : Parameters.List;
   pragma Warnings (On);

begin
   Put_Line ("Count      " & Natural'Image (Parameters.Count (Empty)));
   Put_Line ("Count-2    " & Natural'Image (Parameters.Count (Empty, "a")));
   Put_Line ("Name_Count " & Natural'Image (Parameters.Name_Count (Empty)));
   Put_Line ("Get        " & Parameters.Get (Empty, "toto"));
   Put_Line ("Get_Name   " & Parameters.Get_Name (Empty, 1));
   Put_Line ("Get_Value  " & Parameters.Get_Value (Empty, 1));

   declare
      N : constant Parameters.Vstring_Array := Parameters.Get_Names (Empty);
   begin
      for K in N'Range loop
         Put_Line (" N " & Natural'Image (K) & " = " & To_String (N (K)));
      end loop;
   end;

   declare
      V : constant Parameters.Vstring_Array
        := Parameters.Get_Values (Empty, "titi");
   begin
      for K in V'Range loop
         Put_Line (" V " & Natural'Image (K) & " = " & To_String (V (K)));
      end loop;
   end;
end No_Param;
