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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Parameters;

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
