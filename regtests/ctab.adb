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

with AWS.Containers.Tables.Set;

with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure CTab is
   use AWS.Containers.Tables;
   use Ada.Text_IO;

   Table : Table_Type;

   procedure Print_Table;

   procedure Print_Table
   is
      Names : VString_Array := Get_Names (Table, Sort => True);
   begin
      Put_Line ("----------------------------");
      for K in 1 .. Name_Count (Table) loop
         declare
            Name : String := Ada.Strings.Unbounded.To_String (Names (K));
            Last : Natural := AWS.Containers.Tables.Count (Table, Name);
         begin
            Put (Name & ": ");
            for L in 1 .. Last loop
               Put (Get (Table, Name, L));

               if L = Last then
                  Put_Line (";");
               else
                  Put (", ");
               end if;
            end loop;
         end;
      end loop;
   end Print_Table;

begin
   Set.Reset (Table);
   Set.Case_Sensitive (Table, False);
   Set.Add (Table, "name_1", "value_1_1");
   Set.Add (Table, "Name_1", "value_1_2");
   Set.Add (Table, "NAME_1", "value_1_3");
   Set.Add (Table, "name_2", "value_2_1");
   Set.Add (Table, "Name_2", "value_2_2");
   Set.Add (Table, "NAME_2", "value_2_3");
   Set.Add (Table, "name_3", "value_3_1");
   Set.Add (Table, "Name_3", "value_3_2");
   Set.Add (Table, "NAME_3", "value_3_3");
   Set.Add (Table, "name_4", "value_4_1");
   Set.Add (Table, "Name_4", "value_4_2");
   Set.Add (Table, "NAME_4", "value_4_3");
   Print_Table;
   Set.Update (Table, "NAme_1", "Value (1, 4)", 4);
   Set.Update (Table, "NAME_1", "Value (1, 3)", 3);
   Set.Update (Table, "Name_1", "Value (1, 2)", 2);
   Set.Update (Table, "name_1", "Value (1, 1)", 1);

   Set.Update (Table, "NAme_2", "Value (2, 4)", 4);
   Set.Update (Table, "NAME_2", "Value (2, 3)", 3);
   Set.Update (Table, "Name_2", "Value (2, 2)", 2);
   Set.Update (Table, "name_2", "Value (2, 1)", 1);

   Set.Update (Table, "NAme_3", "Value (3, 4)", 4);
   Set.Update (Table, "NAME_3", "Value (3, 3)", 3);
   Set.Update (Table, "Name_3", "Value (3, 2)", 2);
   Set.Update (Table, "name_3", "Value (3, 1)", 1);

   Set.Update (Table, "name_4", "Value (4, 1)", 1);
   Set.Update (Table, "Name_4", "Value (4, 2)", 2);
   Set.Update (Table, "NAME_4", "Value (4, 3)", 3);
   Set.Update (Table, "NAme_4", "Value (4, 4)", 4);

   Set.Update (Table, "name_5", "Value (5, 1)", 1);
   Set.Update (Table, "Name_5", "Value (5, 2)", 2);
   Set.Update (Table, "NAME_5", "Value (5, 3)", 3);
   Set.Update (Table, "NAme_5", "Value (5, 4)", 4);
   Print_Table;
   begin
      Set.Update (Table, "NAme_5", "Value", 6);
      Put_Line ("Error.");
   exception
      when Constraint_Error => null;
   end;
   begin
      Set.Update (Table, "NAme_6", "Value", 2);
      Put_Line ("Error.");
   exception
      when Constraint_Error =>
         Put_Line ("Ok.");
   end;
end CTab;